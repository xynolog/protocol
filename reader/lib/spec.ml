(* The pinned Protocol's specs/src, every file of it accounted for and every
   JSON file read into the model. A file is one of:
   - the two OpenAPI documents (specs/README.md: "the gateway and Archivist
     OpenAPI documents");
   - a path item, which a document's "paths" names, its file named by its
     operationId;
   - a response, which a document's components or an operation names, its
     file its name in PascalCase, as a schema's is;
   - a table, each of which the model names here and types in Tables;
   - a fixture, which smelter/extraction-cases.json names;
   - a README.md;
   - a JSON Schema: every other JSON file.
   Any other file is refused, as is a file the model names that Protocol does
   not have, and every "$ref" must name a file of the kind its place calls
   for. *)

let ( let+ ) result f = Result.map f result

let ( and+ ) a b =
  match (a, b) with
  | Ok a, Ok b -> Ok (a, b)
  | Error e, Ok _ | Ok _, Error e -> Error e
  | Error a, Error b -> Error (a @ b)

let ( let* ) = Result.bind

let all results =
  List.fold_right
    (fun result rest ->
      let+ value = result and+ values = rest in
      value :: values)
    results (Ok [])

type t = {
  gateway : Openapi.document;
  archivist : Openapi.document;
  path_items : (string * Openapi.path_item) list;
  responses : (string * Openapi.response) list;
  schemas : (string * Schema.t) list;
  services : string list;
      (** The services' schemas ([Places.service_schemas]), as paths from
          specs/src. *)
  tables : Tables.t;
  table_files : string list;
  ids : Decode.ids;  (** Each schema's [$id], and its file. *)
}

type kind = Document | Path_item | Response | Table | Schema_file

let kind_name = function
  | Document -> "an OpenAPI document"
  | Path_item -> "a path item"
  | Response -> "a response"
  | Table -> "a table"
  | Schema_file -> "a JSON Schema"

let refusal file pointer message = { Decode.file; pointer; message }

let refused_at (reference : Decode.reference) message =
  refusal reference.file reference.pointer message

let rec walk root relative =
  let path = Filename.concat root relative in
  if Sys.is_directory path then
    Sys.readdir path |> Array.to_list |> List.sort compare
    |> List.concat_map (fun name ->
        walk root (if relative = "" then name else relative ^ "/" ^ name))
  else [ relative ]

let operations (item : Openapi.path_item) =
  List.filter_map Fun.id [ item.get; item.put; item.post ]

let kebab name =
  let spelled = Buffer.create (String.length name + 8) in
  String.iteri
    (fun i c ->
      if i > 0 && c >= 'A' && c <= 'Z' then Buffer.add_char spelled '-';
      Buffer.add_char spelled (Char.lowercase_ascii c))
    name;
  Buffer.contents spelled

(* D5: a path item's file is named by its operation's operationId, in kebab
   case; one of several operations by the name their operationIds share after
   each one's verb, as getResourceAnchoredText and putResourceAnchoredText
   share resource-anchored-text. *)
let path_item_name item =
  let without_verb name =
    match String.index_opt name '-' with
    | Some i -> Some (String.sub name (i + 1) (String.length name - i - 1))
    | None -> None
  in
  match
    List.map
      (fun (operation : Openapi.operation) -> kebab operation.operation_id)
      (operations item)
  with
  | [ name ] -> Some name
  | names -> (
      match List.sort_uniq compare (List.map without_verb names) with
      | [ Some shared ] -> Some shared
      | _ -> None)

let operation_responses item =
  List.concat_map
    (fun (operation : Openapi.operation) ->
      List.filter_map
        (function
          | _, Openapi.Ref reference -> Some reference
          | _, Openapi.Inline _ -> None)
        operation.responses)
    (operations item)

let component_responses (document : Openapi.document) =
  match document.components.responses with
  | None -> []
  | Some responses -> List.map snd responses

(* The files the references name that Protocol has; a reference naming one it
   does not have is refused where it is written. *)
let named_files ids files references =
  List.sort_uniq compare
    (List.filter_map (fun r -> Option.map fst (Decode.names ids r)) references)
  |> List.filter (fun file -> List.mem file files)

let same (a : Decode.reference) (b : Decode.reference) =
  a.file = b.file && a.pointer = b.pointer

(* Where a file states [$id] or [$schema]: each place but a schema file's
   root, which states both, its [$id] its name under [Decode.base]. Another
   file's decoder refuses either at its root, as a member it does not know. *)
let identity kind file (json : Yojson.Safe.t) =
  let rec walk pointer (json : Yojson.Safe.t) =
    match json with
    | `Assoc members ->
        List.concat_map
          (fun (key, value) ->
            (if (key = "$id" || key = "$schema") && pointer <> "" then
               [
                 refusal file pointer
                   (Printf.sprintf
                      "states %s, which only a JSON Schema's file states, at \
                       its root"
                      key);
               ]
             else [])
            @ walk (pointer ^ "/" ^ Decode.escape key) value)
          members
    | `List items ->
        List.concat
          (List.mapi
             (fun i item -> walk (pointer ^ "/" ^ string_of_int i) item)
             items)
    | _ -> []
  in
  let root =
    match (kind, json) with
    | Schema_file, `Assoc members -> (
        let id = Decode.base ^ Filename.basename file in
        (match List.assoc_opt "$schema" members with
          | Some _ -> []
          | None ->
              [
                refusal file ""
                  ("states no $schema: a JSON Schema states " ^ Decode.dialect);
              ])
        @
        match List.assoc_opt "$id" members with
        | Some (`String stated) when stated = id -> []
        | Some stated ->
            [
              refusal file "/$id"
                (Printf.sprintf
                   "is %s, and a JSON Schema's $id is its name under %s: %s"
                   (Yojson.Safe.to_string stated)
                   Decode.base id);
            ]
        | None -> [ refusal file "" ("states no $id: its $id is " ^ id) ])
    | _ -> []
  in
  root @ walk "" json

(* Where a discriminator's mapping, in [file], names what is not a JSON
   Schema's [$id]. *)
let mappings ids file (json : Yojson.Safe.t) =
  let rec walk pointer (json : Yojson.Safe.t) =
    match json with
    | `Assoc members ->
        (match List.assoc_opt "discriminator" members with
          | Some (`Assoc discriminator)
            when List.mem_assoc "propertyName" discriminator -> (
              match List.assoc_opt "mapping" discriminator with
              | Some (`Assoc mapping) ->
                  List.filter_map
                    (fun (value, target) ->
                      let at =
                        pointer ^ "/discriminator/mapping/"
                        ^ Decode.escape value
                      in
                      match target with
                      | `String target
                        when String.starts_with ~prefix:Decode.base target
                             && Option.is_some
                                  (Decode.names ids
                                     { Decode.target; file; pointer = at }) ->
                          None
                      | target ->
                          Some
                            (refusal file at
                               (Printf.sprintf
                                  "maps %s to %s, which is no JSON Schema's $id"
                                  value
                                  (Yojson.Safe.to_string target))))
                    mapping
              | _ -> [])
          | _ -> [])
        @ List.concat_map
            (fun (key, value) -> walk (pointer ^ "/" ^ Decode.escape key) value)
            members
    | `List items ->
        List.concat
          (List.mapi
             (fun i item -> walk (pointer ^ "/" ^ string_of_int i) item)
             items)
    | _ -> []
  in
  walk "" json

let load root =
  let* ids = Places.ids root in
  let files = walk root "" in
  let decoded = Hashtbl.create 1024 in
  let references = ref [] in
  let decode kind file decoder =
    if not (List.mem file files) then
      Error
        [ refusal file "" "the model reads this file, and Protocol has none" ]
    else
      match Decode.parse_file ~root file with
      | Error error -> Error [ error ]
      | Ok json -> (
          match Decode.run ~file json decoder with
          | Error error -> Error [ error ]
          | Ok (value, found) ->
              Hashtbl.replace decoded file (kind, json);
              references := found @ !references;
              Ok value)
  in
  let named kind decoder file =
    let+ value = decode kind file decoder in
    (file, value)
  in
  (* The directory smelter/extraction-cases.json's "fixtures" names, from the
     repository's root, within specs/src; and the fixtures its cases name. *)
  let fixtures_of (extraction : Tables.Smelter_extraction_cases.t) =
    let prefix = "specs/src/" in
    let directory = extraction.fixtures in
    if not (String.starts_with ~prefix directory) then
      Error
        [
          refusal "smelter/extraction-cases.json" "/fixtures"
            "names a directory outside specs/src";
        ]
    else
      let within =
        String.sub directory (String.length prefix)
          (String.length directory - String.length prefix)
      in
      Ok
        ( within,
          List.filter_map
            (fun (case : Tables.Smelter_extraction_cases.cases_item) ->
              match case with
              | Read { fixture; _ } | File_declined { fixture; _ } ->
                  Some (within ^ "/" ^ fixture)
              | Bytes_declined _ -> None)
            extraction.cases )
  in
  let* gateway, archivist, tables =
    let+ gateway = decode Document "openapi.json" Openapi.document
    and+ archivist = decode Document "archivist/openapi.json" Openapi.document
    and+ tables =
      Tables.load { table = (fun file decoder -> decode Table file decoder) }
    in
    (gateway, archivist, tables)
  in
  let documents = [ gateway; archivist ] in
  let* path_items =
    documents
    |> List.concat_map (fun (document : Openapi.document) ->
        List.map snd document.paths)
    |> named_files ids files
    |> List.map (named Path_item Openapi.path_item)
    |> all
  in
  let response_references =
    List.concat_map component_responses documents
    @ List.concat_map (fun (_, item) -> operation_responses item) path_items
  in
  let* responses =
    named_files ids files response_references
    |> List.map (named Response Openapi.response)
    |> all
  in
  let* fixture_directory, fixtures =
    fixtures_of tables.smelter_extraction_cases
  in
  let is_fixture file =
    String.starts_with ~prefix:(fixture_directory ^ "/") file
  in
  let is_readme file = Filename.basename file = "README.md" in
  let schemas =
    files
    |> List.filter (fun file ->
        Filename.check_suffix file ".json"
        && (not (Hashtbl.mem decoded file))
        && not (is_fixture file))
    |> List.map (named Schema_file Schema.decode)
    |> all
  in
  (* D5: every directory is named in kebab case. *)
  let directory_names =
    let rec parents file =
      match Filename.dirname file with
      | "." -> []
      | parent -> parent :: parents parent
    in
    List.sort_uniq compare (List.concat_map parents files)
    |> List.filter_map (fun directory ->
        let name = Filename.basename directory in
        if Places.is_kebab name then None
        else
          Some
            (refusal directory ""
               (Printf.sprintf
                  "is a directory named %s, and a directory's name is in kebab \
                   case"
                  name)))
  in
  let census =
    directory_names
    @ List.filter_map
        (fun file ->
          if Hashtbl.mem decoded file || is_readme file || is_fixture file then
            None
          else Some (refusal file "" "a file the model does not know"))
        files
    @ List.filter_map
        (fun fixture ->
          if List.mem fixture files then None
          else
            Some
              (refusal "smelter/extraction-cases.json" "/cases"
                 (Printf.sprintf
                    "names the fixture %s, which Protocol does not have" fixture)))
        fixtures
  in
  (* A schema is named by its [$id], and by nothing else; a path item or a
     response by its path. *)
  let by_path (reference : Decode.reference) =
    not
      (String.starts_with ~prefix:Decode.base reference.target
      || String.starts_with ~prefix:"#" reference.target)
  in
  let unnamed (reference : Decode.reference) =
    refused_at reference
      (Printf.sprintf "names %s, an $id no JSON Schema states" reference.target)
  in
  let by_its_id (reference : Decode.reference) target =
    refused_at reference
      (Printf.sprintf
         "names the JSON Schema %s by its path: a JSON Schema is named by its \
          $id, %s"
         reference.target
         (Decode.base ^ Filename.basename target))
  in
  let whole kind (reference : Decode.reference) =
    match Decode.names ids reference with
    | None -> Some (unnamed reference)
    | Some (target, fragment) -> (
        match Hashtbl.find_opt decoded target with
        | Some (Schema_file, _) when by_path reference ->
            Some (by_its_id reference target)
        | Some (found, _) when found = kind && fragment = "" -> None
        | Some (found, _) ->
            Some
              (refused_at reference
                 (Printf.sprintf "names %s, %s where %s belongs"
                    reference.target (kind_name found) (kind_name kind)))
        | None ->
            Some
              (refused_at reference
                 (Printf.sprintf "names %s, which Protocol does not have"
                    reference.target)))
  in
  let schema (reference : Decode.reference) =
    match Decode.names ids reference with
    | None -> Some (unnamed reference)
    | Some (target, fragment) -> (
        match Hashtbl.find_opt decoded target with
        | Some (Schema_file, _) when by_path reference ->
            Some (by_its_id reference target)
        | Some (Schema_file, _) when fragment = "" -> None
        | Some ((Schema_file as found), json) -> (
            match Decode.at_pointer fragment json with
            | None ->
                Some
                  (refused_at reference
                     (Printf.sprintf "names %s, which is nothing in %s"
                        reference.target (kind_name found)))
            | Some node -> (
                match
                  Decode.run ~file:(target ^ "#" ^ fragment) node Schema.decode
                with
                | Ok _ -> None
                | Error error ->
                    Some
                      (refused_at reference
                         (Printf.sprintf
                            "names %s, which is not a JSON Schema: %s"
                            reference.target
                            (Decode.error_to_string error)))))
        | Some (found, _) ->
            Some
              (refused_at reference
                 (Printf.sprintf "names %s, %s where a JSON Schema belongs"
                    reference.target (kind_name found)))
        | None ->
            Some
              (refused_at reference
                 (Printf.sprintf "names %s, which Protocol does not have"
                    reference.target)))
  in
  let path_references =
    List.concat_map
      (fun (document : Openapi.document) -> List.map snd document.paths)
      documents
  in
  let schema_file_references =
    List.concat_map
      (fun (document : Openapi.document) ->
        List.map snd document.components.schemas)
      documents
  in
  let placed = path_references @ response_references @ schema_file_references in
  let unresolved =
    List.filter_map (whole Path_item) path_references
    @ List.filter_map (whole Response) response_references
    @ List.filter_map (whole Schema_file) schema_file_references
    @ List.filter_map schema
        (List.filter
           (fun reference -> not (List.exists (same reference) placed))
           (List.rev !references))
  in
  let stated =
    Hashtbl.fold
      (fun file (kind, json) found ->
        identity kind file json @ mappings ids file json @ found)
      decoded []
    |> List.sort compare
  in
  let tables_here = Places.tables root in
  let stray_case_tables =
    List.filter_map
      (fun file ->
        if Places.is_case_table file && not (List.mem file tables_here) then
          Some
            (refusal file ""
               "is a case table, and a table is at the top of an area")
        else None)
      files
  in
  (* What follows resolves each reference through [ids]. *)
  let* () =
    match (schemas, unresolved @ stated) with
    | Error errors, _ -> Error (errors @ census @ stray_case_tables)
    | Ok _, [] -> Ok ()
    | Ok _, errors -> Error (census @ stray_case_tables @ errors)
  in
  (* D1's generic check: every table has its schema, every schema that is
     a table's has its table, and each table is what its schema says. *)
  let read_json file =
    match Hashtbl.find_opt decoded file with
    | Some (_, json) -> Some json
    | None when Sys.file_exists (Filename.concat root file) ->
        Result.to_option (Decode.parse_file ~root file)
    | None -> None
  in
  let schema_cache = Hashtbl.create 256 in
  let schema_at ({ file; pointer } : Validate.place) =
    match Hashtbl.find_opt schema_cache (file, pointer) with
    | Some found -> found
    | None ->
        let found =
          Option.bind (read_json file) (fun json ->
              Option.bind (Decode.at_pointer pointer json) (fun node ->
                  Result.to_option
                    (Result.map fst (Decode.run ~file node Schema.decode))))
        in
        Hashtbl.replace schema_cache (file, pointer) found;
        found
  in
  let names_case_table file (node : Yojson.Safe.t option) =
    match node with
    | Some (`Assoc [ ("$ref", `String target) ]) ->
        fst (Decode.resolve ids { target; file; pointer = "" })
        = Places.case_table
    | _ -> false
  in
  let orphan_schemas =
    List.filter_map
      (fun file ->
        match String.split_on_char '/' file with
        | [ area; name ]
          when List.mem area (Places.areas root)
               && Filename.check_suffix name ".json"
               && name.[0] >= 'A'
               && name.[0] <= 'Z'
               && not
                    (List.exists
                       (fun table -> Places.schema_of table = file)
                       tables_here) -> (
            match read_json file with
            | Some json ->
                let top =
                  (match Decode.at_pointer "/allOf" json with
                    | Some (`List parts) -> List.map Option.some parts
                    | _ -> [])
                  @ [ Decode.at_pointer "/properties/$comment" json ]
                in
                if List.exists (names_case_table file) top then
                  Some
                    (refusal file ""
                       "is a table's schema, and no table beside it is named \
                        for it")
                else None
            | None -> None)
        | _ -> None)
      files
  in
  let validation =
    List.concat_map
      (fun table ->
        let schema_file = Places.schema_of table in
        match
          (read_json table, schema_at { file = schema_file; pointer = "" })
        with
        | _, None ->
            [
              refusal table ""
                (Printf.sprintf "has no schema, which would be %s" schema_file);
            ]
        | None, _ -> []
        | Some json, Some schema ->
            List.map
              (fun (pointer, message) -> refusal table pointer message)
              (Validate.errors ids schema_at schema json ""))
      tables_here
  in
  (* D9: the gateway's document is the public API. It lists every component
     schema and discovery's, and nothing else; a component refers to
     components alone, and so does a gateway route; and a component only
     Archivist's document reaches is Archivist's own. *)
  let target reference = fst (Decode.resolve ids reference) in
  let is_schema file =
    match Hashtbl.find_opt decoded file with
    | Some (Schema_file, _) -> true
    | _ -> false
  in
  let written_in = Hashtbl.create 1024 in
  List.iter
    (fun (reference : Decode.reference) ->
      Hashtbl.add written_in reference.file reference)
    !references;
  let refers file = Hashtbl.find_all written_in file in
  let reach roots =
    let seen = Hashtbl.create 256 in
    let rec visit file =
      if not (Hashtbl.mem seen file) then (
        Hashtbl.replace seen file ();
        List.iter (fun reference -> visit (target reference)) (refers file))
    in
    List.iter visit roots;
    seen
  in
  let routes (document : Openapi.document) =
    let items =
      List.map (fun (_, reference) -> target reference) document.paths
    in
    items
    @ List.map target
        (component_responses document
        @ List.concat_map
            (fun file ->
              match List.assoc_opt file path_items with
              | Some item -> operation_responses item
              | None -> [])
            items)
  in
  let gateway_routes = routes gateway in
  let listed =
    List.map (fun (_, reference) -> target reference) gateway.components.schemas
  in
  let unlisted =
    List.filter_map
      (fun file ->
        if
          Places.is_component file
          && Filename.check_suffix file ".json"
          && not (List.mem file listed)
        then
          Some
            (refusal file ""
               "is a component schema, and the gateway's document does not \
                list it")
        else None)
      files
  in
  let listed_elsewhere =
    List.filter_map
      (fun (_, (reference : Decode.reference)) ->
        let file = target reference in
        if
          Places.is_component file
          || String.starts_with ~prefix:Places.discovery file
        then None
        else
          Some
            (refused_at reference
               (Printf.sprintf
                  "lists %s, %s, which is neither a component schema nor \
                   discovery's: the gateway's document lists the public \
                   schemas alone"
                  reference.target file)))
      gateway.components.schemas
  in
  let leaves_components =
    List.filter_map
      (fun (reference : Decode.reference) ->
        if
          Places.is_component reference.file
          && not (Places.is_component (target reference))
        then
          Some
            (refused_at reference
               (Printf.sprintf
                  "names %s, %s, outside components/schemas: a component \
                   schema refers to component schemas alone"
                  reference.target (target reference)))
        else None)
      (List.rev !references)
  in
  let route_leaves =
    List.concat_map
      (fun file ->
        List.filter_map
          (fun (reference : Decode.reference) ->
            let named = target reference in
            if is_schema named && not (Places.is_component named) then
              Some
                (refused_at reference
                   (Printf.sprintf
                      "names %s, %s, outside components/schemas: what a \
                       gateway route uses is a component schema"
                      reference.target named))
            else None)
          (List.rev (refers file)))
      (List.sort_uniq compare gateway_routes)
  in
  let bus =
    List.concat_map
      (fun (channel : Tables.Bus_registry.channels_item) ->
        match channel with
        | Schema { schema; validate; _ } | Envelope { schema; validate; _ } ->
            schema :: Option.to_list validate
        | Stored_event { payload; validate; _ } ->
            payload :: Option.to_list validate
        | Void { validate; _ } | Empty { validate; _ } ->
            Option.to_list validate)
      tables.bus_registry.channels
    |> List.map (fun name -> Places.components ^ name ^ ".json")
  in
  let public = reach (gateway_routes @ bus) in
  let archivists =
    reach
      (routes archivist
      @ List.map
          (fun (_, reference) -> target reference)
          archivist.components.schemas)
  in
  let archivists_alone =
    Hashtbl.fold
      (fun file () found ->
        if Places.is_component file && not (Hashtbl.mem public file) then
          refusal file ""
            "is reached by Archivist's document alone, not by a gateway route \
             or the bus: it is Archivist's own, so its place is archivist/"
          :: found
        else found)
      archivists []
    |> List.sort compare
  in
  let public_api =
    unlisted @ listed_elsewhere @ leaves_components @ route_leaves
    @ archivists_alone
  in
  let client_surface =
    Client_surface.refusals ~ids ~read_json ~schema_at
      ~registry:tables.bus_registry tables.client_surface
  in
  let reply_bounds =
    Reply_bounds.refusals ~ids ~schema_at tables.bus_registry
      tables.bus_reply_bounds
  in
  let offset_units = Offset_units.refusals ~root ~read_json files in
  let registry_rules = Registry_rules.refusals ~read_json tables.bus_registry in
  let jobs_rules = Jobs_rules.refusals ~ids ~read_json in
  let archivist_rules = Archivist_rules.refusals ~ids ~read_json in
  let smelter_rules = Smelter_rules.refusals ~read_json in
  let telemetry_rules = Telemetry_rules.refusals ~read_json in
  let worker_rules = Worker_rules.refusals ~read_json in
  let graph_rules = Graph_rules.refusals ~read_json in
  let identifiers_rules =
    Identifiers_rules.refusals ~ids ~read_json files tables.identifiers_kinds
  in
  let client_types_rules = Client_types_rules.refusals ~ids ~files ~read_json in
  let protocol_rules = Protocol_rules.refusals ~ids ~read_json in
  let control_rules =
    Control_rules.refusals ~ids ~read_json tables.control_methods
  in
  let retry_rules =
    Retry_rules.refusals tables.retry_rules tables.retry_cases
  in
  let shape_rules = Shape_rules.refusals ~ids ~files ~schema_at ~read_json in
  let option_defaults = Option_defaults.refusals ~ids ~read_json ~schema_at in
  let codes_rules =
    Codes_rules.refusals ~read_json
      ~schema_files:
        (List.filter
           (fun file ->
             Filename.check_suffix file ".json"
             && (not (Hashtbl.mem decoded file))
             && not (is_fixture file))
           files)
  in
  let path_item_names =
    List.filter_map
      (fun (file, item) ->
        let stated = Filename.remove_extension (Filename.basename file) in
        match path_item_name item with
        | Some name when name = stated -> None
        | Some name ->
            Some
              (refusal file ""
                 (Printf.sprintf
                    "is named %s, and a path item's file is named by its \
                     operationId, %s"
                    stated name))
        | None ->
            Some
              (refusal file ""
                 "holds operations whose operationIds share no name after \
                  their verbs, and a path item's file is named by the name \
                  they share"))
      path_items
  in
  (* D5: a file that names a component, a schema or a response, is the
     component's name in PascalCase. A document lists each component by that
     name, and no two responses share one. *)
  let stem file = Filename.remove_extension (Filename.basename file) in
  let pascal name =
    name <> ""
    && name.[0] >= 'A'
    && name.[0] <= 'Z'
    && String.for_all
         (fun c ->
           (c >= 'A' && c <= 'Z')
           || (c >= 'a' && c <= 'z')
           || (c >= '0' && c <= '9'))
         name
  in
  let component_files =
    Hashtbl.fold
      (fun file (kind, _) found ->
        match kind with
        | (Schema_file | Response) when not (pascal (stem file)) ->
            refusal file ""
              (Printf.sprintf
                 "is named %s, and %s's file is its name in PascalCase"
                 (stem file) (kind_name kind))
            :: found
        | _ -> found)
      decoded []
    |> List.sort compare
  in
  (* D5: every other file is named in kebab case, but a README.md, D5's one
     exception. *)
  let kebab_files =
    List.filter_map
      (fun file ->
        match Hashtbl.find_opt decoded file with
        | Some ((Schema_file | Response), _) -> None
        | _ when is_readme file || Places.is_kebab (stem file) -> None
        | _ ->
            Some
              (refusal file ""
                 (Printf.sprintf
                    "is named %s, and a file that names no component is named \
                     in kebab case"
                    (stem file))))
      files
  in
  let listed_names =
    List.concat_map
      (fun (document : Openapi.document) ->
        List.filter_map
          (fun (name, reference) ->
            let file = target reference in
            if name = stem file then None
            else
              Some
                (refused_at reference
                   (Printf.sprintf
                      "lists %s as %s, and a document lists a component by its \
                       file's name, %s"
                      file name (stem file))))
          (document.components.schemas
          @
          match document.components.responses with
          | Some listed -> listed
          | None -> []))
      documents
  in
  let shared_responses =
    let files = List.map fst responses in
    List.filter_map
      (fun file ->
        match
          List.filter
            (fun other -> other <> file && stem other = stem file)
            files
        with
        | [] -> None
        | others ->
            Some
              (refusal file ""
                 (Printf.sprintf
                    "is the response %s, as %s is: a response is named by its \
                     file, and no two share a name"
                    (stem file)
                    (String.concat ", " others))))
      files
  in
  let services = Places.service_schemas root in
  let empty_services =
    List.filter_map
      (fun area ->
        if
          List.exists
            (fun file -> String.starts_with ~prefix:(area ^ "/") file)
            services
        then None
        else
          Some
            (refusal area ""
               "is a service's area (Places.services) and holds no schema of \
                the service's"))
      Places.services
  in
  let census =
    census @ stray_case_tables @ orphan_schemas @ path_item_names
    @ component_files @ kebab_files @ listed_names @ shared_responses
    @ empty_services @ validation @ public_api @ client_surface @ reply_bounds
    @ offset_units @ registry_rules @ jobs_rules @ archivist_rules
    @ smelter_rules @ identifiers_rules @ client_types_rules @ protocol_rules
    @ control_rules @ retry_rules @ worker_rules @ graph_rules @ shape_rules
    @ codes_rules @ telemetry_rules @ option_defaults
  in
  match (schemas, census) with
  | Ok schemas, [] ->
      let table_files =
        Hashtbl.fold
          (fun file (kind, _) files ->
            if kind = Table then file :: files else files)
          decoded []
        |> List.sort compare
      in
      Ok
        {
          gateway;
          archivist;
          path_items;
          responses;
          schemas;
          services;
          tables;
          table_files;
          ids;
        }
  | Ok _, errors -> Error errors
  (* A file that is no JSON Schema leaves the references to it unresolved, so
     its refusal is given in their place. *)
  | Error errors, _ -> Error (errors @ census)
