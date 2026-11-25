(* The client types agree with what restates them. client/types states the
   shapes every SDK hands its callers, and the client and session tables'
   schemas what an SDK reads from them; each restates a fact stated
   elsewhere.

   - ReconciledSpan's branches are told apart by AnchorMethod's words, one
     each, in order.
   - The driver line's emission states are told apart by CacheStatus's
     words, one each, in order.
   - discovery/DiscoveredKB.json's port is bounded as HttpEndpoint's is: the
     gateway's document refers to whole files only, so the bounds are stated
     twice, and held so.
   - Every object of client/types is closed: by [additionalProperties], or,
     where it is composed, by [unevaluatedProperties] at or around it. A
     [$defs] entry holds open members, and whatever names one closes it.
   - Some case of bus/frame-cases.json is refused for each reason
     FrameRefusalReason names.
   - annotations/reader-cases.json's [readers] are, in order, what its
     schema says a case [reads], and each is a reader of client/surface.json's
     annotations module or one of its functions that reads an annotation
     ([is…]); annotations/builder-cases.json's [builders] are, in order, the
     module's other functions.
   - DeviceCode is what control/UserAction.json carries less its
     [requestId], its [expiresInSeconds] named [expiresIn].
   - A method's [shape] in client/ClientSurface.json is, in order, one of the
     shapes it states.
   - session/SessionCacheCases.json's loads are, in order, of
     client/refresh.json's persisted queries, each keeping its query's
     value.
   - A parameter of client/surface.json, and what a method of it returns,
     names, by its [$id], a component or a client type of client/types, and
     a query's value in
     client/refresh.json a component: an [$id] does not say where its
     schema is.
   - client/connection-states.json states each of ConnectionState's values
     once, in its order, and each row's moves in that order. *)

let ( / ) pointer token = pointer ^ "/" ^ Decode.escape token
let at = Decode.at_pointer
let text = function Some (`String s) -> Some s | _ -> None

let texts = function
  | Some (`List items) ->
      List.filter_map (function `String s -> Some s | _ -> None) items
  | _ -> []

let items = function Some (`List items) -> items | _ -> []
let keys = function Some (`Assoc members) -> List.map fst members | _ -> []
let shown json = Yojson.Safe.to_string json
let words list = shown (`List (List.map (fun s -> `String s) list))
let names_defs = Re.compile (Re.str "#/$defs/")

let reads_an_annotation name =
  String.length name > 2
  && String.starts_with ~prefix:"is" name
  && match name.[2] with 'A' .. 'Z' -> true | _ -> false

let refusals ~ids ~files ~(read_json : string -> Yojson.Safe.t option) =
  let found = ref [] in
  let refuse file pointer fmt =
    Printf.ksprintf
      (fun message -> found := { Decode.file; pointer; message } :: !found)
      fmt
  in
  let missing = Hashtbl.create 16 in
  let read file =
    match read_json file with
    | Some json -> Some json
    | None ->
        if not (Hashtbl.mem missing file) then (
          Hashtbl.replace missing file ();
          refuse file "" "is missing: the client types are checked against it");
        None
  in
  let with_files names f =
    match List.map read names with
    | jsons when List.for_all Option.is_some jsons ->
        f (List.map Option.get jsons)
    | _ -> ()
  in
  let anchor_method = "client/types/AnchorMethod.json" in
  let reconciled_span = "client/types/ReconciledSpan.json" in
  with_files [ anchor_method; reconciled_span ] (function
    | [ anchor; span ] ->
        let tag branch =
          List.find_map
            (fun part -> at "/properties/anchorMethod/enum" part)
            (items (at "/allOf" branch))
        in
        let tags =
          List.map
            (fun branch ->
              match tag branch with Some (`List [ one ]) -> one | _ -> `Null)
            (items (at "/oneOf" span))
        in
        let methods = items (at "/enum" anchor) in
        if tags <> methods then
          refuse reconciled_span "/oneOf"
            "is told apart by anchorMethod %s, not, one branch each and in \
             order, AnchorMethod's %s"
            (shown (`List tags))
            (shown (`List methods))
    | _ -> ());
  let discovered_kb = "discovery/DiscoveredKB.json" in
  let http_endpoint = "components/schemas/HttpEndpoint.json" in
  with_files [ discovered_kb; http_endpoint ] (function
    | [ kb; endpoint ] ->
        List.iter
          (fun bound ->
            let pointer = "/properties/port/" ^ bound in
            if at pointer kb <> at pointer endpoint then
              refuse discovered_kb pointer "is %s, where %s states %s"
                (Option.fold ~none:"nothing" ~some:shown (at pointer kb))
                http_endpoint
                (Option.fold ~none:"nothing" ~some:shown (at pointer endpoint)))
          [ "type"; "minimum"; "maximum" ]
    | _ -> ());
  let cache_status = "client/types/CacheStatus.json" in
  let driver_line = "conformance/ConformanceDriverLine.json" in
  with_files [ cache_status; driver_line ] (function
    | [ status; line ] -> (
        match
          List.find_map
            (fun (i, branch) ->
              Option.map
                (fun state -> (i, state))
                (at "/properties/emission/properties/state" branch))
            (List.mapi (fun i branch -> (i, branch)) (items (at "/oneOf" line)))
        with
        | None ->
            refuse driver_line "/oneOf"
              "has no line with an emission's state, which CacheStatus's words \
               tell apart"
        | Some (i, state) ->
            let tags =
              List.map
                (fun branch ->
                  Option.value ~default:`Null
                    (at "/properties/status/const" branch))
                (items (at "/oneOf" state))
            in
            let statuses = items (at "/enum" status) in
            if tags <> statuses then
              refuse driver_line
                ("" / "oneOf" / string_of_int i / "properties" / "emission"
               / "properties" / "state" / "oneOf")
                "is told apart by status %s, not, one branch each and in \
                 order, CacheStatus's %s"
                (shown (`List tags))
                (shown (`List statuses)))
    | _ -> ());
  List.iter
    (fun file ->
      match read file with
      | None -> ()
      | Some json ->
          let rec walk ~closed pointer = function
            | `Assoc members as node ->
                let closes =
                  List.mem_assoc "additionalProperties" members
                  || List.mem_assoc "unevaluatedProperties" members
                in
                let closing =
                  at "/unevaluatedProperties" node = Some (`Bool false)
                in
                if List.mem_assoc "properties" members && not (closes || closed)
                then
                  refuse file pointer
                    "is an open object: a client type is closed, by \
                     additionalProperties, or by unevaluatedProperties where \
                     it is composed";
                (match text (at "/$ref" node) with
                | Some target
                  when Re.execp names_defs target && not (closing || closed) ->
                    refuse file (pointer / "$ref")
                      "names %s, open members, and nothing here closes them"
                      target
                | _ -> ());
                List.iter
                  (fun (key, value) ->
                    let at_key = pointer / key in
                    match (key, value) with
                    | ("allOf" | "anyOf" | "oneOf"), `List parts ->
                        List.iteri
                          (fun i part ->
                            walk ~closed:(closed || closing)
                              (at_key / string_of_int i)
                              part)
                          parts
                    | "$defs", `Assoc defs ->
                        List.iter
                          (fun (name, def) ->
                            walk ~closed:true (at_key / name) def)
                          defs
                    | "properties", `Assoc properties ->
                        List.iter
                          (fun (name, property) ->
                            walk ~closed:false (at_key / name) property)
                          properties
                    | ("items" | "additionalProperties" | "not"), schema ->
                        walk ~closed:false at_key schema
                    | _ -> ())
                  members
            | _ -> ()
          in
          walk ~closed:false "" json)
    (List.filter
       (fun file ->
         Filename.dirname file = "client/types"
         && Filename.check_suffix file ".json")
       files);
  let reasons = "client/types/FrameRefusalReason.json" in
  with_files [ reasons; "bus/frame-cases.json" ] (function
    | [ reason; frames ] ->
        let refused_for =
          List.filter_map
            (fun case ->
              match at "/refused" case with
              | Some (`Bool true) -> text (at "/reason" case)
              | _ -> None)
            (items (at "/cases" frames))
        in
        List.iteri
          (fun i name ->
            if not (List.mem name refused_for) then
              refuse reasons
                ("/enum" / string_of_int i)
                "FrameRefusalReason names %S, and no case of frame-cases.json \
                 is refused for it"
                name)
          (texts (at "/enum" reason))
    | _ -> ());
  let reader_cases = "annotations/reader-cases.json" in
  let builder_cases = "annotations/builder-cases.json" in
  with_files
    [
      "client/surface.json";
      reader_cases;
      "annotations/AnnotationsReaderCases.json";
      builder_cases;
    ] (function
    | [ surface; readers_table; readers_schema; builders_table ] ->
        let members =
          List.concat_map
            (fun m ->
              if text (at "/module" m) = Some "annotations" then
                items (at "/members" m)
              else [])
            (items (at "/modules" surface))
        in
        let member_of kinds m =
          match (text (at "/kind" m), text (at "/member" m)) with
          | Some kind, Some name -> kinds kind name
          | _ -> None
        in
        let readers = texts (at "/readers" readers_table) in
        let reads =
          keys
            (at "/properties/cases/items/properties/reads/properties"
               readers_schema)
        in
        if readers <> reads then
          refuse reader_cases "/readers"
            "readers %s are not, in order, what its schema says a case reads %s"
            (words readers) (words reads);
        let surface_readers =
          List.filter_map
            (member_of (fun kind name ->
                 if
                   kind = "reader"
                   || (kind = "function" && reads_an_annotation name)
                 then Some name
                 else None))
            members
        in
        List.iteri
          (fun i reader ->
            if not (List.mem reader surface_readers) then
              refuse reader_cases
                ("/readers" / string_of_int i)
                "names the reader %S, which client/surface.json's annotations \
                 module does not have"
                reader)
          readers;
        let builders = texts (at "/builders" builders_table) in
        let surface_builders =
          List.filter_map
            (member_of (fun kind name ->
                 if kind = "function" && not (reads_an_annotation name) then
                   Some name
                 else None))
            members
        in
        if builders <> surface_builders then
          refuse builder_cases "/builders"
            "builders %s are not client/surface.json's other annotations \
             functions %s"
            (words builders) (words surface_builders)
    | _ -> ());
  let device_code = "client/types/DeviceCode.json" in
  with_files [ "control/UserAction.json"; device_code ] (function
    | [ action; device ] ->
        let as_device name =
          if name = "expiresInSeconds" then "expiresIn" else name
        in
        let carried =
          match at "/properties" action with
          | Some (`Assoc members) ->
              List.filter (fun (n, _) -> n <> "requestId") members
          | _ -> []
        in
        let device_members = keys (at "/properties" device) in
        let carried_members = List.map (fun (n, _) -> as_device n) carried in
        if List.sort compare device_members <> List.sort compare carried_members
        then
          refuse device_code "/properties"
            "DeviceCode's members %s are not UserAction's less requestId %s"
            (words device_members) (words carried_members);
        List.iter
          (fun (name, property) ->
            let own = at ("/properties" / as_device name / "type") device in
            let carried_type = at "/type" property in
            let typed = Option.fold ~none:"untyped" ~some:shown in
            if own <> carried_type then
              refuse device_code
                ("/properties" / as_device name)
                "DeviceCode's %s is %s, and UserAction's %s is %s"
                (as_device name) (typed own) name (typed carried_type))
          carried;
        let required = List.sort compare (texts (at "/required" device)) in
        let carried_required =
          texts (at "/required" action)
          |> List.filter (( <> ) "requestId")
          |> List.map as_device |> List.sort compare
        in
        if required <> carried_required then
          refuse device_code "/required"
            "DeviceCode requires %s, and UserAction, less requestId, requires \
             %s"
            (words (texts (at "/required" device)))
            (words carried_required)
    | _ -> ());
  let client_surface = "client/ClientSurface.json" in
  with_files [ client_surface ] (function
    | [ schema ] ->
        let shapes = keys (at "/properties/shapes/properties" schema) in
        let pointer =
          "/properties/namespaces/items/properties/methods/items/properties/shape/enum"
        in
        let method_shapes = texts (at pointer schema) in
        if method_shapes <> shapes then
          refuse client_surface pointer
            "the method shapes %s are not, in order, the shapes it states %s"
            (words method_shapes) (words shapes)
    | _ -> ());
  let cache_cases = "session/SessionCacheCases.json" in
  with_files [ "client/refresh.json"; cache_cases ] (function
    | [ refresh; cases ] ->
        let persisted =
          List.filter
            (fun q -> at "/persisted" q = Some (`Bool true))
            (items (at "/queries" refresh))
        in
        let pointer = "/properties/cases/items/oneOf" in
        let loads =
          List.map
            (fun load ->
              ( at "/properties/query/enum" load,
                at "/properties/kept/items/properties/value" load ))
            (items (at pointer cases))
        in
        let names =
          List.map
            (fun q -> Option.map (fun n -> `List [ n ]) (at "/name" q))
            persisted
        in
        let queries = List.map fst loads in
        if queries <> names then
          refuse cache_cases pointer
            "loads %s, not, in order, refresh.json's persisted queries %s"
            (shown (`List (List.filter_map Fun.id queries)))
            (shown
               (`List
                  (List.concat_map
                     (fun q -> Option.to_list (at "/name" q))
                     persisted)));
        List.iteri
          (fun i query ->
            match List.nth_opt loads i with
            | Some (_, kept) when kept <> at "/value" query ->
                refuse cache_cases
                  ((pointer / string_of_int i)
                  ^ "/properties/kept/items/properties/value")
                  "keeps %s for %s, and refresh.json's value is %s"
                  (Option.fold ~none:"nothing" ~some:shown kept)
                  (Option.fold ~none:"an unnamed query" ~some:shown
                     (at "/name" query))
                  (Option.fold ~none:"nothing" ~some:shown (at "/value" query))
            | _ -> ())
          persisted
    | _ -> ());
  let names_one_of file pointer target ~places ~what =
    let named, _ = Decode.resolve ids { target; file; pointer } in
    if
      not
        (List.exists
           (fun place -> String.starts_with ~prefix:place named)
           places)
    then refuse file pointer "names %s, %s, which is not %s" target named what
  in
  with_files [ "client/surface.json" ] (function
    | [ surface ] ->
        List.iteri
          (fun n namespace ->
            List.iteri
              (fun m method_ ->
                let method_at =
                  "/namespaces" / string_of_int n / "methods" / string_of_int m
                in
                let names place target =
                  names_one_of "client/surface.json" (place / "$ref") target
                    ~places:[ Places.components; "client/types/" ]
                    ~what:"a component or a client type"
                in
                List.iteri
                  (fun k parameter ->
                    match at "/schema/$ref" parameter with
                    | Some (`String target) ->
                        names
                          (method_at / "parameters" / string_of_int k / "schema")
                          target
                    | _ -> ())
                  (items (at "/parameters" method_));
                match at "/returns/$ref" method_ with
                | Some (`String target) -> names (method_at / "returns") target
                | _ -> ())
              (items (at "/methods" namespace)))
          (items (at "/namespaces" surface))
    | _ -> ());
  with_files [ "client/refresh.json" ] (function
    | [ refresh ] ->
        List.iteri
          (fun i query ->
            match at "/value/$ref" query with
            | Some (`String target) ->
                names_one_of "client/refresh.json"
                  ("/queries" / string_of_int i / "value" / "$ref")
                  target ~places:[ Places.components ] ~what:"a component"
            | _ -> ())
          (items (at "/queries" refresh))
    | _ -> ());
  let connection_state = "client/types/ConnectionState.json" in
  let connection_states = "client/connection-states.json" in
  with_files [ connection_state; connection_states ] (function
    | [ state; table ] ->
        let order = texts (at "/enum" state) in
        let rows = items (at "/states" table) in
        let stated = List.map (fun row -> text (at "/state" row)) rows in
        if stated <> List.map Option.some order then
          refuse connection_states "/states"
            "states %s, not, each once and in order, ConnectionState's %s"
            (shown
               (`List
                  (List.map
                     (function Some s -> `String s | None -> `Null)
                     stated)))
            (words order);
        List.iteri
          (fun i row ->
            let moves = texts (at "/moves" row) in
            let ordered = List.filter (fun s -> List.mem s moves) order in
            if moves <> ordered then
              refuse connection_states
                ("/states" / string_of_int i / "moves")
                "lists %s, not in ConnectionState's order (%s)" (words moves)
                (words ordered))
          rows
    | _ -> ());
  List.rev !found
