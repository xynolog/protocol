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
   - A parameter of client/surface.json, what a method of it returns, and
     what a member's [gives] holds, takes and returns, names, by its [$id], a
     component or a client type of client/types, and
     a query's value in
     client/refresh.json a component: an [$id] does not say where its
     schema is.
   - client/connection-states.json states each of ConnectionState's values
     once, in its order, and each row's moves in that order.
   - client/discovery-absent.json states each of DiscoveryAbsentReason's
     values once, in its order.
   - CacheState's branches are told apart by CacheStatus's words, one each,
     in order.
   - JobEvent's branches are told apart by JobEventKind's words, one each,
     in order.
   - A client type names, as [x-xynolog-typeParameter], only a type parameter
     its [x-xynolog-typeParameters] declares, and each it declares.
   - ChannelName states bus/registry.json's channels, each once and in its
     order; ScopedChannelName those its [audience.scoped] lists, and
     UnscopedChannelName the rest, each in that order; and OperationName its
     operations, by their requests, in its order. *)

let ( / ) pointer token = pointer ^ "/" ^ Decode.escape token
let at = Decode.at_pointer
let text = function Some (`String s) -> Some s | _ -> None

let texts = function
  | Some (`List items) ->
      List.filter_map (function `String s -> Some s | _ -> None) items
  | _ -> []

let items = function Some (`List items) -> items | _ -> []

(* The string [key] of each row of [rows]. *)
let texts_of rows key =
  List.filter_map (fun row -> text (at ("/" ^ key) row)) (items rows)

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
        let names place node =
          match at "/$ref" node with
          | Some (`String target) ->
              names_one_of "client/surface.json" (place / "$ref") target
                ~places:[ Places.components; "client/types/" ]
                ~what:"a component or a client type"
          | _ -> ()
        in
        (* What a method, or a member's [gives], takes and returns. *)
        let typed place node =
          List.iteri
            (fun k parameter ->
              Option.iter
                (names (place / "parameters" / string_of_int k / "schema"))
                (at "/schema" parameter))
            (items (at "/parameters" node));
          Option.iter (names (place / "returns")) (at "/returns" node)
        in
        List.iteri
          (fun n namespace ->
            List.iteri
              (fun m method_ ->
                typed
                  ("/namespaces" / string_of_int n / "methods" / string_of_int m)
                  method_)
              (items (at "/methods" namespace)))
          (items (at "/namespaces" surface));
        let gives place members =
          List.iteri
            (fun k member ->
              Option.iter
                (fun gives ->
                  let place = place / string_of_int k / "gives" in
                  Option.iter (names (place / "schema")) (at "/schema" gives);
                  Option.iter
                    (typed (place / "signature"))
                    (at "/signature" gives))
                (at "/gives" member))
            (items members)
        in
        List.iteri
          (fun i handed ->
            gives
              ("/worker" / string_of_int i / "members")
              (at "/members" handed))
          (items (at "/worker" surface));
        List.iteri
          (fun i unit ->
            gives
              ("/stateUnits" / string_of_int i / "members")
              (at "/members" unit))
          (items (at "/stateUnits" surface));
        gives "/wire/members" (at "/wire/members" surface)
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
  let each_once_in_order ~vocabulary ~name table_file ~rows ~key =
    let order = texts (at "/enum" vocabulary) in
    let stated = List.map (fun row -> text (at ("/" ^ key) row)) rows in
    if stated <> List.map Option.some order then
      refuse table_file
        ("/" ^ key ^ "s")
        "states %s, not, each once and in order, %s's %s"
        (shown
           (`List
              (List.map (function Some s -> `String s | None -> `Null) stated)))
        name (words order)
  in
  let connection_state = "client/types/ConnectionState.json" in
  let connection_states = "client/connection-states.json" in
  with_files [ connection_state; connection_states ] (function
    | [ state; table ] ->
        let order = texts (at "/enum" state) in
        let rows = items (at "/states" table) in
        each_once_in_order ~vocabulary:state ~name:"ConnectionState"
          connection_states ~rows ~key:"state";
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
  let discovery_absent_reason = "client/types/DiscoveryAbsentReason.json" in
  let discovery_absent = "client/discovery-absent.json" in
  with_files [ discovery_absent_reason; discovery_absent ] (function
    | [ reason; table ] ->
        each_once_in_order ~vocabulary:reason ~name:"DiscoveryAbsentReason"
          discovery_absent
          ~rows:(items (at "/reasons" table))
          ~key:"reason"
    | _ -> ());
  let registry = "bus/registry.json" in
  let named_by_registry = function
    | [ registry_json; all; scoped; unscoped; operations ] ->
        let channels = texts_of (at "/channels" registry_json) "channel" in
        let scoped_channels = texts (at "/audience/scoped" registry_json) in
        let held file json what expected =
          let stated = texts (at "/enum" json) in
          let rec first i = function
            | [], [] -> None
            | s :: ss, e :: es ->
                if s = e then first (i + 1) (ss, es)
                else Some (i, Some s, Some e)
            | s :: _, [] -> Some (i, Some s, None)
            | [], e :: _ -> Some (i, None, Some e)
          in
          match first 0 (stated, expected) with
          | None -> ()
          | Some (i, stated, expected) ->
              let shown = Option.value ~default:"nothing" in
              refuse file
                ("/enum" / string_of_int i)
                "is %s, where bus/registry.json's %s, each once and in its \
                 order, have %s"
                (shown stated) what (shown expected)
        in
        held "client/types/ChannelName.json" all "channels" channels;
        held "client/types/ScopedChannelName.json" scoped
          "channels audience.scoped lists"
          (List.filter (fun c -> List.mem c scoped_channels) channels);
        held "client/types/UnscopedChannelName.json" unscoped
          "channels audience.scoped does not list"
          (List.filter (fun c -> not (List.mem c scoped_channels)) channels);
        held "client/types/OperationName.json" operations
          "operations, by their requests"
          (texts_of (at "/operations" registry_json) "request")
    | _ -> ()
  in
  with_files
    [
      registry;
      "client/types/ChannelName.json";
      "client/types/ScopedChannelName.json";
      "client/types/UnscopedChannelName.json";
      "client/types/OperationName.json";
    ]
    named_by_registry;
  let cache_state = "client/types/CacheState.json" in
  with_files [ cache_status; cache_state ] (function
    | [ status; state ] ->
        let tags =
          List.map
            (fun branch ->
              match at "/properties/status/enum" branch with
              | Some (`List [ one ]) -> one
              | _ -> `Null)
            (items (at "/oneOf" state))
        in
        let statuses = items (at "/enum" status) in
        if tags <> statuses then
          refuse cache_state "/oneOf"
            "is told apart by status %s, not, one branch each and in order, \
             CacheStatus's %s"
            (shown (`List tags))
            (shown (`List statuses))
    | _ -> ());
  let job_event_kind = "client/types/JobEventKind.json" in
  let job_event = "client/types/JobEvent.json" in
  with_files [ job_event_kind; job_event ] (function
    | [ kind; event ] ->
        let tags =
          List.map
            (fun branch ->
              match at "/properties/kind/enum" branch with
              | Some (`List [ one ]) -> one
              | _ -> `Null)
            (items (at "/oneOf" event))
        in
        let kinds = items (at "/enum" kind) in
        if tags <> kinds then
          refuse job_event "/oneOf"
            "is told apart by kind %s, not, one branch each and in order, \
             JobEventKind's %s"
            (shown (`List tags))
            (shown (`List kinds))
    | _ -> ());
  let rec named pointer = function
    | `Assoc members ->
        (match List.assoc_opt "x-xynolog-typeParameter" members with
          | Some (`String name) -> [ (pointer, name) ]
          | _ -> [])
        @ List.concat_map
            (fun (key, member) -> named (pointer / key) member)
            members
    | `List items ->
        List.concat
          (List.mapi
             (fun i item -> named (pointer / string_of_int i) item)
             items)
    | _ -> []
  in
  List.iter
    (fun file ->
      match read file with
      | None -> ()
      | Some json ->
          let declared = texts (at "/x-xynolog-typeParameters" json) in
          let used = named "" json in
          List.iter
            (fun (pointer, name) ->
              if not (List.mem name declared) then
                refuse file pointer
                  "names the type parameter %s, which its \
                   x-xynolog-typeParameters does not declare"
                  name)
            used;
          List.iteri
            (fun j name ->
              if not (List.exists (fun (_, other) -> other = name) used) then
                refuse file
                  ("/x-xynolog-typeParameters" / string_of_int j)
                  "declares %s, which nothing in it names" name)
            declared)
    (List.filter
       (fun file ->
         Filename.dirname file = "client/types"
         && Filename.check_suffix file ".json")
       files);
  List.rev !found
