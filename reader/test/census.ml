(* census.exe SPECS: the model reads every file of Protocol's specs/src, and
   refuses a copy of it that has what the model does not know, lacks what it
   names, or has a table that is not what its schema says. *)

let rec copy source target =
  if Sys.is_directory source then (
    Sys.mkdir target 0o755;
    Array.iter
      (fun name ->
        copy (Filename.concat source name) (Filename.concat target name))
      (Sys.readdir source))
  else
    Out_channel.with_open_bin target (fun out ->
        Out_channel.output_string out
          (In_channel.with_open_bin source In_channel.input_all))

let id name = Decode.base ^ name

let write path text =
  Out_channel.with_open_bin path (fun out -> Out_channel.output_string out text)

let messages = function
  | Ok _ -> []
  | Error errors -> List.map Decode.error_to_string errors

let failures = ref 0

(* A copy of the specs, changed, must be refused with this refusal among
   those it gives. *)
let refused specs ~why change expected =
  let copied = Filename.concat (Filename.temp_dir "census" "") "src" in
  copy specs copied;
  change copied;
  let found = messages (Spec.load copied) in
  if not (List.mem expected found) then (
    Printf.eprintf "A copy that %s was not refused with:\n  %s\nbut with:\n" why
      expected;
    List.iter (Printf.eprintf "  %s\n") found;
    incr failures)

let rewrite path change =
  write path (Yojson.Safe.to_string (change (Yojson.Safe.from_file path)))

(* A schema's file named [name]: [members], the inside of a JSON object,
   after the dialect and the [$id] every schema states. *)
let identified name members =
  Printf.sprintf {|{"$schema":"%s","$id":"%s",%s}|} Decode.dialect (id name)
    members

let members f = function
  | `Assoc members -> `Assoc (f members)
  | _ -> failwith "not an object"

(* Every "$ref" in [json] that names [from] names [onto] instead. *)
let rec repoint ~from ~onto = function
  | `Assoc members ->
      `Assoc
        (List.map
           (function
             | "$ref", `String target when target = from ->
                 ("$ref", `String onto)
             | name, value -> (name, repoint ~from ~onto value))
           members)
  | `List values -> `List (List.map (repoint ~from ~onto) values)
  | value -> value

(* The gateway's document's listing, changed. *)
let gateway_listing copied change =
  rewrite
    (Filename.concat copied "openapi.json")
    (members
       (List.map (function
         | "components", components ->
             ( "components",
               members
                 (List.map (function
                   | "schemas", `Assoc schemas ->
                       ("schemas", `Assoc (change schemas))
                   | member -> member))
                 components )
         | member -> member)))

let list_in_gateway copied name target =
  gateway_listing copied (fun schemas ->
      schemas @ [ (name, `Assoc [ ("$ref", `String target) ]) ])

(* [json] with the value at [place], a pointer's tokens, changed. *)
let rec update place change (json : Yojson.Safe.t) =
  match (place, json) with
  | [], value -> change value
  | token :: rest, `Assoc members ->
      `Assoc
        (List.map
           (fun (key, value) ->
             if key = token then (key, update rest change value)
             else (key, value))
           members)
  | token :: rest, `List items ->
      `List
        (List.mapi
           (fun index item ->
             if string_of_int index = token then update rest change item
             else item)
           items)
  | _ -> failwith "no such place"

(* A copy with the value at [place] in [file] changed, refused with
   [expected] at that file. *)
let changed specs ~why file place change expected =
  refused specs ~why
    (fun copied ->
      rewrite
        (Filename.concat copied file)
        (update
           (if place = "" then [] else String.split_on_char '/' place)
           change))
    (file ^ ": " ^ expected)

let surface specs ~why place change expected =
  changed specs ~why "client/surface.json" place change expected

(* [text] with each character outside the Basic Multilingual Plane written as
   JSON's escape of its UTF-16 surrogates: the same value, spelled in ASCII. *)
let escape_astral text =
  let out = Buffer.create (String.length text) in
  let rec from i =
    if i < String.length text then (
      let decoded = String.get_utf_8_uchar text i in
      let length = Uchar.utf_decode_length decoded in
      let code = Uchar.to_int (Uchar.utf_decode_uchar decoded) in
      if code > 0xFFFF then
        let v = code - 0x10000 in
        Printf.bprintf out "\\u%04x\\u%04x"
          (0xD800 lor (v lsr 10))
          (0xDC00 lor (v land 0x3FF))
      else Buffer.add_string out (String.sub text i length);
      from (i + length))
  in
  from 0;
  Buffer.contents out

let () =
  let specs =
    match Sys.argv with
    | [| _; specs |] -> specs
    | _ ->
        prerr_endline "usage: census.exe SPECS";
        exit 2
  in
  let spec =
    match Spec.load specs with
    | Ok spec -> spec
    | Error errors ->
        List.iter (fun e -> prerr_endline (Decode.error_to_string e)) errors;
        Printf.eprintf "Protocol has %d refusals\n" (List.length errors);
        exit 1
  in
  (* A table's value is of the type its schema states, through a reference
     into another table's schema too: content.get's content.stream, which
     sdk-telemetry states through service-telemetry's attribute, is the
     boolean true. *)
  let content_get =
    List.find
      (fun (span : Tables.Sdk_telemetry_telemetry.spans_item) ->
        span.name = "content.get")
      spec.tables.sdk_telemetry_telemetry.spans
  in
  (match
     List.find_map
       (fun (attribute :
              Tables.Sdk_telemetry_telemetry
              .service_telemetry_telemetry_attributes_item) ->
         if attribute.key = "content.stream" then Some attribute.values
         else None)
       content_get.attributes
   with
  | Some (Some [ Boolean true ]) -> ()
  | _ ->
      prerr_endline
        "sdk-telemetry/telemetry.json: content.get's content.stream is not the \
         boolean true";
      exit 1);
  refused specs ~why:"has a file the model does not know"
    (fun copied -> write (Filename.concat copied "retry/notes.txt") "")
    "retry/notes.txt: /: a file the model does not know";
  refused specs ~why:"lacks a table the model names"
    (fun copied -> Sys.remove (Filename.concat copied "retry/cases.json"))
    "retry/cases.json: /: the model reads this file, and Protocol has none";
  refused specs ~why:"has a service's area with no schema of the service's"
    (fun copied ->
      List.iter
        (fun schema -> Sys.remove (Filename.concat copied ("weaver/" ^ schema)))
        [ "WeaverConfig.json"; "WeaverHealth.json" ])
    "weaver: /: is a service's area (Places.services) and holds no schema of \
     the service's";
  refused specs ~why:"has a reference naming nothing"
    (fun copied ->
      write
        (Filename.concat copied "components/schemas/Dangling.json")
        (identified "Dangling.json"
           {|"$ref":"https://xynolog.org/protocol/Nowhere.json"|}))
    "components/schemas/Dangling.json: /$ref: names \
     https://xynolog.org/protocol/Nowhere.json, an $id no JSON Schema states";
  changed specs ~why:"has a schema state no $id" "components/schemas/JobId.json"
    ""
    (members (List.remove_assoc "$id"))
    "/: states no $id: its $id is https://xynolog.org/protocol/JobId.json";
  changed specs ~why:"has a schema state an $id not its name"
    "components/schemas/JobId.json" "$id"
    (fun _ -> `String (id "TaskId.json"))
    "/$id: is \"https://xynolog.org/protocol/TaskId.json\", and a JSON \
     Schema's $id is its name under https://xynolog.org/protocol/: \
     https://xynolog.org/protocol/JobId.json";
  changed specs ~why:"has a schema state no dialect"
    "components/schemas/JobId.json" ""
    (members (List.remove_assoc "$schema"))
    "/: states no $schema: a JSON Schema states \
     https://json-schema.org/draft/2020-12/schema";
  changed specs ~why:"has a schema of another dialect"
    "components/schemas/JobId.json" "$schema"
    (fun _ -> `String "http://json-schema.org/draft-07/schema#")
    "/$schema: expected \"https://json-schema.org/draft/2020-12/schema\", \
     found \"http://json-schema.org/draft-07/schema#\"";
  changed specs ~why:"has an $id within a schema"
    "components/schemas/JobDeclined.json" "properties/reason"
    (members (fun m -> ("$id", `String (id "Reason.json")) :: m))
    "/properties/reason: states $id, which only a JSON Schema's file states, \
     at its root";
  changed specs ~why:"has an $id within a table" "client/registry.json"
    "types/ConnectionIdentity"
    (members (fun m -> ("$id", `String (id "ConnectionIdentity.json")) :: m))
    "/types/ConnectionIdentity: states $id, which only a JSON Schema's file \
     states, at its root";
  refused specs ~why:"gives two schemas one $id"
    (fun copied ->
      copy
        (Filename.concat copied "components/schemas/JobId.json")
        (Filename.concat copied "jobs/JobId.json"))
    "jobs/JobId.json: /$id: states the $id \
     https://xynolog.org/protocol/JobId.json, which \
     components/schemas/JobId.json states too";
  surface specs ~why:"has a parameter name an $id no schema states"
    "namespaces/0/methods/0/parameters/0/schema/$ref"
    (fun _ -> `String (id "Nowhere.json"))
    "/namespaces/0/methods/0/parameters/0/schema/$ref: names \
     https://xynolog.org/protocol/Nowhere.json, an $id no JSON Schema states";
  surface specs ~why:"has a parameter name a service's schema"
    "namespaces/0/methods/0/parameters/0/schema/$ref"
    (fun _ -> `String (id "WorkerConfig.json"))
    "/namespaces/0/methods/0/parameters/0/schema/$ref: names \
     https://xynolog.org/protocol/WorkerConfig.json, worker/WorkerConfig.json, \
     which is not a component or a client type";
  surface specs ~why:"has a client send the page the service defaults"
    "namespaces/1/methods/1/cases/0/sends"
    (fun _ -> `Assoc [ ("limit", `Int 100); ("offset", `Int 0) ])
    "/namespaces/1/methods/1/cases/0/sends/limit: is sent, and the request's \
     schema states its default, 50: a client sends nothing its caller left out";
  surface specs ~why:"has a client send a default other than the table's"
    "namespaces/4/methods/1/cases/0/sends/options/depth"
    (fun _ -> `Int 3)
    "/namespaces/4/methods/1/cases/0/sends: gives none of gather.resource's \
     defaulted parameters, and sends 3 at /options/depth, not \
     option-defaults.json's 2 (/defaults/2)";
  surface specs ~why:"has a client send what it was not given"
    "namespaces/1/methods/15/cases/0/sends"
    (members (fun m -> m @ [ ("extra", `Bool true) ]))
    "/namespaces/1/methods/15/cases/0/sends/extra: is neither an argument nor \
     a default of option-defaults.json";
  changed specs ~why:"defaults a parameter its method does not take"
    "client/option-defaults.json" "defaults/0/parameter"
    (fun _ -> `String "path")
    "/defaults/0/parameter: browse.files takes no parameter path";
  changed specs ~why:"defaults an option to a value its schema refuses"
    "client/option-defaults.json" "defaults/2/value"
    (fun _ -> `Int 9)
    "/defaults/2/value: /depth: is more than 3";
  changed specs ~why:"gives a method's default a schema of its own"
    "client/option-defaults.json" "defaults/6"
    (members (fun m ->
         m
         @ [
             ( "schema",
               `Assoc
                 [ ("type", `String "integer"); ("description", `String "x") ]
             );
           ]))
    "/defaults/6/schema: states a schema, and surface.json states \
     match.search's";
  surface specs ~why:"has a method return an $id no schema states"
    "namespaces/1/methods/0/returns/$ref"
    (fun _ -> `String (id "Nowhere.json"))
    "/namespaces/1/methods/0/returns/$ref: names \
     https://xynolog.org/protocol/Nowhere.json, an $id no JSON Schema states";
  surface specs ~why:"has a method return a service's schema"
    "namespaces/1/methods/0/returns/$ref"
    (fun _ -> `String (id "WorkerConfig.json"))
    "/namespaces/1/methods/0/returns/$ref: names \
     https://xynolog.org/protocol/WorkerConfig.json, worker/WorkerConfig.json, \
     which is not a component or a client type";
  surface specs ~why:"has a handed name a record no client type is"
    "worker/1/record"
    (fun _ -> `String "Nowhere")
    "/worker/1/record: names Nowhere, which is no client type";
  surface specs ~why:"has a handed's members out of its record's order"
    "worker/1/members"
    (function
      | `List (first :: second :: rest) -> `List (second :: first :: rest)
      | _ -> failwith "worker/1/members is no list of two")
    "/worker/1/record: names HeldJobRecord, whose properties are not, in its \
     order, the first members, taking nothing";
  surface specs ~why:"has a member of a handed's record take what a verb takes"
    "worker/1/members/7"
    (function
      | `Assoc members ->
          `Assoc
            (members
            @ [ ("takes", `Assoc [ ("mark", `String "MarkJobResult") ]) ])
      | _ -> failwith "worker/1/members/7 is no member")
    "/worker/1/record: names HeldJobRecord, whose properties are not, in its \
     order, the first members, taking nothing";
  surface specs ~why:"has a state unit's member give nothing"
    "stateUnits/0/members/7"
    (fun _ -> `Assoc [ ("member", `String "cancelPending") ])
    "/stateUnits/0/members/7: lacks the member \"gives\"";
  surface specs ~why:"has a member of a handed's record give what it is not"
    "worker/1/members/0/gives/form"
    (fun _ -> `String "watched")
    "/worker/1/members/0/gives: is not \
     {\"form\":\"value\",\"schema\":{\"$ref\":\"https://xynolog.org/protocol/HeldJobRecord.json#/properties/jobId\"}}, \
     what HeldJobRecord's jobId is";
  surface specs
    ~why:"has a member that takes what a verb takes state what it takes"
    "worker/1/members/17/gives"
    (fun _ ->
      `Assoc
        [
          ("form", `String "function");
          ("signature", `Assoc [ ("parameters", `List []); ("returns", `Null) ]);
        ])
    "/worker/1/members/17/gives: is no function one of whose parameters is \
     decided in its takes, and the member takes what a verb takes";
  surface specs ~why:"names a type parameter its unit does not declare"
    "stateUnits/8/members/1/gives/signature/returns/typeParameter"
    (fun _ -> `String "W")
    "/stateUnits/8/members/1/gives/signature/returns: names the type parameter \
     W, which cache does not declare";
  surface specs ~why:"has a unit declare a type parameter none of it names"
    "stateUnits/6/typeParameters"
    (function
      | `List declared ->
          `List
            (declared
            @ [ `Assoc [ ("name", `String "S"); ("docs", `String "Unnamed.") ] ]
            )
      | other -> other)
    "/stateUnits/6/typeParameters/1: declares S, which none of search's \
     members names";
  surface specs ~why:"has the cache declare its type parameters out of order"
    "stateUnits/8/typeParameters"
    (function `List [ k; v ] -> `List [ v; k ] | other -> other)
    "/stateUnits/8/typeParameters: declares [\"V\",\"K\"]: a cache declares K, \
     what a query is keyed by, and V, what it holds, in that order";
  surface specs ~why:"names a type parameter outside a state unit"
    "worker/1/members/15/gives/signature/parameters/0/schema"
    (fun _ -> `Assoc [ ("typeParameter", `String "V") ])
    "/worker/1/members/15/gives/signature/parameters/0/schema: names the type \
     parameter V, and only a state unit's members name one, which their unit \
     declares";
  surface specs ~why:"gives a generic client type other type arguments"
    "stateUnits/6/members/1/gives/schema/typeArguments"
    (fun _ -> `Assoc [ ("S", `Assoc [ ("typeParameter", `String "R") ]) ])
    "/stateUnits/6/members/1/gives/schema: gives SearchState the type \
     arguments [\"S\"], and it is generic over [\"R\"]";
  surface specs ~why:"has a schema decided by no member beside it"
    "worker/1/members/17/gives/signature/parameters/0/schema/decided/by/member"
    (fun _ -> `String "verb")
    "/worker/1/members/17/gives/signature/parameters/0/schema/decided/by/member: \
     names verb, which is no member beside it";
  surface specs ~why:"has a schema decided by a member that holds no enum's"
    "worker/1/members/17/gives/signature/parameters/0/schema/decided/by/member"
    (fun _ -> `String "attempt")
    "/worker/1/members/17/gives/signature/parameters/0/schema/decided/by/member: \
     names attempt, which holds no enum's value";
  surface specs ~why:"has takes miss a value of what decides it"
    "worker/1/members/17/takes"
    (fun _ -> `Assoc [ ("mark", `String "MarkJobResult") ])
    "/worker/1/members/17/takes: states [\"mark\"], not, each once and in \
     order, the values of jobType, [\"mark\",\"yield\"]";
  surface specs ~why:"has a schema decided outside what a function takes"
    "namespaces/0/methods/0/parameters/0/schema"
    (fun _ ->
      `Assoc
        [
          ( "decided",
            `Assoc
              [
                ("by", `Assoc [ ("member", `String "get") ]);
                ("in", `String "takes");
              ] );
        ])
    "/namespaces/0/methods/0/parameters/0/schema: is decided, and only what a \
     function takes or returns is";
  surface specs ~why:"has a schema decided by no other parameter"
    "wire/members/0/gives/signature/parameters/1/schema/decided/by/parameter"
    (fun _ -> `String "kind")
    "/wire/members/0/gives/signature/parameters/1/schema/decided/by/parameter: \
     names kind, which is no other parameter of emit";
  surface specs ~why:"has a schema decided in channels by an operation's name"
    "wire/members/3/gives/signature/returns/decided/in"
    (fun _ -> `String "channels")
    "/wire/members/3/gives/signature/returns/decided/by/parameter: names \
     operation, which is no channel's name: what is decided in channels is \
     decided by a ChannelName, an UnscopedChannelName or a ScopedChannelName";
  surface specs ~why:"has a parameter decide in takes"
    "wire/members/0/gives/signature/parameters/1/schema/decided/in"
    (fun _ -> `String "takes")
    "/wire/members/0/gives/signature/parameters/1/schema/decided/in: is takes, \
     and a parameter decides only in channels, request or result";
  surface specs ~why:"has a member beside a function decide in channels"
    "worker/1/members/17/gives/signature/parameters/0/schema/decided/in"
    (fun _ -> `String "channels")
    "/worker/1/members/17/gives/signature/parameters/0/schema/decided/in: is \
     channels, and a member beside the function decides only in takes";
  surface specs
    ~why:"has a stream give a refused frame as other than a FrameRefusal"
    "wire/members/1/gives/signature/returns/refused/$ref"
    (fun _ -> `String (id "Fault.json"))
    "/wire/members/1/gives/signature/returns/refused: is not FrameRefusal, \
     which a refused frame is given as";
  changed specs ~why:"names a channel the registry does not have"
    "client/types/ChannelName.json" "enum/0"
    (fun _ -> `String "nowhere:here")
    "/enum/0: is nowhere:here, where bus/registry.json's channels, each once \
     and in its order, have yield:created";
  changed specs ~why:"leaves out a channel a resource's scope carries"
    "client/types/ScopedChannelName.json" "enum"
    (function
      | `List names ->
          `List (List.filteri (fun i _ -> i < List.length names - 1) names)
      | other -> other)
    "/enum/12: is nothing, where bus/registry.json's channels audience.scoped \
     lists, each once and in its order, have job:failed";
  changed specs ~why:"names an operation the registry does not have"
    "client/types/OperationName.json" "enum/0"
    (fun _ -> `String "x:y")
    "/enum/0: is x:y, where bus/registry.json's operations, by their requests, \
     each once and in its order, have bind:update-body";
  surface specs ~why:"has a method take what is the SDK's own"
    "namespaces/0/methods/0/parameters/0"
    (members (List.remove_assoc "schema"))
    "/namespaces/0/methods/0/parameters/0: states no schema, and a method \
     takes only what Protocol states";
  surface specs ~why:"has a case give a parameter that is the SDK's own"
    "namespaces/0/methods/0/parameters/0"
    (members (List.remove_assoc "schema"))
    "/namespaces/0/methods/0/cases/0/args/type: is the SDK's own, which a case \
     cannot give";
  changed specs ~why:"tells CacheState's branches apart by other statuses"
    "client/types/CacheState.json" "oneOf/0/properties/status/enum/0"
    (fun _ -> `String "stale")
    "/oneOf: is told apart by status [\"stale\",\"ready\",\"failed\"], not, \
     one branch each and in order, CacheStatus's \
     [\"pending\",\"ready\",\"failed\"]";
  changed specs ~why:"names a type parameter its client type does not declare"
    "client/types/SearchState.json"
    "properties/results/items/x-xynolog-typeParameter"
    (fun _ -> `String "S")
    "/properties/results/items: names the type parameter S, which its \
     x-xynolog-typeParameters does not declare";
  changed specs ~why:"declares a type parameter its client type does not name"
    "client/types/SearchState.json" "x-xynolog-typeParameters"
    (fun _ -> `List [ `String "R"; `String "S" ])
    "/x-xynolog-typeParameters/1: declares S, which nothing in it names";
  surface specs
    ~why:"has a member's gives name what is no component or client type"
    "worker/0/members/3/gives/schema/$ref"
    (fun _ -> `String (id "WorkerConfig.json"))
    "/worker/0/members/3/gives/schema/$ref: names \
     https://xynolog.org/protocol/WorkerConfig.json, worker/WorkerConfig.json, \
     which is not a component or a client type";
  refused specs ~why:"has a member a case expects null of always hold one"
    (fun copied ->
      rewrite
        (Filename.concat copied "client/surface.json")
        (update [ "stateUnits"; "4"; "members"; "0"; "gives"; "absent" ]
           (fun _ -> `Bool false)))
    "client/state-units.json: \
     /cases/4/steps/0/expect/unit/hoveredAnnotationId: is null, and beckon's \
     hoveredAnnotationId always holds one";
  changed specs ~why:"expects a member to hold what its gives does not"
    "client/state-units.json" "cases/34/steps/0/expect/unit/isGenerating"
    (fun _ -> `String "no")
    "/cases/34/steps/0/expect/unit/isGenerating: is \"no\", which is no boolean";
  changed specs ~why:"expects a member its unit does not have"
    "client/state-units.json" "cases/34/steps/0/expect/unit"
    (function
      | `Assoc members -> `Assoc (members @ [ ("isGoing", `Bool true) ])
      | _ -> failwith "cases/34/steps/0/expect/unit is no object")
    "/cases/34/steps/0/expect/unit/isGoing: is no member of yield";
  changed specs ~why:"has a case of a unit the surface has not"
    "client/state-units.json" "cases/34/unit"
    (fun _ -> `String "nowhere")
    "/cases/34/unit: names nowhere, which is no state unit of \
     client/surface.json";
  changed specs ~why:"has a query's value name an $id no schema states"
    "client/refresh.json" "queries/0/value/$ref"
    (fun _ -> `String (id "Nowhere.json"))
    "/queries/0/value/$ref: names https://xynolog.org/protocol/Nowhere.json, \
     an $id no JSON Schema states";
  changed specs ~why:"has a query's value name a client type"
    "client/refresh.json" "queries/0/value/$ref"
    (fun _ -> `String (id "DeviceCode.json"))
    "/queries/0/value/$ref: names \
     https://xynolog.org/protocol/DeviceCode.json, \
     client/types/DeviceCode.json, which is not a component";
  let control_id =
    "does not match ^https://xynolog\\.org/protocol/[A-Z][A-Za-z]*\\.json$"
  in
  changed specs ~why:"has a control method name its params by path"
    "control/methods.json" "methods/0/params"
    (fun _ -> `String "./AboutParams.json")
    ("/methods/0/params: " ^ control_id);
  changed specs ~why:"has a control notification name its params by name"
    "control/methods.json" "notifications/0/params"
    (fun _ -> `String "Progress")
    ("/notifications/0/params: " ^ control_id);
  changed specs ~why:"has a control method name a node within a schema"
    "control/methods.json" "methods/0/result"
    (fun _ -> `String (id "AboutResult.json#/properties/version"))
    ("/methods/0/result: " ^ control_id);
  changed specs ~why:"has a run run a service telemetry names none of"
    "control/RunService.json" "enum/0"
    (fun _ -> `String "browser")
    "/enum: names \
     [browser,dispatcher,gateway,librarian,smelter,weaver,worker], where \
     service-telemetry/ServiceTelemetryTelemetry.json names \
     [archivist,dispatcher,gateway,librarian,smelter,weaver,worker]";
  changed specs ~why:"has a local stack generated for a provider none names"
    "control/RootsInitParams.json" "properties/inference/enum/1"
    (fun _ -> `String "bedrock")
    "/properties/inference/enum: names [anthropic,bedrock], which are not some \
     of components/schemas/InferenceProvider.json's \
     [anthropic,google,llamacpp,ollama,openai,together,vllm]";
  changed specs ~why:"has a control code answered under no JSON-RPC code"
    "errors/codes.json" "control/codes/23/rpc"
    (fun _ -> `Int (-32001))
    "/control/codes/23/rpc: is -32001, which is no code of \
     control/rpc-codes.json";
  changed specs ~why:"has a control API answer under no JSON-RPC code"
    "control/request-cases.json" "cases/8/answer/error/code"
    (fun _ -> `Int (-32099))
    "/cases/8/answer/error/code: is -32099, which is no code of \
     control/rpc-codes.json";
  changed specs ~why:"has a refusal of params be retryable"
    "control/request-cases.json" "cases/9/answer/error/data/retryable"
    (fun _ -> `Bool true)
    "/cases/9/answer/error/data/retryable: is true, where errors/codes.json \
     states control.invalid-params is false (/control/codes/0/retryable)";
  changed specs ~why:"has an operation's failure answered as invalid params"
    "control/request-cases.json" "cases/9/answer/error/data/code"
    (fun _ -> `String "control.invalid-value")
    "/cases/9/answer/error/data/code: is control.invalid-value, which \
     errors/codes.json answers under -32000, not -32602 (/control/codes/4/rpc)";
  changed specs ~why:"has a method not found carry a control code"
    "control/request-cases.json" "cases/8/answer/error"
    (fun _ ->
      `Assoc
        [
          ("code", `Int (-32601));
          ( "data",
            `Assoc
              [ ("code", `String "control.failed"); ("retryable", `Bool true) ]
          );
        ])
    "/cases/8/answer/error/data: is stated, and no control code of \
     errors/codes.json is answered under -32601";
  changed specs ~why:"has an internal error carry no control code"
    "control/rpc-codes.json" "codes/4/data/code"
    (fun _ -> `String "control.crashed")
    "/codes/4/data/code: is \"control.crashed\", which is no control code of \
     errors/codes.json";
  changed specs ~why:"states one JSON-RPC code twice" "control/rpc-codes.json"
    "codes/5/number"
    (fun _ -> `Int (-32700))
    "/codes/5/number: is -32700, which an earlier code states";
  changed specs ~why:"has a control method send a notification none states"
    "control/methods.json" "methods/2/sends/0"
    (fun _ -> `String "notifications/nowhere")
    "/methods/2/sends/0: notifications/nowhere, which the table states no \
     notification of";
  changed specs ~why:"has a control method send what the client sends"
    "control/methods.json" "methods/2/sends/0"
    (fun _ -> `String "notifications/cancelled")
    "/methods/2/sends/0: notifications/cancelled, which the client sends, not \
     the launcher";
  changed specs ~why:"has control/methods.json name an $id no schema states"
    "control/methods.json" "errorData"
    (fun _ -> `String (id "Nowhere.json"))
    "/errorData: names https://xynolog.org/protocol/Nowhere.json, an $id no \
     JSON Schema states";
  changed specs ~why:"has control/methods.json name a schema outside control/"
    "control/methods.json" "apiVersion"
    (fun _ -> `String (id "JobId.json"))
    "/apiVersion: names https://xynolog.org/protocol/JobId.json, \
     components/schemas/JobId.json, which is not a schema of control/";
  let statuses values = `List (List.map (fun status -> `Int status) values) in
  changed specs ~why:"has a retry set's statuses out of order"
    "retry/rules.json" "boot/statuses"
    (fun _ -> statuses [ 503; 429; 504 ])
    "/boot/statuses: is not in ascending order";
  changed specs ~why:"has a retry set name a status its statusFrom covers"
    "retry/rules.json" "job/statuses"
    (fun _ -> statuses [ 408; 429; 502 ])
    "/job/statusFrom: is 500, and /job/statuses names 502, which it covers";
  changed specs ~why:"has a retry set name a status no row of its rule retries"
    "retry/rules.json" "boot/statuses"
    (fun _ -> statuses [ 429; 503; 504; 507 ])
    "/boot/statuses/3: is 507, and no row of retry/cases.json's boot rule \
     retries it";
  refused specs ~why:"has a retry row a status set disagrees with"
    (fun copied ->
      rewrite
        (Filename.concat copied "retry/rules.json")
        (update [ "transport"; "statuses" ] (fun _ ->
             statuses [ 408; 413; 429; 500; 502; 503 ])))
    "retry/cases.json: /rules/26/retries: is true, and retry/rules.json's \
     transport set says false";
  refused specs ~why:"has a retry row the idempotent methods disagree with"
    (fun copied ->
      rewrite
        (Filename.concat copied "retry/rules.json")
        (update [ "transport"; "idempotentMethods" ] (fun _ ->
             `List [ `String "GET"; `String "HEAD"; `String "PUT" ])))
    "retry/cases.json: /rules/28/retries: is true, and retry/rules.json's \
     transport set says false";
  refused specs ~why:"has a retry row the no-response verdict disagrees with"
    (fun copied ->
      rewrite
        (Filename.concat copied "retry/rules.json")
        (update [ "refresh"; "noResponse" ] (fun _ -> `Bool false)))
    "retry/cases.json: /rules/36/retries: is true, and retry/rules.json's \
     refresh set says false";
  changed specs ~why:"has a shape case read that its schema does not allow"
    "client/shape-cases.json" "cases/0/value"
    (fun _ -> `Assoc [])
    "/cases/0: is read, and Runner does not allow its value, by Protocol's \
     validator";
  changed specs ~why:"has a shape case refused that its schema allows"
    "client/shape-cases.json" "cases/2/value"
    (fun _ -> `Assoc [ ("external", `String "db.example.org:5432") ])
    "/cases/2: is refused, and Runner allows its value, by Protocol's validator";
  changed specs ~why:"has a shape case name no schema file"
    "client/shape-cases.json" "cases/0/schema"
    (fun _ -> `String "Nowhere")
    "/cases/0/schema: Nowhere names no schema file";
  changed specs ~why:"names a schema by its path"
    "components/schemas/JobCancelRequest.json" "properties/jobId/$ref"
    (fun _ -> `String "./JobId.json")
    "/properties/jobId/$ref: names the JSON Schema ./JobId.json by its path: a \
     JSON Schema is named by its $id, https://xynolog.org/protocol/JobId.json";
  changed specs ~why:"maps a discriminator's value to a schema's path"
    "components/schemas/JobCreateCommand.json" "discriminator/mapping/mark"
    (fun _ -> `String "./MarkJobCreateCommand.json")
    "/discriminator/mapping/mark: maps mark to \
     \"./MarkJobCreateCommand.json\", which is no JSON Schema's $id";
  refused specs ~why:"has a schema with a keyword the model does not know"
    (fun copied ->
      write
        (Filename.concat copied "components/schemas/Unknown.json")
        (identified "Unknown.json"
           {|"type":"string","contentEncoding":"base64"|}))
    "components/schemas/Unknown.json: /contentEncoding: an unknown member";
  refused specs ~why:"lacks a path item a document names"
    (fun copied -> Sys.remove (Filename.concat copied "paths/get-health.json"))
    "openapi.json: /paths/~1api~1health/$ref: names paths/get-health.json, \
     which Protocol does not have";
  changed specs ~why:"names a path item by what is not its operationId"
    "paths/get-status.json" "get/operationId"
    (fun _ -> `String "getState")
    "/: is named get-status, and a path item's file is named by its \
     operationId, get-state";
  changed specs ~why:"has a path item whose operations share no name"
    "archivist/paths/resource-anchored-text.json" "put/operationId"
    (fun _ -> `String "putAnchoredText")
    "/: holds operations whose operationIds share no name after their verbs, \
     and a path item's file is named by the name they share";
  refused specs ~why:"lists a component by what is not its file's name"
    (fun copied ->
      gateway_listing copied
        (List.map (function
          | "HealthResponse", reference -> ("Health", reference)
          | member -> member)))
    "openapi.json: /components/schemas/Health/$ref: lists \
     components/schemas/HealthResponse.json as Health, and a document lists a \
     component by its file's name, HealthResponse";
  refused specs ~why:"names a response's file in kebab case"
    (fun copied ->
      let from = "components/responses/AtCapacity.json" in
      let onto = "components/responses/at-capacity.json" in
      Sys.rename (Filename.concat copied from) (Filename.concat copied onto);
      rewrite (Filename.concat copied "openapi.json") (repoint ~from ~onto);
      rewrite
        (Filename.concat copied "paths/subscribe-to-bus.json")
        (repoint ~from:("../" ^ from) ~onto:("../" ^ onto)))
    "components/responses/at-capacity.json: /: is named at-capacity, and a \
     response's file is its name in PascalCase";
  refused specs ~why:"names a JSON Schema's file in kebab case"
    (fun copied ->
      write
        (Filename.concat copied "components/schemas/lower-case.json")
        (identified "lower-case.json" {|"type":"string"|}))
    "components/schemas/lower-case.json: /: is named lower-case, and a JSON \
     Schema's file is its name in PascalCase";
  refused specs ~why:"has a directory not named in kebab case"
    (fun copied ->
      Sys.rename
        (Filename.concat copied "librarian/elements")
        (Filename.concat copied "librarian/Elements"))
    "librarian/Elements: /: is a directory named Elements, and a directory's \
     name is in kebab case";
  refused specs ~why:"has a case table in an area not named in kebab case"
    (fun copied ->
      Sys.mkdir (Filename.concat copied "Stray_Area") 0o755;
      write
        (Filename.concat copied "Stray_Area/notes-cases.json")
        (identified "notes-cases.json" {|"type":"object"|}))
    "Stray_Area: /: is a directory named Stray_Area, and a directory's name is \
     in kebab case";
  refused specs ~why:"names a fixture's file not in kebab case"
    (fun copied ->
      write (Filename.concat copied "smelter/fixtures/Stray.pdf") "")
    "smelter/fixtures/Stray.pdf: /: is named Stray, and a file that names no \
     component is named in kebab case";
  refused specs ~why:"has two responses of one name"
    (fun copied ->
      write
        (Filename.concat copied "archivist/responses/Unauthorized.json")
        (In_channel.with_open_bin
           (Filename.concat copied
              "archivist/responses/ArchivistUnauthorized.json")
           In_channel.input_all);
      rewrite
        (Filename.concat copied "archivist/paths/list-streams.json")
        (repoint ~from:"../responses/ArchivistUnauthorized.json"
           ~onto:"../responses/Unauthorized.json"))
    "archivist/responses/Unauthorized.json: /: is the response Unauthorized, \
     as components/responses/Unauthorized.json is: a response is named by its \
     file, and no two share a name";
  refused specs ~why:"lacks a table's schema"
    (fun copied -> Sys.remove (Filename.concat copied "retry/RetryCases.json"))
    "retry/cases.json: /: has no schema, which would be retry/RetryCases.json";
  refused specs ~why:"has a table's schema with no table"
    (fun copied ->
      write
        (Filename.concat copied "retry/RetryLost.json")
        (identified "RetryLost.json"
           {|"allOf":[{"$ref":"https://xynolog.org/protocol/CaseTable.json"}]|}))
    "retry/RetryLost.json: /: is a table's schema, and no table beside it is \
     named for it";
  refused specs ~why:"has a case table off the top of an area"
    (fun copied ->
      write (Filename.concat copied "components/stray-cases.json") "{}")
    "components/stray-cases.json: /: is a case table, and a table is at the \
     top of an area";
  refused specs ~why:"has a table value its schema refuses"
    (fun copied ->
      rewrite
        (Filename.concat copied "jobs/retry-cases.json")
        (members
           (List.map (function
             | "cases", `List (`Assoc case :: rest) ->
                 ( "cases",
                   `List
                     (`Assoc
                        (List.map
                           (function
                             | "retryCount", _ -> ("retryCount", `Int (-1))
                             | member -> member)
                           case)
                     :: rest) )
             | member -> member))))
    "jobs/retry-cases.json: /cases/0/retryCount: is less than 0";
  refused specs
    ~why:"has a component a route uses that the gateway does not list"
    (fun copied -> gateway_listing copied (List.remove_assoc "HealthResponse"))
    "components/schemas/HealthResponse.json: /: is a component schema, and the \
     gateway's document does not list it";
  refused specs ~why:"has a service's schema listed by the gateway"
    (fun copied ->
      list_in_gateway copied "WorkerConfig" (id "WorkerConfig.json"))
    "openapi.json: /components/schemas/WorkerConfig/$ref: lists \
     https://xynolog.org/protocol/WorkerConfig.json, worker/WorkerConfig.json, \
     which is neither a component schema nor discovery's: the gateway's \
     document lists the public schemas alone";
  refused specs ~why:"has a gateway route that uses a service's schema"
    (fun copied ->
      rewrite
        (Filename.concat copied "paths/get-health.json")
        (repoint ~from:(id "HealthResponse.json")
           ~onto:(id "DispatcherHealth.json")))
    "paths/get-health.json: \
     /get/responses/200/content/application~1json/schema/$ref: names \
     https://xynolog.org/protocol/DispatcherHealth.json, \
     dispatcher/DispatcherHealth.json, outside components/schemas: what a \
     gateway route uses is a component schema";
  refused specs ~why:"has a component schema that refers into a service's area"
    (fun copied ->
      write
        (Filename.concat copied "components/schemas/Stray.json")
        (identified "Stray.json"
           {|"$ref":"https://xynolog.org/protocol/WorkerConfig.json"|});
      list_in_gateway copied "Stray" (id "Stray.json"))
    "components/schemas/Stray.json: /$ref: names \
     https://xynolog.org/protocol/WorkerConfig.json, worker/WorkerConfig.json, \
     outside components/schemas: a component schema refers to component \
     schemas alone";
  refused specs ~why:"has Archivist's own schema put back among the components"
    (fun copied ->
      let at = Filename.concat copied in
      Sys.rename
        (at "archivist/ArchivistHealth.json")
        (at "components/schemas/ArchivistHealth.json");
      list_in_gateway copied "ArchivistHealth" (id "ArchivistHealth.json"))
    "components/schemas/ArchivistHealth.json: /: is reached by Archivist's \
     document alone, not by a gateway route or the bus: it is Archivist's own, \
     so its place is archivist/";
  surface specs ~why:"may return nothing, and returns nothing at all"
    "stateUnits/9/members/9/gives/signature/returns"
    (fun _ -> `Null)
    "/stateUnits/9/members/9/gives/signature/absent: says it may return \
     nothing, and it returns nothing at all: a function that may return \
     nothing returns something when it does";
  surface specs ~why:"returns a handed worker has not"
    "worker/0/members/0/gives/signature/returns/handed"
    (fun _ -> `String "nobody")
    "/worker/0/members/0/gives/signature/returns/handed: names nobody, which \
     is no handed of worker";
  surface specs ~why:"returns a function as a member that is none"
    "stateUnits/9/members/10/gives/signature/returns/function"
    (fun _ -> `String "token")
    "/stateUnits/9/members/10/gives/signature/returns/function: names token, \
     which is no function";
  surface specs ~why:"gives a method an argument it does not take"
    "namespaces/8/methods/4/cases/0/args"
    (members (fun args -> args @ [ ("jobID", `String "job-1") ]))
    "/namespaces/8/methods/4/cases/0/args/jobID: is no parameter of job.status";
  surface specs ~why:"lacks an argument a method requires"
    "namespaces/2/methods/1/cases/0/args"
    (members (List.remove_assoc "annotationId"))
    "/namespaces/2/methods/1/cases/0/args: lacks annotationId, which \
     mark.delete requires";
  surface specs ~why:"gives an argument its parameter's schema refuses"
    "namespaces/7/methods/4/cases/0/args/annotationId"
    (fun _ -> `Int 5)
    "/namespaces/7/methods/4/cases/0/args/annotationId: is none of the anyOf";
  surface specs ~why:"misspells a member of an object argument"
    "namespaces/5/methods/0/cases/1/args/options"
    (members
       (List.map (function
         | "limit", value -> ("limmit", value)
         | member -> member)))
    "/namespaces/5/methods/0/cases/1/args/options/limmit: is a member the \
     schema does not state";
  surface specs ~why:"gives an argument below its scalar's minimum"
    "namespaces/6/methods/1/cases/1/args/stallDeadlineMs"
    (fun _ -> `Int (-1))
    "/namespaces/6/methods/1/cases/1/args/stallDeadlineMs: is less than 0";
  changed specs ~why:"has a fixture name itself through another"
    "client/state-units.json" "fixtures/context"
    (fun _ -> `Assoc [ ("$fixture", `String "generation") ])
    "/cases/34/steps/1/args/params/context: names the fixture generation, \
     which names itself";
  changed specs ~why:"invokes a unit's member with an argument it does not take"
    "client/state-units.json" "cases/17/steps/0/args"
    (members (fun args -> args @ [ ("contextWindow", `Int 500) ]))
    "/cases/17/steps/0/args/contextWindow: is no parameter of \
     gather.gatherAnnotation";
  changed specs ~why:"invokes a unit's member without an argument it requires"
    "client/state-units.json" "cases/17/steps/0/args"
    (members (List.remove_assoc "annotationId"))
    "/cases/17/steps/0/args: lacks annotationId, which gather.gatherAnnotation \
     requires";
  changed specs
    ~why:"invokes a unit's member with an argument its schema refuses"
    "client/state-units.json" "cases/17/steps/0/args/annotationId"
    (fun _ -> `Int 5)
    "/cases/17/steps/0/args/annotationId: is 5, which is no string";
  changed specs
    ~why:"invokes a unit's member with null where its parameter is not absent"
    "client/state-units.json" "cases/7/steps/0/args/annotationId"
    (fun _ -> `Null)
    "/cases/7/steps/0/args/annotationId: is null, which is no string";
  refused specs
    ~why:"has a case invoke a unit's member with what is the SDK's own"
    (fun copied ->
      rewrite
        (Filename.concat copied "client/surface.json")
        (update
           [
             "stateUnits";
             "4";
             "members";
             "1";
             "gives";
             "signature";
             "parameters";
             "0";
           ]
           (members (List.remove_assoc "schema"))))
    "client/state-units.json: /cases/7/steps/0/args/annotationId: is the SDK's \
     own, which a case cannot give";
  surface specs ~why:"names a fixture the table has not"
    "namespaces/5/methods/0/cases/0/args/context"
    (fun _ -> `Assoc [ ("$fixture", `String "nowhere") ])
    "/namespaces/5/methods/0/cases/0/args/context: names the fixture nowhere, \
     which the table has not";
  surface specs ~why:"gives a callback as an argument"
    "namespaces/8/methods/5/cases/0/args"
    (members (fun args -> args @ [ ("onStatus", `Assoc []) ]))
    "/namespaces/8/methods/5/cases/0/args/onStatus: is a callback, which a \
     case cannot give";
  surface specs ~why:"has a method take a parameter twice"
    "namespaces/8/methods/4/parameters"
    (function
      | `List [ parameter ] -> `List [ parameter; parameter ]
      | _ -> failwith "job.status takes one parameter")
    "/namespaces/8/methods/4/parameters/1: names jobId, which job.status takes \
     already";
  surface specs ~why:"has a method say nothing of what it returns"
    "namespaces/0/methods/0"
    (members (List.remove_assoc "returns"))
    "/namespaces/0/methods/0: lacks the member \"returns\"";
  surface specs ~why:"has a signal return a value"
    "namespaces/1/methods/17/returns"
    (fun _ -> `Assoc [ ("$ref", `String (id "ResourceId.json")) ])
    "/namespaces/1/methods/17/returns: is a value, and browse.click is a \
     signal, whose shape says all it gives: it returns null";
  surface specs ~why:"has a count return a value"
    "namespaces/7/methods/0/returns"
    (fun _ ->
      `Assoc
        [ ("type", `String "integer"); ("description", `String "How many.") ])
    "/namespaces/7/methods/0/returns: is a value, and beckon.attention is a \
     count, whose shape says all it gives: it returns null";
  surface specs ~why:"has a stream return nothing"
    "namespaces/4/methods/0/returns"
    (fun _ -> `Null)
    "/namespaces/4/methods/0/returns: is null, and gather.annotation is a \
     stream, which gives a value";
  surface specs ~why:"has an events method return another channel's schema"
    "namespaces/8/methods/0/returns/$ref"
    (fun _ -> `String (id "JobFailCommand.json"))
    "/namespaces/8/methods/0/returns: is not \
     https://xynolog.org/protocol/JobQueuedEvent.json, the schema \
     bus/registry.json states for the channel job.queued observes";
  let bounds = "bus/reply-bounds.json" in
  changed specs ~why:"names a reply of no operation" bounds "replies"
    (members (fun replies ->
         replies
         @ [
             ( "nowhere:requested",
               `Assoc [ ("response", `Assoc [ ("grows", `String "always") ]) ]
             );
           ]))
    "/replies/nowhere:requested: names nowhere:requested, which is no \
     operation of the registry";
  refused specs ~why:"leaves a reply's unbounded list unnamed"
    (fun copied ->
      rewrite
        (Filename.concat copied bounds)
        (update [ "replies" ]
           (members (List.remove_assoc "browse:entity-types-requested"))))
    "components/schemas/GetEntityTypesResponse.json: /properties/entityTypes: \
     is response.entityTypes of browse:entity-types-result, a list with no \
     bound that bus/reply-bounds.json does not name: bound it in its schema, \
     or say what bounds it";
  refused specs ~why:"names a list its schema bounds"
    (fun copied ->
      rewrite
        (Filename.concat copied "components/schemas/GetEntityTypesResponse.json")
        (update
           [ "properties"; "entityTypes" ]
           (members (fun schema -> schema @ [ ("maxItems", `Int 64) ]))))
    "bus/reply-bounds.json: \
     /replies/browse:entity-types-requested/response.entityTypes: names \
     response.entityTypes of browse:entity-types-result, which its schema \
     bounds";
  changed specs ~why:"names a list a reply does not carry" bounds
    "replies/browse:entity-types-requested"
    (members (fun entries ->
         entries
         @ [ ("response.nothing", `Assoc [ ("grows", `String "always") ]) ]))
    "/replies/browse:entity-types-requested/response.nothing: names \
     response.nothing of browse:entity-types-result, which it does not carry";
  changed specs ~why:"bounds a list by no member of its request" bounds
    "replies/match:search-requested/response/by"
    (fun _ -> `String "nowhere")
    "/replies/match:search-requested/response/by: is nowhere, which is no \
     member of match:search-requested";
  changed specs ~why:"bounds a list by a member that bounds nothing" bounds
    "replies/match:search-requested/response/by"
    (fun _ -> `String "useSemanticScoring")
    "/replies/match:search-requested/response/by: is useSemanticScoring of \
     match:search-requested, which is neither an array nor an integer with a \
     maximum";
  changed specs ~why:"has a description of an offset leave its unit out"
    "components/schemas/TextPositionSelector.json"
    "properties/start/description"
    (fun _ -> `String "Where the selection starts.")
    "/properties/start: carries an offset or a length of text, and its \
     description does not say that it counts code points";
  changed specs ~why:"loses a member that carries an offset"
    "client/types/TextSpan.json" "$defs/Members/allOf/0/properties"
    (members (List.remove_assoc "end"))
    "/$defs/Members/allOf/0/properties/end: is listed as carrying an offset or \
     a length of text, and is not there";
  changed specs ~why:"has a new description speak of characters"
    "components/schemas/UnitCursor.json" "properties/size/description"
    (fun _ -> `String "How many characters the piece was cut at.")
    "/properties/size: speaks of an offset or of characters and does not say \
     \"code points\": \"How many characters the piece was cut at.\"";
  refused specs ~why:"spells a counting table's astral characters in ASCII"
    (fun copied ->
      let path = Filename.concat copied "text/offset-cases.json" in
      write path
        (escape_astral (In_channel.with_open_bin path In_channel.input_all)))
    "text/offset-cases.json: /: counts offsets or lengths of text and has no \
     character outside the Basic Multilingual Plane to tell code points from \
     UTF-16 code units or bytes";
  changed specs ~why:"lets a cancellation name more than a job"
    "components/schemas/JobCancelRequest.json" "properties"
    (members (fun m ->
         m @ [ ("reason", `Assoc [ ("type", `String "string") ]) ]))
    "/: is not exactly one required jobId, a JobId: a cancellation names one \
     job, and selects no other way";
  changed specs ~why:"tells filters apart by another property"
    "components/schemas/JobFilter.json" "discriminator/propertyName"
    (fun _ -> `String "kind")
    "/discriminator: JobFilter is not told apart by jobType";
  changed specs ~why:"maps a job description to no yield job"
    "components/schemas/JobCreateCommand.json" "discriminator/mapping"
    (members (List.remove_assoc "yield"))
    "/discriminator/mapping: JobCreateCommand is told apart by [mark], JobType \
     is [mark,yield]";
  changed specs ~why:"has a filter select the other job type"
    "components/schemas/MarkJobFilter.json" "properties/jobType/enum"
    (fun _ -> `List [ `String "yield" ])
    "/properties/jobType: JobFilter selects \
     https://xynolog.org/protocol/MarkJobFilter.json by jobType mark, whose \
     jobType is [yield]";
  changed specs ~why:"opens a motivation's parameters"
    "components/schemas/HighlightingJobParams.json" ""
    (members (List.remove_assoc "additionalProperties"))
    "/: a highlighting job's parameters are open: a parameter it does not take \
     would be accepted";
  changed specs ~why:"announces a mark job with other parameters"
    "components/schemas/MarkJobQueuedEvent.json" "properties/params/$ref"
    (fun _ -> `String (id "GenerationJobParams.json"))
    "/properties/params: a mark job is announced with \
     https://xynolog.org/protocol/GenerationJobParams.json, and created with \
     https://xynolog.org/protocol/MarkJobParams.json";
  changed specs ~why:"states a yield job's parameters in three parts"
    "components/schemas/GenerationJobParams.json" "allOf"
    (function
      | `List parts -> `List (parts @ [ `Assoc [ ("type", `String "object") ] ])
      | _ -> failwith "no allOf")
    "/allOf: a yield job's parameters are not stated as what is asked for and \
     its input";
  changed specs ~why:"gives a yield job an input beside its context"
    "components/schemas/GenerationJobParams.json" "allOf/1/properties"
    (members (fun m -> m @ [ ("more", `Assoc [ ("type", `String "string") ]) ]))
    "/allOf/1: a yield job's input is [context,more], and an announcement \
     leaves out exactly its context";
  changed specs ~why:"opens a filter" "components/schemas/YieldJobFilter.json"
    ""
    (members (List.remove_assoc "additionalProperties"))
    "/: a yield filter is open";
  changed specs ~why:"has a filter name what a job does not have"
    "components/schemas/YieldJobFilter.json" "properties"
    (members (fun m ->
         m @ [ ("nowhere", `Assoc [ ("type", `String "string") ]) ]))
    "/properties/nowhere: a yield filter names nowhere, which a yield job \
     description does not have";
  changed specs ~why:"lets a mark filter leave out its motivation"
    "components/schemas/MarkJobFilter.json" "properties/params/required"
    (fun _ -> `List [])
    "/properties/params: a mark filter does not always state its motivation";
  changed specs ~why:"gives JobResult a discriminant"
    "components/schemas/JobResult.json" ""
    (members (fun m ->
         m @ [ ("discriminator", `Assoc [ ("propertyName", `String "kind") ]) ]))
    "/discriminator: JobResult has a discriminant of its own";
  changed specs ~why:"opens a result" "components/schemas/JobDeclined.json" ""
    (members (List.remove_assoc "additionalProperties"))
    "/: https://xynolog.org/protocol/JobDeclined.json is open: another result \
     would decode as it";
  changed specs ~why:"has a result state a kind"
    "components/schemas/JobDeclined.json" "properties"
    (members (fun m -> m @ [ ("kind", `Assoc [ ("type", `String "string") ]) ]))
    "/properties/kind: https://xynolog.org/protocol/JobDeclined.json states a \
     kind";
  refused specs ~why:"has two results require one member"
    (fun copied ->
      rewrite (Filename.concat copied "components/schemas/JobDeclined.json")
        (fun json ->
          json
          |> update [ "properties" ]
               (members (fun m ->
                    m @ [ ("found", `Assoc [ ("type", `String "integer") ]) ]))
          |> update [ "required" ] (function
            | `List r -> `List (r @ [ `String "found" ])
            | _ -> failwith "no required")))
    "components/schemas/JobDeclined.json: /required: \
     https://xynolog.org/protocol/JobDeclined.json and \
     https://xynolog.org/protocol/MarkJobDone.json both require found";
  changed specs ~why:"has JobResult hold a result no verb reports"
    "components/schemas/JobResult.json" "oneOf"
    (function
      | `List rs ->
          `List (rs @ [ `Assoc [ ("$ref", `String (id "JobPending.json")) ] ])
      | _ -> failwith "no oneOf")
    "/oneOf: the verbs report \
     [https://xynolog.org/protocol/JobDeclined.json,https://xynolog.org/protocol/MarkJobDone.json,https://xynolog.org/protocol/YieldJobDone.json], \
     JobResult is \
     [https://xynolog.org/protocol/MarkJobDone.json,https://xynolog.org/protocol/YieldJobDone.json,https://xynolog.org/protocol/JobDeclined.json,https://xynolog.org/protocol/JobPending.json]";
  changed specs ~why:"names a storage record that is no schema"
    "jobs/storage.json" "bucket/record"
    (fun _ -> `String "Nowhere")
    "/bucket/record: the storage layout's record Nowhere is not a schema";
  changed specs ~why:"leaves a job type without a retry budget"
    "jobs/budgets.json" "budgets"
    (function
      | `List (first :: _) -> `List [ first ] | _ -> failwith "no budgets")
    "/budgets: the budgets are of [mark], and JobType is [mark,yield]";
  changed specs ~why:"has the dispatcher refuse an operation it does not answer"
    "dispatcher/messages.json" "refusals/0/operation"
    (fun _ -> `String "job:limits-requested")
    "/refusals/0/operation: job:limits-requested is no operation the registry \
     says the dispatcher answers";
  changed specs ~why:"has the Archivist refuse an operation it does not answer"
    "archivist/messages.json" "refusals/0/operations/0"
    (fun _ -> `String "job:create")
    "/refusals/0/operations/0: job:create is no operation the registry says \
     the Archivist answers";
  changed specs ~why:"has a relationship of the graph go to no kind of node"
    "graph/schema.json" "relationships/taggedAs/to"
    (fun _ -> `String "tag")
    "/relationships/taggedAs/to: tag is no kind of node under /nodes";
  changed specs ~why:"makes the graph's stub of no kind of node"
    "graph/schema.json" "stub/node"
    (fun _ -> `String "link")
    "/stub/node: link is no kind of node under /nodes";
  changed specs ~why:"gives a node of the graph a row the codec does not store"
    "graph/schema.json" "nodes/annotation/row"
    (fun _ -> `String "annotationRead")
    "/nodes/annotation/row: annotationRead is no part of \
     graph/codec-cases.json that stores a row";
  changed specs ~why:"keys a node of the graph by a property its row may lack"
    "graph/schema.json" "nodes/resource/key"
    (fun _ -> `String "storageUri")
    "/nodes/resource/key: storageUri is no property resourceStored's row \
     requires";
  changed specs ~why:"finds a node of the graph by a property its row lacks"
    "graph/schema.json" "nodes/annotation/lookups/0"
    (fun _ -> `String "resource")
    "/nodes/annotation/lookups/0: resource is no property of \
     annotationStored's row";
  changed specs ~why:"finds a node without a row by more than its key"
    "graph/schema.json" "nodes/entityType/lookups"
    (fun _ -> `List [ `String "label" ])
    "/nodes/entityType/lookups: a node without a row holds its key alone, and \
     is found by nothing else";
  changed specs ~why:"marks the graph's stub by a property of its row"
    "graph/schema.json" "stub/marker"
    (fun _ -> `String "archived")
    "/stub/marker: archived is a property of resourceStored's row";
  changed specs ~why:"has the Archivist answer an error of a route it lacks"
    "archivist/messages.json" "http/0/method"
    (fun _ -> `String "PUT")
    "/http/0/route: PUT /events is no operation of the Archivist's API";
  changed specs ~why:"has the Smelter refuse an operation it does not answer"
    "smelter/messages.json" "refusals/0/operation"
    (fun _ -> `String "job:create")
    "/refusals/0/operation: job:create is no operation the registry says the \
     Smelter answers";
  changed specs ~why:"states an answer format for no motivation"
    "worker/answer-formats.json" "formats/5/name"
    (fun _ -> `String "counting")
    "/formats: the formats are \
     [assessing,commenting,counting,highlighting,linking,tagging], and \
     MarkJobParams' motivations and the count are \
     [assessing,commenting,count,highlighting,linking,tagging]";
  changed specs ~why:"has a highlighting prompt ask for comments"
    "worker/prompt-cases.json" "cases/0/asks"
    (fun _ -> `String "commenting")
    "/cases/0/asks: a highlighting job's prompt asks for commenting";
  changed specs ~why:"has a prompt ask for a format the table lacks"
    "worker/prompt-cases.json" "cases/0/asks"
    (fun _ -> `String "quoting")
    "/cases/0/asks: quoting is no row of worker/answer-formats.json";
  changed specs ~why:"has a prompt that does not carry its piece"
    "worker/prompt-cases.json" "cases/0/piece"
    (fun _ -> `String "A piece no prompt carries.")
    "/cases/0/piece: the prompt does not carry its piece";
  changed specs ~why:"names two prompts alike" "worker/prompt-cases.json"
    "cases/1/name"
    (fun _ -> `String "highlighting")
    "/cases/0/name: highlighting names another case too";
  changed specs ~why:"hands a highlighting job a tag schema"
    "worker/prompt-cases.json" "cases/0"
    (members (fun case -> case @ [ ("schema", `Assoc []) ]))
    "/cases/0/schema: a highlighting job is handed no tag schema";
  changed specs ~why:"hands a tagging job no tag schema"
    "worker/prompt-cases.json" "cases/15"
    (members (List.filter (fun (name, _) -> name <> "schema")))
    "/cases/15: a tagging job is handed the tag schema its schemaId names, and \
     this one is not";
  changed specs ~why:"has a linking prompt about a type its job lacks"
    "worker/prompt-cases.json" "cases/10/about"
    (fun _ -> `String "Planet")
    "/cases/10/about: Planet is none of the job's entityTypes";
  changed specs ~why:"has a highlighting prompt about an entity type"
    "worker/prompt-cases.json" "cases/0"
    (members (fun case -> case @ [ ("about", `String "Person") ]))
    "/cases/0/about: a highlighting prompt is about no entity type or category";
  refused specs ~why:"has a JobType the worker's words name no job by"
    (fun copied ->
      rewrite
        (Filename.concat copied "components/schemas/JobType.json")
        (update [ "enum" ] (function
          | `List types -> `List (types @ [ `String "gather" ])
          | _ -> failwith "no enum")))
    "worker/messages.json: /job: it names jobs of [mark,yield], and JobType is \
     [gather,mark,yield]";
  changed specs ~why:"shares a boot refusal no service states"
    "worker/messages.json" "boot/0/bootRefusal"
    (fun _ -> `String "unheard-of")
    "/boot/0/bootRefusal: unheard-of is no refusal of \
     service-environment/boot-refusals.json";
  changed specs ~why:"states how a provider EmbeddingConfig lacks is asked"
    "embedding/requests.json" "providers"
    (members (fun providers ->
         providers
         @ [
             ( "cohere",
               `Assoc [ ("textsPerRequest", `Int 96); ("atOnce", `Int 1) ] );
           ]))
    "/providers: the providers are [cohere,ollama,voyage], and \
     EmbeddingConfig's are [ollama,voyage]";
  refused specs
    ~why:"words the dispatcher's entity-type refusal unlike the Archivist's"
    (fun copied ->
      rewrite
        (Filename.concat copied "dispatcher/messages.json")
        (update [ "refusals"; "1"; "message" ] (fun _ ->
             `String "Unknown entity type: {types}")))
    "archivist/messages.json: /refusals: the Archivist refuses an entity type \
     not registered in \"Entity type not registered: {types}\", and the \
     dispatcher in \"Unknown entity type: {types}\"";
  changed specs ~why:"selects a job by a status it does not state"
    "components/schemas/JobPending.json" "properties/status/enum"
    (fun _ -> `List [ `String "running" ])
    "/properties/status: Job's discriminator selects \
     https://xynolog.org/protocol/JobPending.json by status pending, whose \
     status is [running]";
  changed specs ~why:"has the dispatcher's document default a field"
    "dispatcher/DispatcherConfig.json" "properties/port"
    (members (fun m -> m @ [ ("default", `Int 8080) ]))
    "/properties/port/default: DispatcherConfig defaults port";
  changed specs ~why:"has a worker's document leave a field optional"
    "worker/WorkerConfig.json" "required"
    (function
      | `List r -> `List (List.filter (( <> ) (`String "port")) r)
      | _ -> failwith "no required")
    "/properties/port: WorkerConfig leaves port optional";
  refused specs ~why:"keeps a schema of what the Archivist keeps elsewhere"
    (fun copied ->
      Sys.rename
        (Filename.concat copied "components/schemas/ProfilesProjection.json")
        (Filename.concat copied "principals/ProfilesProjection.json"))
    "archivist/ProfilesProjection.json: /: ProfilesProjection has no schema in \
     specs/src/archivist or specs/src/components/schemas";
  changed specs ~why:"has ArchivistConfig default a field"
    "archivist/ArchivistConfig.json" "properties/port"
    (members (fun m -> m @ [ ("default", `Int 9000) ]))
    "/properties/port/default: ArchivistConfig defaults port";
  changed specs ~why:"has ArchivistConfig leave a field optional"
    "archivist/ArchivistConfig.json" "required"
    (function
      | `List r -> `List (List.filter (( <> ) (`String "root")) r)
      | _ -> failwith "no required")
    "/properties/root: ArchivistConfig leaves root optional";
  changed specs ~why:"gives the Archivist no yield engine"
    "components/schemas/ArchivistEngines.json" "properties/jobs/properties"
    (members (List.remove_assoc "yield"))
    "/properties/jobs: the engines' job types are [mark], JobType is \
     [mark,yield]";
  changed specs ~why:"gives the Archivist no tagging engine"
    "components/schemas/ArchivistEngines.json"
    "properties/jobs/properties/mark/properties"
    (members (List.remove_assoc "tagging"))
    "/properties/jobs/properties/mark: the engines' mark motivations are \
     [highlighting,commenting,assessing,linking], Motivation is \
     [assessing,commenting,highlighting,linking,tagging]";
  changed specs ~why:"restates an extraction's method otherwise"
    "archivist/AnchoredTextExtractedEntry.json" "properties/method"
    (fun _ -> `Assoc [ ("type", `String "string") ])
    "/properties/method: AnchoredTextEntry's method is not ExtractedText's";
  changed specs ~why:"restates the decline classes otherwise"
    "archivist/AnchoredTextDeclinedEntry.json" "properties/declined"
    (fun _ -> `Assoc [ ("type", `String "string") ])
    "/properties/declined: AnchoredTextEntry's declined is not \
     ExtractionDeclined's";
  changed specs ~why:"names a kind with no schema" "identifiers/kinds.json"
    "kinds/2/schema"
    (fun _ -> `String "TaskId")
    "/kinds/2/schema: TaskId has no schema: components/schemas/TaskId.json";
  changed specs ~why:"gives a kind's schema more than its rule"
    "components/schemas/ResourceId.json" ""
    (members (fun m -> m @ [ ("minLength", `Int 1) ]))
    "/: ResourceId must be exactly a string with a pattern and a description; \
     it has [$id,$schema,description,minLength,pattern,type]";
  changed specs ~why:"has a kind accept what its pattern does not"
    "identifiers/kinds.json" "kinds/0/accepts"
    (function
      | `List l ->
          `List
            (l
            @ [ `Assoc [ ("id", `String "a b"); ("why", `String "a break") ] ])
      | _ -> failwith "not a list")
    "/kinds/0/accepts/4/id: ResourceId accepts \"a b\" (a break), and its \
     pattern ^[A-Za-z0-9_-]{1,128}$ does not";
  changed specs ~why:"has a kind refuse what its pattern accepts"
    "identifiers/kinds.json" "kinds/0/refuses"
    (function
      | `List l ->
          `List
            (l
            @ [ `Assoc [ ("id", `String "abc"); ("why", `String "a break") ] ])
      | _ -> failwith "not a list")
    "/kinds/0/refuses/14/id: ResourceId refuses \"abc\" (a break), and its \
     pattern ^[A-Za-z0-9_-]{1,128}$ accepts it";
  changed specs ~why:"has a kind accept a DID with a no-break space"
    "identifiers/kinds.json" "kinds/3/accepts"
    (function
      | `List l ->
          `List
            (l
            @ [
                `Assoc
                  [
                    ("id", `String "did:web:x:a b"); ("why", `String "a break");
                  ];
              ])
      | _ -> failwith "not a list")
    "/kinds/3/accepts/4/id: UserId accepts \"did:web:x:a b\" (a break), and \
     its pattern ^did:\\S+$ does not";
  changed specs ~why:"names a property for two kinds" "identifiers/kinds.json"
    "kinds/2/properties"
    (function
      | `List l -> `List (l @ [ `String "resourceId" ])
      | _ -> failwith "not a list")
    "/kinds/2/properties/1: resourceId is named for two kinds: ResourceId and \
     JobId";
  changed specs ~why:"has a property among others that is named for a kind"
    "identifiers/kinds.json" "others"
    (function
      | `List l ->
          `List
            (l
            @ [
                `Assoc
                  [
                    ("property", `String "resourceId");
                    ("docs", `String "a break");
                  ];
              ])
      | _ -> failwith "not a list")
    "/others/14/property: resourceId is among `others` and is named for \
     ResourceId";
  changed specs ~why:"has a resource's id refer to an annotation's"
    "components/schemas/BeckonFocusEvent.json" "properties/resourceId/$ref"
    (fun _ -> `String (id "AnnotationId.json"))
    "/properties/resourceId: resourceId must refer to ResourceId, and refers \
     to AnnotationId";
  changed specs
    ~why:"has a property named like an id that is classified nowhere"
    "components/schemas/BeckonFocusEvent.json" "properties"
    (members (fun m ->
         m @ [ ("widgetId", `Assoc [ ("type", `String "string") ]) ]))
    "/properties/widgetId: widgetId is named like an id and is classified \
     nowhere: name it for a kind, or among `others`, in \
     specs/src/identifiers/kinds.json";
  changed specs ~why:"has a property among others refer to a kind"
    "components/schemas/SessionJoinedEvent.json" "properties/connectionId"
    (fun _ -> `Assoc [ ("$ref", `String (id "ResourceId.json")) ])
    "/properties/connectionId: connectionId is among `others` and refers to \
     ResourceId";
  changed specs ~why:"names a property no schema has" "identifiers/kinds.json"
    "kinds/0/properties"
    (function
      | `List l -> `List (l @ [ `String "ghostResourceId" ])
      | _ -> failwith "not a list")
    "/: names ghostResourceId, which no schema of the spec has";
  changed specs ~why:"has a path parameter refer to no kind"
    "archivist/paths/get-resource-events.json" "get/parameters/0/schema"
    (fun _ -> `Assoc [ ("type", `String "string") ])
    "/get/parameters/0: the path parameter {resourceId} must refer to a kind \
     of id";
  changed specs ~why:"has a pattern ECMA-262 and ocaml-re read apart"
    "components/schemas/ResourceId.json" "pattern"
    (fun _ -> `String "^\\p{L}+$")
    "/pattern: the pattern escapes p, which ECMA-262 and ocaml-re read apart";
  changed specs ~why:"lists anchor methods that are not AnchorMethod's"
    "service-telemetry/telemetry.json" "metrics/27/attributes/1/values"
    (function
      | `List l -> `List (List.filteri (fun i _ -> i < List.length l - 1) l)
      | _ -> failwith "not a list")
    "/metrics/27/attributes/1/values: lists anchor.method values \
     [\"unique-match\",\"context-recovered\",\"first-of-many\"], not \
     AnchorMethod's \
     [\"unique-match\",\"context-recovered\",\"first-of-many\",\"fuzzy-match\"]";
  changed specs ~why:"bounds a discovered port otherwise than an endpoint's"
    "discovery/DiscoveredKB.json" "properties/port/maximum"
    (fun _ -> `Int 65536)
    "/properties/port/maximum: is 65536, where \
     components/schemas/HttpEndpoint.json states 65535";
  changed specs ~why:"names a vocabulary errors/codes.json does not state"
    "control/ErrorData.json" "properties/code/x-xynolog-codes"
    (fun _ -> `String "controls")
    "/properties/code: x-xynolog-codes names \"controls\", which is no \
     vocabulary of errors/codes.json (busRequest, transport, job, session, \
     signIn, kbIdentity, spanRefusal, control)";
  changed specs ~why:"tells an emission's states apart by a status of its own"
    "conformance/ConformanceDriverLine.json"
    "oneOf/10/properties/emission/properties/state/oneOf/2/properties/status/const"
    (fun _ -> `String "errored")
    "/oneOf/10/properties/emission/properties/state/oneOf: is told apart by \
     status [\"pending\",\"ready\",\"errored\"], not, one branch each and in \
     order, CacheStatus's [\"pending\",\"ready\",\"failed\"]";
  changed specs
    ~why:"lists inference providers that are not InferenceProvider's"
    "service-telemetry/telemetry.json" "spans/12/attributes/0/values"
    (function
      | `List l -> `List (List.filteri (fun i _ -> i < List.length l - 1) l)
      | _ -> failwith "not a list")
    "/spans/12/attributes/0/values: lists inference.provider values \
     [\"anthropic\",\"google\",\"llamacpp\",\"ollama\",\"openai\",\"together\"], \
     not InferenceProvider's \
     [\"anthropic\",\"google\",\"llamacpp\",\"ollama\",\"openai\",\"together\",\"vllm\"]";
  changed specs ~why:"has a fuzzy match name no search"
    "annotations/reconcile-cases.json" "cases/22/reconciled"
    (members (List.remove_assoc "matchQuality"))
    "/cases/22/reconciled: is none of the anyOf";
  changed specs ~why:"gives a quote a member QuotedText does not state"
    "annotations/reconcile-cases.json" "cases/0/quoted"
    (members (fun m -> m @ [ ("extra", `String "x") ]))
    "/cases/0/quoted/extra: is a member the schema does not state";
  changed specs ~why:"tells a reconciled span apart by a word of its own"
    "client/types/ReconciledSpan.json"
    "oneOf/3/allOf/1/properties/anchorMethod/enum"
    (fun _ -> `List [ `String "fuzzy" ])
    "/oneOf: is told apart by anchorMethod \
     [\"unique-match\",\"context-recovered\",\"first-of-many\",\"fuzzy\"], \
     not, one branch each and in order, AnchorMethod's \
     [\"unique-match\",\"context-recovered\",\"first-of-many\",\"fuzzy-match\"]";
  changed specs ~why:"states the absent reasons out of their order"
    "client/discovery-absent.json" "reasons"
    (function
      | `List (a :: b :: rest) -> `List (b :: a :: rest)
      | _ -> failwith "not a list")
    "/reasons: states \
     [\"not-json\",\"not-found\",\"invalid\",\"unsupported-version\",\"unreadable\"], \
     not, each once and in order, DiscoveryAbsentReason's \
     [\"not-found\",\"not-json\",\"invalid\",\"unsupported-version\",\"unreadable\"]";
  changed specs ~why:"opens a client type" "client/types/SearchState.json" ""
    (members (List.remove_assoc "additionalProperties"))
    "/: is an open object: a client type is closed, by additionalProperties, \
     or by unevaluatedProperties where it is composed";
  changed specs ~why:"names open members and leaves them open"
    "client/types/ReconciledSpan.json" "oneOf/0"
    (members (List.remove_assoc "unevaluatedProperties"))
    "/oneOf/0/allOf/0/$ref: names \
     https://xynolog.org/protocol/TextSpan.json#/$defs/Members, open members, \
     and nothing here closes them";
  changed specs ~why:"names a refusal reason no frame is refused for"
    "client/types/FrameRefusalReason.json" "enum"
    (function
      | `List l -> `List (l @ [ `String "stale" ]) | _ -> failwith "not a list")
    "/enum/2: FrameRefusalReason names \"stale\", and no case of \
     frame-cases.json is refused for it";
  refused specs ~why:"has the surface lose a reader the cases read"
    (fun copied ->
      rewrite
        (Filename.concat copied "client/surface.json")
        (update [ "modules"; "0"; "members"; "3"; "member" ] (fun _ ->
             `String "commentBody")))
    "annotations/reader-cases.json: /readers/9: names the reader \
     \"commentText\", which client/surface.json's annotations module does not \
     have";
  changed specs ~why:"lists the builders out of order"
    "annotations/builder-cases.json" "builders"
    (function `List l -> `List (List.rev l) | _ -> failwith "not a list")
    "/builders: builders \
     [\"annotationOfResource\",\"annotationOfSpan\",\"reconcile\"] are not \
     client/surface.json's other annotations functions \
     [\"reconcile\",\"annotationOfSpan\",\"annotationOfResource\"]";
  changed specs ~why:"gives DeviceCode a member UserAction does not carry"
    "client/types/DeviceCode.json" "properties"
    (members (fun m ->
         m @ [ ("interval", `Assoc [ ("type", `String "integer") ]) ]))
    "/properties: DeviceCode's members \
     [\"userCode\",\"verificationUri\",\"verificationUriComplete\",\"expiresIn\",\"interval\"] \
     are not UserAction's less requestId \
     [\"verificationUri\",\"verificationUriComplete\",\"userCode\",\"expiresIn\"]";
  changed specs ~why:"types a DeviceCode member otherwise"
    "client/types/DeviceCode.json" "properties/userCode/type"
    (fun _ -> `String "integer")
    "/properties/userCode: DeviceCode's userCode is \"integer\", and \
     UserAction's userCode is \"string\"";
  changed specs ~why:"has DeviceCode require less than UserAction"
    "client/types/DeviceCode.json" "required"
    (function
      | `List l -> `List (List.filter (( <> ) (`String "expiresIn")) l)
      | _ -> failwith "not a list")
    "/required: DeviceCode requires [\"userCode\",\"verificationUri\"], and \
     UserAction, less requestId, requires \
     [\"expiresIn\",\"userCode\",\"verificationUri\"]";
  changed specs ~why:"lists the method shapes out of order"
    "client/ClientSurface.json"
    "properties/namespaces/items/properties/methods/items/properties/shape/enum"
    (function `List l -> `List (List.rev l) | _ -> failwith "not a list")
    "/properties/namespaces/items/properties/methods/items/properties/shape/enum: \
     the method shapes \
     [\"claims\",\"events\",\"count\",\"signal\",\"cache\",\"upload\",\"delegation\",\"stream\",\"promise\"] \
     are not, in order, the shapes it states \
     [\"promise\",\"stream\",\"delegation\",\"upload\",\"cache\",\"signal\",\"count\",\"events\",\"claims\"]";
  changed specs ~why:"keeps another value for a persisted query"
    "session/SessionCacheCases.json"
    "properties/cases/items/oneOf/0/properties/kept/items/properties/value/$ref"
    (fun _ -> `String (id "Annotation.json"))
    "/properties/cases/items/oneOf/0/properties/kept/items/properties/value: \
     keeps {\"$ref\":\"https://xynolog.org/protocol/Annotation.json\"} for \
     \"resource\", and refresh.json's value is \
     {\"$ref\":\"https://xynolog.org/protocol/ResourceDescriptor.json\"}";
  changed specs ~why:"leaves a security scheme undescribed" "openapi.json"
    "components/securitySchemes/bearerAuth/description"
    (fun _ -> `String " ")
    "/components/securitySchemes/bearerAuth: securitySchemes.bearerAuth: has \
     no description — the claims a credential must carry are protocol";
  changed specs ~why:"names a security scheme nothing defines"
    "paths/emit-on-bus.json" "post/security"
    (fun _ -> `List [ `Assoc [ ("nobody", `List []) ] ])
    "/post/security: POST /bus/emit: names security scheme \"nobody\", which \
     components.securitySchemes does not define";
  changed specs ~why:"declares no 500" "paths/emit-on-bus.json" "post/responses"
    (members (List.remove_assoc "500"))
    "/post/responses: POST /bus/emit: declares no 500";
  changed specs ~why:"takes a body and declares no 400" "paths/emit-on-bus.json"
    "post/responses"
    (members (List.remove_assoc "400"))
    "/post/responses: POST /bus/emit: takes a request body and declares no 400";
  changed specs ~why:"leaves a JSON body unbounded" "paths/emit-on-bus.json"
    "post/x-xynolog-limits"
    (members (List.remove_assoc "maxBodyBytes"))
    "/post: POST /bus/emit: takes a JSON body and states no \
     x-xynolog-limits.maxBodyBytes";
  changed specs ~why:"takes a JSON body and declares no 413"
    "paths/emit-on-bus.json" "post/responses"
    (members (List.remove_assoc "413"))
    "/post/responses: POST /bus/emit: takes a JSON body and declares no 413";
  changed specs ~why:"protects an operation and declares no 401"
    "paths/emit-on-bus.json" "post/responses"
    (members (List.remove_assoc "401"))
    "/post/responses: POST /bus/emit: is protected and declares no 401";
  refused specs ~why:"challenges no one"
    (fun copied ->
      rewrite
        (Filename.concat copied "components/responses/Unauthorized.json")
        (update [] (members (List.remove_assoc "headers"))))
    "paths/emit-on-bus.json: /post/responses/401: POST /bus/emit 401: carries \
     no WWW-Authenticate header";
  refused specs ~why:"refuses a caller without saying when to come back"
    (fun copied ->
      rewrite
        (Filename.concat copied "components/responses/TooManyRequests.json")
        (update [] (members (List.remove_assoc "headers"))))
    "paths/emit-on-bus.json: /post/responses/429: POST /bus/emit 429: carries \
     no Retry-After header";
  changed specs ~why:"gives an error no body" "paths/emit-on-bus.json"
    "post/responses/400"
    (members (List.remove_assoc "content"))
    "/post/responses/400: POST /bus/emit 400: declares no body";
  changed specs ~why:"gives an error a body that is not JSON"
    "paths/emit-on-bus.json" "post/responses/400/content"
    (members (List.map (fun (_, media) -> ("text/plain", media))))
    "/post/responses/400: POST /bus/emit 400: an error body is text/plain, not \
     application/json";
  refused specs ~why:"gives an error a body that is not ErrorResponse"
    (fun copied ->
      rewrite
        (Filename.concat copied "paths/emit-on-bus.json")
        (update
           [
             "post";
             "responses";
             "400";
             "content";
             "application/json";
             "schema";
             "$ref";
           ] (fun _ -> `String (id "BusEmitRequest.json"))))
    "paths/emit-on-bus.json: /post/responses/400: POST /bus/emit 400: an error \
     body is not ErrorResponse, or a refinement of it";
  refused specs ~why:"writes a stream's event names nowhere"
    (fun copied ->
      rewrite
        (Filename.concat copied "components/schemas/BusStreamMessage.json")
        (update []
           (members
              (List.map (fun (k, v) ->
                   ((if k = "oneOf" then "anyOf" else k), v))))))
    "paths/subscribe-to-bus.json: /post/responses/200: POST /bus/subscribe 200 \
     text/event-stream: the stream schema is not a oneOf of its messages, so \
     its event names are written nowhere";
  changed specs ~why:"gives a role a coefficient shaped otherwise"
    "paths/emit-on-bus.json"
    "post/x-xynolog-limits/emitsPerPrincipal/roles/xynolog-service"
    (fun _ -> `Int 5)
    "/post/x-xynolog-limits/emitsPerPrincipal/roles/xynolog-service: POST \
     /bus/emit: x-xynolog-limits.emitsPerPrincipal.roles.xynolog-service is 5, \
     neither \"unlimited\" nor shaped like the baseline";
  changed specs ~why:"gives a role no token carries a coefficient"
    "paths/subscribe-to-bus.json"
    "post/x-xynolog-limits/streamsPerPrincipal/roles"
    (fun _ -> `Assoc [ ("xynolog-admin", `String "unlimited") ])
    "/post/x-xynolog-limits/streamsPerPrincipal/roles/xynolog-admin: POST \
     /bus/subscribe: x-xynolog-limits.streamsPerPrincipal.roles names \
     xynolog-admin, which is no role of principals/roles.json";
  changed specs ~why:"states a limit that is not positive" "openapi.json"
    "x-xynolog-limits/requestHeadSeconds"
    (fun _ -> `Int 0)
    "/x-xynolog-limits/requestHeadSeconds: the document: \
     x-xynolog-limits.requestHeadSeconds is 0, not a positive integer or a \
     baseline with coefficients by role";
  refused specs ~why:"holds a read longer than its caller waits"
    (fun copied ->
      rewrite
        (Filename.concat copied "archivist/paths/resource-anchored-text.json")
        (update [ "get"; "x-xynolog-limits"; "settleWaitSeconds" ] (fun _ ->
             `Int 1000)))
    "archivist/openapi.json: /: waits that nest: settleWaitSeconds (1000) is \
     not shorter than readSeconds (20): a caller stops waiting before the \
     Archivist answers a read it holds";
  changed specs ~why:"calls the Archivist longer than a client waits"
    "archivist/openapi.json" "x-xynolog-limits/connectSeconds"
    (fun _ -> `Int 1000)
    "/: waits that nest: connectSeconds (1000) and readSeconds (20) together \
     are not shorter than client/timing.json httpRequestTimeoutMs (30000): a \
     client stops waiting before the gateway answers a read the Archivist does \
     not";
  refused specs ~why:"forgets an emit's key before its attempts end"
    (fun copied ->
      rewrite
        (Filename.concat copied "paths/emit-on-bus.json")
        (update [ "post"; "x-xynolog-limits"; "idempotencySeconds" ] (fun _ ->
             `Int 1)))
    "openapi.json: /paths/~1bus~1emit: POST /bus/emit: holds a key for 1 s, \
     and a client's attempts at one emit may span 377.5 s (two emitRetry \
     budgets and a refreshRetry, specs/src/client/timing.json)";
  let rekey ~from ~onto =
    members
      (List.map (fun (path, item) ->
           ((if path = from then onto else path), item)))
  in
  let add_path path item =
    members (fun paths -> paths @ [ (path, `Assoc [ ("$ref", `String item) ]) ])
  in
  changed specs ~why:"joins its paths to a server with two slashes"
    "openapi.json" "servers/0/url"
    (fun _ -> `String "http://{host}:{port}/")
    "/servers/0: server http://{host}:{port}/: ends in a slash, so a path \
     joined to it begins with two";
  changed specs ~why:"names a server variable it does not define" "openapi.json"
    "servers/0/url"
    (fun _ -> `String "http://{host}:{port}/{base}")
    "/servers/0: server http://{host}:{port}/{base}: names {base}, which its \
     variables do not define";
  changed specs ~why:"defines a server variable its URL does not name"
    "openapi.json" "servers/0/variables"
    (members (fun variables ->
         variables @ [ ("base", `Assoc [ ("default", `String "x") ]) ]))
    "/servers/0/variables/base: server http://{host}:{port}: defines variable \
     \"base\", which its URL does not name";
  changed specs ~why:"ends a path in a slash" "openapi.json" "paths"
    (rekey ~from:"/api/status" ~onto:"/api/status/")
    "/paths/~1api~1status~1: path \"/api/status/\": is not \"/\" or a \"/\" \
     before each of its segments, each a whole {parameter} or holding none of \
     \"{\", \"}\", \"?\" and \"#\"";
  changed specs ~why:"has two paths that differ by a parameter's name"
    "openapi.json" "paths"
    (add_path "/resources/{rid}/jsonld" "paths/get-resource-json-ld.json")
    "/paths/~1resources~1{rid}~1jsonld: paths /resources/{id}/jsonld and \
     /resources/{rid}/jsonld: one request could match both";
  changed specs ~why:"has two paths one request could match" "openapi.json"
    "paths"
    (add_path "/resources/special/{id}" "paths/get-resource-json-ld.json")
    "/paths/~1resources~1special~1{id}: paths /resources/{id}/jsonld and \
     /resources/special/{id}: one request could match both";
  refused specs ~why:"names a path parameter its operation does not declare"
    (fun copied ->
      rewrite
        (Filename.concat copied "openapi.json")
        (update [ "paths" ]
           (rekey ~from:"/api/status" ~onto:"/api/status/{since}")))
    "paths/get-status.json: /get/parameters: GET /api/status/{since}: declares \
     no path parameter \"since\", which its path names";
  refused specs ~why:"declares a path parameter its path does not name"
    (fun copied ->
      rewrite
        (Filename.concat copied "openapi.json")
        (update [ "paths" ]
           (rekey ~from:"/resources/{id}/jsonld" ~onto:"/resources/jsonld")))
    "paths/get-resource-json-ld.json: /get/parameters: GET /resources/jsonld: \
     declares path parameter \"id\", which its path does not name";
  changed specs ~why:"declares a header twice, in another case"
    "paths/emit-on-bus.json" "post/parameters"
    (function
      | `List (first :: rest) ->
          `List
            ((first :: rest)
            @ [ update [ "name" ] (fun _ -> `String "idempotency-key") first ])
      | _ -> failwith "no parameters")
    "/post/parameters: POST /bus/emit: declares header parameter \
     \"idempotency-key\" twice";
  changed specs ~why:"gives two operations one operationId" "openapi.json"
    "paths"
    (add_path "/api/status-again" "paths/get-status.json")
    "/paths/~1api~1status-again: GET /api/status-again: has operationId \
     \"getStatus\", which GET /api/status has too";
  changed specs ~why:"keys a response by a range" "paths/get-status.json"
    "get/responses"
    (members (fun responses ->
         responses @ [ ("2XX", List.assoc "200" responses) ]))
    "/get/responses: GET /api/status: answers \"2XX\", which is no HTTP status";
  changed specs ~why:"declares no 2xx" "paths/get-status.json" "get/responses"
    (members (List.remove_assoc "200"))
    "/get/responses: GET /api/status: declares no 2xx";
  changed specs ~why:"takes a parameter and declares no 4xx"
    "paths/get-health.json" "get"
    (members (fun operation ->
         operation
         @ [
             ( "parameters",
               Yojson.Safe.from_string
                 {|[{"name":"verbose","in":"query","required":false,
                     "description":"d","schema":{"type":"boolean"}}]|}
             );
           ]))
    "/get/responses: GET /api/health: takes a credential, a parameter or a \
     body and declares no 4xx";
  changed specs ~why:"lets a client type's member be overridden that it lacks"
    "client/timing.json" "timing/25/override"
    (fun _ -> `String "JobPollOptions.nowhere")
    "/timing/25/override: nowhere is no member of the client type \
     JobPollOptions";
  changed specs ~why:"lets an owner be overridden that surface.json lacks"
    "client/timing.json" "timing/37/override"
    (fun _ -> `String "nowhereUnit.delay")
    "/timing/37/override: names nowhereUnit, which is no client type, state \
     unit or function of client/surface.json";
  changed specs ~why:"lets two rows be overridden at one place"
    "client/timing.json" "timing/26/override"
    (fun _ -> `String "JobPollOptions.everyMs")
    "/timing/26/override: jobPollTimeoutMs is overridden where jobPollMs is: \
     JobPollOptions.everyMs";
  if !failures > 0 then exit 1;
  print_endline
    "The model reads every file of Protocol, and refuses a copy with a stray \
     file, a missing table, a dangling reference, a schema without its dialect \
     or its $id, an $id within a file or twice, a schema named by its path, an \
     unknown keyword, a missing path item, a table without its schema, a \
     schema without its table, a case table off an area's top, a value its \
     schema refuses, a component the gateway does not list, a service's schema \
     it lists, a gateway route or a component that refers into a service's \
     area, Archivist's own schema among the components, a client surface case \
     whose arguments are not its method's parameters, a reply's list neither \
     bounded nor named, an offset whose unit is unsaid, a job protocol that \
     disagrees with itself, an Archivist stated short, a graph schema that \
     names what neither it nor the codec holds, an id nothing checks, a client \
     type that disagrees with what restates it, a control schema named by \
     other than its $id, a retry row its rule's sets disagree with, a shape \
     case its schema disagrees with, a server URL its variables disagree with, \
     a malformed or ambiguous path, a path parameter undeclared or unnamed, a \
     parameter or an operationId twice, a response keyed by no status, an \
     operation without the 2xx or the 4xx it owes, a timing overridden where \
     nothing is or where another is, or a protocol fact left to the code; and \
     it reads each value as the type its schema states"
