(* Protocol, as the harness reads it: the reader's model of specs/src, and
   what the harness asks of it. Every shape a case holds anything to, every
   table it reads and every name it uses of the spec's comes from here, by
   way of [Xynolog_protocol]: nothing is restated.

   A harness program runs from the repository's root, where specs/src is. *)

open Xynolog_protocol

let root = "specs/src"

let spec =
  lazy
    (if not (Sys.file_exists root && Sys.is_directory root) then
       failwith
         "the harness runs from the repository's root, and there is no \
          specs/src here";
     match Spec.load root with
     | Ok spec -> spec
     | Error refusals ->
         failwith
           (String.concat "\n"
              ("the reader refuses specs/src:"
              :: List.map Decode.error_to_string refusals)))

let spec () = Lazy.force spec

(* The value at a path of members within [json], when every step is an
   object holding the next. *)
let rec at json = function
  | [] -> Some json
  | name :: rest -> (
      match json with
      | `Assoc members ->
          Option.bind (List.assoc_opt name members) (fun value -> at value rest)
      | _ -> None)

let tables () = (spec ()).tables

(* The members of a trace as the wire carries it: client/types/TraceCarrier's
   properties, in order. *)
let trace_members () =
  match List.assoc_opt "client/types/TraceCarrier.json" (spec ()).schemas with
  | Some (Schema.Schema { properties = Some properties; _ }) ->
      List.map fst properties
  | _ -> failwith "client/types/TraceCarrier.json states no members"

(* ── Schemas ─────────────────────────────────────────────────────────── *)

let files = Hashtbl.create 64

let json_of file =
  match Hashtbl.find_opt files file with
  | Some json -> json
  | None ->
      let json =
        match Decode.parse_file ~root file with
        | Ok json -> Some json
        | Error _ -> None
      in
      Hashtbl.replace files file json;
      json

let schemas = Hashtbl.create 256

(* The schema at a place of specs/src, as [Spec.load] reads one. *)
let schema_at ({ file; pointer } : Validate.place) =
  match Hashtbl.find_opt schemas (file, pointer) with
  | Some found -> found
  | None ->
      let found =
        Option.bind (json_of file) (fun json ->
            Option.bind (Decode.at_pointer pointer json) (fun node ->
                Result.to_option
                  (Result.map fst (Decode.run ~file node Schema.decode))))
      in
      Hashtbl.replace schemas (file, pointer) found;
      found

let place_of (reference : Decode.reference) =
  let file, pointer = Decode.resolve (spec ()).ids reference in
  { Validate.file; pointer }

let schema_of reference =
  match schema_at (place_of reference) with
  | Some schema -> schema
  | None -> failwith ("specs/src names no schema at " ^ reference.target)

(* How [value] is not what [schema] says: each refusal as the pointer into
   the value and why. Empty when it is. *)
let refusals schema value =
  List.map
    (fun (pointer, why) -> (if pointer = "" then "/" else pointer) ^ " " ^ why)
    (Validate.errors (spec ()).ids schema_at schema value "")

(* A service's own schema (Places.service_schemas): its configuration
   document, its health, by its name. *)
let service_schema name =
  match
    List.find_opt
      (fun file -> Filename.basename file = name ^ ".json")
      (spec ()).services
  with
  | Some file -> (
      match schema_at { file; pointer = "" } with
      | Some schema -> schema
      | None -> failwith (file ^ " is no schema"))
  | None -> failwith ("specs/src has no service schema " ^ name)

(* ── Operations ──────────────────────────────────────────────────────── *)

type document = Gateway | Archivist

let document = function
  | Gateway -> (spec ()).gateway
  | Archivist -> (spec ()).archivist

(* A component schema of a document, by its name. *)
let component which name =
  match List.assoc_opt name (document which).components.schemas with
  | Some reference -> schema_of reference
  | None -> failwith ("the document has no component schema " ^ name)

(* A component schema of a document, by its name, as specs/src writes it. *)
let component_json which name =
  match List.assoc_opt name (document which).components.schemas with
  | Some reference -> (
      let { Validate.file; pointer } = place_of reference in
      match Option.bind (json_of file) (Decode.at_pointer pointer) with
      | Some json -> json
      | None -> failwith ("specs/src names no schema at " ^ reference.target))
  | None -> failwith ("the document has no component schema " ^ name)

(* How [value] is not the gateway's component schema [name]. *)
let against name value = refusals (component Gateway name) value

let find_operation which meth path : Openapi.operation option =
  Option.bind (List.assoc_opt path (document which).paths) (fun reference ->
      let item : Openapi.path_item =
        List.assoc
          (fst (Decode.resolve (spec ()).ids reference))
          (spec ()).path_items
      in
      match meth with
      | "GET" -> item.get
      | "PUT" -> item.put
      | "POST" -> item.post
      | _ -> None)

let operation which meth path =
  match find_operation which meth path with
  | Some operation -> operation
  | None -> failwith (Printf.sprintf "the document declares no %s %s" meth path)

(* [path], which [which] must declare an operation of [meth] on. *)
let path which meth path =
  ignore (operation which meth path : Openapi.operation);
  path

(* The declared response of an operation for a status, or None. *)
let response which meth path status : Openapi.response option =
  match
    List.assoc_opt (string_of_int status) (operation which meth path).responses
  with
  | None -> None
  | Some (Openapi.Inline response) -> Some response
  | Some (Openapi.Ref reference) ->
      List.assoc_opt
        (fst (Decode.resolve (spec ()).ids reference))
        (spec ()).responses

(* One of an operation's x-xynolog-limits that is a count. *)
let limit which meth path key =
  match (operation which meth path).limits with
  | Some limits -> (
      match List.assoc_opt key limits with
      | Some (Openapi.Count n) -> n
      | _ -> failwith (Printf.sprintf "%s %s states no count %s" meth path key))
  | None -> failwith (Printf.sprintf "%s %s states no limits" meth path)

(* One of an operation's x-xynolog-limits that is a limit per principal:
   its baseline, and its limit per role. *)
let principal_limit which meth path key =
  match (operation which meth path).limits with
  | Some limits -> (
      match List.assoc_opt key limits with
      | Some (Openapi.Limits per) -> (
          match (List.assoc_opt "baseline" per, List.assoc_opt "roles" per) with
          | Some baseline, Some (Openapi.Limits roles) -> (baseline, roles)
          | _ ->
              failwith
                (Printf.sprintf "%s %s's %s states no baseline and roles" meth
                   path key))
      | _ ->
          failwith
            (Printf.sprintf "%s %s states no principal limit %s" meth path key))
  | None -> failwith (Printf.sprintf "%s %s states no limits" meth path)

(* A limit that must be a count. *)
let count = function
  | Openapi.Count n -> n
  | _ -> failwith "a limit that is not a count"

(* The most items BusSubscribeRequest's array member [name] may hold: its
   maxItems. *)
let subscribe_max_items name =
  match component Gateway "BusSubscribeRequest" with
  | Xynolog_protocol_schema.Schema.Schema { properties = Some properties; _ }
    -> (
      match List.assoc_opt name properties with
      | Some (Schema { max_items = Some n; _ }) -> n
      | _ -> failwith ("BusSubscribeRequest's " ^ name ^ " states no maxItems"))
  | _ -> failwith "BusSubscribeRequest states no properties"

(* One of a document's own x-xynolog-limits that is a count. *)
let document_limit which key =
  match List.assoc_opt key (document which).limits with
  | Some (Openapi.Count n) -> n
  | _ -> failwith (Printf.sprintf "the document states no count %s" key)

(* One of a gateway security scheme's x-xynolog-limits that is a count. *)
let scheme_limit scheme key =
  match List.assoc_opt scheme (spec ()).gateway.components.security_schemes with
  | Some (Openapi.Http { limits = Some limits; _ }) -> (
      match List.assoc_opt key limits with
      | Some (Openapi.Count n) -> n
      | _ -> failwith (Printf.sprintf "%s states no count %s" scheme key))
  | _ -> failwith (Printf.sprintf "the gateway's %s states no limits" scheme)

(* The template of a document's path that [concrete] fills, with an
   operation of [meth]: the template, and each of its parameters' values by
   the parameter's name. *)
let route which meth concrete =
  let parts path = String.split_on_char '/' path in
  List.find_map
    (fun (template, _) ->
      let rec matched acc = function
        | [], [] -> Some (List.rev acc)
        | a :: rest, b :: others when String.length a > 2 && a.[0] = '{' ->
            matched
              ((String.sub a 1 (String.length a - 2), Uri.pct_decode b) :: acc)
              (rest, others)
        | a :: rest, b :: others when a = b -> matched acc (rest, others)
        | _ -> None
      in
      match matched [] (parts template, parts concrete) with
      | Some values when find_operation which meth template <> None ->
          Some (template, values)
      | _ -> None)
    (document which).paths

(* ── The bus registry ─────────────────────────────────────────────────── *)

module Registry = Tables.Bus_registry

let operation_for request =
  match
    List.find_opt
      (fun (op : Registry.operations_item) -> op.request = request)
      (tables ()).bus_registry.operations
  with
  | Some op -> op
  | None -> failwith ("the registry declares no operation " ^ request)

type carried = Schema of string | Envelope of string | Stored_event | Nothing

let carried channel =
  List.find_map
    (fun (row : Registry.channels_item) ->
      match row with
      | Schema { channel = c; schema; _ } when c = channel ->
          Some (Schema schema)
      | Envelope { channel = c; schema; _ } when c = channel ->
          Some (Envelope schema)
      | Stored_event { channel = c; _ } when c = channel -> Some Stored_event
      | (Void { channel = c; _ } | Empty { channel = c; _ }) when c = channel ->
          Some Nothing
      | _ -> None)
    (tables ()).bus_registry.channels

(* The persisted channels: the facts. *)
let facts () =
  List.filter_map
    (fun (row : Registry.channels_item) ->
      match row with Stored_event { channel; _ } -> Some channel | _ -> None)
    (tables ()).bus_registry.channels

(* A payload without the gateway's stamps (EVENT-BUS.md), which are the
   bus's and not the payload's. *)
let unstamped = function
  | `Assoc members ->
      `Assoc
        (List.filter
           (fun (key, _) -> not (String.length key > 0 && key.[0] = '_'))
           members)
  | other -> other

(* What the registry's schema for [channel] says against [payload]: None
   when the payload is what the channel carries. An envelope carries its
   schema under [response]. *)
let against_channel channel payload =
  let said schema value =
    match against schema value with
    | [] -> None
    | refusals ->
        Some
          (Printf.sprintf "not a %s: %s" schema (String.concat "; " refusals))
  in
  match carried channel with
  | None -> Some "the registry does not declare the channel"
  | Some (Schema schema) -> said schema payload
  | Some (Envelope schema) ->
      said schema (Option.value ~default:`Null (at payload [ "response" ]))
  | Some (Stored_event | Nothing) ->
      Some "the registry names no schema for the channel"

(* ── Telemetry ────────────────────────────────────────────────────────── *)

module Service_rows = Tables.Service_telemetry_telemetry
module Sdk_rows = Tables.Sdk_telemetry_telemetry

type service = Service_rows.services_item
type value = String of string | Boolean of bool
type attribute = { key : string; only : bool; values : value list option }

type span_row = {
  name : string;
  kind : int;  (** OTLP's number for the kind the row names. *)
  span_expected : bool;
  span_attributes : attribute list;
}

type metric_row = {
  metric : string;
  instrument : string;  (** As the receiver reports what a metric came as. *)
  metric_expected : bool;
  metric_attributes : attribute list;
}

let otlp_kind : Service_rows.kind -> int = function
  | Internal -> 1
  | Server -> 2
  | Client -> 3
  | Producer -> 4
  | Consumer -> 5

let sdk_kind : Sdk_rows.service_telemetry_telemetry_kind -> int = function
  | Internal -> 1
  | Server -> 2
  | Client -> 3
  | Producer -> 4
  | Consumer -> 5

let instrument_word : Service_rows.instrument -> string = function
  | Counter -> "counter"
  | Up_down_counter -> "up-down counter"
  | Gauge -> "gauge"
  | Histogram -> "histogram"

let sdk_instrument_word :
    Sdk_rows.service_telemetry_telemetry_instrument -> string = function
  | Counter -> "counter"
  | Up_down_counter -> "up-down counter"
  | Gauge -> "gauge"
  | Histogram -> "histogram"

let attributes (rows : Service_rows.attributes_item list) =
  List.map
    (fun (a : Service_rows.attributes_item) ->
      {
        key = a.key;
        only = a.only <> None;
        values =
          Option.map
            (List.map (function
              | Service_rows.String s -> String s
              | Boolean b -> Boolean b))
            a.values;
      })
    rows

let sdk_attributes
    (rows : Sdk_rows.service_telemetry_telemetry_attributes_item list) =
  List.map
    (fun (a : Sdk_rows.service_telemetry_telemetry_attributes_item) ->
      {
        key = a.key;
        only = a.only <> None;
        values =
          Option.map
            (List.map (function
              | Sdk_rows.String s -> String s
              | Boolean b -> Boolean b))
            a.values;
      })
    rows

let transport_word : Sdk_rows.transport -> string = function
  | Bus -> "bus"
  | Content -> "content"

(* What a row names of the SDK table, which a service names it by: a
   transport for all its rows, or one row. *)
let names_sdk_row used (transport, name) =
  List.mem (transport_word transport) used || List.mem name used

(* The rows a service exports: its rows of the service table, and the SDK
   table's rows it names; each marked expected when its cases must make it
   export it, on [plane] where a row names planes, and where it runs on no
   plane a row that names planes is not. *)
let telemetry ?plane (service : service) =
  let service_table = (tables ()).service_telemetry_telemetry in
  let sdk_table = (tables ()).sdk_telemetry_telemetry in
  let used =
    Option.value ~default:[] (List.assoc_opt service service_table.sdk)
  in
  List.iter
    (fun entry ->
      if
        not
          (List.exists
             (fun (r : Sdk_rows.spans_item) ->
               names_sdk_row [ entry ] (r.transport, r.name))
             sdk_table.spans
          || List.exists
               (fun (r : Sdk_rows.metrics_item) ->
                 names_sdk_row [ entry ] (r.transport, r.name))
               sdk_table.metrics)
      then
        failwith
          ("service-telemetry/telemetry.json names " ^ entry
         ^ " of the SDK table, which is neither a transport nor a row of it"))
    used;
  let expected (when_ : Service_rows.when_) planes =
    (when_ = Export || when_ = Traffic)
    &&
    match (planes, plane) with
    | None, _ -> true
    | Some planes, Some plane -> List.mem plane planes
    | Some _, None -> false
  in
  let spans =
    List.filter_map
      (fun (r : Service_rows.spans_item) ->
        if List.mem service r.services then
          Some
            {
              name = r.name;
              kind = otlp_kind r.kind;
              span_expected = expected r.when_ None;
              span_attributes = attributes r.attributes;
            }
        else None)
      service_table.spans
    @ List.filter_map
        (fun (r : Sdk_rows.spans_item) ->
          if names_sdk_row used (r.transport, r.name) then
            Some
              {
                name = r.name;
                kind = sdk_kind r.kind;
                span_expected = true;
                span_attributes = sdk_attributes r.attributes;
              }
          else None)
        sdk_table.spans
  in
  let metrics =
    List.filter_map
      (fun (r : Service_rows.metrics_item) ->
        if List.mem service r.services then
          Some
            {
              metric = r.name;
              instrument = instrument_word r.instrument;
              metric_expected = expected r.when_ r.planes;
              metric_attributes = attributes r.attributes;
            }
        else None)
      service_table.metrics
    @ List.filter_map
        (fun (r : Sdk_rows.metrics_item) ->
          if names_sdk_row used (r.transport, r.name) then
            Some
              {
                metric = r.name;
                instrument = sdk_instrument_word r.instrument;
                metric_expected = true;
                metric_attributes = sdk_attributes r.attributes;
              }
          else None)
        sdk_table.metrics
  in
  (spans, metrics)

(* The rows an SDK's transports export (sdk-telemetry/telemetry.json), each
   expected: every row, or, for a client that uses [transport] alone, that
   transport's rows. *)
let sdk_telemetry ?transport () =
  let sdk_table = (tables ()).sdk_telemetry_telemetry in
  let used row_transport =
    match transport with None -> true | Some t -> t = row_transport
  in
  let spans =
    List.filter_map
      (fun (r : Sdk_rows.spans_item) ->
        if used r.transport then
          Some
            {
              name = r.name;
              kind = sdk_kind r.kind;
              span_expected = true;
              span_attributes = sdk_attributes r.attributes;
            }
        else None)
      sdk_table.spans
  in
  let metrics =
    List.filter_map
      (fun (r : Sdk_rows.metrics_item) ->
        if used r.transport then
          Some
            {
              metric = r.name;
              instrument = sdk_instrument_word r.instrument;
              metric_expected = true;
              metric_attributes = sdk_attributes r.attributes;
            }
        else None)
      sdk_table.metrics
  in
  (match (transport, spans, metrics) with
  | Some t, [], [] ->
      failwith
        ("sdk-telemetry/telemetry.json lists no row for the transport "
       ^ transport_word t)
  | _ -> ());
  (spans, metrics)

(* The keys of the resource a service exports under: exactly these. *)
let resource_keys service =
  List.filter_map
    (fun (r : Service_rows.resource_item) ->
      if List.mem service r.services then Some r.key else None)
    (tables ()).service_telemetry_telemetry.resource

(* What an exported span's name looks like for a row: each {placeholder}
   stands for any text. *)
let span_pattern name =
  let marked =
    Re.replace_string (Re.Pcre.regexp "\\{[A-Za-z]+\\}") ~by:"\000" name
  in
  let pieces = String.split_on_char '\000' marked in
  Re.compile
    (Re.seq
       (Re.bos
        :: List.concat
             (List.mapi
                (fun at piece ->
                  if at = 0 then [ Re.str piece ]
                  else [ Re.rep1 Re.any; Re.str piece ])
                pieces)
       @ [ Re.eos ]))

let all_span_names () =
  List.map
    (fun (r : Service_rows.spans_item) -> r.name)
    (tables ()).service_telemetry_telemetry.spans
  @ List.map
      (fun (r : Sdk_rows.spans_item) -> r.name)
      (tables ()).sdk_telemetry_telemetry.spans

(* A span's exported name, from the row the spec lists, its placeholders
   filled. *)
let span_name template fill =
  if not (List.mem template (all_span_names ())) then
    failwith ("the spec lists no span " ^ template);
  Re.replace
    (Re.compile (Re.Pcre.re "\\{([A-Za-z]+)\\}"))
    ~f:(fun group ->
      let key = Re.Group.get group 1 in
      match List.assoc_opt key fill with
      | Some value -> value
      | None -> failwith (Printf.sprintf "span %s needs {%s}" template key))
    template

let metric_name name =
  if
    not
      (List.exists
         (fun (r : Service_rows.metrics_item) -> r.name = name)
         (tables ()).service_telemetry_telemetry.metrics
      || List.exists
           (fun (r : Sdk_rows.metrics_item) -> r.name = name)
           (tables ()).sdk_telemetry_telemetry.metrics)
  then failwith ("the spec lists no metric " ^ name);
  name

(* The variables a service reads (service-environment/variables.json). *)
let service_environment
    (service :
      Tables.Service_environment_variables
      .service_telemetry_telemetry_services_item) =
  match
    List.filter_map
      (fun (row : Tables.Service_environment_variables.variables_item) ->
        if List.mem service row.services then Some row.name else None)
      (tables ()).service_environment_variables.variables
  with
  | [] ->
      failwith
        "service-environment/variables.json lists no variables for the service"
  | names -> names

(* A duration client/timing.json states, in milliseconds. *)
let client_timing name =
  match
    List.find_opt
      (fun (row : Tables.Client_timing.timing_item) -> row.name = name)
      (tables ()).client_timing.timing
  with
  | Some { value = Integer ms; _ } when String.ends_with ~suffix:"Ms" name -> ms
  | _ -> failwith ("client/timing.json states no duration " ^ name)

(* ── Knowledge bases and principals ───────────────────────────────────── *)

type kb = { name : string; domain : string; did : string; resource : string }

(* The case of kb-identity/cases.json the suite's knowledge base is. *)
let suite_kb = "the shape xynolog init writes"

let kb_identity why =
  match
    List.find_opt
      (fun (c : Tables.Kb_identity_cases.cases_item) -> c.why = why)
      (tables ()).kb_identity_cases.cases
  with
  | Some
      {
        name = Some name;
        domain = Some domain;
        did = Some did;
        resource = Some resource;
        _;
      } ->
      { name; domain; did; resource }
  | Some _ ->
      failwith ("kb-identity/cases.json's \"" ^ why ^ "\" declares no identity")
  | None -> failwith ("kb-identity/cases.json has no \"" ^ why ^ "\" case")

(* encodeURIComponent's encoding, which principals/cases.json states its
   forms in, and its cases hold. *)
let uri_component text =
  let kept = function
    | 'A' .. 'Z'
    | 'a' .. 'z'
    | '0' .. '9'
    | '-' | '_' | '.' | '!' | '~' | '*' | '\'' | '(' | ')' ->
        true
    | _ -> false
  in
  String.concat ""
    (List.map
       (fun c ->
         if kept c then String.make 1 c
         else Printf.sprintf "%%%02X" (Char.code c))
       (List.of_seq (String.to_seq text)))

let filled form domain subject =
  let replace pattern by text =
    Re.replace_string (Re.compile (Re.str pattern)) ~by ~all:false text
  in
  replace "{subject}" (uri_component subject) (replace "{domain}" domain form)

let principals_checked =
  lazy
    (let table = (tables ()).principals_cases in
     List.iter
       (fun (kind, form, cases) ->
         List.iter
           (fun (c : Tables.Principals_cases.people_item) ->
             let made = filled form c.domain c.subject in
             if made <> c.did then
               failwith
                 (Printf.sprintf
                    "principals/cases.json: \"%s\": the %s form gives %s, the \
                     case says %s"
                    c.why kind made c.did))
           cases)
       [
         ("person", table.person, table.people);
         ("agent", table.agent, table.agents);
       ];
     table)

let person_did domain subject =
  filled (Lazy.force principals_checked).person domain subject

let agent_did domain subject =
  filled (Lazy.force principals_checked).agent domain subject

(* ── Roles ────────────────────────────────────────────────────────────── *)

(* A name the bearerAuth scheme's description declares, in backticks: the
   roles and the claim that marks an agent's account. *)
let declared word =
  match
    List.assoc_opt "bearerAuth" (spec ()).gateway.components.security_schemes
  with
  | Some (Openapi.Http { description = Some description; _ })
    when Re.execp (Re.compile (Re.str ("`" ^ word ^ "`"))) description ->
      word
  | _ -> failwith ("the bearerAuth scheme no longer declares " ^ word)

let service_role () = declared "xynolog-service"
let worker_role () = declared "xynolog-worker"
let kind_claim () = declared "kind"
let agent_kind () = declared "agent"

(* ── What the gateway answers ─────────────────────────────────────────── *)

(* [template] with each of its {placeholders} filled from [fill], which must
   fill every one. *)
let fill_template template fill =
  let text =
    List.fold_left
      (fun text (name, value) ->
        Re.replace_string
          (Re.compile (Re.str ("{" ^ name ^ "}")))
          ~by:value text)
      template fill
  in
  match Re.exec_opt (Re.compile (Re.Perl.re "\\{[A-Za-z]+\\}")) text with
  | Some left ->
      failwith
        (Printf.sprintf "%S is left with %s unfilled" template
           (Re.Group.get left 0))
  | None -> text

(* A row of gateway/messages.json, by its name. *)
let message name : Tables.Gateway_messages.messages_item =
  match
    List.find_opt
      (fun (row : Tables.Gateway_messages.messages_item) -> row.name = name)
      (tables ()).gateway_messages.messages
  with
  | Some row -> row
  | None -> failwith ("gateway/messages.json states no message " ^ name)

(* The error text of the message [name], its placeholders filled. *)
let message_text ?(fill = []) name = fill_template (message name).error fill

(* The code of the message [name], which states one. *)
let message_code name =
  match (message name).code with
  | Some code -> code
  | None -> failwith ("gateway/messages.json's " ^ name ^ " states no code")

(* A failure the gateway answers on the bus, by its name. *)
let failure name : Tables.Gateway_messages.failures_item =
  match
    List.find_opt
      (fun (row : Tables.Gateway_messages.failures_item) -> row.name = name)
      (tables ()).gateway_messages.failures
  with
  | Some row -> row
  | None -> failwith ("gateway/messages.json states no failure " ^ name)

(* The WWW-Authenticate challenges, for the gateway at [origin]. *)
let challenge_no_credential origin =
  fill_template (tables ()).gateway_messages.challenges.no_credential
    [ ("origin", origin) ]

let challenge_refused origin =
  fill_template (tables ()).gateway_messages.challenges.refused
    [ ("origin", origin) ]

(* The concrete path of [template], which [which] must declare an operation
   of [meth] on, its parameters filled from [fill], each encoded. *)
let path_of which meth template fill =
  fill_template (path which meth template)
    (List.map (fun (name, value) -> (name, uri_component value)) fill)

(* The row of identifiers/kinds.json for the identifier schema [kind]: what
   it accepts and refuses. *)
let identifiers kind : Tables.Identifiers_kinds.kinds_item =
  match
    List.find_opt
      (fun (row : Tables.Identifiers_kinds.kinds_item) -> row.schema = kind)
      (tables ()).identifiers_kinds.kinds
  with
  | Some row -> row
  | None -> failwith ("identifiers/kinds.json names no kind " ^ kind)

(* The service.name [service] exports under when OTEL_SERVICE_NAME is unset:
   the resource row's default, its {service} filled. *)
let default_service_name service =
  match
    List.find_opt
      (fun (row : Service_rows.resource_item) -> row.key = "service.name")
      (tables ()).service_telemetry_telemetry.resource
  with
  | Some { default = Some form; _ } ->
      fill_template form
        [ ("service", Service_rows.services_item_value service) ]
  | _ ->
      failwith "service-telemetry/telemetry.json states no default service.name"
