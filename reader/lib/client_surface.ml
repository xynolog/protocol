(* client/surface.json's cases against their methods' parameters: a method
   takes each parameter once, and each case gives it nothing it does not take,
   everything it requires, and each argument as its parameter's schema says,
   with each [{"$fixture": name}] read as the table's fixture of that name.

   And each method's [returns] against its shape: a [signal] or [count]
   returns null, its shape saying all it gives; a promise may return null,
   resolving with nothing; any other shape gives a value. An [events] method
   returns the schema bus/registry.json states for the channel it observes.

   And each module constant's [names]: each of [repliesOf] is an operation of
   bus/registry.json, by its request, and each of [channels] a channel it
   states.

   And each [record] a worker's handed names: a client type of client/types,
   whose properties are, in its order, the handed's first members, none of
   which takes anything. *)

let file = "client/surface.json"
let ( / ) pointer token = pointer ^ "/" ^ Decode.escape token
let refusal pointer message = { Decode.file; pointer; message }
let indexed items = List.mapi (fun index item -> (index, item)) items

let name : Tables.Client_surface.parameters_item -> string = function
  | Value { parameter; _ } | Callback { parameter; _ } -> parameter

let rec with_fixtures fixtures at (value : Yojson.Safe.t) =
  match value with
  | `Assoc [ ("$fixture", `String fixture) ] -> (
      match Decode.at_pointer ("" / fixture) fixtures with
      | Some found -> (found, [])
      | None ->
          ( value,
            [
              refusal at
                (Printf.sprintf "names the fixture %s, which the table has not"
                   fixture);
            ] ))
  | `Assoc members ->
      let members, refusals =
        List.split
          (List.map
             (fun (key, member) ->
               let member, refusals =
                 with_fixtures fixtures (at / key) member
               in
               ((key, member), refusals))
             members)
      in
      (`Assoc members, List.concat refusals)
  | `List items ->
      let items, refusals =
        List.split
          (List.map
             (fun (index, item) ->
               with_fixtures fixtures (at / string_of_int index) item)
             (indexed items))
      in
      (`List items, List.concat refusals)
  | other -> (other, [])

(* The method at [at], which a caller calls as [called]. *)
let method_refusals ~ids ~schema_at ~fixtures ~at ~called
    (method_ : Tables.Client_surface.methods_item) =
  let parameters = indexed method_.parameters in
  let twice =
    List.filter_map
      (fun (k, parameter) ->
        if
          List.exists
            (fun (j, other) -> j < k && name other = name parameter)
            parameters
        then
          Some
            (refusal
               (at / "parameters" / string_of_int k)
               (Printf.sprintf "names %s, which %s takes already"
                  (name parameter) called))
        else None)
      parameters
  in
  let argument args_at (key, value) =
    let at_argument = args_at / key in
    match
      List.find_opt (fun (_, parameter) -> name parameter = key) parameters
    with
    | None ->
        [ refusal at_argument (Printf.sprintf "is no parameter of %s" called) ]
    | Some (_, Callback _) ->
        [ refusal at_argument "is a callback, which a case cannot give" ]
    | Some (k, Value _) -> (
        let value, unnamed = with_fixtures fixtures at_argument value in
        let place : Validate.place =
          { file; pointer = at / "parameters" / string_of_int k / "schema" }
        in
        unnamed
        @
        match schema_at place with
        | None -> [ refusal place.pointer "is no schema the reader can read" ]
        | Some schema ->
            List.map
              (fun (pointer, message) -> refusal pointer message)
              (Validate.errors ids schema_at schema value at_argument))
  in
  let case (c, (case : Tables.Client_surface.cases_item)) =
    let args_at = at / "cases" / string_of_int c / "args" in
    match case.args with
    | `Assoc args ->
        List.filter_map
          (function
            | Tables.Client_surface.Value { parameter; required = true; _ }
              when not (List.mem_assoc parameter args) ->
                Some
                  (refusal args_at
                     (Printf.sprintf "lacks %s, which %s requires" parameter
                        called))
            | _ -> None)
          method_.parameters
        @ List.concat_map (argument args_at) args
    | _ -> [ refusal args_at "is no object of arguments" ]
  in
  twice @ List.concat_map case (indexed method_.cases)

(* The schema the registry states for [name], by its [$id]. *)
let channel_schema (registry : Tables.Bus_registry.t) name =
  List.find_map
    (fun (c : Tables.Bus_registry.channels_item) ->
      match c with
      | Schema { channel; schema; _ } when channel = name ->
          Some (Decode.base ^ schema ^ ".json")
      | _ -> None)
    registry.channels

let returns_refusals ~registry ~at ~called
    (method_ : Tables.Client_surface.methods_item) =
  let via_at = at / "via" and at = at / "returns" in
  let shape = Tables.Client_surface.shape_value method_.shape in
  match (method_.shape, method_.returns) with
  | (Signal | Count), Some _ ->
      [
        refusal at
          (Printf.sprintf
             "is a value, and %s is a %s, whose shape says all it gives: it \
              returns null"
             called shape);
      ]
  | (Stream | Delegation | Upload | Cache | Events | Claims), None ->
      [
        refusal at
          (Printf.sprintf "is null, and %s is a %s, which gives a value" called
             shape);
      ]
  | Events, Some returned -> (
      let observed =
        match method_.via with
        | Observes { observes } -> Some observes
        | _ -> None
      in
      match (Option.bind observed (channel_schema registry), returned) with
      | Some stated, Reference { ref = { target; _ } } when target = stated ->
          []
      | Some stated, _ ->
          [
            refusal at
              (Printf.sprintf
                 "is not %s, the schema bus/registry.json states for the \
                  channel %s observes"
                 stated called);
          ]
      | None, _ ->
          [
            refusal via_at
              (Printf.sprintf
                 "names no channel bus/registry.json states a schema for, so \
                  what %s returns is unstated"
                 called);
          ])
  | _ -> []

let field name = function
  | `Assoc members -> List.assoc_opt name members
  | _ -> None

let items = function Some (`List items) -> items | _ -> []

(* What a constant's [names] names that bus/registry.json does not state. *)
let names_refusals ~registry_json surface =
  let texts value =
    List.filter_map (function `String s -> Some s | _ -> None) (items value)
  in
  let operations =
    List.filter_map
      (fun op ->
        match field "request" op with Some (`String s) -> Some s | _ -> None)
      (items (field "operations" registry_json))
  in
  let channels =
    List.filter_map
      (fun c ->
        match field "channel" c with Some (`String s) -> Some s | _ -> None)
      (items (field "channels" registry_json))
  in
  List.concat_map
    (fun (i, module_) ->
      List.concat_map
        (fun (j, member) ->
          match field "names" member with
          | None -> []
          | Some names ->
              let at =
                "" / "modules" / string_of_int i / "members" / string_of_int j
                / "names"
              in
              List.filter_map
                (fun operation ->
                  if List.mem operation operations then None
                  else
                    Some
                      (refusal (at / "repliesOf")
                         (Printf.sprintf
                            "names %s, which is no operation of \
                             bus/registry.json"
                            operation)))
                (texts (field "repliesOf" names))
              @ List.filter_map
                  (fun channel ->
                    if List.mem channel channels then None
                    else
                      Some
                        (refusal (at / "channels")
                           (Printf.sprintf
                              "names %s, which is no channel of \
                               bus/registry.json"
                              channel)))
                  (texts (field "channels" names)))
        (indexed (items (field "members" module_))))
    (indexed (items (field "modules" surface)))

(* How a handed is not what the [record] it names says it is. *)
let record_refusals ~read_json surface =
  List.concat_map
    (fun (i, handed) ->
      match field "record" handed with
      | Some (`String record) -> (
          let at = "" / "worker" / string_of_int i / "record" in
          match read_json ("client/types/" ^ record ^ ".json") with
          | None ->
              [
                refusal at
                  (Printf.sprintf "names %s, which is no client type" record);
              ]
          | Some json ->
              let properties =
                match field "properties" json with
                | Some (`Assoc members) -> List.map fst members
                | _ -> []
              in
              let first =
                List.filteri
                  (fun j _ -> j < List.length properties)
                  (items (field "members" handed))
              in
              if
                List.filter_map
                  (fun member ->
                    match field "member" member with
                    | Some (`String name) -> Some name
                    | _ -> None)
                  first
                = properties
                && List.for_all
                     (fun member -> field "takes" member = None)
                     first
              then []
              else
                [
                  refusal at
                    (Printf.sprintf
                       "names %s, whose properties are not, in its order, the \
                        first members, taking nothing"
                       record);
                ])
      | _ -> [])
    (indexed (items (field "worker" surface)))

let refusals ~ids ~read_json ~schema_at ~registry
    (surface : Tables.Client_surface.t) =
  let each f =
    List.concat_map
      (fun (n, (namespace : Tables.Client_surface.namespaces_item)) ->
        List.concat_map
          (fun (m, (method_ : Tables.Client_surface.methods_item)) ->
            f
              ~at:
                ("" / "namespaces" / string_of_int n / "methods"
               / string_of_int m)
              ~called:(namespace.namespace ^ "." ^ method_.method_)
              method_)
          (indexed namespace.methods))
      (indexed surface.namespaces)
  in
  each (returns_refusals ~registry)
  @ (match (read_json file, read_json "bus/registry.json") with
    | Some json, Some registry_json ->
        names_refusals ~registry_json json @ record_refusals ~read_json json
    | _ -> [])
  @
  match Option.bind (read_json file) (Decode.at_pointer "/fixtures") with
  | None -> [ refusal "" "has no fixtures" ]
  | Some fixtures -> each (method_refusals ~ids ~schema_at ~fixtures)
