(* client/surface.json's cases against their methods' parameters: a method
   takes each parameter once, and each case gives it nothing it does not take,
   everything it requires, and each argument as its parameter's schema says,
   or null where the parameter is absent, with each [{"$fixture": name}] read
   as the table's fixture of that name.

   And each method's [returns] against its shape: a [signal] or [count]
   returns null, its shape saying all it gives; a promise may return null,
   resolving with nothing; any other shape gives a value. An [events] method
   returns the schema bus/registry.json states for the channel it observes.

   And each module constant's [names]: each of [repliesOf] is an operation of
   bus/registry.json, by its request, and each of [channels] a channel it
   states.

   And each [record] a worker's handed names: a client type of client/types,
   whose properties are, in its order, the handed's first members, none of
   which takes anything.

   And each member's [gives] against its handed's record, its [takes] and
   itself; each type parameter against the unit that declares it, each
   reference to a client type against the type parameters it is generic over,
   and each schema a value decides against the member that decides it; and
   each [expect] of client/state-units.json against the [gives] of the
   members it names. *)

let file = "client/surface.json"
let ( / ) pointer token = pointer ^ "/" ^ Decode.escape token
let refusal pointer message = { Decode.file; pointer; message }
let indexed items = List.mapi (fun index item -> (index, item)) items

let name : Tables.Client_surface.parameters_item -> string = function
  | Value { parameter; _ } | Callback { parameter; _ } -> parameter

(* [value], at [at] in [file], with each [{"$fixture": name}] read as the
   fixture of that name in [fixtures]. *)
let rec with_fixtures ~file fixtures at (value : Yojson.Safe.t) =
  match value with
  | `Assoc [ ("$fixture", `String fixture) ] -> (
      match Decode.at_pointer ("" / fixture) fixtures with
      | Some found -> (found, [])
      | None ->
          ( value,
            [
              {
                Decode.file;
                pointer = at;
                message =
                  Printf.sprintf "names the fixture %s, which the table has not"
                    fixture;
              };
            ] ))
  | `Assoc members ->
      let members, refusals =
        List.split
          (List.map
             (fun (key, member) ->
               let member, refusals =
                 with_fixtures ~file fixtures (at / key) member
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
               with_fixtures ~file fixtures (at / string_of_int index) item)
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
    | Some (_, Value { schema = None; _ }) ->
        [ refusal at_argument "is the SDK's own, which a case cannot give" ]
    | Some (_, Value { absent = Some true; _ }) when value = `Null -> []
    | Some (k, Value _) -> (
        let value, unnamed = with_fixtures ~file fixtures at_argument value in
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
          (fun (parameter : Tables.Client_surface.parameters_item) ->
            match parameter with
            | Value { parameter; required = true; _ }
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
  let own =
    List.filter_map
      (fun (k, (parameter : Tables.Client_surface.parameters_item)) ->
        match parameter with
        | Value { schema = None; _ } ->
            Some
              (refusal
                 (at / "parameters" / string_of_int k)
                 "states no schema, and a method takes only what Protocol \
                  states")
        | _ -> None)
      parameters
  in
  twice @ own @ List.concat_map case (indexed method_.cases)

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

let text name json =
  match field name json with Some (`String s) -> Some s | _ -> None

let named name rows =
  List.find_opt (fun (_, row) -> text "member" row = Some name) rows

(* How a member's [gives] is not what the rest of the surface says of it: a
   member of its handed's [record] is a value of the record's property, a
   member that [takes] what a verb takes is a function one of whose
   parameters is decided in its takes, and a function states its parameters and what it returns together, or neither.
   A function that may return nothing returns something when it does; one
   that returns a handed names one of [worker]'s, and one that returns a
   function names a function beside it that returns no function. *)
let gives_refusals ~read_json surface =
  let handeds =
    List.filter_map (text "handed") (items (field "worker" surface))
  in
  let member_refusals ~record ~siblings place (j, member) =
    let at = place / string_of_int j / "gives" in
    let gives = field "gives" member in
    let of_record =
      match (record, text "member" member) with
      | Some (record, properties), Some name when List.mem name properties ->
          let expected =
            `Assoc
              [
                ("form", `String "value");
                ( "schema",
                  `Assoc
                    [
                      ( "$ref",
                        `String
                          (Decode.base ^ record ^ ".json#/properties/" ^ name)
                      );
                    ] );
              ]
          in
          if gives = Some expected then []
          else
            [
              refusal at
                (Printf.sprintf "is not %s, what %s's %s is"
                   (Yojson.Safe.to_string expected)
                   record name);
            ]
      | _ -> []
    in
    let taking =
      let decided parameter =
        Option.bind (field "schema" parameter) (field "decided") <> None
      in
      if
        field "takes" member <> None
        && not
             (Option.bind gives (text "form") = Some "function"
             && List.exists decided
                  (items (Option.bind gives (field "parameters"))))
      then
        [
          refusal at
            "is no function one of whose parameters is decided in its takes, \
             and the member takes what a verb takes";
        ]
      else []
    in
    let together =
      match gives with
      | Some gives when text "form" gives = Some "function" -> (
          match (field "parameters" gives, field "returns" gives) with
          | Some _, None | None, Some _ ->
              [
                refusal at
                  "states one of what it takes and what it returns: a function \
                   states both, or neither";
              ]
          | _ -> [])
      | _ -> []
    in
    let returned =
      match gives with
      | Some gives when text "form" gives = Some "function" -> (
          let returns = field "returns" gives in
          (match (field "absent" gives, returns) with
            | Some (`Bool true), (None | Some `Null) ->
                [
                  refusal (at / "absent")
                    "says it may return nothing, and it returns nothing at \
                     all: a function that may return nothing returns something \
                     when it does";
                ]
            | _ -> [])
          @
          match returns with
          | Some (`Assoc [ ("handed", `String handed) ])
            when not (List.mem handed handeds) ->
              [
                refusal
                  (at / "returns" / "handed")
                  (Printf.sprintf "names %s, which is no handed of worker"
                     handed);
              ]
          | Some (`Assoc [ ("function", `String name) ]) -> (
              match named name siblings with
              | Some (_, sibling) -> (
                  let sibling_gives = field "gives" sibling in
                  match
                    ( Option.bind sibling_gives (text "form"),
                      Option.bind sibling_gives (field "returns") )
                  with
                  | Some "function", Some (`Assoc [ ("function", _) ]) ->
                      [
                        refusal
                          (at / "returns" / "function")
                          (Printf.sprintf
                             "names %s, which returns a function itself" name);
                      ]
                  | Some "function", _ -> []
                  | _ ->
                      [
                        refusal
                          (at / "returns" / "function")
                          (Printf.sprintf "names %s, which is no function" name);
                      ])
              | None ->
                  [
                    refusal
                      (at / "returns" / "function")
                      (Printf.sprintf "names %s, which is no member beside it"
                         name);
                  ])
          | _ -> [])
      | _ -> []
    in
    of_record @ taking @ together @ returned
  in
  let members ~record place row =
    let siblings = indexed (items (field "members" row)) in
    List.concat_map (member_refusals ~record ~siblings place) siblings
  in
  List.concat_map
    (fun (i, handed) ->
      let record =
        Option.bind (text "record" handed) (fun record ->
            match read_json ("client/types/" ^ record ^ ".json") with
            | Some json -> (
                match field "properties" json with
                | Some (`Assoc properties) ->
                    Some (record, List.map fst properties)
                | _ -> None)
            | None -> None)
      in
      members ~record ("" / "worker" / string_of_int i / "members") handed)
    (indexed (items (field "worker" surface)))
  @ List.concat_map
      (fun (i, unit) ->
        members ~record:None
          ("" / "stateUnits" / string_of_int i / "members")
          unit)
      (indexed (items (field "stateUnits" surface)))
  @ Option.fold ~none:[]
      ~some:(members ~record:None ("" / "wire" / "members"))
      (field "wire" surface)

(* Every object in [json], by its pointer. *)
let rec objects at (json : Yojson.Safe.t) =
  match json with
  | `Assoc members ->
      (at, members)
      :: List.concat_map
           (fun (key, member) -> objects (at / key) member)
           members
  | `List items ->
      List.concat_map
        (fun (index, item) -> objects (at / string_of_int index) item)
        (indexed items)
  | _ -> []

let starts ~prefix text =
  String.length text >= String.length prefix
  && String.sub text 0 (String.length prefix) = prefix

let words list =
  Yojson.Safe.to_string (`List (List.map (fun word -> `String word) list))

(* How the surface's type parameters are not what their units declare: each
   [{"typeParameter": name}] is within a state unit's members, and names one
   its unit declares; each a unit declares, once, is named by one of its
   members; and a cache declares K, what a query is keyed by, and V, what it
   holds, as each method of shape [cache] binds them. *)
let type_parameter_refusals surface =
  let units = indexed (items (field "stateUnits" surface)) in
  let declared row =
    indexed (List.filter_map (text "name") (items (field "typeParameters" row)))
  in
  let named =
    List.filter_map
      (fun (pointer, members) ->
        match members with
        | [ ("typeParameter", `String name) ] -> Some (pointer, name)
        | _ -> None)
      (objects "" surface)
  in
  let unit_of pointer =
    List.find_opt
      (fun (u, _) ->
        starts
          ~prefix:(("" / "stateUnits" / string_of_int u / "members") ^ "/")
          pointer)
      units
  in
  List.filter_map
    (fun (pointer, name) ->
      match unit_of pointer with
      | None ->
          Some
            (refusal pointer
               (Printf.sprintf
                  "names the type parameter %s, and only a state unit's \
                   members name one, which their unit declares"
                  name))
      | Some (_, row) when List.mem name (List.map snd (declared row)) -> None
      | Some (_, row) ->
          Some
            (refusal pointer
               (Printf.sprintf
                  "names the type parameter %s, which %s does not declare" name
                  (Option.value ~default:"" (text "unit" row)))))
    named
  @ List.concat_map
      (fun (u, row) ->
        let unit_name = Option.value ~default:"" (text "unit" row) in
        let at = "" / "stateUnits" / string_of_int u / "typeParameters" in
        let names = declared row in
        let members_at = "" / "stateUnits" / string_of_int u / "members" in
        List.filter_map
          (fun (j, name) ->
            if List.exists (fun (k, other) -> k < j && other = name) names then
              Some
                (refusal
                   (at / string_of_int j)
                   (Printf.sprintf "declares %s twice" name))
            else if
              not
                (List.exists
                   (fun (pointer, used) ->
                     used = name && starts ~prefix:(members_at ^ "/") pointer)
                   named)
            then
              Some
                (refusal
                   (at / string_of_int j)
                   (Printf.sprintf
                      "declares %s, which none of %s's members names" name
                      unit_name))
            else None)
          names
        @
        if unit_name = "cache" && List.map snd names <> [ "K"; "V" ] then
          [
            refusal at
              (Printf.sprintf
                 "declares %s: a cache declares K, what a query is keyed by, \
                  and V, what it holds, in that order"
                 (words (List.map snd names)));
          ]
        else [])
      units

(* How a reference to a client type gives it other type arguments than the
   type parameters it is generic over: one each, by name, or none where it is
   generic over nothing. *)
let type_argument_refusals ~read_json surface =
  List.concat_map
    (fun (pointer, members) ->
      match List.assoc_opt "$ref" members with
      | Some (`String target)
        when starts ~prefix:Decode.base target
             && not (String.contains target '#') ->
          let name =
            Filename.remove_extension
              (String.sub target
                 (String.length Decode.base)
                 (String.length target - String.length Decode.base))
          in
          let declared =
            match read_json ("client/types/" ^ name ^ ".json") with
            | Some json ->
                List.filter_map
                  (function `String word -> Some word | _ -> None)
                  (items (field "x-xynolog-typeParameters" json))
            | None -> []
          in
          let given =
            match List.assoc_opt "typeArguments" members with
            | Some (`Assoc arguments) -> List.map fst arguments
            | _ -> []
          in
          if List.sort compare given = List.sort compare declared then []
          else
            [
              refusal pointer
                (Printf.sprintf
                   "gives %s the type arguments %s, and it is generic over %s"
                   name (words given) (words declared));
            ]
      | _ -> [])
    (objects "" surface)

(* The enum [schema] holds a value of, through each lone [$ref] it is. *)
let rec enum_of ~ids ~read_json ~file (schema : Yojson.Safe.t) =
  match schema with
  | `Assoc members when List.mem_assoc "enum" members ->
      Some
        (List.filter_map
           (function `String word -> Some word | _ -> None)
           (items (List.assoc_opt "enum" members)))
  | `Assoc (("$ref", `String target) :: _) ->
      let file, pointer = Decode.resolve ids { target; file; pointer = "" } in
      Option.bind (read_json file) (fun json ->
          Option.bind
            (Decode.at_pointer pointer json)
            (enum_of ~ids ~read_json ~file))
  | _ -> None

(* How a [{"decided": …}] is not decided: it is a parameter of a handed's
   function whose member [takes] what a verb takes, by a member beside it
   that holds an enum's value, and its [takes] states each of the enum's
   values once, in its order. *)
let decided_refusals ~ids ~read_json surface =
  let handeds = indexed (items (field "worker" surface)) in
  List.concat_map
    (fun (pointer, members) ->
      match members with
      | [ ("decided", decided) ] -> (
          match String.split_on_char '/' pointer with
          | [
           ""; "worker"; i; "members"; j; "gives"; "parameters"; _; "schema";
          ] -> (
              let siblings =
                indexed
                  (items
                     (field "members" (List.assoc (int_of_string i) handeds)))
              in
              let member = List.assoc (int_of_string j) siblings in
              let member_name =
                Option.value ~default:"" (text "member" member)
              in
              let by =
                Option.bind (field "by" decided) (text "member")
                |> Option.value ~default:""
              in
              let by_at = pointer / "decided" / "by" / "member" in
              match (field "takes" member, named by siblings) with
              | None, _ ->
                  [
                    refusal pointer
                      (Printf.sprintf "is decided in takes, and %s states none"
                         member_name);
                  ]
              | _, None ->
                  [
                    refusal by_at
                      (Printf.sprintf "names %s, which is no member beside it"
                         by);
                  ]
              | Some takes, Some (_, sibling) -> (
                  match
                    Option.bind
                      (Option.bind (field "gives" sibling) (field "schema"))
                      (enum_of ~ids ~read_json ~file)
                  with
                  | None ->
                      [
                        refusal by_at
                          (Printf.sprintf
                             "names %s, which holds no enum's value" by);
                      ]
                  | Some values ->
                      let stated =
                        match takes with
                        | `Assoc verbs -> List.map fst verbs
                        | _ -> []
                      in
                      if stated = values then []
                      else
                        [
                          refusal
                            ("" / "worker" / i / "members" / j / "takes")
                            (Printf.sprintf
                               "states %s, not, each once and in order, the \
                                values of %s, %s"
                               (words stated) by (words values));
                        ]))
          | _ ->
              [
                refusal pointer
                  "is decided by a member beside it, and only a parameter of a \
                   handed's function is";
              ])
      | _ -> [])
    (objects "" surface)

(* Each [expect] of client/state-units.json against client/surface.json: a
   case's unit is a row of [stateUnits], each member it expects one of the
   row's, and each value what the member's [gives] holds, with each
   [{"$fixture": name}] read as the cases' fixture of that name. A member may
   hold null only where it may hold nothing; one whose [gives] names no
   schema holds the SDK's own (an error), which no schema checks, and a
   [{"$has": …}] states only some of what is held, so it is not checked
   whole. *)
let expect_refusals ~ids ~read_json ~schema_at surface =
  let cases_file = "client/state-units.json" in
  let refuse pointer message = { Decode.file = cases_file; pointer; message } in
  match read_json cases_file with
  | None -> [ refuse "" "is missing: its expects are checked against it" ]
  | Some table -> (
      match field "fixtures" table with
      | None -> [ refuse "" "has no fixtures" ]
      | Some fixtures ->
          let units = indexed (items (field "stateUnits" surface)) in
          let held ~unit_name ~u at (m, member) value =
            let gives = field "gives" member in
            (* A reference with type arguments is checked against its client
               type, whose type parameters admit any value: what they are is
               the unit's maker's. *)
            let schema_place : Validate.place =
              match Option.bind gives (field "schema") with
              | Some (`Assoc members)
                when List.mem_assoc "typeArguments" members -> (
                  match List.assoc_opt "$ref" members with
                  | Some (`String target) ->
                      let file, pointer =
                        Decode.resolve ids { target; file; pointer = "" }
                      in
                      { file; pointer }
                  | _ -> { file; pointer = "" })
              | _ ->
                  {
                    file;
                    pointer =
                      "" / "stateUnits" / string_of_int u / "members"
                      / string_of_int m / "gives" / "schema";
                  }
            in
            let name = Option.value ~default:"" (text "member" member) in
            match
              ( Option.bind gives (text "form"),
                Option.bind gives (field "schema"),
                value )
            with
            | Some ("value" | "watched"), _, `Null ->
                if Option.bind gives (field "absent") = Some (`Bool true) then
                  []
                else
                  [
                    refuse at
                      (Printf.sprintf "is null, and %s's %s always holds one"
                         unit_name name);
                  ]
            | Some ("value" | "watched"), None, _ -> []
            | Some ("value" | "watched"), Some _, `Assoc [ ("$has", _) ] -> []
            | Some ("value" | "watched"), Some _, value -> (
                let value, unnamed =
                  with_fixtures ~file:cases_file fixtures at value
                in
                unnamed
                @
                match schema_at schema_place with
                | None ->
                    [
                      refusal schema_place.pointer
                        "is no schema the reader can read";
                    ]
                | Some schema ->
                    List.map
                      (fun (pointer, message) -> refuse pointer message)
                      (Validate.errors ids schema_at schema value at))
            | _ ->
                [
                  refuse at
                    (Printf.sprintf
                       "is what %s's %s holds, and it holds nothing: what a \
                        unit holds is a value or a watched value"
                       unit_name name);
                ]
          in
          List.concat_map
            (fun (c, case) ->
              let case_at = "" / "cases" / string_of_int c in
              let unit_name = Option.value ~default:"" (text "unit" case) in
              match
                List.find_opt
                  (fun (_, row) -> text "unit" row = Some unit_name)
                  units
              with
              | None ->
                  [
                    refuse (case_at / "unit")
                      (Printf.sprintf
                         "names %s, which is no state unit of \
                          client/surface.json"
                         unit_name);
                  ]
              | Some (u, row) ->
                  let members = indexed (items (field "members" row)) in
                  List.concat_map
                    (fun (s, step) ->
                      match field "expect" step with
                      | Some (`Assoc made) ->
                          List.concat_map
                            (fun (made_name, expected) ->
                              let at =
                                case_at / "steps" / string_of_int s / "expect"
                                / made_name
                              in
                              match expected with
                              | `Assoc expected ->
                                  List.concat_map
                                    (fun (member_name, value) ->
                                      let at = at / member_name in
                                      match named member_name members with
                                      | None ->
                                          [
                                            refuse at
                                              (Printf.sprintf
                                                 "is no member of %s" unit_name);
                                          ]
                                      | Some member ->
                                          held ~unit_name ~u at member value)
                                    expected
                              | _ -> [ refuse at "is no object of members" ])
                            made
                      | _ -> [])
                    (indexed (items (field "steps" case))))
            (indexed (items (field "cases" table))))

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
        names_refusals ~registry_json json
        @ record_refusals ~read_json json
        @ gives_refusals ~read_json json
        @ type_parameter_refusals json
        @ type_argument_refusals ~read_json json
        @ decided_refusals ~ids ~read_json json
        @ expect_refusals ~ids ~read_json ~schema_at json
    | _ -> [])
  @
  match Option.bind (read_json file) (Decode.at_pointer "/fixtures") with
  | None -> [ refusal "" "has no fixtures" ]
  | Some fixtures -> each (method_refusals ~ids ~schema_at ~fixtures)
