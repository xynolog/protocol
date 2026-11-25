(* control/methods.json names each schema of the control API by its [$id],
   as a [$ref] does: the version's, the error data's, each method's params and
   result, and each notification's params. Each names a schema some file
   states, and that schema is control/'s. A name not under [Decode.base] is
   its table's schema's to refuse, by its pattern. Each notification a method
   sends is one the table states, from the launcher.

   control/RunService.json, the services a run runs, names the seven
   service-telemetry/telemetry.json names, in its order: the control schemas
   name no schema outside control/, so the list is stated twice and held so.

   control/RootsInitParams.json's [inference], the providers a local stack
   is generated for, names some of components/schemas/InferenceProvider.json's,
   each once: it is a subset stated by hand, and held to the vocabulary.

   control/rpc-codes.json states each JSON-RPC code the API answers with,
   each once. Each control code of errors/codes.json is answered under one
   of them, its [rpc]. A request-cases.json answer's code is one of them,
   and its data is what the code's row says:
   - under a code with [data] (-32603, E4), that data, of a control code;
   - under a code some control code is answered under (-32602, -32000: V1,
     P1, E2), a control code answered under it, retryable as that code is
     (E3);
   - under any other, none. *)

let file = "control/methods.json"
let area = "control/"
let ( / ) pointer token = pointer ^ "/" ^ Decode.escape token
let run_service = "control/RunService.json"
let telemetry = "service-telemetry/ServiceTelemetryTelemetry.json"

let telemetry_services =
  "/properties/spans/items/properties/services/items/enum"

let words = function
  | Some (`List items) ->
      Some (List.filter_map (function `String s -> Some s | _ -> None) items)
  | _ -> None

let shown = function
  | Some names -> "[" ^ String.concat "," names ^ "]"
  | None -> "none"

let run_services ~(read_json : string -> Yojson.Safe.t option) =
  let at file pointer =
    words (Option.bind (read_json file) (Decode.at_pointer pointer))
  in
  let named = at run_service "/enum"
  and owned = at telemetry telemetry_services in
  if named <> None && named = owned then []
  else
    [
      {
        Decode.file = run_service;
        pointer = "/enum";
        message =
          Printf.sprintf "names %s, where %s names %s" (shown named) telemetry
            (shown owned);
      };
    ]

let roots_init_params = "control/RootsInitParams.json"
let inference_provider = "components/schemas/InferenceProvider.json"

let local_providers ~(read_json : string -> Yojson.Safe.t option) =
  let at file pointer =
    words (Option.bind (read_json file) (Decode.at_pointer pointer))
  in
  let named = at roots_init_params "/properties/inference/enum"
  and providers = at inference_provider "/enum" in
  match (named, providers) with
  | Some named, Some providers
    when named <> []
         && List.for_all (fun p -> List.mem p providers) named
         && List.length (List.sort_uniq compare named) = List.length named ->
      []
  | _ ->
      [
        {
          Decode.file = roots_init_params;
          pointer = "/properties/inference/enum";
          message =
            Printf.sprintf "names %s, which are not some of %s's %s"
              (shown named) inference_provider (shown providers);
        };
      ]

let named_refusals ~ids (table : Tables.Control_methods.t) =
  let named pointer target =
    if not (String.starts_with ~prefix:Decode.base target) then []
    else
      let refuse fmt =
        Printf.ksprintf
          (fun message -> [ { Decode.file; pointer; message } ])
          fmt
      in
      match Decode.names ids { target; file; pointer } with
      | None -> refuse "names %s, an $id no JSON Schema states" target
      | Some (schema, _) when not (String.starts_with ~prefix:area schema) ->
          refuse "names %s, %s, which is not a schema of %s" target schema area
      | Some _ -> []
  in
  named "/apiVersion" table.api_version
  @ named "/errorData" table.error_data
  @ List.concat
      (List.mapi
         (fun m (method_ : Tables.Control_methods.methods_item) ->
           let at = "" / "methods" / string_of_int m in
           named (at / "params") method_.params
           @ named (at / "result") method_.result)
         table.methods)
  @ List.concat
      (List.mapi
         (fun m (method_ : Tables.Control_methods.methods_item) ->
           List.concat
             (List.mapi
                (fun s sent ->
                  let pointer =
                    "" / "methods" / string_of_int m / "sends" / string_of_int s
                  in
                  let refuse message =
                    [
                      { Decode.file; pointer; message = sent ^ ", " ^ message };
                    ]
                  in
                  match
                    List.find_opt
                      (fun (n : Tables.Control_methods.notifications_item) ->
                        n.name = sent)
                      table.notifications
                  with
                  | None -> refuse "which the table states no notification of"
                  | Some { from = Client; _ } ->
                      refuse "which the client sends, not the launcher"
                  | Some { from = Launcher; _ } -> [])
                method_.sends))
         table.methods)
  @ List.concat
      (List.mapi
         (fun n (notification : Tables.Control_methods.notifications_item) ->
           named
             ("" / "notifications" / string_of_int n / "params")
             notification.params)
         table.notifications)

let rpc_table = "control/rpc-codes.json"
let codes_table = "errors/codes.json"
let request_cases = "control/request-cases.json"

let rpc_codes ~(read_json : string -> Yojson.Safe.t option) =
  let at = Decode.at_pointer in
  let items = function Some (`List items) -> items | _ -> [] in
  let found = ref [] in
  let refuse file pointer fmt =
    Printf.ksprintf
      (fun message -> found := { Decode.file; pointer; message } :: !found)
      fmt
  in
  (match (read_json rpc_table, read_json codes_table) with
  | None, _ -> refuse rpc_table "" "is missing"
  | _, None ->
      refuse codes_table "" "is missing: control/rpc-codes.json names it"
  | Some rpc, Some codes ->
      let rows = List.mapi (fun i row -> (i, row)) (items (at "/codes" rpc)) in
      let number row =
        match at "/number" row with Some (`Int n) -> Some n | _ -> None
      in
      let row_of n = List.find_opt (fun (_, row) -> number row = Some n) rows in
      List.iter
        (fun (i, row) ->
          List.iter
            (fun key ->
              let value = at ("/" ^ key) row in
              if
                List.exists
                  (fun (j, other) -> j < i && at ("/" ^ key) other = value)
                  rows
              then
                refuse rpc_table
                  ("" / "codes" / string_of_int i / key)
                  "is %s, which an earlier code states"
                  (Option.fold ~none:"nothing" ~some:Yojson.Safe.to_string value))
            [ "number"; "name" ])
        rows;
      let control =
        List.mapi (fun i code -> (i, code)) (items (at "/control/codes" codes))
      in
      let control_code name =
        List.find_opt
          (fun (_, code) -> at "/code" code = Some (`String name))
          control
      in
      List.iter
        (fun (i, code) ->
          match at "/rpc" code with
          | Some (`Int n) when row_of n <> None -> ()
          | stated ->
              refuse codes_table
                ("" / "control" / "codes" / string_of_int i / "rpc")
                "is %s, which is no code of %s"
                (Option.fold ~none:"nothing" ~some:Yojson.Safe.to_string stated)
                rpc_table)
        control;
      List.iter
        (fun (i, row) ->
          match at "/data/code" row with
          | None -> ()
          | Some (`String name) when control_code name <> None -> ()
          | Some stated ->
              refuse rpc_table
                ("" / "codes" / string_of_int i / "data" / "code")
                "is %s, which is no control code of %s"
                (Yojson.Safe.to_string stated)
                codes_table)
        rows;
      let cases = Option.value ~default:`Null (read_json request_cases) in
      List.iteri
        (fun c case ->
          let error = "" / "cases" / string_of_int c / "answer" / "error" in
          match at "/answer/error/code" case with
          | None -> ()
          | Some stated -> (
              let data = at "/answer/error/data" case in
              let data_at = error / "data" in
              let shown = Yojson.Safe.to_string stated in
              match
                Option.bind
                  (match stated with `Int n -> Some n | _ -> None)
                  row_of
              with
              | None ->
                  refuse request_cases (error / "code")
                    "is %s, which is no code of %s" shown rpc_table
              | Some (r, row) -> (
                  let answered =
                    List.filter
                      (fun (_, code) -> at "/rpc" code = Some stated)
                      control
                  in
                  match (at "/data" row, answered, data) with
                  | Some fixed, _, Some data ->
                      List.iter
                        (fun key ->
                          if at ("/" ^ key) data <> at ("/" ^ key) fixed then
                            refuse request_cases (data_at / key)
                              "is %s, where %s states every answer under %s \
                               carries %s (/codes/%d/data)"
                              (Option.fold ~none:"nothing"
                                 ~some:Yojson.Safe.to_string
                                 (at ("/" ^ key) data))
                              rpc_table shown
                              (Option.fold ~none:"nothing"
                                 ~some:Yojson.Safe.to_string
                                 (at ("/" ^ key) fixed))
                              r)
                        [ "code"; "retryable" ]
                  | Some _, _, None | None, _ :: _, None ->
                      refuse request_cases error
                        "carries no data, and every answer under %s carries a \
                         control code"
                        shown
                  | None, [], Some _ ->
                      refuse request_cases data_at
                        "is stated, and no control code of %s is answered \
                         under %s"
                        codes_table shown
                  | None, _ :: _, Some data -> (
                      match at "/code" data with
                      | Some (`String name) -> (
                          match control_code name with
                          | None ->
                              refuse request_cases (data_at / "code")
                                "is %s, which is no control code of %s" name
                                codes_table
                          | Some (k, code) ->
                              if at "/rpc" code <> Some stated then
                                refuse request_cases (data_at / "code")
                                  "is %s, which %s answers under %s, not %s \
                                   (/control/codes/%d/rpc)"
                                  name codes_table
                                  (Option.fold ~none:"nothing"
                                     ~some:Yojson.Safe.to_string
                                     (at "/rpc" code))
                                  shown k
                              else if
                                at "/retryable" data <> at "/retryable" code
                              then
                                refuse request_cases (data_at / "retryable")
                                  "is %s, where %s states %s is %s \
                                   (/control/codes/%d/retryable)"
                                  (Option.fold ~none:"nothing"
                                     ~some:Yojson.Safe.to_string
                                     (at "/retryable" data))
                                  codes_table name
                                  (Option.fold ~none:"nothing"
                                     ~some:Yojson.Safe.to_string
                                     (at "/retryable" code))
                                  k)
                      | _ -> ())
                  | None, [], None -> ())))
        (items (at "/cases" cases)));
  List.rev !found

let refusals ~ids ~read_json table =
  run_services ~read_json @ local_providers ~read_json @ rpc_codes ~read_json
  @ named_refusals ~ids table
