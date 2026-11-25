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
   each once: it is a subset stated by hand, and held to the vocabulary. *)

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

let refusals ~ids ~read_json table =
  run_services ~read_json @ local_providers ~read_json
  @ named_refusals ~ids table
