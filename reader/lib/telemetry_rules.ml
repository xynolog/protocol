(* service-telemetry/telemetry.json lists the values some attributes carry,
   and the table's format states each list in full: an attribute whose values
   are a vocabulary's words lists that vocabulary's [enum], in its order,
   wherever a span or a metric carries it.

   - [anchor.method]: client/types/AnchorMethod.json.
   - [inference.provider]: components/schemas/InferenceProvider.json. *)

let telemetry = "service-telemetry/telemetry.json"

let vocabularies =
  [
    ("anchor.method", "client/types/AnchorMethod.json");
    ("inference.provider", "components/schemas/InferenceProvider.json");
  ]

let ( / ) pointer token = pointer ^ "/" ^ Decode.escape token
let at = Decode.at_pointer
let items = function Some (`List items) -> items | _ -> []
let shown = Option.fold ~none:"none" ~some:Yojson.Safe.to_string

let refusals ~(read_json : string -> Yojson.Safe.t option) =
  let found = ref [] in
  let refuse file pointer fmt =
    Printf.ksprintf
      (fun message -> found := { Decode.file; pointer; message } :: !found)
      fmt
  in
  (match read_json telemetry with
  | None -> refuse telemetry "" "is missing: its attribute values are checked"
  | Some table ->
      List.iter
        (fun (key, vocabulary) ->
          match read_json vocabulary with
          | None ->
              refuse vocabulary ""
                "is missing: telemetry's %s values are checked against it" key
          | Some schema ->
              let words = at "/enum" schema in
              let carried =
                List.concat_map
                  (fun rows ->
                    List.concat
                      (List.mapi
                         (fun i row ->
                           List.concat
                             (List.mapi
                                (fun j attribute ->
                                  match at "/key" attribute with
                                  | Some (`String k) when k = key ->
                                      [
                                        ( "" / rows / string_of_int i
                                          / "attributes" / string_of_int j
                                          / "values",
                                          at "/values" attribute );
                                      ]
                                  | _ -> [])
                                (items (at "/attributes" row))))
                         (items (at ("" / rows) table))))
                  [ "spans"; "metrics" ]
              in
              if carried = [] then
                refuse telemetry "" "has no `%s` attribute" key;
              List.iter
                (fun (pointer, values) ->
                  if values <> words then
                    refuse telemetry pointer "lists %s values %s, not %s's %s"
                      key (shown values)
                      (Filename.remove_extension (Filename.basename vocabulary))
                      (shown words))
                carried)
        vocabularies);
  List.rev !found
