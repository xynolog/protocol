(* What errors/codes.json's vocabularies are named and read from agree with
   what states them elsewhere.
   - A schema that says its string is a code ([x-xynolog-codes]) names a
     vocabulary errors/codes.json states: one of its members that lists
     codes.
   - Each of a transport code's [limitRefusals] is a refusal the gateway
     makes: a row of gateway/messages.json with its status and code. *)

let table = "errors/codes.json"
let messages = "gateway/messages.json"
let ( / ) pointer token = pointer ^ "/" ^ Decode.escape token

let vocabularies = function
  | `Assoc members ->
      List.filter_map
        (fun (name, value) ->
          match value with
          | `Assoc fields when List.mem_assoc "codes" fields -> Some name
          | _ -> None)
        members
  | _ -> []

(* Each [x-xynolog-codes] in [json], by its schema's pointer. *)
let rec named pointer : Yojson.Safe.t -> (string * Yojson.Safe.t) list =
  function
  | `Assoc members ->
      List.concat_map
        (fun (key, value) ->
          (if key = "x-xynolog-codes" then [ (pointer, value) ] else [])
          @ named (pointer / key) value)
        members
  | `List items ->
      List.concat
        (List.mapi (fun i v -> named (pointer / string_of_int i) v) items)
  | _ -> []

let vocabulary_names ~schema_files ~(read_json : string -> Yojson.Safe.t option)
    =
  match read_json table with
  | None ->
      [
        {
          Decode.file = table;
          pointer = "";
          message = "is missing: x-xynolog-codes names its vocabularies";
        };
      ]
  | Some codes ->
      let known = vocabularies codes in
      List.concat_map
        (fun file ->
          match read_json file with
          | None -> []
          | Some json ->
              List.filter_map
                (fun (pointer, value) ->
                  match value with
                  | `String group when List.mem group known -> None
                  | stated ->
                      Some
                        {
                          Decode.file;
                          pointer;
                          message =
                            Printf.sprintf
                              "x-xynolog-codes names %s, which is no \
                               vocabulary of %s (%s)"
                              (Yojson.Safe.to_string stated)
                              table (String.concat ", " known);
                        })
                (named "" json))
        schema_files

let limit_refusals ~(read_json : string -> Yojson.Safe.t option) =
  match (read_json table, read_json messages) with
  | Some codes, Some gateway ->
      let field name = function
        | `Assoc members -> List.assoc_opt name members
        | _ -> None
      in
      let list = function Some (`List items) -> items | _ -> [] in
      let made =
        List.map
          (fun row -> (field "status" row, field "code" row))
          (list (field "messages" gateway))
      in
      List.concat
        (List.mapi
           (fun i row ->
             List.concat
               (List.mapi
                  (fun j refusal ->
                    let pair = (field "status" refusal, field "code" refusal) in
                    if List.mem pair made then []
                    else
                      [
                        {
                          Decode.file = table;
                          pointer =
                            "" / "transport" / "codes" / string_of_int i
                            / "limitRefusals" / string_of_int j;
                          message =
                            Printf.sprintf "is %s, which no row of %s states"
                              (Yojson.Safe.to_string refusal)
                              messages;
                        };
                      ])
                  (list (field "limitRefusals" row))))
           (list (Option.bind (field "transport" codes) (field "codes"))))
  | _ -> []

let refusals ~schema_files ~(read_json : string -> Yojson.Safe.t option) =
  limit_refusals ~read_json @ vocabulary_names ~schema_files ~read_json
