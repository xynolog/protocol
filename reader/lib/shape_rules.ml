(* client/shape-cases.json agrees with Protocol's own validator.

   - Each case names the schema it reads its value as by the stem of one
     schema file of specs/src.
   - A case that is read is a value that schema allows ([Validate]), and a
     case that is refused is one it does not. *)

let table = "client/shape-cases.json"
let ( / ) pointer token = pointer ^ "/" ^ Decode.escape token
let at pointer json = Decode.at_pointer pointer json
let stem file = Filename.remove_extension (Filename.basename file)

let refusals ~ids ~files ~schema_at
    ~(read_json : string -> Yojson.Safe.t option) =
  let found = ref [] in
  let refuse pointer fmt =
    Printf.ksprintf
      (fun message ->
        found := { Decode.file = table; pointer; message } :: !found)
      fmt
  in
  let schema_named name =
    List.filter_map
      (fun file ->
        if stem file = name then
          Option.map
            (fun schema -> (file, schema))
            (schema_at { Validate.file; pointer = "" })
        else None)
      files
  in
  (match Option.bind (read_json table) (at "/cases") with
  | Some (`List cases) ->
      List.iteri
        (fun i case ->
          let pointer = "/cases" / string_of_int i in
          match (at "/schema" case, at "/value" case) with
          | Some (`String name), Some value -> (
              match schema_named name with
              | [ (_, schema) ] ->
                  let allowed =
                    Validate.errors ids schema_at schema value "" = []
                  in
                  let refused = at "/refused" case = Some (`Bool true) in
                  if allowed && refused then
                    refuse pointer
                      "is refused, and %s allows its value, by Protocol's \
                       validator"
                      name
                  else if (not allowed) && not refused then
                    refuse pointer
                      "is read, and %s does not allow its value, by Protocol's \
                       validator"
                      name
              | [] -> refuse (pointer / "schema") "%s names no schema file" name
              | _ ->
                  refuse (pointer / "schema")
                    "%s names more than one schema file" name)
          | _ -> ())
        cases
  | _ -> ());
  List.rev !found
