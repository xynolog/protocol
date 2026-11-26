(* client/timing.json's [override], held to what it names.

   A row with [override] is one a caller may override, and the member says
   where: [client] and [transport] are the client's record and the transport's;
   [Owner.member] is
   - a member of the client type Owner (client/types/Owner.json), which the
     type states, or
   - a parameter of a state unit (client/surface.json's [stateUnits]) or of a
     function of one of its [modules], which surface.json states no
     parameters of, so that Owner is all the reader can hold it to.
   No two rows are overridden at the same place.

   The conformance corpus's cases override no row without [override]
   (Corpus). *)

let file = "client/timing.json"
let surface_file = "client/surface.json"
let ( / ) pointer token = pointer ^ "/" ^ Decode.escape token
let at = Decode.at_pointer
let text = function Some (`String s) -> Some s | _ -> None
let items = function Some (`List items) -> items | _ -> []

(* The names of the rows of [table] that a caller may override, each with
   where. *)
let overridable (table : Tables.Client_timing.t) =
  List.filter_map
    (fun (row : Tables.Client_timing.timing_item) ->
      Option.map (fun place -> (row.name, place)) row.override)
    table.timing

let refusals ~(read_json : string -> Yojson.Safe.t option) =
  let found = ref [] in
  let refuse pointer fmt =
    Printf.ksprintf
      (fun message -> found := { Decode.file; pointer; message } :: !found)
      fmt
  in
  (match (read_json file, read_json surface_file) with
  | None, _ -> refuse "" "is missing"
  | _, None -> refuse "" "is missing: %s names it" surface_file
  | Some table, Some surface ->
      let modules = items (at "/modules" surface) in
      let state_unit owner =
        List.exists
          (fun unit -> text (at "/unit" unit) = Some owner)
          (items (at "/stateUnits" surface))
      in
      let function_ owner =
        List.exists
          (fun module_ ->
            List.exists
              (fun member ->
                text (at "/member" member) = Some owner
                && text (at "/kind" member) = Some "function")
              (items (at "/members" module_)))
          modules
      in
      let places = Hashtbl.create 32 in
      List.iteri
        (fun i row ->
          let row_at = "/timing" / string_of_int i / "override" in
          let name = Option.value ~default:"" (text (at "/name" row)) in
          match text (at "/override" row) with
          | None | Some ("client" | "transport") -> ()
          | Some place -> (
              (match Hashtbl.find_opt places place with
              | Some other ->
                  refuse row_at "%s is overridden where %s is: %s" name other
                    place
              | None -> Hashtbl.add places place name);
              match String.split_on_char '.' place with
              | [ owner; member ] -> (
                  match read_json ("client/types/" ^ owner ^ ".json") with
                  | Some type_ ->
                      if at ("/properties" / member) type_ = None then
                        refuse row_at "%s is no member of the client type %s"
                          member owner
                  | None ->
                      if not (state_unit owner || function_ owner) then
                        refuse row_at
                          "names %s, which is no client type, state unit or \
                           function of %s"
                          owner surface_file)
              | _ -> refuse row_at "is not Owner.member: %s" place))
        (items (at "/timing" table)));
  List.rev !found
