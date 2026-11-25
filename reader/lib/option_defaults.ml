(* client/option-defaults.json against what it names.

   - A row names a method or a function of client/surface.json, and a
     parameter it takes: a namespace's method's parameter, or a member of a
     module, which states no parameters.
   - A row's value is of its option's schema: the one surface.json states for
     the parameter, or the parameter's member [option], where surface.json
     states it; the row's own [schema] otherwise, and only then.
   - A row [sends] only for a namespace's method, which has cases to send it.
   - A case of a method that gives none of the parameters its rows default
     sends its arguments and each row's value where the row [sends] it, and
     nothing else.
   - A case that gives a method only the parameters it requires sends no
     member its request's schema states a [default] of: that default is the
     service's, and a client sends nothing its caller left out. *)

let file = "client/option-defaults.json"
let surface_file = "client/surface.json"
let ( / ) pointer token = pointer ^ "/" ^ Decode.escape token
let at = Decode.at_pointer
let text = function Some (`String s) -> Some s | _ -> None
let items = function Some (`List items) -> items | _ -> []
let members = function Some (`Assoc members) -> members | _ -> []
let indexed items = List.mapi (fun index item -> (index, item)) items
let shown = Yojson.Safe.to_string

(* The schema node a [$ref] names, by its [$id], written in [file]. *)
let referenced ~ids ~read_json ~file target =
  Option.bind
    (Decode.names ids { Decode.target; file; pointer = "" })
    (fun (named, pointer) -> Option.bind (read_json named) (at pointer))

(* [node] with each [$ref] it is followed, until one is not. *)
let rec followed ~ids ~read_json ~file node =
  match text (at "/$ref" node) with
  | Some target -> (
      match referenced ~ids ~read_json ~file target with
      | Some next -> followed ~ids ~read_json ~file next
      | None -> node)
  | None -> node

(* [sent] less what [pointer] names in it, and less each object that is then
   empty. *)
let rec without pointer (sent : Yojson.Safe.t) =
  match (String.split_on_char '/' pointer, sent) with
  | [ ""; key ], `Assoc fields -> `Assoc (List.remove_assoc key fields)
  | "" :: key :: rest, `Assoc fields -> (
      match List.assoc_opt key fields with
      | None -> sent
      | Some inner -> (
          match without ("/" ^ String.concat "/" rest) inner with
          | `Assoc [] -> `Assoc (List.remove_assoc key fields)
          | inner ->
              `Assoc
                (List.map
                   (fun (k, v) -> if k = key then (k, inner) else (k, v))
                   fields)))
  | _ -> sent

let refusals ~ids ~(read_json : string -> Yojson.Safe.t option)
    ~(schema_at : Validate.place -> Schema.t option) =
  let found = ref [] in
  let refuse file pointer fmt =
    Printf.ksprintf
      (fun message -> found := { Decode.file; pointer; message } :: !found)
      fmt
  in
  (match (read_json file, read_json surface_file) with
  | None, _ -> refuse file "" "is missing"
  | _, None -> refuse surface_file "" "is missing: option defaults name it"
  | Some table, Some surface ->
      let rows = indexed (items (at "/defaults" table)) in
      let namespaces = indexed (items (at "/namespaces" surface)) in
      let modules = items (at "/modules" surface) in
      let channels =
        items (Option.bind (read_json "bus/registry.json") (at "/channels"))
      in
      (* The namespace's method a name of it, with where surface.json states it. *)
      let surface_method name =
        match String.split_on_char '.' name with
        | [ namespace; called ] ->
            List.find_map
              (fun (n, ns) ->
                if text (at "/namespace" ns) <> Some namespace then None
                else
                  List.find_map
                    (fun (m, method_) ->
                      if text (at "/method" method_) = Some called then
                        Some
                          ( "" / "namespaces" / string_of_int n / "methods"
                            / string_of_int m,
                            method_ )
                      else None)
                    (indexed (items (at "/methods" ns))))
              namespaces
        | _ -> None
      in
      let module_function name =
        match String.split_on_char '.' name with
        | [ module_; member ] ->
            List.exists
              (fun m ->
                text (at "/module" m) = Some module_
                && List.exists
                     (fun mm ->
                       text (at "/member" mm) = Some member
                       && text (at "/kind" mm) = Some "function")
                     (items (at "/members" m)))
              modules
        | _ -> false
      in
      let errors schema value pointer =
        List.map snd (Validate.errors ids schema_at schema value pointer)
      in
      (* Each row's own value, held to its option's schema. *)
      List.iter
        (fun (i, row) ->
          let row_at = "" / "defaults" / string_of_int i in
          let name = Option.value ~default:"" (text (at "/method" row)) in
          let parameter =
            Option.value ~default:"" (text (at "/parameter" row))
          in
          let option = text (at "/option" row) in
          let value = Option.value ~default:`Null (at "/value" row) in
          let own_schema = at "/schema" row in
          match surface_method name with
          | Some (method_at, method_) -> (
              if own_schema <> None then
                refuse file (row_at / "schema")
                  "states a schema, and surface.json states %s's" name;
              match
                List.find_opt
                  (fun (_, p) -> text (at "/parameter" p) = Some parameter)
                  (indexed (items (at "/parameters" method_)))
              with
              | None ->
                  refuse file (row_at / "parameter") "%s takes no parameter %s"
                    name parameter
              | Some (k, _) -> (
                  let place : Validate.place =
                    {
                      file = surface_file;
                      pointer =
                        method_at / "parameters" / string_of_int k / "schema";
                    }
                  in
                  match schema_at place with
                  | None ->
                      refuse file (row_at / "parameter")
                        "has no schema the reader can read"
                  | Some schema ->
                      let checked, within =
                        match option with
                        | None -> (value, "")
                        | Some option ->
                            (`Assoc [ (option, value) ], "" / option)
                      in
                      List.iter
                        (fun (pointer, message) ->
                          if String.starts_with ~prefix:within pointer then
                            refuse file (row_at / "value") "%s: %s" pointer
                              message)
                        (Validate.errors ids schema_at schema checked "")))
          | None ->
              if not (module_function name) then
                refuse file (row_at / "method")
                  "names %s, which is no method or function of surface.json"
                  name
              else (
                if at "/sends" row <> None then
                  refuse file (row_at / "sends")
                    "%s is a function of a module, which no case sends" name;
                match schema_at { file; pointer = row_at / "schema" } with
                | None ->
                    refuse file row_at
                      "states no schema, and surface.json states none for %s"
                      name
                | Some schema ->
                    List.iter
                      (fun message ->
                        refuse file (row_at / "value") "%s" message)
                      (errors schema value "")))
        rows;
      (* The cases of each namespace's method. *)
      List.iter
        (fun (n, ns) ->
          List.iter
            (fun (m, method_) ->
              let called =
                Option.value ~default:"" (text (at "/namespace" ns))
                ^ "."
                ^ Option.value ~default:"" (text (at "/method" method_))
              in
              let method_at =
                "" / "namespaces" / string_of_int n / "methods"
                / string_of_int m
              in
              let defaulted =
                List.filter
                  (fun (_, row) -> text (at "/method" row) = Some called)
                  rows
              in
              let parameters = items (at "/parameters" method_) in
              let required =
                List.filter_map
                  (fun p ->
                    if at "/required" p = Some (`Bool true) then
                      text (at "/parameter" p)
                    else None)
                  parameters
              in
              let request =
                Option.bind
                  (text (at "/via/request" method_))
                  (fun channel ->
                    List.find_map
                      (fun c ->
                        if text (at "/channel" c) = Some channel then
                          Option.bind
                            (text (at "/schema" c))
                            (fun schema ->
                              referenced ~ids ~read_json
                                ~file:"bus/registry.json"
                                (Decode.base ^ schema ^ ".json"))
                        else None)
                      channels)
              in
              List.iter
                (fun (c, case) ->
                  let case_at = method_at / "cases" / string_of_int c in
                  let args = members (at "/args" case) in
                  let sent =
                    Option.value ~default:(`Assoc []) (at "/sends" case)
                  in
                  let gives_defaulted =
                    List.exists
                      (fun (_, row) ->
                        List.mem_assoc
                          (Option.value ~default:""
                             (text (at "/parameter" row)))
                          args)
                      defaulted
                  in
                  (if defaulted <> [] && not gives_defaulted then
                     let rest =
                       List.fold_left
                         (fun rest (i, row) ->
                           match text (at "/sends" row) with
                           | None -> rest
                           | Some pointer ->
                               let value =
                                 Option.value ~default:`Null (at "/value" row)
                               in
                               if at pointer sent <> Some value then
                                 refuse surface_file (case_at / "sends")
                                   "gives none of %s's defaulted parameters, \
                                    and sends %s at %s, not \
                                    option-defaults.json's %s (/defaults/%d)"
                                   called
                                   (Option.fold ~none:"nothing" ~some:shown
                                      (at pointer sent))
                                   pointer (shown value) i;
                               without pointer rest)
                         sent defaulted
                     in
                     List.iter
                       (fun (key, value) ->
                         if not (List.exists (fun (_, arg) -> arg = value) args)
                         then
                           refuse surface_file
                             (case_at / "sends" / key)
                             "is neither an argument nor a default of \
                              option-defaults.json")
                       (members (Some rest)));
                  let only_required =
                    List.for_all (fun (key, _) -> List.mem key required) args
                  in
                  match request with
                  | Some schema when only_required ->
                      let rec defaults pointer node (value : Yojson.Safe.t) =
                        let node =
                          followed ~ids ~read_json ~file:"bus/registry.json"
                            node
                        in
                        List.iter
                          (fun (key, inner) ->
                            match at ("" / "properties" / key) node with
                            | None -> ()
                            | Some property ->
                                let property =
                                  followed ~ids ~read_json
                                    ~file:"bus/registry.json" property
                                in
                                (match at "/default" property with
                                | Some default ->
                                    refuse surface_file (pointer / key)
                                      "is sent, and the request's schema \
                                       states its default, %s: a client sends \
                                       nothing its caller left out"
                                      (shown default)
                                | None -> ());
                                defaults (pointer / key) property inner)
                          (members (Some value))
                      in
                      defaults (case_at / "sends") schema sent
                  | _ -> ())
                (indexed (items (at "/cases" method_))))
            (indexed (items (at "/methods" ns))))
        namespaces);
  List.rev !found
