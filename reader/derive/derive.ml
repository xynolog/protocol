(* derive.exe SPECS: every table's model (Model), as OCaml source on
   standard output, and the conformance harness's models
   (Places.harness_models). SPECS is specs/src.

   Each table is a module named as its schema (Places.model_of), of types
   and strict decoders, its own model [t] and [decode] last; then [t], every
   table's model, and [load], which decodes each table through its own.
   Every refusal is printed, and any stops the build. *)

open Xynolog_protocol_schema
open Xynolog_protocol_model.Model

(* Printing. A type's decoder has its name, but a table's own, [decode]. *)
let decoder_name = function "t" -> "decode" | name -> name

(* A constant's type, which holds it. *)
let constant_type : Yojson.Safe.t -> ty = function
  | `String _ -> String
  | `Int _ -> Int
  | `Float _ -> Float
  | `Bool _ -> Bool
  | v ->
      invalid_arg
        ("a constant of no scalar type, which no table states: "
       ^ Yojson.Safe.to_string v)

(* A constant as an OCaml expression of its type. *)
let constant_expr : Yojson.Safe.t -> string = function
  | `String s -> Printf.sprintf "%S" s
  | `Int n -> Printf.sprintf "(%d)" n
  | `Float f -> Printf.sprintf "(%h)" f
  | `Bool b -> string_of_bool b
  | v -> invalid_arg (Yojson.Safe.to_string v)

(* What checks that a value is the constant, as a decoder of unit. *)
let check = function
  | `String s -> Printf.sprintf "(Decode.literal %S)" s
  | v ->
      Printf.sprintf "(Decode.constant (Yojson.Safe.from_string %S))"
        (Yojson.Safe.to_string v)

let rec type_expr = function
  | String -> "string"
  | Int -> "int"
  | Float -> "float"
  | Bool -> "bool"
  | Json -> "Yojson.Safe.t"
  | Json_schema -> "Schema.t"
  | Reference -> "Decode.reference"
  | Constant v -> type_expr (constant_type v)
  | List t -> "(" ^ type_expr t ^ ") list"
  | Map (keys, values) ->
      "(" ^ type_expr keys ^ " * " ^ type_expr values ^ ") list"
  | Nullable t -> "(" ^ type_expr t ^ ") option"
  | Named n -> n

let rec decoder = function
  | String -> "Decode.string"
  | Int -> "Decode.int"
  | Float -> "Decode.number"
  | Bool -> "Decode.bool"
  | Json -> "Decode.json"
  | Json_schema -> "Schema.decode"
  | Reference -> "Decode.reference"
  | Constant v ->
      Printf.sprintf "(fun cursor -> %s cursor; %s)" (check v) (constant_expr v)
  | List t -> "(Decode.list " ^ decoder t ^ ")"
  | Map (String, values) -> "(Decode.map " ^ decoder values ^ ")"
  | Map (keys, values) ->
      "(Decode.map_by " ^ decoder keys ^ " " ^ decoder values ^ ")"
  | Nullable t -> "(Decode.nullable " ^ decoder t ^ ")"
  | Named n -> decoder_name n

let strings xs =
  "[" ^ String.concat "; " (List.map (Printf.sprintf "%S") xs) ^ "]"

let print_decl buffer = function
  | Record (name, []) ->
      Printf.bprintf buffer
        "type %s = unit\n\nlet %s = Decode.obj (fun _ -> ())\n\n" name
        (decoder_name name)
  | Record (name, fields) ->
      Printf.bprintf buffer "type %s = {\n" name;
      List.iter
        (fun f ->
          Printf.bprintf buffer "  %s : %s;\n" f.label
            (if f.required then type_expr f.ty
             else "(" ^ type_expr f.ty ^ ") option"))
        fields;
      Printf.bprintf buffer
        "}\n\nlet %s =\n  Decode.obj (fun o : %s ->\n    {\n"
        (decoder_name name) name;
      List.iter
        (fun f ->
          Printf.bprintf buffer "      %s = Decode.%s o %S %s;\n" f.label
            (if f.required then "required" else "optional")
            f.key (decoder f.ty))
        fields;
      Printf.bprintf buffer "    })\n\n"
  | Enum (name, cases) ->
      Printf.bprintf buffer "type %s = %s\n\n" name
        (String.concat " | " (List.map fst cases));
      Printf.bprintf buffer "let %s = Decode.enum [ %s ]\n\n" name
        (String.concat "; "
           (List.map (fun (c, v) -> Printf.sprintf "(%S, %s)" v c) cases));
      Printf.bprintf buffer "let %s_value = function %s\n\n" name
        (String.concat " | "
           (List.map (fun (c, v) -> Printf.sprintf "%s -> %S" c v) cases));
      Printf.bprintf buffer "let %s_all = [ %s ]\n\n" name
        (String.concat "; " (List.map fst cases))
  | Variant (name, cases) ->
      Printf.bprintf buffer "type %s =\n" name;
      List.iter
        (fun c ->
          match c.payload with
          | Constant _ -> Printf.bprintf buffer "  | %s\n" c.name
          | ty -> Printf.bprintf buffer "  | %s of %s\n" c.name (type_expr ty))
        cases;
      let build c =
        match c.payload with
        | Constant v ->
            Printf.sprintf "(fun cursor -> %s cursor; %s)" (check v) c.name
        | ty ->
            Printf.sprintf "(fun cursor -> %s (%s cursor))" c.name (decoder ty)
      in
      let objects =
        List.filter
          (fun c -> match c.dispatch with Object _ -> true | Kind _ -> false)
          cases
      in
      Printf.bprintf buffer
        "\nlet %s cursor =\n  match Decode.kind cursor with\n" name;
      if objects <> [] then (
        Printf.bprintf buffer "  | Decode.Object ->\n      Decode.one_of [\n";
        List.iter
          (fun c ->
            match c.dispatch with
            | Object { required; stated; fixed } ->
                Printf.bprintf buffer
                  "        { Decode.required = %s; stated = %s; fixed = [%s]; \
                   decode = %s };\n"
                  (strings required) (strings stated)
                  (String.concat "; "
                     (List.map
                        (fun (k, v) ->
                          Printf.sprintf "(%S, Yojson.Safe.from_string %S)" k
                            (Yojson.Safe.to_string v))
                        fixed))
                  (build c)
            | Kind _ -> ())
          objects;
        Printf.bprintf buffer "      ] cursor\n");
      List.iter
        (fun c ->
          match c.dispatch with
          | Kind ks ->
              Printf.bprintf buffer "  | %s -> %s cursor\n"
                (String.concat " | " (List.map (fun k -> "Decode." ^ k) ks))
                (build c)
          | Object _ -> ())
        cases;
      Printf.bprintf buffer
        "  | _ -> Decode.refuse cursor \"is none of the oneOf's members\"\n\n"

let root =
  match Sys.argv with
  | [| _; root |] -> root
  | _ ->
      prerr_endline "usage: derive.exe SPECS";
      exit 2

let ocaml { tables; harness } =
  let buffer = Buffer.create (1 lsl 20) in
  Buffer.add_string buffer
    "(* Every table's model, derived from its schema by reader/derive. *)\n\n";
  List.iter
    (fun m ->
      Printf.bprintf buffer "module %s = struct\n"
        (String.capitalize_ascii m.model_name);
      Printf.bprintf buffer "  (* %s, by %s *)\n\n" m.path m.schema;
      List.iter (print_decl buffer) m.decls;
      Buffer.add_string buffer "end\n\n")
    tables;
  (* Each model of the conformance harness's files, the corpus's and the
     manifest's: a module of the types its schema gives, its own [t] and
     [decode] last, though it is no table's. *)
  List.iter
    (fun m ->
      Printf.bprintf buffer "module %s = struct\n"
        (String.capitalize_ascii m.model_name);
      Printf.bprintf buffer "  (* %s, the conformance harness's *)\n\n" m.path;
      List.iter (print_decl buffer) m.decls;
      Buffer.add_string buffer "end\n\n")
    harness;
  let derived = List.map (fun m -> (m.path, m.model_name)) tables in
  Buffer.add_string buffer "type t = {\n";
  List.iter
    (fun (_, model) ->
      Printf.bprintf buffer "  %s : %s.t;\n" model
        (String.capitalize_ascii model))
    derived;
  Buffer.add_string buffer "}\n\n";
  Buffer.add_string buffer
    "type loader = { table : 'a. string -> (Decode.cursor -> 'a) -> ('a, \
     Decode.error list) result }\n\n\
     let ( let+ ) result f = Result.map f result\n\n\
     let ( and+ ) a b =\n\
    \  match (a, b) with\n\
    \  | Ok a, Ok b -> Ok (a, b)\n\
    \  | Error e, Ok _ | Ok _, Error e -> Error e\n\
    \  | Error a, Error b -> Error (a @ b)\n\n\
     let files = [ ";
  Buffer.add_string buffer
    (String.concat "; "
       (List.map (fun (table, _) -> Printf.sprintf "%S" table) derived));
  Buffer.add_string buffer " ]\n\nlet load { table } =\n";
  List.iteri
    (fun i (table, model) ->
      Printf.bprintf buffer "  %s %s = table %S %s.decode\n"
        (if i = 0 then "let+" else "and+")
        model table
        (String.capitalize_ascii model))
    derived;
  Printf.bprintf buffer "  in\n  { %s }\n"
    (String.concat "; " (List.map snd derived));
  print_string (Buffer.contents buffer)

let () =
  match derive root with
  | Ok model -> ocaml model
  | Error refusals ->
      List.iter prerr_endline refusals;
      Printf.eprintf "%d refusals\n" (List.length refusals);
      exit 1
