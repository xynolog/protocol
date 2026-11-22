(** Each table's model, derived from its schema in the table dialect
    ([specs/README.md], "Table schemas"), and the conformance harness's models:
    the types each one's values have, as data. [derive.exe] writes them as OCaml
    ([Tables]); another reader of Protocol writes them as what it writes. Names
    are OCaml's: a label or a type is the snake case of what it is written as, a
    constructor that capitalized. *)

(** The type a node derives. *)
type ty =
  | String
  | Int
  | Float
  | Bool
  | Json  (** JSON by nature, as its [$comment] says. *)
  | Json_schema  (** A JSON Schema, [JsonSchema.json]'s. *)
  | Reference  (** A member named [$ref]: a reference the reader resolves. *)
  | Constant of Yojson.Safe.t  (** One [enum] value or a [const]. *)
  | List of ty
  | Map of ty * ty
      (** Its keys' type, a string's or an enum's, and its values', in the
          file's order. *)
  | Nullable of ty
  | Named of string  (** A declaration of the model, by its name. *)

type field = { label : string; key : string; ty : ty; required : bool }
(** A record's member: its OCaml [label], the JSON member [key] it is read from,
    its type, and whether the record requires it. *)

type case = { name : string; payload : ty; dispatch : dispatch }
(** A variant's constructor, its payload, and what tells its values apart. *)

and dispatch =
  | Object of {
      required : string list;
      stated : string list;
      fixed : (string * Yojson.Safe.t) list;
    }
      (** An object: the members it requires and states, and those it fixes to a
          value. *)
  | Kind of string list  (** A value of another JSON kind: [Decode]'s. *)

type decl =
  | Record of string * field list
  | Enum of string * (string * string) list
      (** Its constructors, each with the string value it is. *)
  | Variant of string * case list

type model = {
  path : string;
  schema : string;
  model_name : string;
  decls : decl list;
}
(** A table's model, or one of the harness's: the file it reads (a table, or the
    harness's own schema), its schema's file, its model's name
    ([Places.model_of], [Places.corpus_model_of]) and its declarations, each
    after what it names, its own [t] last. *)

type t = { tables : model list; harness : model list }

val derive : string -> (t, string list) result
(** [derive specs]: every model of [specs], a [specs/src] directory, in
    [Places.tables]' order and then [Places.harness_models]'; or every refusal,
    each with its file and JSON pointer. *)
