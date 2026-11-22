# The reader

Protocol's one reader of itself, in OCaml: the dune package
`xynolog-protocol`, from the repository's root `dune-project`. It reads
`specs/src` into a model that refuses what it does not know, decodes each
table through a model derived from its schema, and holds the specs to the
rules a schema cannot state. Every check of the specs is the reader's, and
Protocol's consumers read Protocol through it rather than restating
it: sdk's generator, and the conformance harness in
[`../conformance/ocaml`](../conformance/ocaml/README.md).

## The program

`xynolog-protocol` takes the repository's root:

| Command | What it does |
| --- | --- |
| `xynolog-protocol check ROOT` | Reads every file of `specs/src` into the model, every table through its schema's model and every rule of `lib/` held; reads the conformance corpus (`conformance/sdk`, `conformance/worker`) and `contract/ARCHIVIST.md` against the spec; and refuses a JSON file of `specs/src` not spelled as `JSON.stringify(value, null, 2)` and a newline spell it. |
| `xynolog-protocol bundle ROOT` | Reads `specs/src`, then writes each OpenAPI document as one file: `specs/openapi.json` and `specs/archivist.openapi.json`. |

Each refusal is printed with its file and JSON pointer, and any refusal fails
the run. `make reader` runs `check`, `make bundle` runs `bundle`, and
`make validate` runs both, in the OCaml toolchain's container.

## What is where

| Directory | Library | What it holds |
| --- | --- | --- |
| [`schema/`](schema/) | `xynolog-protocol.schema` | JSON Schema 2020-12 and OpenAPI 3.1 as Protocol writes them (`Schema`, `Openapi`), the strict decoder every model is built on (`Decode`), a value held to a schema (`Validate`), a `pattern` read as ECMA-262 reads it (`Pattern`), and where each table and its schema are (`Places`). |
| [`lib/`](lib/) | `xynolog-protocol` | `Spec`, the whole of `specs/src` read into the model; `Tables`, every table's model, derived at build time; the rules a schema cannot state, one module each (`*_rules.ml`, `Reply_bounds`, `Offset_units`, `Client_surface`); `Corpus`, the conformance case files; `Bundle`; and `Repository`, how each JSON file is spelled. |
| [`model/`](model/) | `xynolog-protocol.model` | `Model`, each table's model derived from its schema, as data: its records, enums and variants, in the order they are written. |
| [`derive/`](derive/) | | `derive.exe`, which writes `Model` as OCaml, `Tables`: a dune rule in `lib/dune` runs it whenever `specs/src` changes, so no derived model is committed. |
| [`bin/`](bin/) | | The `xynolog-protocol` program. |
| [`test/`](test/) | | The reader's own tests, among them a census that the model reads every file of `specs/src` and refuses copies of it changed. |

## Reading Protocol from another repository

A consumer vendors its pinned Protocol and links the libraries:
`xynolog-protocol.schema` for the schema model alone, `xynolog-protocol`
for `Spec` and the tables' models, and `xynolog-protocol.model` for each
table's model as data, which a generator writes as another language's
types. sdk's generator has the submodule at
`protocol/`, a link to it in `codegen/`, and `(vendored_dirs protocol)` in
`codegen/dune`. `Spec.load` reads a copy of `specs/src` and holds it to
every rule of `lib/`, so a consumer refuses what Protocol refuses. The
checks of the checkout itself, the spelling, the corpus and
`contract/ARCHIVIST.md`, are `check`'s alone.
