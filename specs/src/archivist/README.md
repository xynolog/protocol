# archivist — the Archivist's HTTP surface

A second OpenAPI document, beside the gateway's `../openapi.json`: the routes
the Archivist serves over HTTP, which the gateway calls and a gateway in any
language must call the same way.

## Why it is its own document

Its routes are not the gateway's. Were they in `../openapi.json`, every
consumer of the gateway's API would believe the gateway serves them, and the
gateway, which refuses to start unless its routes are exactly its document's,
would have to. So the paths live here, and so does the one piece of protocol
that differs: the 401 (`responses/ArchivistUnauthorized.json`), whose challenge names no
resource metadata, because the Archivist publishes none.

What is the Archivist's alone is here, beside its document: its health
answer, its event streams, the anchored-text store's entries, and its
configuration document (`ArchivistConfig`). Nothing the gateway serves reaches
them, so the gateway's document does not list them. The shapes the two
documents share are not repeated: bodies `$ref` the shared
`../components/schemas/`, which the gateway's document lists. The upload body
(`ResourceUpload`) is one schema that both documents' `POST /resources` name,
because the gateway forwards it untouched.

## What holds each side to it

- The reader's protocol rules (`reader/lib/protocol_rules.ml`, `make reader`)
  hold this document to the same rules as the gateway's: every operation says
  whether it is public, every response has a body schema, every error is an
  `ErrorResponse`, every 401 carries its challenge, every operation declares a
  500. They also hold this document's waits in order: the hold on a read of
  anchored text (`settleWaitSeconds`) shorter than `readSeconds`, and
  `connectSeconds` and `readSeconds` together shorter than a client's
  `httpRequestTimeoutMs`.
- The Archivist conformance suite (`conformance/ocaml/archivist`): every reply
  it reads from the Archivist's HTTP surface must match its declaration here.
- The gateway conformance suite's stand-in Archivist
  (`conformance/ocaml/harness/archivist_standin.ml`): every request the
  gateway sends it must be an operation declared here, and every reply the
  stand-in gives must match its declaration.
