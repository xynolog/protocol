# The launcher's control API

`xynolog control` serves the launcher's control API: a program drives the
launcher the way a person drives its commands. Desktop is its client. Each
method is one of the launcher's commands, run by the code the command runs;
what the command would print is the request's progress, and what it would
report is the request's result.

This document states the rules every launcher and every client keep. The
shapes are not stated here.
[`specs/src/control/methods.json`](../specs/src/control/methods.json)
names each method, whether it is exclusive, the notifications it may send,
and the schema of its params and of its result, each schema by its `$id`
(`https://xynolog.org/protocol/AboutParams.json`), never by its path; the
schemas beside it are the shapes, and each it names is one of them; the
`control` group of
[`specs/src/errors/codes.json`](../specs/src/errors/codes.json) is
why a request failed, the JSON-RPC code each is answered under, and whether
it is retryable; and
[`specs/src/control/rpc-codes.json`](../specs/src/control/rpc-codes.json)
is each JSON-RPC code the API answers with. `the consumer-owned generator`
generates the launcher's Go types and Desktop's TypeScript types from them,
and the schemas both sides validate against.

**How this document is held.** Each rule ends with *Held by* and what fails
when a launcher or a client breaks it: a case table, a census or a test, by
its path. A rule marked "Held by no case" is one nothing checks. Nothing
checks that a path named here exists.

## Framing

- **F1.** The API is JSON-RPC 2.0. Requests and the client's notifications
  arrive on stdin; answers and the launcher's notifications leave on stdout.
  Each message is one line of UTF-8 JSON. Nothing else is written to stdout:
  what an operation prints there or to stderr is its request's progress.
  *Held by `cli/control_wire_test.go`.*
- **F2.** A line that is not JSON, or holds more than one JSON value, is
  answered with -32700 under a null id. A batch is answered with -32600 under
  a null id, since each request goes on a line of its own. So is a message that
  does not state `"jsonrpc": "2.0"` and a method, or that has a member JSON-RPC
  does not define.
  *Held by `specs/src/control/request-cases.json`.*
- **F3.** A request's id is a string or an integer. A null id or a fraction is
  answered with -32600, because an id names one request. A request whose id is
  one in flight is answered with -32600. An id can be used again once its
  request is answered.
  *Held by `specs/src/control/request-cases.json`.*
- **F4.** A method the API has none of is answered with -32601.
  *Held by `specs/src/control/request-cases.json`.*

## Versions

- **V1.** Every request's params state `apiVersion`, the one string
  [`ApiVersion.json`](../specs/src/control/ApiVersion.json) fixes. If the
  version is one the launcher does not speak, it answers with -32602, and the
  data's code is `control.unsupported-api-version`, with `supported` naming
  each version it does speak. A request that states no version is refused for
  its params (P1).
  *Held by `specs/src/control/request-cases.json`.*
- **V2.** A client sends `launcher.about` first. Its answer gives the
  launcher's version, whether it is a development build, and the API versions
  it speaks. `launcher.about` is not exclusive and reads nothing of the
  machine but which runtimes are installed.
  *Held by `cli/control_wire_test.go`.*

## Params

- **P1.** A request's params are an object, and are held to its method's
  params schema before anything runs. If they fail it, the answer is -32602,
  the data's code is `control.invalid-params`, and `fields` names each fault
  as an RFC 6901 pointer into the params:
  - a member the schema does not take: the member's pointer;
  - a required member that is missing: the member's pointer;
  - a value that matches none of a `oneOf`'s forms: the `oneOf`'s own
    location, never the faults of each form;
  - any other fault: where it is.

  A client holds its own params to the same schemas before it sends them, and
  refuses what the launcher would refuse, naming the same fields. Each
  keyword that can refuse a value in the control schemas is one both sides
  locate a fault of.
  *Held by `specs/src/control/request-cases.json`,
  `cli/internal/launcher/control_cases_test.go`,
  `desktop/electron/control-cases.test.ts`,
  `cli/internal/launcher/control_census_test.go`.*
- **P2.** A target is local or a codespace. A local target may name a root,
  and names no repository. A codespace target names its repository and no
  root. A request that names no target is resolved where the control server
  was started, as the command would resolve it standing there.
  *Held by `specs/src/control/request-cases.json`.*

- **P3.** `identity.useradd` names one local root explicitly and creates an
  account only in that root's launcher-managed Keycloak realm. The supplied
  password travels only in the local control request; it is never included in
  progress, diagnostics, or launcher state. If the launcher generates a
  password, that secret appears only in the one result returned to the caller.
  The request refuses codespace targets and external identity providers.
  *Held by the method and schemas in `specs/src/control/` and the
  handler in `cli/internal/launcher/control_ops.go`.*

- **P4.** `identity.users.list` names one local root explicitly and returns
  only each account's sign-in name, Xynolog identity and enabled state. It
  returns no passwords, tokens or profile secrets. Like `identity.useradd`,
  it refuses codespace targets and external identity providers.
  *Held by the method and schemas in `specs/src/control/` and the
  handler in `cli/internal/launcher/control_ops.go`.*

- **P5.** `service.run` and `service.signal` name one local root explicitly,
  as identity's methods do: a run belongs to that knowledge base's stack, and
  a codespace's stack runs none. A run states its `environment`, `mounts`,
  `pathFirst` and `ports`, each empty when it has none, and names one of the
  seven Xynolog services
  ([`RunService.json`](../specs/src/control/RunService.json), which the
  reader holds to `service-telemetry/telemetry.json`'s).
  *Held by `specs/src/control/request-cases.json`.*

## Answers and errors

- **E1.** Every request gets exactly one answer: a result that holds to its
  method's result schema, or an error.
  *Held by `cli/control_wire_test.go`.*
- **E2.** If an operation fails, the answer is -32000, with data that holds to
  [`ErrorData.json`](../specs/src/control/ErrorData.json). The data has a code
  from the `control` group of `codes.json`, says whether the request is
  `retryable`, and has these members where they apply:
  - `fields`, for the params at fault;
  - `choices`, every target that `control.target-ambiguous` could mean;
  - `supported`, the versions the launcher speaks;
  - `mayStillRun`, for a start that was cancelled.

  The message is the command line's own refusal, as it would print it. Each
  control code is answered under the code its `rpc` names: -32602 for
  `control.invalid-params` and `control.unsupported-api-version`, -32000 for
  every other.
  *Held by `cli/internal/launcher/control_census_test.go`.*
- **E3.** A request is `retryable` when it can succeed if it is sent again,
  unchanged, after something outside it changes. Examples: a stack that is not
  running, a start in progress elsewhere, a runtime or dependency that is
  missing, a knowledge base, issuer or forward that does not answer,
  unreadable state, a stack still
  running that must stop first, a sign-in that failed, or the launcher's own
  failure. Each control code's `retryable` in `codes.json` says whether it is,
  and the data's `retryable` is that code's.
  *Held by `specs/src/control/request-cases.json` for a refusal of params; for
  an operation's failure, by no case.*
- **E4.** The launcher holds each message it sends to its schema. A result or
  an error's data that does not hold is never sent. The request is answered
  with -32603 and the code `control.failed`, and stderr says what did not
  hold. A notification that does not hold is not sent, and stderr says so. A
  handler that panics is answered with -32603 and the code `control.failed`.
  An answer under -32603 is never retryable, though `control.failed` under
  -32000 is: the same request fails the same way.
  *Held by `cli/internal/launcher/control_ops_test.go`.*
- **E5.** A client holds every result, every error's data and every
  notification it receives to the same schemas. On a mismatch it fails
  loudly: it does not use the message, and it does not fill in what is
  missing.
  *Held by `desktop/electron/control-client.test.ts`.*

## Progress

- **O1.** While a request runs, the launcher sends `notifications/progress`
  for it, one per line the command would print, without colour. The kind is
  how the command would print the line: narration (`log`, `note`), an outcome
  (`ok`, `warn`, `fail`), a heading (`banner`, `section`), a timestamp
  (`stamp`), or a runtime command about to run (`command`).
  *Held by `cli/internal/launcher/control_census_test.go`.*
- **O2.** If an exclusive request's operation prints directly to stdout or
  stderr, rather than through the launcher's narration, each line becomes that
  request's progress, with the kind `output`. A line printed between requests
  is a diagnostic on the launcher's stderr.
  *Held by no case.*
- **O3.** All of a request's progress is sent before its answer, and none is
  sent after it.
  *Held by `cli/control_wire_test.go`.*
- **O4.** Progress is the person's view of a request, and a client may show
  it or not. A client never reads a fact out of a progress line: what a client
  acts on is in a result.
  *Held by no case.*

## Concurrency

- **C1.** Exclusive methods (`exclusive` in `methods.json`) run one at a time,
  in the order their requests arrived. A request that is not exclusive runs
  at once. A request waiting for its turn that is cancelled is answered with
  `control.cancelled`, and never runs.
  *Held by `cli/internal/launcher/control_units_test.go`.*
- **C2.** Three methods give up their turn before they are answered:
  - `auth.login`, once it has sent the device code, because it then waits for
    a person and holds no other request off;
  - `logs.follow`, once it has found the sources it follows, because it
    streams until it is cancelled;
  - `service.run`, once its instance's container runs, because it is
    answered only when the instance exits (R6).
  *Held by no case.*
- **C3.** Each turn ends with the server back in the directory it was started
  in, so one request's start in a root does not move where the next request
  resolves.
  *Held by no case.*

## Cancellation

- **K1.** A client gives up on a request by sending `notifications/cancelled`
  with its id. The launcher answers that request with `control.cancelled`,
  unless it has already answered it. A cancellation of a request the launcher
  does not know is ignored. A notification is never answered, and neither is
  a notification the API does not define.
  *Held by `specs/src/control/request-cases.json`.*
- **K2.** A start cannot be stopped partway. Cancelled, `stack.start` is
  answered with `control.cancelled` and `mayStillRun: true`, and the start goes
  on to its end, holding its turn. Nothing it started is stopped. A client that
  wants the stack gone sends `stack.stop` next, which waits for that turn.
  *Held by no case.*
- **K3.** `logs.follow` sends `notifications/log` for each line a service
  writes. A follow that the client cancels is answered with
  `control.cancelled`. A follow whose every source ends of its own accord is
  answered with its result.
  *Held by no case.*

## Shutdown

- **S1.** When stdin ends, or the launcher gets SIGINT or SIGTERM, it reads no
  more requests. It cancels each request in flight, waits for each to end, and
  exits with status 0. It exits with status 1 if reading stdin failed.
  *Held by `cli/control_wire_test.go`.*
- **S2.** A client that quits cancels its requests, closes the launcher's
  stdin, and waits a bounded time for it to exit before it kills it.
  *Held by `desktop/electron/control-client.test.ts`.*

## Runs

`service.run` runs one instance of one of a knowledge base's own services,
once, so that a program can hold the service to Protocol: the conformance
harness starts, stops, crashes and misconfigures a service through it, and
reads the service's exit and every line it writes. It is not how a stack runs
a service: `stack.start` does that, supervised, on the documents the launcher
writes. `service.signal` signals a run's instance, and `service.host` says the
address an instance and this machine meet at.

- **R1.** A run is unsupervised. Its instance's image runs without the
  supervisor (`XYNOLOG_SUPERVISE` is not set), is never restarted, and is not
  waited on for its health: the run's answer is its instance's exit, and its
  `exitCode` is the service's own. A program that needs it serving asks its
  health itself.
  *Held by no case.*
- **R2.** An instance runs the image the target's stack documents name for
  the service, with the launcher's checks of that image, as a start would.
  It is given the image's own environment and the run's `environment`, and
  nothing of the launcher's: no service account, no OTLP endpoint. With a
  `document`, the text is written as it is at the path the image's own
  arguments name, and `notifications/serviceStarted` says that path; `args`
  replaces the image's arguments to its program, and `pathFirst` the front
  of its PATH.
  *Held by no case.*
- **R3.** An instance's `ports` are published at the same numbers on the host
  address `service.host` names for the runtime, and the instance reaches this
  machine's listeners at that address. A document names that address for
  each of this machine's services it is to reach, and a caller reaches the
  instance there.
  *Held by no case.*
- **R4.** Each of a run's `mounts` and `pathFirst` directories is mounted
  read-write at its own path, and the instance runs as the launcher's own
  user, so that a file it writes there is the caller's to read, change and
  remove between runs.
  *Held by no case.*
- **R5.** Each value of a run's `environment`, and its `document`, enters the
  launcher's list of secret values (Z1) when the request is read, so that
  neither appears in progress, in an error's message or in a diagnostic.
  `notifications/serviceOutput` carries each line as the instance wrote it:
  the instance holds none of the launcher's secrets (R2), and what it writes
  of the caller's is the caller's to read.
  *Held by no case.*
- **R6.** `service.run` is exclusive until its instance's container runs,
  then gives up its turn (C2). It sends `notifications/serviceStarted` once,
  then `notifications/serviceOutput` for every line the instance writes,
  each with the stream it was on, and is answered when the instance exits,
  after the last of them (O3). An instance's name is unique among the
  target's instances that run on this machine: a run of a name in use is
  refused with `control.instance-in-use`, whichever control server or
  command runs it. `service.signal` of an instance that has exited, whose
  run is not yet answered, is answered with `delivered: false`; of a name
  with no run in flight, it is refused with `control.instance-not-found`.
  *Held by no case.*
- **R7.** A cancelled run's instance is sent SIGTERM, and SIGKILL if it has
  not exited 15 seconds later, and the run is answered with
  `control.cancelled`. When the control server shuts down (S1), each run in
  flight is cancelled so. An instance a launcher left running (a control
  server that was killed) is removed by `stack.stop` of its target, which
  stops and removes each of the target's instances with its stack.
  *Held by no case.*
- **R8.** What a client acts on of a run is in its notifications and its
  answer: the document's path, the instance's lines, its exit code and how
  long it ran. None is read out of progress (O4).
  *Held by no case.*

## stdin and the person

- **I1.** An operation that would ask a person something reads end of file
  instead: stdin is the protocol's, and nothing an operation reads comes from
  it. Each such operation does what its command does when nobody answers.
  The only thing the API passes to a person is a sign-in's device code, in
  `notifications/userAction`.
  *Held by no case.*
- **I2.** A deletion needs confirmation. A request for `stack.stop` that asks
  to `delete` without `deleteConfirmed` is refused with
  `control.confirmation-required`. A request that states `deleteConfirmed`
  without `delete` is refused with `control.invalid-request`.
  *Held by `cli/internal/launcher/control_ops_test.go`.*

## Secrets

- **Z1.** No line the launcher sends carries a secret value it holds. This
  covers progress, log records, error messages and the diagnostics on stderr.
  Each value enters the launcher's list when the launcher reads or writes it:
  - a custody read or write;
  - a secret provider's answer;
  - a secret in the environment;
  - a password it generated;
  - a sign-in's tokens;
  - a run's environment values and its document, which never reach its
    instance's output lines (R5).

  Each value on the list is replaced wherever it appears, the longest first.
  A value shorter than eight characters is not replaced, since replacing it
  would mangle ordinary text, and every value the launcher generates is far
  longer.
  *Held by `cli/internal/launcher/control_units_test.go`.*
- **Z2.** No result carries a token. A session is described, never handed
  over. A sign-in's device code goes in `notifications/userAction` and nowhere
  else: not in progress, not in a log record, not in a diagnostic.
  *Held by `specs/src/control/Session.json`.*

## The command line and the API

- **L1.** [`cli.json`](../specs/src/control/cli.json) states, for each option
  of each command the API covers, one of three things:
  - the params field or fields it becomes;
  - the method it selects;
  - why it has no place in the API.

  Each command's options, as its parser reads them, are the table's options.
  Each field the table names is a member of its method's params. Each field
  that becomes a flag of `start` or `stop` reaches that flag in the command
  line a request is turned into. A new option fails the census until the
  table says what it is.
  *Held by `cli/internal/launcher/control_census_test.go`.*
- **L2.** `roots.init` creates and registers a new local knowledge base by
  running `xynolog init` with explicit params and `--yes`. Its caller states
  the destination directory and permanent did:web domain. The method never
  overwrites an existing KB and does not expose template copying, `--force`,
  or `--no-git`.
  *Held by `specs/src/control/cli.json`.*
- **L3.** The API reports what the command reports. It differs from the
  command line in two places, both so that a client is never left to guess:
  - `status.snapshot` reports a stack that is starting, marked `starting`,
    where `xynolog status --root` refuses to report it;
  - `auth.login` with a valid stored session is refused with
    `control.already-signed-in`, where `xynolog login` signs in again.
  *Held by no case.*
