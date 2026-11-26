(* Runs one case against one SDK's driver (conformance/sdk/README.md, "A
   case"). The client is the driver's; the backend is the world's real
   gateway behind the client proxy, with a participant the suite plays in
   place of the services.

   Beyond its steps, every case holds the client to four things it never
   has to state: each request it sent is one the spec declares, with a body
   the spec accepts; it sent nothing the case did not account for; the
   frames it delivered on a channel are exactly the events the gateway sent
   it there, once each and in order; and every failure it reported carries a
   code from specs/src/errors/codes.json.

   The layers account for the wire differently. A wire case reads it as a
   transcript: every request, in order, as a worker case reads a worker's
   (conformance/worker/README.md). A live case reads what the live contract
   means on it: each request the client makes of a service (a fetch), in
   order and none unaccounted, and the scopes its stream has come to name.
   How many times it reopened the stream to get there is the wire layer's to
   judge. *)

open Xynolog_protocol
module Conformance = Tables.Conformance_case

(* How long a step waits for what it expects. Never compared: a conforming
   client is not timed, only waited for. *)
let wait = 15.0

(* How much sooner than a stated Retry-After a client may return and still
   have waited it: the two ends read different clocks, a timer's and the
   proxy's, and a moment between them is not a client returning early. *)
let wait_grace = 0.1
let failf format = Printf.ksprintf failwith format
let json = Yojson.Safe.to_string

let member name = function
  | `Assoc members -> List.assoc_opt name members
  | _ -> None

let string_member name value =
  match member name value with Some (`String s) -> Some s | _ -> None

(* The error codes a client may report: every section's of the table every
   SDK generates its own from. *)
let vocabulary () =
  match Protocol.json_of "errors/codes.json" with
  | Some (`Assoc sections) ->
      List.concat_map
        (fun (_, section) ->
          match member "codes" section with
          | Some (`List entries) ->
              List.filter_map (string_member "code") entries
          | _ -> [])
        sections
  | _ -> failwith "specs/src/errors/codes.json is no table"

(* ── Reading a request ───────────────────────────────────────────────── *)

let pathname path = List.hd (String.split_on_char '?' path)

(* The operation the gateway's document declares for a request, and the
   values its path carried. *)
let declared meth path =
  let meth = String.uppercase_ascii meth in
  Option.map
    (fun (template, values) -> (meth ^ " " ^ template, template, values))
    (Protocol.route Gateway meth (pathname path))

let sorted = function
  | `List items -> `List (List.sort compare items)
  | other -> other

(* A subscription by what it means, not by how a client wrote it: an absent
   list is an empty one, and the order of channels, scopes and awaited
   replies carries nothing. *)
let subscription = function
  | `Assoc members ->
      let listed name =
        Option.value ~default:(`List []) (List.assoc_opt name members)
      in
      let scope_of entry =
        match member "scope" entry with
        | Some (`String s) -> s
        | Some other -> json other
        | None -> json entry
      in
      let channels_sorted = function
        | `Assoc entry ->
            `Assoc
              (List.map
                 (fun (key, value) ->
                   if key = "channels" then (key, sorted value) else (key, value))
                 entry)
        | other -> other
      in
      let scoped =
        match listed "scoped" with `List entries -> entries | _ -> []
      in
      `Assoc
        ((match List.assoc_opt "clientId" members with
           | Some id -> [ ("clientId", id) ]
           | None -> [])
        @ [
            ("global", sorted (listed "global"));
            ( "scoped",
              `List
                (List.stable_sort
                   (fun a b -> compare (scope_of a) (scope_of b))
                   (List.map channels_sorted scoped)) );
            ("pendingReplies", sorted (listed "pendingReplies"));
          ])
  | other -> other

(* A request as a case reads it: the operation the spec declares for it,
   and what it carried. *)
type seen = {
  operation : string;
  params : Yojson.Safe.t;
  body : Yojson.Safe.t option;
  token : string option;
}

(* A request the client made of a service, as a fetch step reads it: a bus
   request's payload and correlation id, or an HTTP read's path values. *)
type asked = {
  fetch : string;
  payload : Yojson.Safe.t option;
  params : Yojson.Safe.t option;
  correlation_id : Yojson.Safe.t option;
}

let asked_json a =
  `Assoc
    ([ ("fetch", `String a.fetch) ]
    @ List.filter_map
        (fun (key, value) -> Option.map (fun v -> (key, v)) value)
        [
          ("payload", a.payload);
          ("params", a.params);
          ("correlationId", a.correlation_id);
        ])

type reports = {
  mutable claimed : int;
  mutable refused : int;
  mutable signalled : int;
  mutable stalled : int;
}

(* An operation started with [as]: its id, the operation, and the arguments
   it was sent. *)
type started = {
  operation : int;
  op : string;
  sent : (string * Yojson.Safe.t) list;
}

type run = {
  layer : Corpus.layer;
  world : World.t;
  proxy : Client_proxy.t;
  driver : Driver.t;
  bindings : Bindings.t;
  codes : string list;
  problems : string Dynarray.t;
      (** What the client did outside the spec, whatever the case was about. *)
  participant_token : string;
  participant_client : string;
  client : string;  (** The client's DID. *)
  mutable wire_cursor : int;
  mutable fetch_cursor : int;
      (** In a live case: how many of the client's requests of a service its
          fetch steps have accounted for. *)
  emission_cursor : (string, int) Hashtbl.t;
  mutable state_cursor : int;
  mutable failure_cursor : int;
  frame_cursor : (string, int) Hashtbl.t;
  reports : reports;
      (** In a worker case: how many of each thing the worker reported the case
          has read. *)
  mutable pending : (string * started) list;
      (** Operations started with [as], by name, in the order started. *)
  named : (string, Client_proxy.request) Hashtbl.t;
  listening : (string * int) Dynarray.t;
      (** For each channel the client listens to, the proxy's clock when it
          began. *)
  handed_over : (string * string) Dynarray.t;
      (** In a worker case: each job the backend handed over, with the
          correlation id of the claim it answered. *)
}

let problem run text = Dynarray.add_last run.problems text

let read run (r : Client_proxy.request) =
  let token =
    match Client_proxy.header r "authorization" with
    | Some bearer when String.starts_with ~prefix:"Bearer " bearer ->
        Some (String.sub bearer 7 (String.length bearer - 7))
    | _ -> None
  in
  match declared r.meth r.path with
  | None ->
      problem run
        (Printf.sprintf "%s %s, which the spec does not declare" r.meth r.path);
      {
        operation = r.meth ^ " " ^ r.path;
        params = `Assoc [];
        body = r.json;
        token;
      }
  | Some (name, template, values) ->
      let operation =
        Protocol.operation Gateway (String.uppercase_ascii r.meth) template
      in
      (match operation.request_body with
      | Some { content; _ } -> (
          match List.assoc_opt "application/json" content with
          | Some media -> (
              match
                Protocol.refusals media.schema
                  (Option.value ~default:`Null r.json)
              with
              | [] -> ()
              | refusals ->
                  problem run
                    (Printf.sprintf
                       "a %s whose body the spec does not accept (%s): %s" name
                       (String.concat "; " refusals)
                       (Bindings.show r.json)))
          | None -> ())
      | None -> ());
      List.iter
        (fun (p : Openapi.parameter) ->
          if p.in_ = Header then
            match Client_proxy.header r p.name with
            | None ->
                if p.required then
                  problem run
                    (Printf.sprintf "a %s without its %s header" name p.name)
            | Some value -> (
                match Protocol.refusals p.schema (`String value) with
                | [] -> ()
                | refusals ->
                    problem run
                      (Printf.sprintf
                         "a %s whose %s header the spec does not accept (%s): \
                          %S"
                         name p.name
                         (String.concat "; " refusals)
                         value)))
        (Option.value ~default:[] operation.parameters);
      {
        operation = name;
        params = `Assoc (List.map (fun (k, v) -> (k, `String v)) values);
        body =
          (if name = "POST /bus/subscribe" then Option.map subscription r.json
           else r.json);
        token;
      }

let matcher what (r : Client_proxy.request) =
  match declared r.meth r.path with
  | Some (name, _, _) -> name = what
  | None -> false

let requests run = Dynarray.to_list run.proxy.requests

let is_emit (r : Client_proxy.request) =
  r.meth = "POST" && pathname r.path = "/bus/emit"

(* The client's emits, in order. *)
let emits run = List.filter is_emit (requests run)

(* In a live case: the client's requests of a service, in order: each emit,
   and each read over HTTP. *)
let asked run =
  List.filter
    (fun (r : Client_proxy.request) ->
      is_emit r || not (String.starts_with ~prefix:"/bus/" r.path))
    (requests run)

(* [items] with each told once, in the order first told. *)
let unique items =
  List.rev
    (List.fold_left
       (fun kept item -> if List.mem item kept then kept else item :: kept)
       [] items)

let rec drop n = function
  | _ :: rest when n > 0 -> drop (n - 1) rest
  | list -> list

let rec take n = function
  | x :: rest when n > 0 -> x :: take (n - 1) rest
  | _ -> []

(* ── What a driver reports, as a case reads it ───────────────────────── *)

let failure_json (f : Driver.failure) =
  `Assoc
    (List.filter_map Fun.id
       [
         Option.map (fun c -> ("code", `String c)) f.code;
         Option.map (fun s -> ("status", `Int s)) f.status;
         Option.map
           (fun c ->
             ( "failureClass",
               `String (Tables.Conformance_driver_line.failure_class_value c) ))
           f.failure_class;
       ])

let frame_json (f : Driver.frame) =
  `Assoc
    (List.filter_map Fun.id
       [
         Option.map (fun p -> ("payload", p)) f.payload;
         Option.map (fun c -> ("correlationId", `String c)) f.correlation_id;
         Option.map (fun s -> ("scope", `String s)) f.scope;
       ])

let state_json : Driver.state -> Yojson.Safe.t = function
  | Pending -> `Assoc [ ("status", `String "pending") ]
  | Ready value -> `Assoc [ ("status", `String "ready"); ("value", value) ]
  | Failed f ->
      `Assoc
        [
          ("status", `String "failed");
          ( "error",
            match failure_json f with
            | `Assoc members ->
                `Assoc (members @ [ ("detail", `String f.detail) ])
            | other -> other );
        ]

let outcome_json : Driver.outcome -> Yojson.Safe.t = function
  | Returned ok -> `Assoc [ ("ok", ok) ]
  | Failure f ->
      `Assoc [ ("error", failure_json f); ("detail", `String f.detail) ]
  | Abandoned -> `Assoc [ ("abandoned", `Bool true) ]
  | Unsupported -> `Assoc [ ("unsupported", `Bool true) ]
  | Misuse why -> `Assoc [ ("misuse", `String why) ]

let in_vocabulary run (f : Driver.failure) =
  match f.code with
  | None -> failf "the client failed with no code at all: %s" f.detail
  | Some code when not (List.mem code run.codes) ->
      failf
        "the client reported the code %s, which specs/src/errors/codes.json \
         does not list (%s)"
        code f.detail
  | Some _ -> ()

(* A state as a case reads it: a failure by its code, without the SDK's own
   words for it. *)
let observed run : Driver.state -> Yojson.Safe.t = function
  | Failed f ->
      in_vocabulary run f;
      `Assoc [ ("status", `String "failed"); ("error", failure_json f) ]
  | state -> state_json state

let matches run pattern value = Bindings.matches run.bindings pattern value

type expected = {
  returns : Yojson.Safe.t option;
  fails : Yojson.Safe.t option;
  abandoned : bool;
}

let nothing_expected = { returns = None; fails = None; abandoned = false }

let judge run op (outcome : Driver.outcome) expected =
  match outcome with
  | Unsupported -> failf "the driver does not implement %s" op
  | Misuse why ->
      failf "the case asked the driver for a %s it could not act on: %s" op why
  | Abandoned ->
      if not expected.abandoned then
        failf "%s was abandoned; the case expects it to settle on its own" op
  | _ when expected.abandoned ->
      failf "%s settled with %s; abandoned, it was to report nothing" op
        (json (outcome_json outcome))
  | Returned ok -> (
      match (expected.fails, expected.returns) with
      | Some _, _ -> failf "%s succeeded with %s; it was to fail" op (json ok)
      | None, Some pattern -> (
          match matches run pattern (Some ok) with
          | Some difference ->
              failf "%s returned otherwise than expected: %s" op difference
          | None -> ())
      | None, None -> ())
  | Failure f -> (
      match expected.fails with
      | Some pattern -> (
          in_vocabulary run f;
          match matches run pattern (Some (failure_json f)) with
          | Some difference ->
              failf "%s failed otherwise than expected: %s (%s)" op difference
                f.detail
          | None -> ())
      | None -> failf "%s failed: %s (%s)" op (json (failure_json f)) f.detail)

let outcome run id what =
  Driver.outcome ~timeout:wait run.driver id (what ^ " to settle")

(* Everything the client reported before it answers this is read by the time
   it does. *)
let sync run =
  let id = Driver.send run.driver "sync" [] in
  judge run "sync" (outcome run id "sync") nothing_expected

let started run name =
  match List.assoc_opt name run.pending with
  | Some started -> started
  | None -> failf "no operation was started as %s" name

let named run name =
  match Hashtbl.find_opt run.named name with
  | Some r -> r
  | None -> failf "no request was named %s" name

let proxy_until run what probe =
  Client_proxy.until ~timeout:wait run.proxy what probe

let driver_until ?(timeout = wait) run what probe =
  Driver.until ~timeout run.driver what probe

(* A failure of [until] with what the case saw added to it. *)
let explained explain body =
  match body () with
  | value -> value
  | exception Run.Timed_out what -> failwith (what ^ explain ())

(* ── What every case is held to ──────────────────────────────────────── *)

let nothing_unaccounted run ~when_ =
  (* A live case answers for every request the client made of a service,
     and for anything that is neither that nor its stream. *)
  let unaccounted =
    match run.layer with
    | Live ->
        drop run.fetch_cursor (asked run)
        @ List.filter
            (fun (r : Client_proxy.request) ->
              String.starts_with ~prefix:"/bus/" r.path
              && not
                   (List.mem (pathname r.path)
                      [ "/bus/emit"; "/bus/subscribe" ]))
            (requests run)
    | Wire | Worker -> drop run.wire_cursor (requests run)
  in
  match
    List.map
      (fun r ->
        let seen = read run r in
        seen.operation ^ " " ^ Bindings.show seen.body)
      unaccounted
  with
  | [] -> ()
  | extra ->
      failf "the client sent what the case does not account for, %s: %s" when_
        (String.concat "; " extra)

(* A worker has told the suite nothing a step has not read. *)
let nothing_unreported run ~when_ =
  let unread name list from = (name, drop from (Dynarray.to_list list)) in
  List.iter
    (fun (what, more) ->
      if more <> [] then
        failf "the worker said it %s, %s, and the case does not expect it: %s"
          what when_
          (json (`List more)))
    [
      unread "claimed a job" run.driver.claimed run.reports.claimed;
      ( "was refused a claim",
        List.map
          (fun f -> failure_json f)
          (drop run.reports.refused (Dynarray.to_list run.driver.refusals)) );
      ( "signalled a cancellation",
        List.map
          (fun s -> `String s)
          (drop run.reports.signalled (Dynarray.to_list run.driver.signalled))
      );
      ( "stalled",
        List.map
          (fun s -> `String s)
          (drop run.reports.stalled (Dynarray.to_list run.driver.stalled)) );
    ]

(* The frames the gateway sent the client on [channel] since it began
   listening: each event once, in the order it first came. *)
let carried run channel since =
  let events =
    List.concat_map
      (fun (r : Client_proxy.request) -> Dynarray.to_list r.events)
      (requests run)
    |> List.filter (fun (e : Client_proxy.event) ->
        e.event = "bus-event" && e.at > since)
    |> List.stable_sort (fun (a : Client_proxy.event) b -> compare a.at b.at)
  in
  let seen = Hashtbl.create 16 in
  List.filter_map
    (fun (e : Client_proxy.event) ->
      let frame =
        match Yojson.Safe.from_string e.data with
        | frame -> frame
        | exception Yojson.Json_error why ->
            failf "a stream carried a bus-event that is not JSON (%s): %s" why
              e.data
      in
      if string_member "channel" frame <> Some channel then None
      else
        match e.id with
        | Some id when Hashtbl.mem seen id -> None
        | id ->
            Option.iter (fun id -> Hashtbl.replace seen id ()) id;
            Some
              {
                Driver.payload = member "payload" frame;
                correlation_id = string_member "correlationId" frame;
                scope = string_member "scope" frame;
              })
    events

let same_frame (a : Driver.frame) (b : Driver.frame) =
  Bindings.same_option a.payload b.payload
  && a.correlation_id = b.correlation_id
  && a.scope = b.scope

(* Wait for the client to have delivered what it was sent, so a frame still
   on its way is not read as one it lost. *)
let settle run =
  Dynarray.iter
    (fun (channel, since) ->
      let expected = List.length (carried run channel since) in
      match
        driver_until ~timeout:5.0 run
          (Printf.sprintf "the %d frames sent on %s to be delivered" expected
             channel) (fun () ->
            if List.length (Driver.frames run.driver channel) >= expected then
              Some ()
            else None)
      with
      | () -> ()
      | exception Run.Timed_out _ -> ())
    run.listening

(* Judge everything the case did not have to state. Called once the driver
   has stopped. *)
let finish run =
  if run.pending <> [] then
    failf "the case never settled %s"
      (String.concat ", " (List.rev_map fst run.pending));
  nothing_unaccounted run ~when_:"by the end of the case";
  Dynarray.iter
    (fun (channel, since) ->
      let delivered = Driver.frames run.driver channel in
      let sent = carried run channel since in
      if
        List.length delivered <> List.length sent
        || not (List.for_all2 same_frame delivered sent)
      then
        failf
          "on %s the client delivered %d frames and was sent %d, or they differ:\n\
          \  delivered %s\n\
          \  sent      %s"
          channel (List.length delivered) (List.length sent)
          (json (`List (List.map frame_json delivered)))
          (json (`List (List.map frame_json sent)));
      let expected =
        Option.value ~default:0 (Hashtbl.find_opt run.frame_cursor channel)
      in
      if List.length delivered <> expected then
        failf "the client delivered %d frames on %s; the case expects %d"
          (List.length delivered) channel expected)
    run.listening;
  nothing_unreported run ~when_:"by the end of the case";
  (match drop run.failure_cursor (Dynarray.to_list run.driver.failures) with
  | [] -> ()
  | unexpected ->
      failf "the error stream carried what the case does not expect: %s"
        (json
           (`List
              (List.map
                 (fun (f : Driver.failure) ->
                   `Assoc
                     [
                       ("failure", failure_json f); ("detail", `String f.detail);
                     ])
                 unexpected))));
  (match Dynarray.to_list run.driver.violations with
  | [] -> ()
  | violations ->
      failf "the driver broke its protocol: %s" (String.concat "; " violations));
  match Dynarray.to_list run.problems with
  | [] -> ()
  | problems ->
      failf "the client sent %s" (String.concat "; " (unique problems))

(* What happened, for a failing case's message. *)
let account run =
  let wire =
    List.mapi
      (fun index (r : Client_proxy.request) ->
        let name =
          match declared r.meth r.path with
          | Some (name, _, _) -> name
          | None -> r.meth ^ " " ^ r.path
        in
        let answered =
          match r.status with
          | None -> "unanswered"
          | Some status ->
              string_of_int status
              ^ if r.answer_lost then ", kept from the client" else ""
        in
        Printf.sprintf "    %s %s → %s %s"
          (if index < run.wire_cursor then " " else "?")
          name answered (Bindings.show r.json))
      (requests run)
  in
  let per list show =
    List.map
      (fun (key, items) ->
        Printf.sprintf "    %s: %s" key
          (json (`List (List.map show (Dynarray.to_list items)))))
      (Dynarray.to_list list)
  in
  let listed list show = json (`List (List.map show (Dynarray.to_list list))) in
  String.concat "\n"
    ([ "  on the wire:" ] @ wire
    @ [
        "  states: " ^ String.concat ", " (Dynarray.to_list run.driver.states);
        "  frames delivered:";
      ]
    @ per run.driver.frames frame_json
    @ [ "  observers:" ]
    @ per run.driver.emissions state_json
    @ [
        "  error stream: "
        ^ listed run.driver.failures (fun f ->
            `Assoc [ ("failure", failure_json f); ("detail", `String f.detail) ]);
      ]
    @ (match run.layer with
      | Worker ->
          [
            "  claimed: " ^ listed run.driver.claimed Fun.id;
            "  refused: " ^ listed run.driver.refusals failure_json;
            "  signalled: " ^ listed run.driver.signalled (fun s -> `String s);
            "  stalled: " ^ listed run.driver.stalled (fun s -> `String s);
          ]
      | Wire | Live -> [])
    @ [ "  the driver's stderr:" ]
    @ List.map (fun line -> "    " ^ line) (Driver.stderr_tail 20 run.driver))

(* ── The backend ─────────────────────────────────────────────────────── *)

(* The trace a reply is sent in: that of the client's request it answers,
   the one that carried [correlation_id], as a service answers in the trace
   of what it answers. None for an emit that answers nothing, or a request
   sent under no trace. *)
let trace_answered run correlation_id =
  match correlation_id with
  | None -> []
  | Some id -> (
      match
        List.find_opt
          (fun (r : Client_proxy.request) ->
            match r.json with
            | Some body -> member "correlationId" body = Some (`String id)
            | None -> false)
          (emits run)
      with
      | None -> []
      | Some asked ->
          List.filter_map
            (fun name ->
              Option.map (fun v -> (name, v)) (Client_proxy.header asked name))
            (Protocol.trace_members ()))

let backend run directive (args : (string * Yojson.Safe.t) list) =
  let word = Conformance.conformance_step_backend_value directive in
  let text name =
    match List.assoc_opt name args with
    | Some (`String s) -> s
    | _ -> failf "backend %s needs %s, a string" word name
  in
  let number name =
    match Option.bind (List.assoc_opt name args) Bindings.number with
    | Some n -> n
    | None -> failf "backend %s needs %s, a number" word name
  in
  let count name = int_of_float (number name) in
  let optional_text name =
    match List.assoc_opt name args with
    | None -> None
    | Some _ -> Some (text name)
  in
  let allowed names =
    match List.filter (fun (name, _) -> not (List.mem name names)) args with
    | [] -> ()
    | strays ->
        failf "backend %s does not take %s" word
          (String.concat ", " (List.map fst strays))
  in
  let emit (body : (string * Yojson.Safe.t) list) =
    let reply =
      Call.call ~token:run.participant_token
        ~json:(`Assoc (body @ [ ("clientId", `String run.participant_client) ]))
        ~headers:
          (trace_answered run
             (match List.assoc_opt "correlationId" body with
             | Some (`String id) -> Some id
             | _ -> None)
          @ [ ("idempotency-key", Fresh.uuid ()) ])
        (World.origin run.world) "POST"
        (Protocol.path Gateway "POST" "/bus/emit")
    in
    if reply.status <> 202 then
      failf "the gateway refused the backend's emit: %d %s" reply.status
        reply.text
  in
  let archivist = run.world.archivist in
  let object_ name =
    match List.assoc_opt name args with
    | Some (`Assoc _ as value) -> value
    | _ -> failf "backend %s needs %s, an object" word name
  in
  match directive with
  | Listen ->
      allowed [ "channels"; "scope" ];
      let channels =
        match List.assoc_opt "channels" args with
        | Some (`List items)
          when List.for_all (function `String _ -> true | _ -> false) items ->
            List.map (function `String s -> s | _ -> assert false) items
        | _ -> failwith "backend listen needs channels, a list of strings"
      in
      let body =
        match optional_text "scope" with
        | None ->
            Bus_stream.subscribe_body ~client_id:run.participant_client
              ~global:channels ()
        | Some scope ->
            Bus_stream.subscribe_body ~client_id:run.participant_client
              ~scoped:[ Bus_stream.scope scope channels ]
              ()
      in
      ignore
        (World.subscribe run.world run.participant_token body : Bus_stream.t)
  | Emit -> (
      allowed [ "channel"; "payload"; "scope"; "correlationId" ];
      let payload = object_ "payload" in
      let scope = optional_text "scope" in
      let correlation_id = optional_text "correlationId" in
      emit
        ([ ("channel", `String (text "channel")); ("payload", payload) ]
        @ (match scope with Some s -> [ ("scope", `String s) ] | None -> [])
        @
        match correlation_id with
        | Some c -> [ ("correlationId", `String c) ]
        | None -> []);
      match
        ( text "channel",
          correlation_id,
          Option.bind (member "response" payload) (member "metadata") )
      with
      | "job:claimed", Some correlation, Some metadata -> (
          match string_member "id" metadata with
          | Some job -> Dynarray.add_last run.handed_over (job, correlation)
          | None -> ())
      | _ -> ())
  | Record ->
      allowed
        [
          "resource";
          "channel";
          "sequence";
          "count";
          "live";
          "payload";
          "enriched";
          "unscoped";
        ];
      let resource = text "resource" in
      let first = count "sequence" in
      let many =
        match List.assoc_opt "count" args with
        | None -> 1
        | Some _ -> count "count"
      in
      let merged base extra =
        match (base, extra) with
        | `Assoc members, Some (`Assoc more) ->
            `Assoc
              (List.filter (fun (k, _) -> not (List.mem_assoc k more)) members
              @ more)
        | _ -> base
      in
      for sequence = first to first + many - 1 do
        let event =
          merged
            (Archivist_standin.stored_event (text "channel") resource sequence)
            (match List.assoc_opt "payload" args with
            | Some (`Assoc _ as payload) ->
                Some (`Assoc [ ("payload", payload) ])
            | _ -> None)
        in
        Hashtbl.replace archivist.events resource
          (Option.value ~default:[] (Hashtbl.find_opt archivist.events resource)
          @ [ event ]);
        (* Published as the record's own service publishes it: with what it
           enriched the event with, and on the resource's scope unless the
           channel is one every client hears. *)
        let published =
          merged event
            (match List.assoc_opt "enriched" args with
            | Some (`Assoc _ as enriched) -> Some enriched
            | _ -> None)
        in
        if List.assoc_opt "live" args = Some (`Bool true) then
          emit
            ([ ("channel", `String (text "channel")); ("payload", published) ]
            @
            if List.assoc_opt "unscoped" args = Some (`Bool true) then []
            else [ ("scope", `String resource) ])
      done
  | Archivist -> (
      allowed [ "replayFails" ];
      match List.assoc_opt "replayFails" args with
      | Some (`Bool fails) -> archivist.mode.replay_fails <- fails
      | _ -> failwith "backend archivist needs replayFails, a boolean")
  | Content ->
      allowed [ "resource"; "mediaType"; "bytes" ];
      let storage_uri = "file://seeded/" ^ text "resource" in
      Hashtbl.replace archivist.resources (text "resource")
        { Archivist_standin.storage_uri; media_type = text "mediaType" };
      Hashtbl.replace archivist.content storage_uri
        (Base64.decode_exn (text "bytes"))
  | Description ->
      allowed [ "resource"; "graph" ];
      Hashtbl.replace archivist.descriptions (text "resource")
        (match List.assoc_opt "graph" args with
        | Some graph -> graph
        | None -> failwith "backend description needs graph")
  | Annotations ->
      allowed [ "resource"; "annotations" ];
      let annotations = object_ "annotations" in
      Hashtbl.replace archivist.annotations (text "resource") (fun () ->
          Some annotations)
  | Anchored_text ->
      allowed [ "resource"; "answer" ];
      let answer = object_ "answer" in
      Hashtbl.replace archivist.anchored_texts (text "resource") (fun () ->
          answer)
  | Uploaded -> (
      allowed [ "resource"; "fields"; "bytes" ];
      match
        Dynarray.find_opt
          (fun (u : Archivist_standin.upload) ->
            u.resource_id = text "resource")
          archivist.uploads
      with
      | None -> failf "the Archivist recorded no upload as %s" (text "resource")
      | Some upload ->
          (match
             matches run
               (Option.value ~default:`Null (List.assoc_opt "fields" args))
               (Some
                  (`Assoc
                     (List.map (fun (k, v) -> (k, `String v)) upload.fields)))
           with
          | Some difference -> failf "the upload's fields differ: %s" difference
          | None -> ());
          if upload.file <> Base64.decode_exn (text "bytes") then
            failf "the upload's bytes differ: the Archivist stored %d bytes"
              (String.length upload.file);
          if upload.principal <> run.client then
            failf "the upload was recorded for %s, not the client (%s)"
              upload.principal run.client)
  | Refuse ->
      allowed [ "wire"; "status"; "retryAfter"; "headers"; "body"; "times" ];
      let wire = text "wire" in
      let meth, template =
        match String.index_opt wire ' ' with
        | Some space ->
            ( String.sub wire 0 space,
              String.sub wire (space + 1) (String.length wire - space - 1) )
        | None -> failf "refuse's wire %s is not METHOD /path" wire
      in
      let body =
        match List.assoc_opt "body" args with
        | Some body -> body
        | None -> failwith "backend refuse needs body"
      in
      (* Headers the refusal declares beside these, such as a 401's
         challenge. *)
      let stated =
        match List.assoc_opt "headers" args with
        | None -> []
        | Some (`Assoc headers) ->
            List.map
              (fun (name, value) ->
                match value with
                | `String v -> (String.lowercase_ascii name, v)
                | _ -> failf "refuse's header %s is not a string" name)
              headers
        | Some _ -> failwith "refuse takes headers, an object"
      in
      let headers =
        List.filter
          (fun (name, _) -> not (List.mem_assoc name stated))
          ([ ("content-type", "application/json") ]
          @
          match List.assoc_opt "retryAfter" args with
          | None -> []
          | Some _ ->
              [
                ("retry-after", Bindings.text_of (`Float (number "retryAfter")));
              ])
        @ stated
      in
      let text_body = json body in
      let status = count "status" in
      (* The proxy speaks for the gateway here, so what it says is held to
         what the spec lets a gateway say. *)
      (match
         Call.non_conformance Gateway meth template
           {
             Call.status;
             headers = Http.Header.of_list headers;
             text = text_body;
             json = Some body;
           }
       with
      | [] -> ()
      | off ->
          failf "the case scripts a refusal the spec does not allow: %s"
            (String.concat "; " off));
      Client_proxy.answer run.proxy (matcher wire)
        { Serve.status; headers; body = text_body }
        (count "times")
  | Unanswered ->
      allowed [ "wire"; "times" ];
      Client_proxy.leave_unanswered run.proxy
        (matcher (text "wire"))
        (count "times")
  | Answer_lost ->
      allowed [ "wire"; "times" ];
      Client_proxy.lose_answer run.proxy (matcher (text "wire")) (count "times")
  | Hold ->
      allowed [ "wire" ];
      Client_proxy.hold_requests run.proxy (matcher (text "wire"))
  | Release ->
      allowed [];
      Client_proxy.release run.proxy
  | Rechunk ->
      allowed [ "bytes" ];
      Client_proxy.rechunk run.proxy (count "bytes")
  | Carry -> (
      allowed [ "frame" ];
      match List.assoc_opt "frame" args with
      | Some frame -> Client_proxy.carry run.proxy (json frame)
      | None -> failwith "backend carry needs frame")
  | Cut ->
      allowed [];
      Client_proxy.cut run.proxy
  | Down ->
      allowed [];
      Client_proxy.down run.proxy
  | Up ->
      allowed [];
      Client_proxy.up run.proxy

(* ── The steps ───────────────────────────────────────────────────────── *)

let resolved run = function
  | None -> []
  | Some members -> (
      match Bindings.resolve run.bindings (`Assoc members) with
      | `Assoc members -> members
      | _ -> assert false)

let operate run (step : Conformance.driver) =
  let args = resolved run step.with_ in
  let args =
    if step.driver = "open" then
      List.filter (fun (k, _) -> k <> "baseUrl") args
      @ [ ("baseUrl", `String run.proxy.origin) ]
      @
      if List.mem_assoc "token" args then []
      else [ ("token", Bindings.get run.bindings "token") ]
    else args
  in
  (* A case listens before anything is sent on the channel, so the clock
     here is before every frame the client could deliver there. *)
  (if step.driver = "listen" then
     match List.assoc_opt "channel" args with
     | Some (`String channel) -> (
         let now = Client_proxy.now run.proxy in
         match
           Dynarray.find_index (fun (c, _) -> c = channel) run.listening
         with
         | Some at -> Dynarray.set run.listening at (channel, now)
         | None -> Dynarray.add_last run.listening (channel, now))
     | _ -> ());
  let id = Driver.send run.driver step.driver args in
  match step.as_ with
  | Some name ->
      if List.mem_assoc name run.pending then failf "%s is already started" name;
      run.pending <-
        run.pending
        @ [ (name, { operation = id; op = step.driver; sent = args }) ];
      (* A driver says at once that it has no such operation, so the case
         learns it here and not at whatever it would have waited for
         next. *)
      sync run;
      if Driver.settled run.driver id = Some Unsupported then
        failf "the driver does not implement %s" step.driver
  | None ->
      judge run step.driver
        (outcome run id step.driver)
        { returns = step.returns; fails = step.fails; abandoned = false }

let wire run (step : Conformance.wire) =
  if run.layer = Live then
    failwith "a live case accounts for the wire with fetch and scopes, not wire";
  let index = run.wire_cursor in
  let r =
    proxy_until run ("the client's next request, a " ^ step.wire) (fun () ->
        if index < Dynarray.length run.proxy.requests then
          Some (Dynarray.get run.proxy.requests index)
        else None)
  in
  run.wire_cursor <- run.wire_cursor + 1;
  let seen = read run r in
  if seen.operation <> step.wire then
    failf "the client's next request is a %s (%s), not a %s" seen.operation
      (Bindings.show seen.body) step.wire;
  let at = Option.value ~default:Conformance.Answer step.at in
  if at <> Arrival then
    proxy_until run
      ("the " ^ step.wire ^ " to be answered")
      (fun () -> Option.map ignore r.status);
  if at = Live then
    proxy_until run
      ("the " ^ step.wire ^ " stream to catch up")
      (fun () ->
        if
          Dynarray.exists
            (fun (e : Client_proxy.event) -> e.event = "ping")
            r.events
        then Some ()
        else None);
  if step.answer <> None then
    proxy_until run ("the answer to the " ^ step.wire) (fun () ->
        Option.map ignore r.answer);
  List.iter
    (fun (field, pattern, value) ->
      match pattern with
      | None -> ()
      | Some pattern -> (
          match matches run pattern value with
          | Some difference ->
              failf "the %s differs in its %s: %s" step.wire field difference
          | None -> ()))
    [
      ( "status",
        Option.map (fun s -> `Int s) step.status,
        Option.map (fun s -> `Int s) r.status );
      ("params", step.params, Some seen.params);
      ("body", step.body, seen.body);
      ("token", step.token, Option.map (fun t -> `String t) seen.token);
      ( "headers",
        Option.map (fun h -> `Assoc h) step.headers,
        Option.map
          (fun headers ->
            `Assoc
              (List.filter_map
                 (fun (name, _) ->
                   Option.map
                     (fun v -> (name, `String v))
                     (Client_proxy.header r name))
                 headers))
          step.headers );
      ("answer", step.answer, r.answer);
    ];
  Option.iter
    (fun earlier_name ->
      let earlier = named run earlier_name in
      match earlier.answered with
      | Some answered when answered <= r.arrived -> ()
      | _ ->
          failf "the %s arrived before %s was answered" step.wire earlier_name)
    step.follows;
  Option.iter
    (fun refused_name ->
      let refused = named run refused_name in
      match (refused.retry_after, refused.answered_at) with
      | Some retry_after, Some answered_at ->
          let waited = r.arrived_at -. answered_at in
          if waited < retry_after -. wait_grace then
            failf
              "the %s came %.0f ms after %s was refused, which stated \
               Retry-After: %s"
              step.wire (waited *. 1000.0) refused_name
              (Bindings.text_of (`Float retry_after))
      | _ -> failf "%s was not answered with a Retry-After" refused_name)
    step.waited;
  Option.iter (fun name -> Hashtbl.replace run.named name r) step.as_

(* The client's next requests of a service are these, in whatever order it
   made them. *)
let fetches run (steps : Conformance.conformance_step_fetches_item list) =
  let from = run.fetch_cursor in
  let n = List.length steps in
  let what =
    String.concat ", "
      (List.map
         (fun (step : Conformance.conformance_step_fetches_item) ->
           match step with
           | Bus_fetch { fetch; _ } -> fetch
           | Http_fetch { fetch; _ } -> fetch)
         steps)
  in
  let records =
    proxy_until run
      (Printf.sprintf "the client's next %s: %s"
         (if n = 1 then "request" else Printf.sprintf "%d requests" n)
         what)
      (fun () ->
        let next = take n (drop from (asked run)) in
        if
          List.length next = n
          && List.for_all
               (fun (r : Client_proxy.request) -> r.status <> None)
               next
        then Some next
        else None)
  in
  run.fetch_cursor <- run.fetch_cursor + n;
  let made =
    ref
      (List.map
         (fun (r : Client_proxy.request) ->
           let seen = read run r in
           let status = Option.get r.status in
           if seen.operation <> "POST /bus/emit" then (
             if status <> 200 then
               failf "the client's %s %s was answered %d" seen.operation
                 (json seen.params) status;
             {
               fetch = seen.operation;
               payload = None;
               params = Some seen.params;
               correlation_id = None;
             })
           else (
             if status <> 202 then
               failf "the gateway answered %s with %d" (Bindings.show seen.body)
                 status;
             match Option.bind seen.body (member "correlationId") with
             | Some (`String _ as correlation_id) ->
                 let body = Option.get seen.body in
                 {
                   fetch =
                     (match member "channel" body with
                     | Some channel -> Bindings.text_of channel
                     | None -> "undefined");
                   payload = member "payload" body;
                   params = None;
                   correlation_id = Some correlation_id;
                 }
             | _ ->
                 failf
                   "the client's request %s carries no correlation id, so \
                    nothing can answer it"
                   (Bindings.show seen.body)))
         records)
  in
  List.iter
    (fun (step : Conformance.conformance_step_fetches_item) ->
      let fetch, wanted, field, as_ =
        match step with
        | Bus_fetch { fetch; payload; as_ } ->
            (fetch, payload, (fun a -> a.payload), Some as_)
        | Http_fetch { fetch; params } ->
            (fetch, params, (fun a -> a.params), None)
      in
      let rec pick before = function
        | [] ->
            failf "none of the client's next requests is a %s%s: it made %s"
              fetch
              (match wanted with
              | None -> ""
              | Some w -> " with " ^ json (Bindings.resolve run.bindings w))
              (json (`List (List.map asked_json !made)))
        | a :: rest ->
            if
              a.fetch = fetch
              &&
              match wanted with
              | None -> true
              | Some w -> matches run w (field a) = None
            then (
              made := List.rev_append before rest;
              a)
            else pick (a :: before) rest
      in
      let request = pick [] !made in
      Option.iter
        (fun name ->
          Bindings.bind run.bindings name
            (Option.value ~default:`Null request.correlation_id))
        as_)
    steps

(* The observer comes to hold the state: the first it reports, from where
   the case has read to, that is it. *)
let reaches run observer pattern =
  let from =
    Option.value ~default:0 (Hashtbl.find_opt run.emission_cursor observer)
  in
  let seen = ref [] in
  let at =
    explained
      (fun () ->
        "; since the case last read it, it reported " ^ json (`List !seen))
      (fun () ->
        driver_until run (observer ^ " to reach that state") (fun () ->
            seen :=
              List.map (observed run)
                (drop from (Driver.emissions run.driver observer));
            let rec find index = function
              | [] -> None
              | state :: rest ->
                  if matches run pattern (Some state) = None then
                    Some (from + index)
                  else find (index + 1) rest
            in
            find 0 !seen))
  in
  Hashtbl.replace run.emission_cursor observer (at + 1)

(* The next thing of its kind the worker reported is this one. *)
let reported run ~what ~cursor ~bump said to_json pattern =
  let at = cursor () in
  let next =
    driver_until run ("the next " ^ what) (fun () ->
        if Dynarray.length said > at then Some (to_json (Dynarray.get said at))
        else None)
  in
  bump ();
  match matches run pattern (Some next) with
  | Some difference -> failf "the next %s differs: %s" what difference
  | None -> ()

let scopes run expected =
  let expected =
    List.sort compare
      (List.map
         (fun scope -> Bindings.text_of (Bindings.resolve run.bindings scope))
         expected)
  in
  let named = ref [] in
  explained
    (fun () ->
      Printf.sprintf ": it names %s, not %s"
        (json (`List (List.map (fun s -> `String s) !named)))
        (json (`List (List.map (fun s -> `String s) expected))))
    (fun () ->
      proxy_until run "the stream to name those scopes" (fun () ->
          (* One stream, and caught up: an event published next cannot race
             the subscription, and no stream it superseded is still open to
             carry the event a second time. *)
          let open_ =
            List.filter
              (fun (r : Client_proxy.request) ->
                r.path = "/bus/subscribe" && r.status = Some 200 && not r.closed)
              (requests run)
          in
          let body =
            match open_ with
            | [ stream ]
              when Dynarray.exists
                     (fun (e : Client_proxy.event) -> e.event = "ping")
                     stream.events ->
                Option.map subscription stream.json
            | _ -> None
          in
          (named :=
             match Option.bind body (member "scoped") with
             | Some (`List entries) ->
                 List.map
                   (fun entry ->
                     match member "scope" entry with
                     | Some scope -> Bindings.text_of scope
                     | None -> Bindings.text_of entry)
                   entries
             | _ -> []);
          if !named = expected then Some () else None))

let step run (step : Conformance.conformance_step) =
  match step with
  | Let { let_ } ->
      List.iter
        (fun (name, value) ->
          Bindings.bind run.bindings name (Bindings.resolve run.bindings value))
        let_
  | Driver driver -> operate run driver
  | Abandon { abandon } ->
      let { operation = id; _ } = started run abandon in
      let sent = Driver.send run.driver "abandon" [ ("request", `Int id) ] in
      judge run "abandon" (outcome run sent "abandon") nothing_expected
  | Settles { settles; returns; fails; abandoned } ->
      let { operation = id; op; _ } = started run settles in
      run.pending <- List.remove_assoc settles run.pending;
      judge run op
        (outcome run id (Printf.sprintf "%s (%s)" op settles))
        { returns; fails; abandoned = abandoned = Some true }
  | Wire w -> wire run w
  | Carried { carried; is } ->
      let stream = named run carried in
      let difference = ref None in
      explained
        (fun () ->
          match !difference with
          | None -> ": it carried no frames"
          | Some d -> "; the last it carried differs: " ^ d)
        (fun () ->
          proxy_until run
            ("the stream " ^ carried ^ " to carry that frame")
            (fun () ->
              Dynarray.to_list stream.events
              |> List.find_map (fun (e : Client_proxy.event) ->
                  if e.event <> "bus-event" then None
                  else (
                    difference :=
                      matches run is (Some (Yojson.Safe.from_string e.data));
                    if !difference = None then Some () else None))))
  | Closed { closed } ->
      let r = named run closed in
      proxy_until run (closed ^ " to close") (fun () ->
          if r.closed then Some () else None);
      if r.status <> None && not r.answer_lost then
        failf
          "%s was answered %d before it closed; it was to close with no answer \
           reaching the client"
          closed (Option.get r.status)
  | Progress { progress } ->
      let { operation = id; op; sent = arguments } = started run progress in
      let reports () = Driver.progress run.driver id in
      (* An [upload] states its length, so every report says how many bytes
         there are, and all of it is that many. An [upload-unsized] states
         none, so no report says, and all of it is at least the bytes the case
         gave it: what an SDK counts is the body it sends, which holds them
         with the other fields. *)
      let stated =
        match op with
        | "upload" -> true
        | "upload-unsized" -> false
        | other -> failf "%s is a %s, which reports no progress" progress other
      in
      let total, all_sent =
        if stated then (
          let total =
            driver_until run (progress ^ " to report its total") (fun () ->
                match reports () with
                | (_, Some total) :: _ -> Some total
                | (_, None) :: _ ->
                    failf "%s states its length, and reported no total" progress
                | [] -> None)
          in
          if total <= 0 then
            failf "%s reported a total of %d bytes" progress total;
          (Some total, fun sent -> sent = total))
        else
          match List.assoc_opt "bytes" arguments with
          | Some (`String bytes) ->
              let given = String.length (Base64.decode_exn bytes) in
              (None, fun sent -> sent >= given)
          | _ -> failf "%s was given no bytes" progress
      in
      driver_until run (progress ^ " to report all of it sent") (fun () ->
          match List.rev (reports ()) with
          | (sent, _) :: _ when all_sent sent -> Some ()
          | _ -> None);
      ignore
        (List.fold_left
           (fun before (sent, of_) ->
             (match (total, of_) with
             | None, Some of_ ->
                 failf "%s states no length, and reported a total of %d bytes"
                   progress of_
             | Some total, Some of_ when of_ <> total ->
                 failf "%s reported a total of %d bytes, then of %d" progress
                   total of_
             | Some total, None ->
                 failf "%s reported a total of %d bytes, then none" progress
                   total
             | _ -> ());
             if sent < before then
               failf "%s reported %d bytes sent after %d" progress sent before;
             (match total with
             | Some total when sent > total ->
                 failf "%s reported %d of %d bytes" progress sent total
             | _ -> ());
             sent)
           0 (reports ())
          : int)
  | Reaches { reaches = observer; state } -> reaches run observer state
  | Holds { holds; state } -> (
      sync run;
      let states = Driver.emissions run.driver holds in
      match List.rev states with
      | [] -> failf "%s has reported no state" holds
      | latest :: _ ->
          (match matches run state (Some (observed run latest)) with
          | Some difference ->
              failf "%s no longer holds that state: %s" holds difference
          | None -> ());
          Hashtbl.replace run.emission_cursor holds (List.length states))
  | State { state } ->
      let word = Conformance.connection_state_value state in
      let at =
        driver_until run
          (Printf.sprintf "the state %s (reported since: %s)" word
             (match
                drop run.state_cursor (Dynarray.to_list run.driver.states)
              with
             | [] -> "none"
             | since -> String.concat ", " since))
          (fun () ->
            let rec find at =
              if at >= Dynarray.length run.driver.states then None
              else if Dynarray.get run.driver.states at = word then Some at
              else find (at + 1)
            in
            find run.state_cursor)
      in
      run.state_cursor <- at + 1
  | Stays { stays } -> (
      sync run;
      let word = Conformance.connection_state_value stays in
      (match drop run.state_cursor (Dynarray.to_list run.driver.states) with
      | [] -> ()
      | since ->
          failf
            "the transport did not stay %s: since the case last read its state \
             it reported %s"
            word (String.concat ", " since));
      match Dynarray.find_last run.driver.states with
      | Some last when last = word -> ()
      | last ->
          failf "the transport is %s, not %s"
            (Option.value ~default:"in no state" last)
            word)
  | Frames { frames; count } ->
      let cursor =
        Option.value ~default:0 (Hashtbl.find_opt run.frame_cursor frames)
      in
      driver_until run (Printf.sprintf "%d more frames on %s" count frames)
        (fun () ->
          if List.length (Driver.frames run.driver frames) >= cursor + count
          then Some ()
          else None);
      Hashtbl.replace run.frame_cursor frames (cursor + count)
  | Frame { frame = channel; is } -> (
      let cursor =
        Option.value ~default:0 (Hashtbl.find_opt run.frame_cursor channel)
      in
      let frame =
        driver_until run ("a frame on " ^ channel) (fun () ->
            List.nth_opt (Driver.frames run.driver channel) cursor)
      in
      Hashtbl.replace run.frame_cursor channel (cursor + 1);
      match matches run is (Some (frame_json frame)) with
      | Some difference ->
          failf "the next frame on %s differs: %s" channel difference
      | None -> ())
  | Error { error } -> (
      let cursor = run.failure_cursor in
      let failure =
        driver_until run "a failure on the error stream" (fun () ->
            if Dynarray.length run.driver.failures > cursor then
              Some (Dynarray.get run.driver.failures cursor)
            else None)
      in
      run.failure_cursor <- run.failure_cursor + 1;
      in_vocabulary run failure;
      match matches run error (Some (failure_json failure)) with
      | Some difference ->
          failf "the next failure on the error stream differs: %s (%s)"
            difference failure.detail
      | None -> ())
  | Backend { backend = directive; with_ } ->
      backend run directive (resolved run with_)
  | Quiet { quiet } ->
      Run.sleep (float_of_int quiet /. 1000.0);
      let when_ = Printf.sprintf "within %d ms" quiet in
      nothing_unaccounted run ~when_;
      (* A worker that is quiet has also said nothing the case has not read:
         a cancellation signalled too soon is told once, and would otherwise
         pass for the one a later step expects. *)
      if run.layer = Worker then nothing_unreported run ~when_
  | Observe { observe = { query; resource; annotation; search; filters }; as_ }
    ->
      let query =
        `Assoc
          (("query", `String query)
          :: List.filter_map
               (fun (key, value) -> Option.map (fun v -> (key, v)) value)
               [
                 ("resource", resource);
                 ("annotation", annotation);
                 ("search", search);
                 ("filters", filters);
               ])
      in
      let id =
        Driver.send run.driver "observe"
          [
            ("observer", `String as_);
            ("query", Bindings.resolve run.bindings query);
          ]
      in
      judge run "observe" (outcome run id "observe") nothing_expected
  | Leave { leave } ->
      let id =
        Driver.send run.driver "unobserve" [ ("observer", `String leave) ]
      in
      judge run "unobserve" (outcome run id "unobserve") nothing_expected
  | Completes { completes } ->
      driver_until run (completes ^ "'s live query to complete") (fun () ->
          if List.mem completes run.driver.completed then Some () else None)
  | Bus_fetch fetch -> fetches run [ Bus_fetch fetch ]
  | Http_fetch fetch -> fetches run [ Http_fetch fetch ]
  | Fetches { fetches = steps } -> fetches run steps
  | Claimed { claimed } ->
      reported run ~what:"job the worker claims"
        ~cursor:(fun () -> run.reports.claimed)
        ~bump:(fun () -> run.reports.claimed <- run.reports.claimed + 1)
        run.driver.claimed Fun.id claimed
  | Refused { refused } -> (
      let at = run.reports.refused in
      let failure =
        driver_until run "the worker to report a refused claim" (fun () ->
            if Dynarray.length run.driver.refusals > at then
              Some (Dynarray.get run.driver.refusals at)
            else None)
      in
      run.reports.refused <- run.reports.refused + 1;
      (* A refusal the worker made itself carries no code; one the bus made
         carries one of the table's. *)
      if failure.code <> None then in_vocabulary run failure;
      match matches run refused (Some (failure_json failure)) with
      | Some difference ->
          failf "the next refusal the worker reports differs: %s (%s)"
            difference failure.detail
      | None -> ())
  | Signalled { signalled } ->
      reported run ~what:"cancellation the worker signals"
        ~cursor:(fun () -> run.reports.signalled)
        ~bump:(fun () -> run.reports.signalled <- run.reports.signalled + 1)
        run.driver.signalled
        (fun job -> `String job)
        signalled
  | Stalled { stalled } ->
      reported run ~what:"stall the worker reports"
        ~cursor:(fun () -> run.reports.stalled)
        ~bump:(fun () -> run.reports.stalled <- run.reports.stalled + 1)
        run.driver.stalled
        (fun job -> `String job)
        stalled
  | Scopes { scopes = expected } -> scopes run expected

(* What a worker that exported did against WORKER-CONTRACT T1, read from the
   trace each of its claims was sent under and from the spans it exported
   that carry a job.id: those its driver, standing in for a worker's code,
   opened around each job it was handed. *)
let traces run (otlp : Otlp.t) =
  let against = Dynarray.create () in
  let said text = Dynarray.add_last against text in
  let claims =
    List.filter_map
      (fun (r : Client_proxy.request) ->
        match r.json with
        | Some body when string_member "channel" body = Some "job:claim" ->
            Some
              ( member "correlationId" body,
                Option.bind (Client_proxy.header r "traceparent") (fun sent ->
                    List.nth_opt (String.split_on_char '-' sent) 1) )
        | _ -> None)
      (emits run)
  in
  let spans =
    List.filter_map
      (fun (span : Otlp.span) ->
        match List.assoc_opt "job.id" span.attributes with
        | Some (Otlp.Text job) -> Some (job, span.trace_id, span.name)
        | _ -> None)
      (Dynarray.to_list otlp.spans)
  in
  let handed correlation =
    Option.map fst
      (Dynarray.find_opt
         (fun (_, c) -> Some (`String c) = correlation)
         run.handed_over)
  in
  List.iteri
    (fun index (correlation, trace) ->
      let nth = Printf.sprintf "claim %d" (index + 1) in
      match trace with
      | None -> said ("sent " ^ nth ^ " under no trace")
      | Some trace ->
          (match
             List.find_index (fun (_, other) -> other = Some trace) claims
           with
          | Some earlier when earlier < index ->
              said
                (Printf.sprintf "sent %s in the trace of claim %d" nth
                   (earlier + 1))
          | _ -> ());
          let job = handed correlation in
          List.iter
            (fun (span_job, span_trace, name) ->
              if Some span_job <> job && span_trace = trace then
                said
                  (Printf.sprintf
                     "sent %s in the trace of its %s span for %s, a job that \
                      claim did not hand over"
                     nth name span_job))
            spans)
    claims;
  (* A span is exported once it has ended, and a driver ends a job's when it
     has settled the job. *)
  let settled =
    List.filter_map
      (fun (r : Client_proxy.request) ->
        match r.json with
        | Some body
          when List.mem
                 (Option.value ~default:"" (string_member "channel" body))
                 [ "job:complete"; "job:fail"; "job:cancel" ] ->
            Option.bind (member "payload" body) (member "jobId")
        | _ -> None)
      (emits run)
  in
  Dynarray.iter
    (fun (job, correlation) ->
      let trace =
        match
          List.find_opt (fun (c, _) -> c = Some (`String correlation)) claims
        with
        | Some (_, trace) -> trace
        | None ->
            failf
              "the backend handed %s over in answer to no claim the worker sent"
              job
      in
      let own = List.filter (fun (j, _, _) -> j = job) spans in
      if own = [] && List.mem (`String job) settled then
        said
          (Printf.sprintf
             "exported no span carrying job.id %s: a driver runs each job it \
              is handed in a span that carries it"
             job);
      List.iter
        (fun (_, span_trace, name) ->
          if Some span_trace <> trace then
            said
              (Printf.sprintf
                 "ran %s in a %s span of the trace %s, not of the trace of the \
                  claim that handed it over (%s)"
                 job name span_trace
                 (Option.value ~default:"none" trace)))
        own)
    run.handed_over;
  Dynarray.to_list against

(* Run [case] against the driver [command] starts, on [world]'s gateway.
   Fails on the first thing a conforming client would not have done. *)
let run_case world command (case : Corpus.case) layer =
  let proxy = Client_proxy.start (World.origin world) in
  let otlp =
    if case.case.telemetry = Some true then Some (Otlp.start ()) else None
  in
  let driver =
    match otlp with
    | Some otlp ->
        Driver.start
          ~env:[ ("OTEL_EXPORTER_OTLP_ENDPOINT", otlp.endpoint) ]
          command
    | None -> Driver.start command
  in
  let subject = "client-" ^ Fresh.uuid () in
  let participant_roles = [ Protocol.service_role () ] in
  let participant_token, participant_did =
    World.agent world ~roles:participant_roles
      ("conformance-participant-" ^ Fresh.uuid ())
  in
  let run =
    {
      layer;
      world;
      proxy;
      driver;
      bindings = Bindings.create ();
      codes = vocabulary ();
      problems = Dynarray.create ();
      participant_token;
      participant_client = Fresh.uuid ();
      client = World.person_did world subject;
      wire_cursor = 0;
      fetch_cursor = 0;
      emission_cursor = Hashtbl.create 8;
      state_cursor = 0;
      failure_cursor = 0;
      frame_cursor = Hashtbl.create 8;
      reports = { claimed = 0; refused = 0; signalled = 0; stalled = 0 };
      pending = [];
      named = Hashtbl.create 8;
      listening = Dynarray.create ();
      handed_over = Dynarray.create ();
    }
  in
  let bind name value = Bindings.bind run.bindings name value in
  let person () =
    `String
      (World.person world ~claims:[ ("jti", `String (Fresh.uuid ())) ] subject)
  in
  (* A worker signs in as an agent whose token carries the worker role
     beside the service's; every other client is a person. *)
  bind "token"
    (match layer with
    | Worker ->
        `String
          (fst
             (World.agent world
                ~roles:[ Protocol.service_role (); Protocol.worker_role () ]
                ("conformance-worker-" ^ Fresh.uuid ())))
    | Wire | Live -> person ());
  bind "token2" (person ());
  bind "client" (`String run.client);
  bind "participant" (`String participant_did);
  bind "participantRoles"
    (`List (List.map (fun role -> `String role) participant_roles));
  (* In name order, so a case can write a subscription's scopes in the order
     the suite reads them. *)
  List.iteri
    (fun index resource ->
      bind (Printf.sprintf "r%d" (index + 1)) (`String resource))
    (List.sort compare (List.init 3 (fun _ -> "res-" ^ Fresh.uuid ())));
  List.iter
    (fun name -> bind name (`String ("ann-" ^ Fresh.uuid ())))
    [ "a1"; "a2" ];
  bind "resourceScopedChannels"
    (`List
       (List.map
          (fun channel -> `String channel)
          (List.sort compare (Protocol.tables ()).bus_registry.audience.scoped)));
  let steps = case.case.steps in
  let step_json at =
    match member "steps" case.json with
    | Some (`List written) -> (
        match List.nth_opt written at with Some s -> json s | None -> "")
    | _ -> ""
  in
  let at = ref 0 in
  let ending () =
    ignore (Driver.stop driver : int option);
    Client_proxy.close proxy;
    Option.iter Otlp.close otlp;
    (* A case that fails between switching the Archivist and switching it
       back must not fail the next. *)
    world.archivist.mode.replay_fails <- false
  in
  Fun.protect ~finally:ending @@ fun () ->
  match
    List.iteri
      (fun index s ->
        at := index;
        step run s)
      steps;
    at := List.length steps;
    settle run;
    (match Driver.stop driver with
    | Some 0 -> ()
    | Some code -> failf "the driver exited with code %d" code
    | None -> failwith "a signal ended the driver");
    finish run;
    Option.iter
      (fun otlp ->
        at := List.length steps + 1;
        (* A worker reaches the bus and nothing else, so it is held to the
           bus's rows. *)
        (match
           Telemetry.outside_the_sdk_table
             ?transport:(if layer = Worker then Some Bus else None)
             otlp
         with
        | [] -> ()
        | outside -> failf "the client exported %s" (String.concat "; " outside));
        match if layer = Worker then traces run otlp else [] with
        | [] -> ()
        | against ->
            failf
              "each job a worker runs has a trace of its own, and this worker \
               %s"
              (String.concat "; " against))
      otlp
  with
  | () -> ()
  | exception Failure why ->
      let where =
        if !at < List.length steps then
          Printf.sprintf "step %d %s" (!at + 1) (step_json !at)
        else if !at = List.length steps then "the end of the case"
        else "what it exported"
      in
      failf "%s, at %s: %s\n%s" case.name where why (account run)
  | exception Run.Timed_out why ->
      let where =
        if !at < List.length steps then
          Printf.sprintf "step %d %s" (!at + 1) (step_json !at)
        else "the end of the case"
      in
      failf "%s, at %s: %s\n%s" case.name where why (account run)

(* Every case of the corpus [directory], read through the case's model. *)
let corpus directory =
  match Corpus.load ~root:"." directory with
  | Ok cases -> cases
  | Error refusals ->
      failf "the corpus %s does not read: %s" directory
        (String.concat "; " (List.map Decode.error_to_string refusals))

(* Whether [case] runs on [plane]: one that names its planes runs on those
   alone. *)
let on_plane (case : Corpus.case) plane =
  match case.case.planes with
  | None -> true
  | Some planes ->
      List.exists
        (fun p ->
          Conformance.gateway_config_type_value p
          = Protocol.Service_rows.gateway_config_type_value plane)
        planes

let has needle text =
  let n = String.length needle in
  let rec at i =
    i + n <= String.length text && (String.sub text i n = needle || at (i + 1))
  in
  at 0

(* The tests of [corpus]: each case on each plane it runs on, under
   [title], against the driver [command] starts, on the world [make] makes
   for the plane. [exempt] says why a case is one the driver cannot be put
   through. *)
let tests ~make ~world ~close ?(exempt = []) title command corpus layer =
  List.concat_map
    (fun plane ->
      World.each_plane ~planes:[ plane ] ~make ~world ~close title
        (List.filter_map
           (fun (c : Corpus.case) ->
             if not (on_plane c plane) then None
             else
               match List.assoc_opt c.name exempt with
               | None ->
                   Some
                     (Case.case
                        (c.name ^ ": " ^ c.case.about)
                        (fun shared -> run_case (world shared) command c layer))
               | Some why ->
                   (* An exemption is held, not taken on trust: the driver
                      must say it cannot do what the case asks. One that can
                      is not exempt. *)
                   Some
                     (Case.case
                        (c.name ^ ": exempt, because " ^ why)
                        (fun shared ->
                          match run_case (world shared) command c layer with
                          | () ->
                              failf
                                "%s passed, so the driver is not exempt from it"
                                c.name
                          | exception Failure why
                            when has "the driver does not implement " why ->
                              ())))
           corpus))
    Protocol.Service_rows.gateway_config_type_all
