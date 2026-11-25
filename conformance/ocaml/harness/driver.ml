(* The suite's end of the driver protocol (conformance/sdk/README.md, "The
   driver protocol"): it starts an SDK's driver, sends it operations, and
   keeps what the driver says it did and saw: each operation's outcome, the
   states its transport passed through, the frames it delivered, the
   failures its error stream carried and the progress its uploads reported.
   A worker's driver (conformance/worker/README.md) also says what its worker
   claimed, what it was refused, and when a held job's cancellation was
   signalled or the job stalled. Each line is read through the driver line's
   model (specs/src/conformance/ConformanceDriverLine.json); a line it
   refuses is a violation of the protocol, kept for the case to fail on. *)

module Line = Xynolog_protocol.Tables.Conformance_driver_line

type failure = Line.conformance_failure

(* A frame the SDK delivered to its caller. *)
type frame = {
  payload : Yojson.Safe.t option;
  correlation_id : string option;
  scope : string option;
}

(* A state a live query's observer was in. *)
type state = Pending | Ready of Yojson.Safe.t | Failed of failure

type outcome =
  | Returned of Yojson.Safe.t
  | Failure of failure
  | Abandoned
  | Unsupported
  | Misuse of string

type t = {
  process : Process.t;
  mutable read : int;  (** How many of its lines are taken in. *)
  mutable ready : bool;
  states : string Dynarray.t;
  frames : (string * frame Dynarray.t) Dynarray.t;
      (** Per channel, in the order the first frame on each came. *)
  failures : failure Dynarray.t;  (** What the error stream carried. *)
  emissions : (string * state Dynarray.t) Dynarray.t;  (** Per observer. *)
  mutable completed : string list;
  progress : (int * (int * int) Dynarray.t) Dynarray.t;
      (** Per upload: bytes uploaded and total, in order. *)
  claimed : Yojson.Safe.t Dynarray.t;
  refusals : failure Dynarray.t;
  signalled : string Dynarray.t;
  stalled : string Dynarray.t;
  violations : string Dynarray.t;
  outcomes : (int, outcome) Hashtbl.t;
  mutable next_id : int;
  mutable input_open : bool;
}

let shown line =
  if String.length line > 200 then String.sub line 0 200 else line

let entry list key =
  match Dynarray.find_opt (fun (k, _) -> k = key) list with
  | Some (_, items) -> items
  | None ->
      let items = Dynarray.create () in
      Dynarray.add_last list (key, items);
      items

let entries list key =
  match Dynarray.find_opt (fun (k, _) -> k = key) list with
  | Some (_, items) -> Dynarray.to_list items
  | None -> []

let frames t channel = entries t.frames channel
let emissions t observer = entries t.emissions observer
let progress t upload = entries t.progress upload

let settle t id outcome line =
  if Hashtbl.mem t.outcomes id then
    Dynarray.add_last t.violations
      (Printf.sprintf "operation %d settled twice: %s" id (shown line))
  else Hashtbl.replace t.outcomes id outcome

let take t line =
  match Yojson.Safe.from_string line with
  | exception Yojson.Json_error _ ->
      Dynarray.add_last t.violations ("a line that is not JSON: " ^ shown line)
  | json -> (
      match Decode.run ~file:"the driver's output" json Line.t with
      | Error error ->
          Dynarray.add_last t.violations
            (Printf.sprintf "a line the protocol does not allow (%s): %s"
               (Decode.error_to_string error)
               (shown line))
      | Ok (said, _) -> (
          match said with
          | Ready _ -> t.ready <- true
          | Ok { id; ok } -> settle t id (Returned ok) line
          | Failed { id; error } -> settle t id (Failure error) line
          | Abandoned { id; _ } -> settle t id Abandoned line
          | Unsupported { id; _ } -> settle t id Unsupported line
          | Misuse { id; misuse } -> settle t id (Misuse misuse) line
          | State { state } -> Dynarray.add_last t.states state
          | Frame { frame = { channel; payload; correlation_id; scope } } ->
              Dynarray.add_last (entry t.frames channel)
                { payload; correlation_id; scope }
          | Error_stream { error } -> Dynarray.add_last t.failures error
          | Progress { progress = { upload; bytes_uploaded; total_bytes } } ->
              Dynarray.add_last (entry t.progress upload)
                (bytes_uploaded, total_bytes)
          | Emission { emission = { observer; state } } ->
              Dynarray.add_last
                (entry t.emissions observer)
                (match state with
                | Pending _ -> Pending
                | Ready { value; _ } -> Ready value
                | Failed { error; _ } -> Failed error)
          | Completed { completed } -> t.completed <- completed :: t.completed
          | Claimed _ ->
              Dynarray.add_last t.claimed
                (Option.get (Decode.at_pointer "/claimed" json))
          | Refused { refused } -> Dynarray.add_last t.refusals refused
          | Signalled { signalled } -> Dynarray.add_last t.signalled signalled
          | Stalled { stalled } -> Dynarray.add_last t.stalled stalled))

(* Take in every line it has written since last asked. *)
let catch_up t =
  while t.read < Dynarray.length t.process.stdout do
    take t (Dynarray.get t.process.stdout t.read);
    t.read <- t.read + 1
  done

let exited t = Eio.Promise.peek t.process.exited

(* What [probe] answers, once it does; asked again whenever the driver says
   anything. Fails if the driver exits first. *)
let until ?timeout t what probe =
  Run.until t.process.said ?timeout what (fun () ->
      catch_up t;
      match probe () with
      | Some found -> Some found
      | None -> (
          match exited t with
          | Some _ ->
              failwith
                (Printf.sprintf "the driver exited (%s) before %s"
                   (Process.exited_text t.process)
                   what)
          | None -> None))

(* Start [command], with [env] beside the run's environment, and wait for it
   to say it is ready. *)
let start ?(env = []) command =
  let process =
    Process.spawn ~input:true "driver" command (Process.run_environment () @ env)
  in
  let t =
    {
      process;
      read = 0;
      ready = false;
      states = Dynarray.create ();
      frames = Dynarray.create ();
      failures = Dynarray.create ();
      emissions = Dynarray.create ();
      completed = [];
      progress = Dynarray.create ();
      claimed = Dynarray.create ();
      refusals = Dynarray.create ();
      signalled = Dynarray.create ();
      stalled = Dynarray.create ();
      violations = Dynarray.create ();
      outcomes = Hashtbl.create 16;
      next_id = 1;
      input_open = true;
    }
  in
  until ~timeout:30.0 t "the driver to be ready" (fun () ->
      if t.ready then Some () else None);
  t

(* Send an operation; its outcome arrives later, under the id returned. *)
let send t op (args : (string * Yojson.Safe.t) list) =
  let id = t.next_id in
  t.next_id <- id + 1;
  Process.write t.process
    (Yojson.Safe.to_string
       (`Assoc (("id", `Int id) :: ("op", `String op) :: args))
    ^ "\n");
  id

(* How an operation settled, if it has. *)
let settled t id =
  catch_up t;
  Hashtbl.find_opt t.outcomes id

let outcome ?timeout t id what =
  until ?timeout t what (fun () -> Hashtbl.find_opt t.outcomes id)

(* End its input, which is its instruction to dispose and exit, and wait
   until it has: its exit code, None when a signal ended it. *)
let stop t =
  if t.input_open then (
    t.input_open <- false;
    if Process.running t.process then Process.close_input t.process);
  let code =
    match
      Eio.Time.with_timeout (Run.clock ()) 10.0 (fun () ->
          Ok (Eio.Promise.await t.process.exited))
    with
    | Ok code -> code
    | Error `Timeout ->
        Process.signal t.process Sys.sigkill;
        Eio.Promise.await t.process.exited
  in
  catch_up t;
  code

(* What it wrote to stderr, last [n] lines: shown when a case fails. *)
let stderr_tail n t =
  let all = Dynarray.to_list t.process.stderr in
  List.filteri (fun at _ -> at >= List.length all - n) all
