(* corpus.exe ROOT: the reader holds the conformance corpus (Corpus) to its
   schema and to the spec, ROOT's. A corpus of one case per directory is
   accepted, and each change below to one of its cases is refused with the
   refusal it names, among those it gives. *)

let rec copy source target =
  if Sys.is_directory source then (
    Sys.mkdir target 0o755;
    Array.iter
      (fun name ->
        copy (Filename.concat source name) (Filename.concat target name))
      (Sys.readdir source))
  else
    Out_channel.with_open_bin target (fun out ->
        Out_channel.output_string out
          (In_channel.with_open_bin source In_channel.input_all))

let rec mkdirs path =
  if not (Sys.file_exists path) then (
    mkdirs (Filename.dirname path);
    Sys.mkdir path 0o755)

(* A case of each directory the corpus has, each what its layer reads. *)
let wire =
  {|{"about": "a", "source": "s", "steps": [{"wire": "POST /bus/emit", "status": 202}, {"backend": "listen", "with": {"channels": ["mark:added"]}}, {"frame": "mark:added", "is": {}}]}|}

let live =
  {|{"about": "a", "source": "s", "tier": "fleet", "steps": [{"observe": {"query": "resource"}, "as": "one"}, {"fetch": "browse:resource-requested", "as": "first"}, {"fetch": "GET /resources/{id}/annotations"}]}|}

let held =
  {|"jobId": "job-1", "jobType": "mark", "resourceId": {"$var": "r1"}, "params": {"$var": "_"}, "completedUnits": [], "unitCursors": {}, "retryCount": 0|}

let claimed row =
  Printf.sprintf
    {|{"about": "a", "source": "s", "steps": [{"driver": "claim", "returns": {}}, {"claimed": {%s}}]}|}
    row

let worker = claimed (held ^ {|, "maxRetries": 3|})
let base = [ ("wire", wire); ("live", live); ("worker", worker) ]

let directory = function
  | "wire" -> "conformance/sdk/wire"
  | "live" -> "conformance/sdk/live"
  | _ -> "conformance/worker/cases"

let failures = ref 0

(* The corpus of [base], each case [changed] where it names one, at a copy of
   [root]'s specs: its refusals. *)
let refusals ~root spec ?(name = "a-case") changed =
  let copied = Filename.temp_dir "corpus" "" in
  mkdirs (Filename.concat copied "specs");
  copy (Filename.concat root "specs/src") (Filename.concat copied "specs/src");
  List.iter
    (fun (layer, text) ->
      let dir = Filename.concat copied (directory layer) in
      mkdirs dir;
      let text = Option.value ~default:text (List.assoc_opt layer changed) in
      Out_channel.with_open_bin
        (Filename.concat dir
           ((if List.mem_assoc layer changed then name else "a-case") ^ ".json"))
        (fun out -> Out_channel.output_string out text))
    base;
  List.map Decode.error_to_string (Corpus.refusals ~root:copied spec)

let refused ~root spec ~why ?name layer text expected =
  let found = refusals ~root spec ?name [ (layer, text) ] in
  if not (List.mem expected found) then (
    Printf.eprintf
      "A corpus whose %s case %s was not refused with:\n  %s\nbut with:\n" layer
      why expected;
    List.iter (Printf.eprintf "  %s\n") found;
    incr failures)

let () =
  let root =
    match Sys.argv with
    | [| _; root |] -> root
    | _ ->
        prerr_endline "usage: corpus.exe ROOT";
        exit 2
  in
  let spec =
    match Spec.load (Filename.concat root "specs/src") with
    | Ok spec -> spec
    | Error errors ->
        List.iter (fun e -> prerr_endline (Decode.error_to_string e)) errors;
        exit 1
  in
  (match refusals ~root spec [] with
  | [] -> ()
  | found ->
      prerr_endline "The corpus of one case per directory was refused:";
      List.iter (Printf.eprintf "  %s\n") found;
      incr failures);
  let refused = refused ~root spec in
  let at layer = directory layer ^ "/a-case.json" in
  refused ~why:"is named otherwise than in kebab case" ~name:"A_case" "wire"
    wire "conformance/sdk/wire/A_case.json: /: is not named in kebab case";
  refused ~why:"states a member a case has none of" "wire"
    {|{"about": "a", "source": "s", "steps": [{"quiet": 1}], "name": "x"}|}
    (at "wire" ^ ": /name: is a member the schema does not state");
  refused ~why:"has no steps" "wire"
    {|{"about": "a", "source": "s", "steps": []}|}
    (at "wire" ^ ": /steps: has fewer than 1 items");
  refused ~why:"has a step of no kind" "wire"
    {|{"about": "a", "source": "s", "steps": [{"wait": 1}]}|}
    (at "wire" ^ ": /steps/0: is none of the oneOf");
  refused ~why:"expects an operation both to return and to fail" "worker"
    {|{"about": "a", "source": "s", "steps": [{"driver": "claim", "returns": {}, "fails": {}}]}|}
    (at "worker" ^ ": /steps/0: is none of the oneOf");
  List.iter
    (fun (a, b) ->
      refused
        ~why:(Printf.sprintf "settles an operation with both %s and %s" a b)
        "worker"
        (Printf.sprintf
           {|{"about": "a", "source": "s", "steps": [{"driver": "claim", "as": "c"}, {"settles": "c", "%s": %s, "%s": %s}]}|}
           a
           (if a = "abandoned" then "true" else "{}")
           b
           (if b = "abandoned" then "true" else "{}"))
        (at "worker" ^ ": /steps/1: is none of the oneOf"))
    [ ("returns", "fails"); ("returns", "abandoned"); ("fails", "abandoned") ];
  refused ~why:"names an operation the gateway does not declare" "wire"
    {|{"about": "a", "source": "s", "steps": [{"wire": "POST /bus/shout"}]}|}
    (at "wire"
   ^ ": /steps/0/wire: names the operation POST /bus/shout, which the \
      gateway's document does not declare");
  refused ~why:"scripts an answer to an operation the gateway does not declare"
    "wire"
    {|{"about": "a", "source": "s", "steps": [{"backend": "refuse", "with": {"wire": "GET /nowhere", "status": 503, "times": 1}}]}|}
    (at "wire"
   ^ ": /steps/0/with/wire: names the operation GET /nowhere, which the \
      gateway's document does not declare");
  refused ~why:"listens on a channel the registry does not have" "wire"
    {|{"about": "a", "source": "s", "steps": [{"backend": "listen", "with": {"channels": ["mark:shouted"]}}]}|}
    (at "wire"
   ^ ": /steps/0/with/channels/0: names the channel mark:shouted, which the \
      bus registry does not have");
  refused ~why:"reads a frame of a channel the registry does not have" "wire"
    {|{"about": "a", "source": "s", "steps": [{"frame": "mark:shouted", "is": {}}]}|}
    (at "wire"
   ^ ": /steps/0/frame: names the channel mark:shouted, which the bus registry \
      does not have");
  refused ~why:"expects a request no operation of the registry asks" "live"
    {|{"about": "a", "source": "s", "tier": "fleet", "steps": [{"fetch": "browse:nothing-requested", "as": "x"}]}|}
    (at "live"
   ^ ": /steps/0/fetch: names the request browse:nothing-requested, which no \
      operation of the bus registry asks");
  refused ~why:"expects a read the gateway does not declare" "live"
    {|{"about": "a", "source": "s", "tier": "fleet", "steps": [{"fetch": "GET /nowhere"}]}|}
    (at "live"
   ^ ": /steps/0/fetch: names the read GET /nowhere, which the gateway's \
      document does not declare");
  refused ~why:"states no tier" "live"
    {|{"about": "a", "source": "s", "steps": [{"quiet": 1}]}|}
    (at "live" ^ ": /: is a live case, and states no tier");
  refused ~why:"states a tier" "wire"
    {|{"about": "a", "source": "s", "tier": "fleet", "steps": [{"quiet": 1}]}|}
    (at "wire" ^ ": /tier: states a tier, which only a live case has");
  refused ~why:"has a wire step" "live"
    {|{"about": "a", "source": "s", "tier": "fleet", "steps": [{"wire": "POST /bus/emit"}]}|}
    (at "live"
   ^ ": /steps/0: is a wire step: a live case accounts for the wire with fetch \
      and scopes");
  refused ~why:"observes" "wire"
    {|{"about": "a", "source": "s", "steps": [{"observe": {"query": "resource"}, "as": "one"}]}|}
    (at "wire"
   ^ ": /steps/0: is an observation, which this directory's cases have none of"
    );
  refused ~why:"hears what a worker reports" "wire"
    {|{"about": "a", "source": "s", "steps": [{"stalled": "job-1"}]}|}
    (at "wire"
   ^ ": /steps/0: is what a worker reports, which this directory's cases have \
      none of");
  refused ~why:"expects a claimed row without a member of the record" "worker"
    (claimed held)
    (at "worker"
   ^ ": /steps/1/claimed: lacks maxRetries, a member of HeldJobRecord");
  refused ~why:"expects a claimed row with a member the record has not" "worker"
    (claimed (held ^ {|, "maxRetries": 3, "attempt": 1|}))
    (at "worker" ^ ": /steps/1/claimed/attempt: is no member of HeldJobRecord");
  refused ~why:"expects a claimed row with a value its member's schema refuses"
    "worker"
    (claimed (held ^ {|, "maxRetries": -1|}))
    (at "worker" ^ ": /steps/1/claimed/maxRetries: is less than 0");
  refused ~why:"overrides a timing client/timing.json fixes" "wire"
    {|{"about": "a", "source": "s", "steps": [{"driver": "open", "with": {"timing": {"reconnectMs": 50, "emitTimeoutMs": 50}}}]}|}
    (at "wire"
   ^ ": /steps/0/with/timing/emitTimeoutMs: overrides emitTimeoutMs, which \
      client/timing.json does not let a caller override");
  (match
     refusals ~root spec
       [
         ( "wire",
           {|{"about": "a", "source": "s", "steps": [{"driver": "open", "with": {"timing": {"reconnectMs": 50, "emitRetry": {"attempts": 2, "initialDelayMs": 1, "maxDelayMs": 2}}}}]}|}
         );
       ]
   with
  | [] -> ()
  | found ->
      prerr_endline "A case that overrides a timing a caller may was refused:";
      List.iter (Printf.eprintf "  %s\n") found;
      incr failures);
  if !failures > 0 then exit 1;
  print_endline
    "The reader holds the conformance corpus to its schema, refusing a case \
     named otherwise than in kebab case, a member or a step of no kind, no \
     steps, an outcome both returned and failed, a settling of two kinds; and \
     to the spec, refusing an operation the gateway does not declare, a \
     channel or a request the registry does not have, a live case without its \
     tier or another with one, a step of another layer, a timing overridden that no caller may, and a claimed row that \
     is not the record a driver's claimed line carries"
