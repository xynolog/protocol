(* Each service's protocol is in the spec: a client, a conformance suite or a
   second implementation reads specs/src and learns everything the service
   answers. The gateway's document (openapi.json) and the Archivist's
   (archivist/openapi.json) leave nothing to be learned from the code:

   - a server's URL does not end in a slash, and it names each of its
     variables, and only those;
   - a path is "/" or a "/" before each of its segments, and a segment is a
     whole [{parameter}] or holds none of "{", "}", "?" and "#";
   - no request could match two paths of a document: two with as many
     segments and as many parameters match one request when each segment
     where neither has a parameter is the same in both;
   - an operation declares a path parameter for each parameter its path
     names, and no other, and no parameter twice (a header's name in any
     case);
   - an operationId is an operation's alone in its document;
   - every response is keyed by an HTTP status, from 100 to 599;
   - every operation declares a 2xx, and one that takes a credential, a
     parameter or a body a 4xx;
   - every security scheme is described, and every one an operation names is
     defined;
   - a protected operation declares a 401 that carries WWW-Authenticate
     (that each says whether it is public is Openapi's model's);
   - every operation declares a 500, one with a request body a 400, and one
     with a JSON body its [x-xynolog-limits.maxBodyBytes] and a 413;
   - a 429 carries Retry-After (that a declared header has a schema is
     Openapi's model's);
   - every response but a 204 or a 3xx has a body with a schema, and an
     error's is application/json and ErrorResponse, or an [allOf] that
     refines it;
   - a stream is a [oneOf] of its messages, each requiring a one-value
     [event] and a [data], and each [id] format has a pattern;
   - a limit is a positive integer, or a baseline with coefficients by role,
     each coefficient [unlimited] or shaped like the baseline, and each role
     one of principals/roles.json's;
   - a wait ends inside the wait around it: the Archivist's hold on a read
     of anchored text inside its [readSeconds], and the gateway's call of
     the Archivist inside a client's [httpRequestTimeoutMs];
   - the gateway holds an emit's key longer than a client's attempts at it
     can last ([idempotencySeconds] against client/timing.json). *)

let ( / ) pointer token = pointer ^ "/" ^ Decode.escape token

let member name = function
  | `Assoc members -> List.assoc_opt name members
  | _ -> None

let members = function Some (`Assoc members) -> members | _ -> []
let text = function Some (`String s) -> Some s | _ -> None
let shown = Yojson.Safe.to_string
let gateway = "openapi.json"
let archivist = "archivist/openapi.json"
let timing = "client/timing.json"
let roles_table = "principals/roles.json"
let error_response = Places.components ^ "ErrorResponse.json"
let methods = [ "get"; "put"; "post"; "delete"; "patch"; "head"; "options" ]

let positive = function
  | `Int n -> n > 0
  | `Float f -> Float.is_integer f && f > 0.
  | _ -> false

let integer = function
  | `Int n -> Some n
  | `Float f -> Some (int_of_float f)
  | _ -> None

let contains_any characters text =
  String.exists (fun c -> String.contains characters c) text

(* A segment that is a whole [{parameter}]: its name. *)
let parameter_name segment =
  let length = String.length segment in
  if
    length > 2
    && segment.[0] = '{'
    && segment.[length - 1] = '}'
    && not (contains_any "{}" (String.sub segment 1 (length - 2)))
  then Some (String.sub segment 1 (length - 2))
  else None

let well_formed path =
  path = "/"
  || String.starts_with ~prefix:"/" path
     && List.for_all
          (fun segment ->
            segment <> ""
            && (Option.is_some (parameter_name segment)
               || not (contains_any "{}?#" segment)))
          (List.tl (String.split_on_char '/' path))

let path_parameters path =
  List.filter_map parameter_name (String.split_on_char '/' path)

(* Redocly's no-ambiguous-paths, which no-identical-paths is a case of. *)
let ambiguous a b =
  let a = String.split_on_char '/' a and b = String.split_on_char '/' b in
  let names segments = List.length (List.filter_map parameter_name segments) in
  List.length a = List.length b
  && names a = names b
  && List.for_all2
       (fun x y ->
         Option.is_some (parameter_name x)
         || Option.is_some (parameter_name y)
         || x = y)
       a b

(* The [{variable}]s a server's URL names. *)
let url_variables url =
  let rec from i found =
    match String.index_from_opt url i '{' with
    | None -> List.rev found
    | Some opening -> (
        match String.index_from_opt url opening '}' with
        | None -> List.rev found
        | Some closing ->
            from (closing + 1)
              (String.sub url (opening + 1) (closing - opening - 1) :: found))
  in
  from 0 []

(* A coefficient's shape: an integer, or the names of an object of them. *)
let shape_of = function
  | value when positive value -> Some "integer"
  | `Assoc (_ :: _ as entries)
    when List.for_all (fun (_, v) -> positive v) entries ->
      Some (String.concat "," (List.sort compare (List.map fst entries)))
  | _ -> None

let refusals ~ids ~(read_json : string -> Yojson.Safe.t option) =
  let found = ref [] in
  let refuse file pointer fmt =
    Printf.ksprintf
      (fun message -> found := { Decode.file; pointer; message } :: !found)
      fmt
  in
  (* The node a [$ref] chain from [node], written in [file], ends at, with
     the file it is in; a reference that names nothing is the census's to
     refuse. *)
  let rec deref ?(seen = []) file node =
    match text (member "$ref" node) with
    | None -> Some (file, node)
    | Some target ->
        let ((named, pointer) as place) =
          Decode.resolve ids { target; file; pointer = "" }
        in
        if List.mem place seen then None
        else
          Option.bind (read_json named) (fun json ->
              Option.bind
                (Decode.at_pointer pointer json)
                (deref ~seen:(place :: seen) named))
  in
  let ref_target file node =
    match text (member "$ref" node) with
    | Some target when not (String.starts_with ~prefix:"#" target) ->
        Some (fst (Decode.resolve ids { target; file; pointer = "" }))
    | _ -> None
  in
  let is_error_body file schema =
    ref_target file schema = Some error_response
    ||
    match deref file schema with
    | Some (at, node) -> (
        match member "allOf" node with
        | Some (`List parts) ->
            List.exists
              (fun part -> ref_target at part = Some error_response)
              parts
        | _ -> false)
    | None -> false
  in
  let known_roles =
    match
      Option.bind (read_json roles_table) (fun table ->
          match member "roles" table with
          | Some (`List rows) ->
              Some (List.filter_map (fun row -> text (member "role" row)) rows)
          | _ -> None)
    with
    | Some roles -> roles
    | None ->
        refuse roles_table "" "is missing: x-xynolog-limits names its roles";
        []
  in
  let check_limits file pointer where limits =
    List.iter
      (fun (key, value) ->
        let label = "x-xynolog-limits." ^ key in
        let pointer = pointer / "x-xynolog-limits" / key in
        if not (positive value) then
          match value with
          | `Assoc entries when List.mem_assoc "baseline" entries -> (
              let extra =
                List.filter
                  (fun k -> k <> "baseline" && k <> "roles")
                  (List.map fst entries)
              in
              if extra <> [] then
                refuse file pointer
                  "%s: %s has %s beside its baseline and roles" where label
                  (String.concat ", " extra);
              let baseline = List.assoc "baseline" entries in
              match shape_of baseline with
              | None ->
                  refuse file (pointer / "baseline")
                    "%s: %s.baseline is %s, not a positive integer or an \
                     object of them"
                    where label (shown baseline)
              | Some shape ->
                  List.iter
                    (fun (role, coefficient) ->
                      if not (List.mem role known_roles) then
                        refuse file
                          (pointer / "roles" / role)
                          "%s: %s.roles names %s, which is no role of %s" where
                          label role roles_table;
                      if
                        coefficient <> `String "unlimited"
                        && shape_of coefficient <> Some shape
                      then
                        refuse file
                          (pointer / "roles" / role)
                          "%s: %s.roles.%s is %s, neither \"unlimited\" nor \
                           shaped like the baseline"
                          where label role (shown coefficient))
                    (members (List.assoc_opt "roles" entries)))
          | _ ->
              refuse file pointer
                "%s: %s is %s, not a positive integer or a baseline with \
                 coefficients by role"
                where label (shown value))
      (members (member "x-xynolog-limits" limits))
  in
  let check_stream file pointer where schema =
    match deref file schema with
    | None -> ()
    | Some (at, node) -> (
        match member "oneOf" node with
        | Some (`List (_ :: _ as messages)) ->
            List.iteri
              (fun i message ->
                match deref at message with
                | None -> ()
                | Some (message_file, message) ->
                    let label = Printf.sprintf "%s message %d" where i in
                    let props = member "properties" message in
                    let prop name = Option.bind props (member name) in
                    let event =
                      Option.bind (prop "event") (fun e ->
                          Option.map snd (deref message_file e))
                    in
                    (match Option.bind event (member "enum") with
                    | Some (`List [ _ ]) -> ()
                    | _ ->
                        refuse file pointer
                          "%s: `event` is not a one-value enum" label);
                    if Option.is_none (prop "data") then
                      refuse file pointer "%s: has no `data` schema" label;
                    let required =
                      match member "required" message with
                      | Some (`List r) -> r
                      | _ -> []
                    in
                    if
                      not
                        (List.mem (`String "event") required
                        && List.mem (`String "data") required)
                    then
                      refuse file pointer
                        "%s: `event` and `data` are not both required" label;
                    Option.iter
                      (fun id ->
                        match deref message_file id with
                        | None -> ()
                        | Some (id_file, id) ->
                            let formats =
                              match member "oneOf" id with
                              | Some (`List formats) ->
                                  List.filter_map
                                    (fun f -> Option.map snd (deref id_file f))
                                    formats
                              | _ -> [ id ]
                            in
                            List.iter
                              (fun format ->
                                if
                                  Option.is_none
                                    (text (member "pattern" format))
                                then
                                  refuse file pointer
                                    "%s: an `id` format has no `pattern`" label)
                              formats)
                      (prop "id"))
              messages
        | _ ->
            refuse file pointer
              "%s: the stream schema is not a oneOf of its messages, so its \
               event names are written nowhere"
              where)
  in
  let timing_value name =
    Option.bind (read_json timing) (fun table ->
        match member "timing" table with
        | Some (`List rows) ->
            List.find_map
              (fun row ->
                if text (member "name" row) = Some name then member "value" row
                else None)
              rows
        | _ -> None)
  in
  (* The waits that nest. *)
  Option.iter
    (fun document ->
      let anchored_text =
        Option.bind
          (Option.bind (member "paths" document)
             (member "/resources/{id}/anchored-text"))
          (fun item -> Option.map snd (deref archivist item))
      in
      let limit node name =
        Option.bind (member "x-xynolog-limits" node) (member name)
      in
      let waits =
        [
          ( "GET /resources/{id}/anchored-text \
             x-xynolog-limits.settleWaitSeconds",
            Option.bind
              (Option.bind anchored_text (member "get"))
              (fun get -> limit get "settleWaitSeconds") );
          ("x-xynolog-limits.connectSeconds", limit document "connectSeconds");
          ("x-xynolog-limits.readSeconds", limit document "readSeconds");
          ( "client/timing.json httpRequestTimeoutMs",
            timing_value "httpRequestTimeoutMs" );
        ]
      in
      let stated =
        List.filter_map
          (fun (_, v) ->
            Option.bind v (fun v -> if positive v then integer v else None))
          waits
      in
      List.iter
        (fun (name, value) ->
          if not (Option.fold ~none:false ~some:positive value) then
            refuse archivist ""
              "waits that nest: %s is %s, not a positive integer to order" name
              (Option.fold ~none:"absent" ~some:shown value))
        waits;
      match stated with
      | [ hold; connect; read; request_ms ] ->
          if hold >= read then
            refuse archivist ""
              "waits that nest: settleWaitSeconds (%d) is not shorter than \
               readSeconds (%d): a caller stops waiting before the Archivist \
               answers a read it holds"
              hold read;
          if (connect + read) * 1000 >= request_ms then
            refuse archivist ""
              "waits that nest: connectSeconds (%d) and readSeconds (%d) \
               together are not shorter than client/timing.json \
               httpRequestTimeoutMs (%d): a client stops waiting before the \
               gateway answers a read the Archivist does not"
              connect read request_ms
      | _ -> ())
    (read_json archivist);
  List.iter
    (fun root_file ->
      Option.iter
        (fun root ->
          check_limits root_file "" "the document" root;
          let schemes =
            members
              (Option.bind (member "components" root) (member "securitySchemes"))
          in
          List.iter
            (fun (name, scheme) ->
              let pointer = "/components/securitySchemes" / name in
              (match text (member "description" scheme) with
              | Some d when String.trim d <> "" -> ()
              | _ ->
                  refuse root_file pointer
                    "securitySchemes.%s: has no description — the claims a \
                     credential must carry are protocol"
                    name);
              check_limits root_file pointer ("securitySchemes." ^ name) scheme)
            schemes;
          (match member "servers" root with
          | Some (`List servers) ->
              List.iteri
                (fun i server ->
                  let pointer = "/servers" / string_of_int i in
                  Option.iter
                    (fun url ->
                      if String.ends_with ~suffix:"/" url then
                        refuse root_file pointer
                          "server %s: ends in a slash, so a path joined to it \
                           begins with two"
                          url;
                      let named = url_variables url in
                      let variables =
                        List.map fst (members (member "variables" server))
                      in
                      List.iter
                        (fun name ->
                          if not (List.mem name variables) then
                            refuse root_file pointer
                              "server %s: names {%s}, which its variables do \
                               not define"
                              url name)
                        named;
                      List.iter
                        (fun name ->
                          if not (List.mem name named) then
                            refuse root_file
                              (pointer / "variables" / name)
                              "server %s: defines variable %S, which its URL \
                               does not name"
                              url name)
                        variables)
                    (text (member "url" server)))
                servers
          | _ -> ());
          let paths = members (member "paths" root) in
          List.iteri
            (fun i (path, _) ->
              let pointer = "/paths" / path in
              if not (well_formed path) then
                refuse root_file pointer
                  "path %S: is not \"/\" or a \"/\" before each of its \
                   segments, each a whole {parameter} or holding none of \
                   \"{\", \"}\", \"?\" and \"#\""
                  path;
              List.iteri
                (fun j (earlier, _) ->
                  if j < i && ambiguous earlier path then
                    refuse root_file pointer
                      "paths %s and %s: one request could match both" earlier
                      path)
                paths)
            paths;
          let operation_ids = Hashtbl.create 32 in
          List.iter
            (fun (path, item) ->
              match deref root_file item with
              | None -> ()
              | Some (file, item) ->
                  List.iter
                    (fun method_ ->
                      Option.iter
                        (fun op ->
                          let where =
                            String.uppercase_ascii method_ ^ " " ^ path
                          in
                          let at = "" / method_ in
                          let requirements =
                            (* Openapi's model refuses an operation that
                               does not say [security]. *)
                            match member "security" op with
                            | Some (`List requirements) -> requirements
                            | _ -> []
                          in
                          List.iter
                            (fun requirement ->
                              List.iter
                                (fun (name, _) ->
                                  if not (List.mem_assoc name schemes) then
                                    refuse file (at / "security")
                                      "%s: names security scheme %S, which \
                                       components.securitySchemes does not \
                                       define"
                                      where name)
                                (members (Some requirement)))
                            requirements;
                          let is_public = requirements = [] in
                          check_limits file at where op;
                          let responses = members (member "responses" op) in
                          let has status = List.mem_assoc status responses in
                          let request_body = member "requestBody" op in
                          Option.iter
                            (fun id ->
                              match Hashtbl.find_opt operation_ids id with
                              | Some first ->
                                  refuse root_file ("/paths" / path)
                                    "%s: has operationId %S, which %s has too"
                                    where id first
                              | None -> Hashtbl.add operation_ids id where)
                            (text (member "operationId" op));
                          let parameters =
                            match member "parameters" op with
                            | Some (`List parameters) -> parameters
                            | _ -> []
                          in
                          let located =
                            List.filter_map
                              (fun parameter ->
                                match
                                  ( text (member "name" parameter),
                                    text (member "in" parameter) )
                                with
                                | Some name, Some in_ -> Some (name, in_)
                                | _ -> None)
                              parameters
                          in
                          let template = path_parameters path in
                          let declared =
                            List.filter_map
                              (fun (name, in_) ->
                                if in_ = "path" then Some name else None)
                              located
                          in
                          List.iter
                            (fun name ->
                              if not (List.mem name declared) then
                                refuse file (at / "parameters")
                                  "%s: declares no path parameter %S, which \
                                   its path names"
                                  where name)
                            template;
                          List.iter
                            (fun name ->
                              if not (List.mem name template) then
                                refuse file (at / "parameters")
                                  "%s: declares path parameter %S, which its \
                                   path does not name"
                                  where name)
                            declared;
                          let spelled (name, in_) =
                            ( (if in_ = "header" then String.lowercase_ascii name
                               else name),
                              in_ )
                          in
                          List.iteri
                            (fun i parameter ->
                              if
                                List.exists
                                  (fun earlier ->
                                    spelled earlier = spelled parameter)
                                  (List.filteri (fun j _ -> j < i) located)
                              then
                                refuse file (at / "parameters")
                                  "%s: declares %s parameter %S twice" where
                                  (snd parameter) (fst parameter))
                            located;
                          let status_class hundreds =
                            List.exists
                              (fun (status, _) ->
                                Option.fold ~none:false
                                  ~some:(fun code ->
                                    Int.div code 100 = hundreds)
                                  (int_of_string_opt status))
                              responses
                          in
                          List.iter
                            (fun (status, _) ->
                              match int_of_string_opt status with
                              | Some code
                                when String.length status = 3
                                     && code >= 100 && code <= 599 ->
                                  ()
                              | _ ->
                                  refuse file (at / "responses")
                                    "%s: answers %S, which is no HTTP status"
                                    where status)
                            responses;
                          if not (status_class 2) then
                            refuse file (at / "responses") "%s: declares no 2xx"
                              where;
                          if
                            ((not is_public) || parameters <> []
                            || Option.is_some request_body)
                            && not (status_class 4)
                          then
                            refuse file (at / "responses")
                              "%s: takes a credential, a parameter or a body \
                               and declares no 4xx"
                              where;
                          if not (has "500") then
                            refuse file (at / "responses") "%s: declares no 500"
                              where;
                          if Option.is_some request_body && not (has "400") then
                            refuse file (at / "responses")
                              "%s: takes a request body and declares no 400"
                              where;
                          let takes_json =
                            match Option.bind request_body (deref file) with
                            | Some (_, body) ->
                                List.mem_assoc "application/json"
                                  (members (member "content" body))
                            | None -> false
                          in
                          if
                            takes_json
                            && Option.is_none
                                 (Option.bind
                                    (member "x-xynolog-limits" op)
                                    (member "maxBodyBytes"))
                          then
                            refuse file at
                              "%s: takes a JSON body and states no \
                               x-xynolog-limits.maxBodyBytes"
                              where;
                          if takes_json && not (has "413") then
                            refuse file (at / "responses")
                              "%s: takes a JSON body and declares no 413" where;
                          if (not is_public) && not (has "401") then
                            refuse file (at / "responses")
                              "%s: is protected and declares no 401" where;
                          List.iter
                            (fun (status, response) ->
                              match deref file response with
                              | None -> ()
                              | Some (response_file, response) -> (
                                  let label = where ^ " " ^ status in
                                  let pointer = at / "responses" / status in
                                  let headers =
                                    members (member "headers" response)
                                  in
                                  let carries name =
                                    List.exists
                                      (fun (h, _) ->
                                        String.lowercase_ascii h = name)
                                      headers
                                  in
                                  if
                                    status = "401"
                                    && not (carries "www-authenticate")
                                  then
                                    refuse file pointer
                                      "%s: carries no WWW-Authenticate header"
                                      label;
                                  if
                                    status = "429"
                                    && not (carries "retry-after")
                                  then
                                    refuse file pointer
                                      "%s: carries no Retry-After header" label;
                                  let code = int_of_string_opt status in
                                  let bodiless =
                                    match code with
                                    | Some c -> c = 204 || (c >= 300 && c < 400)
                                    | None -> false
                                  in
                                  match members (member "content" response) with
                                  | [] ->
                                      if not bodiless then
                                        refuse file pointer
                                          "%s: declares no body" label
                                  | content ->
                                      List.iter
                                        (fun (media_type, media) ->
                                          match member "schema" media with
                                          | None ->
                                              refuse file pointer
                                                "%s: %s has no schema" label
                                                media_type
                                          | Some schema ->
                                              if
                                                media_type = "text/event-stream"
                                              then
                                                check_stream file pointer
                                                  (label ^ " " ^ media_type)
                                                  schema;
                                              if
                                                Option.fold ~none:false
                                                  ~some:(fun c -> c >= 400)
                                                  code
                                              then (
                                                if
                                                  media_type
                                                  <> "application/json"
                                                then
                                                  refuse file pointer
                                                    "%s: an error body is %s, \
                                                     not application/json"
                                                    label media_type;
                                                if
                                                  not
                                                    (is_error_body response_file
                                                       schema)
                                                then
                                                  refuse file pointer
                                                    "%s: an error body is not \
                                                     ErrorResponse, or a \
                                                     refinement of it"
                                                    label))
                                        content))
                            responses)
                        (member method_ item))
                    methods)
            paths)
        (read_json root_file))
    [ gateway; archivist ];
  (* An emit's key outlives every attempt at it: a client's attempts at one
     emit span at most two [emitRetry] budgets, before and after a renewed
     credential, and its renewal between. *)
  (let number name = Option.bind (timing_value name) integer in
   let retry name =
     Option.bind (timing_value name) (fun budget ->
         match
           ( Option.bind (member "attempts" budget) integer,
             Option.bind (member "initialDelayMs" budget) integer,
             Option.bind (member "maxDelayMs" budget) integer )
         with
         | Some attempts, Some initial, Some max_delay ->
             Some (attempts, initial, max_delay)
         | _ -> None)
   in
   let budget_ms (attempts, initial, max_delay) attempt_ms =
     let delays = ref 0 in
     for attempt = 1 to attempts - 1 do
       delays := !delays + min (initial * (1 lsl (attempt - 1))) max_delay
     done;
     (attempts * attempt_ms) + !delays
   in
   match
     ( retry "emitRetry",
       number "emitTimeoutMs",
       retry "refreshRetry",
       number "httpRequestTimeoutMs",
       Option.bind (read_json gateway) (fun root ->
           Option.bind
             (Option.bind (member "paths" root) (member "/bus/emit"))
             (fun item ->
               Option.bind (deref gateway item) (fun (_, item) ->
                   member "post" item))) )
   with
   | ( Some emit_retry,
       Some emit_ms,
       Some refresh_retry,
       Some request_ms,
       Some emit ) -> (
       let longest_ms =
         (2 * budget_ms emit_retry emit_ms) + budget_ms refresh_retry request_ms
       in
       match
         Option.bind
           (member "x-xynolog-limits" emit)
           (member "idempotencySeconds")
       with
       | Some held when positive held ->
           let held = Option.get (integer held) in
           if held * 1000 <= longest_ms then
             refuse gateway "/paths/~1bus~1emit"
               "POST /bus/emit: holds a key for %d s, and a client's attempts \
                at one emit may span %g s (two emitRetry budgets and a \
                refreshRetry, specs/src/client/timing.json)"
               held
               (float_of_int longest_ms /. 1000.)
       | _ ->
           refuse gateway "/paths/~1bus~1emit"
             "POST /bus/emit: states no x-xynolog-limits.idempotencySeconds")
   | _ ->
       refuse timing ""
         "the emit's key cannot be held against a client's attempts: \
          emitRetry, emitTimeoutMs, refreshRetry, httpRequestTimeoutMs or POST \
          /bus/emit is missing");
  List.rev !found
