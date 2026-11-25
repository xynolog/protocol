(* The live cases built from specs/src/client/refresh.json: one per row. Each
   has a client observe every live query the table names, of one resource,
   and a second resource and an annotation of it, which no event of the case
   names; causes the row's trigger; and expects exactly what the row says of
   the cache: a request for each query it refetches, of the keys it reaches,
   the written value where it writes, bus.not-found where it removes, and
   nothing else.

   A row is held by being here, so a table that grows a trigger this file
   cannot cause fails the suite rather than going unheld. *)

open Xynolog_conformance
open Xynolog_protocol
module Refresh = Tables.Client_refresh

let failf format = Printf.ksprintf failwith format
let var name = `Assoc [ ("$var", `String name) ]

let timing =
  `Assoc
    [
      ("busRequestTimeoutMs", `Int 5000);
      ("invalidationWindowMs", `Int 100);
      ("reconnectMs", `Int 50);
      ("lazyRemoveMs", `Int 50);
      ("lingerMs", `Int 50);
    ]

let resource name =
  `Assoc
    [
      ("@context", `String "https://schema.org/");
      ("@id", `String name);
      ("name", `String name);
      ("representations", `List []);
    ]

let annotation id of_ motivation =
  `Assoc
    [
      ("@context", `String "http://www.w3.org/ns/anno.jsonld");
      ("type", `String "Annotation");
      ("id", var id);
      ("motivation", `String motivation);
      ("target", `Assoc [ ("source", var of_) ]);
      ("created", `String "2026-01-01T00:00:00.000Z");
    ]

(* The subject's annotation as the services first answer with it, and as an
   enriched event says it now is; and the other resource's. *)
let the_annotation = annotation "a1" "r1" "highlighting"
let updated = annotation "a1" "r1" "commenting"
let other = annotation "a2" "r2" "highlighting"

let listed extra =
  `Assoc
    ([
       ("resources", `List []);
       ("total", `Int 0);
       ("offset", `Int 0);
       ("limit", `Int 100);
     ]
    @ extra)

let list = listed []
let found = listed [ ("matchKind", `String "semantic") ]

(* The operations a refresh of the engines asks beside its own: each key
   holder's limits. *)
let limits_operations () =
  List.filter_map
    (fun (op : Tables.Bus_registry.operations_item) ->
      if String.ends_with ~suffix:":limits-requested" op.request then
        Some op.request
      else None)
    (Protocol.tables ()).bus_registry.operations

(* One request a query makes: on the bus, with the answer the suite gives
   it; or a read over HTTP, which the stand-in Archivist answers from what
   [seeded] gave it. *)
type ask =
  | Bus of {
      operation : string;
      payload : Yojson.Safe.t option;
      response : Yojson.Safe.t;
    }
  | Read of { read : string; params : Yojson.Safe.t }

(* One key the client holds: the observer that holds it, what observing it
   asks, and the value it then shows. [of_] is the resource whose key it
   is, for a query kept per resource or per annotation. *)
type held = {
  observer : string;
  query : string;
  of_ : string option;
  observe : (string * Yojson.Safe.t) list;
  asks : ask list;
  seeded : Yojson.Safe.t option;
      (** What the stand-in Archivist is given to answer a read over HTTP with.
      *)
  value : Yojson.Safe.t;
}

let per_resource observer query of_ operation response value =
  {
    observer;
    query;
    of_ = Some of_;
    observe = [ ("query", `String query); ("resource", var of_) ];
    asks =
      [
        Bus
          {
            operation;
            payload = Some (`Assoc [ ("resourceId", var of_) ]);
            response;
          };
      ];
    seeded = None;
    value;
  }

let unkeyed observer query observe asks value =
  { observer; query; of_ = None; observe; asks; seeded = None; value }

let held () =
  let marked annotation_var of_ answer =
    Bus
      {
        operation = "browse:annotation-requested";
        payload =
          Some
            (`Assoc
               [ ("resourceId", var of_); ("annotationId", var annotation_var) ]);
        response =
          `Assoc
            [
              ("annotation", answer);
              ("resource", `Null);
              ("resolvedResource", `Null);
            ];
      }
  in
  let asked operation payload response = Bus { operation; payload; response } in
  [
    per_resource "one" "resource" "r1" "browse:resource-requested"
      (`Assoc [ ("resource", resource "one"); ("lastSequence", `Int 1) ])
      (resource "one");
    per_resource "two" "resource" "r2" "browse:resource-requested"
      (`Assoc [ ("resource", resource "two"); ("lastSequence", `Int 1) ])
      (resource "two");
    {
      observer = "marks";
      query = "annotations";
      of_ = Some "r1";
      observe = [ ("query", `String "annotations"); ("resource", var "r1") ];
      asks =
        [
          Read
            {
              read = "GET /resources/{id}/annotations";
              params = `Assoc [ ("id", var "r1") ];
            };
        ];
      seeded =
        Some
          (`Assoc
             [
               ("backend", `String "annotations");
               ( "with",
                 `Assoc
                   [
                     ("resource", var "r1");
                     ( "annotations",
                       `Assoc
                         [
                           ("annotations", `List [ the_annotation ]);
                           ("total", `Int 1);
                         ] );
                   ] );
             ]);
      value = `List [ the_annotation ];
    };
    {
      observer = "mark";
      query = "annotation";
      of_ = Some "r1";
      observe =
        [
          ("query", `String "annotation");
          ("resource", var "r1");
          ("annotation", var "a1");
        ];
      asks = [ marked "a1" "r1" the_annotation ];
      seeded = None;
      value = the_annotation;
    };
    {
      observer = "otherMark";
      query = "annotation";
      of_ = Some "r2";
      observe =
        [
          ("query", `String "annotation");
          ("resource", var "r2");
          ("annotation", var "a2");
        ];
      asks = [ marked "a2" "r2" other ];
      seeded = None;
      value = other;
    };
    per_resource "history" "events" "r1" "browse:events-requested"
      (`Assoc
         [ ("events", `List []); ("total", `Int 0); ("resourceId", var "r1") ])
      (`List []);
    per_resource "citing" "referencedBy" "r1" "gather:referenced-by-requested"
      (`Assoc [ ("referencedBy", `List []) ])
      (`List []);
    unkeyed "list" "resources"
      [ ("query", `String "resources") ]
      [ asked "browse:resources-requested" (Some (`Assoc [])) list ]
      list;
    unkeyed "found" "matchedResources"
      [ ("query", `String "matchedResources"); ("search", `String "cat") ]
      [
        asked "match:resources-requested"
          (Some (`Assoc [ ("search", `String "cat") ]))
          found;
      ]
      found;
    unkeyed "types" "entityTypes"
      [ ("query", `String "entityTypes") ]
      [
        asked "browse:entity-types-requested" None
          (`Assoc [ ("entityTypes", `List [ `String "Person" ]) ]);
      ]
      (`List [ `String "Person" ]);
    unkeyed "schemas" "tagSchemas"
      [ ("query", `String "tagSchemas") ]
      [
        asked "browse:tag-schemas-requested" None
          (`Assoc [ ("tagSchemas", `List []) ]);
      ]
      (`List []);
    unkeyed "engines" "engines"
      [ ("query", `String "engines") ]
      (asked "browse:engines-requested" None (`Assoc [ ("engines", `List []) ])
      :: List.map
           (fun operation ->
             asked operation None (`Assoc [ ("limits", `List []) ]))
           (limits_operations ()))
      (`List []);
  ]

let step members = `Assoc members

let backend directive with_ =
  step [ ("backend", `String directive); ("with", `Assoc with_) ]

(* Steps that expect [asks], in whatever order the client makes them, and
   answer each on the bus. [tag] keeps their names apart. *)
let asked asks tag =
  let name index = Printf.sprintf "%s%d" tag (index + 1) in
  let fetches =
    List.mapi
      (fun index -> function
        | Read { read; params } ->
            step [ ("fetch", `String read); ("params", params) ]
        | Bus { operation; payload; _ } ->
            step
              ([ ("fetch", `String operation) ]
              @ (match payload with Some p -> [ ("payload", p) ] | None -> [])
              @ [ ("as", `String (name index)) ]))
      asks
  in
  match fetches with
  | [] -> []
  | _ ->
      (match fetches with
      | [ one ] -> one
      | many -> step [ ("fetches", `List many) ])
      :: List.concat
           (List.mapi
              (fun index -> function
                | Read _ -> []
                | Bus { operation; response; _ } ->
                    [
                      backend "emit"
                        [
                          ( "channel",
                            `String (Protocol.operation_for operation).result );
                          ("correlationId", var (name index));
                          ("payload", `Assoc [ ("response", response) ]);
                        ];
                    ])
              asks)

let record ?(extra = []) channel =
  (* On the resource's scope unless the channel is one every client hears. *)
  backend "record"
    ([
       ("resource", var "r1");
       ("channel", `String channel);
       ("sequence", `Int 1);
       ("live", `Bool true);
     ]
    @ (if List.mem channel (Protocol.tables ()).bus_registry.audience.scoped
       then []
       else [ ("unscoped", `Bool true) ])
    @ extra)

(* What a row is for: its channel, or what befalls the stream. *)
let trigger : Refresh.refresh_item -> string = function
  | On { on; _ } -> on
  | Stream { stream; _ } -> Refresh.stream_stream_value stream

let reach_and_refetches : Refresh.refresh_item -> _ = function
  | On { reach; refetches; _ } | Stream { reach; refetches; _ } ->
      (reach, refetches)

(* The keys of [holding] a row's refetch reaches, as the requests that ask
   for them again. *)
let refetched row holding =
  let reach, refetches = reach_and_refetches row in
  List.concat_map
    (fun query ->
      List.concat_map
        (fun key ->
          if
            key.query = query
            && (reach = Some Refresh.Held || key.of_ = None
              || key.of_ = Some "r1")
          then key.asks
          else [])
        holding)
    (Option.value ~default:[] refetches)

(* How each trigger is caused. The steps leave the client having heard it,
   and nothing else. *)
let cause (row : Refresh.refresh_item) all holding =
  match row with
  | Stream { stream = Reopened; _ } ->
      [
        backend "cut" [];
        step [ ("state", `String "reconnecting") ];
        step [ ("state", `String "open") ];
      ]
  | Stream { stream = Unreadable; _ } ->
      (* Both ways an event cannot be read, each its own round: a frame its
         channel's reading refuses (a stored event with no id), then one
         whose payload's annotation id is present and is no AnnotationId,
         which no channel's reading holds it to. *)
      [
        backend "carry"
          [
            ( "frame",
              `Assoc
                [
                  ("channel", `String "mark:removed");
                  ("scope", var "r1");
                  ( "payload",
                    `Assoc
                      [
                        ("type", `String "mark:removed");
                        ("resourceId", var "r1");
                        ("payload", `Assoc [ ("annotationId", var "a1") ]);
                      ] );
                ] );
          ];
      ]
      @ asked (refetched row holding) "refused"
      @ [
          step [ ("quiet", `Int 200) ];
          record "mark:removed"
            ~extra:
              [
                ( "payload",
                  `Assoc [ ("annotationId", `String "not an annotation id") ] );
              ];
        ]
  | On { on = "bus:resume-gap"; _ } ->
      (* A gap with no drop: the scope is let go and taken again, with the
         watermark an event left it, while the record cannot be read. *)
      let added =
        match
          List.find_opt
            (function
              | Refresh.On { on = "mark:added"; _ } -> true | _ -> false)
            all
        with
        | Some added -> added
        | None ->
            failwith
              "causing bus:resume-gap takes a mark:added, which the table has \
               no row for"
      in
      let of_scope = List.filter (fun key -> key.of_ = Some "r1") holding in
      [ record "mark:added" ]
      @ asked (refetched added holding) "watermark"
      @ List.map (fun key -> step [ ("leave", `String key.observer) ]) of_scope
      @ [
          step [ ("scopes", `List [ var "r2" ]) ];
          backend "archivist" [ ("replayFails", `Bool true) ];
        ]
      @ List.concat_map
          (fun key ->
            [
              step
                [
                  ("observe", `Assoc key.observe);
                  ("as", `String (key.observer ^ "Again"));
                ];
              step
                [
                  ("reaches", `String (key.observer ^ "Again"));
                  ( "state",
                    `Assoc [ ("status", `String "ready"); ("value", key.value) ]
                  );
                ];
            ])
          of_scope
  | On { on = "mark:delete-ok"; _ } ->
      [
        step
          [
            ("driver", `String "delete");
            ("with", `Assoc [ ("resource", var "r1"); ("annotation", var "a1") ]);
            ("as", `String "deleted");
          ];
        step
          [
            ("fetch", `String "mark:delete");
            ( "payload",
              `Assoc [ ("annotationId", var "a1"); ("resourceId", var "r1") ] );
            ("as", `String "deletion");
          ];
        backend "emit"
          [
            ("channel", `String "mark:delete-ok");
            ("correlationId", var "deletion");
            ( "payload",
              `Assoc [ ("response", `Assoc [ ("annotationId", var "a1") ]) ] );
          ];
        step [ ("settles", `String "deleted") ];
      ]
  | On { on = "mark:removed" as on; _ } ->
      [
        record on ~extra:[ ("payload", `Assoc [ ("annotationId", var "a1") ]) ];
      ]
  | On { on = "mark:body-updated" as on; when_; _ } ->
      [
        record on
          ~extra:
            ([
               ( "payload",
                 `Assoc [ ("annotationId", var "a1"); ("operations", `List []) ]
               );
             ]
            @
            if when_ = Some Enriched then
              [ ("enriched", `Assoc [ ("annotation", updated) ]) ]
            else []);
      ]
  | On
      {
        on =
          ( "mark:added" | "mark:entity-tag-added" | "mark:entity-tag-removed"
          | "mark:archived" | "mark:unarchived" | "yield:created"
          | "yield:updated" | "yield:cloned" | "yield:moved"
          | "frame:entity-type-added" | "frame:tag-schema-added" ) as on;
        _;
      } ->
      [ record on ]
  | On { on; _ } ->
      failf
        "specs/src/client/refresh.json has a row for %s, which \
         refresh_cases.ml cannot cause"
        on

let query_name : Refresh.queries_item -> string = function
  | Asks { name; _ } | Reads { name; _ } -> name

let query_asks : Refresh.queries_item -> string = function
  | Asks { asks; _ } -> asks
  | Reads { reads; _ } -> reads

let built (row : Refresh.refresh_item) all queries =
  let holding = held () in
  (match
     List.filter
       (fun query ->
         not (List.exists (fun key -> key.query = query_name query) holding))
       queries
   with
  | [] -> ()
  | unheld ->
      failf
        "refresh_cases.ml observes no %s, which specs/src/client/refresh.json \
         names"
        (String.concat ", " (List.map query_name unheld)));
  List.iter
    (fun key ->
      let query = List.find (fun q -> query_name q = key.query) queries in
      let asking =
        match key.asks with
        | Read { read; _ } :: _ -> read
        | Bus { operation; _ } :: _ -> operation
        | [] -> failf "refresh_cases.ml asks nothing for %s" key.query
      in
      if asking <> query_asks query then
        failf "refresh_cases.ml asks %s for %s; the table says %s" asking
          key.query (query_asks query))
    holding;
  let trigger = trigger row in
  let writes, removes, when_, docs =
    match row with
    | On { writes; removes; when_; docs; _ } -> (writes, removes, when_, docs)
    | Stream { docs; _ } -> (None, None, None, docs)
  in
  (* After a gap the keys of the scope are held by the observers that took
     it again. *)
  let holder key =
    if trigger = "bus:resume-gap" && key.of_ = Some "r1" then
      key.observer ^ "Again"
    else key.observer
  in
  let acted queries =
    List.filter
      (fun key ->
        List.mem key.query (Option.value ~default:[] queries)
        && key.of_ <> Some "r2")
      holding
  in
  let written =
    [ ("annotations", `List [ updated ]); ("annotation", updated) ]
  in
  let requests =
    Sdk_case.unique
      (List.concat_map
         (fun key ->
           List.filter_map
             (function
               | Bus { operation; _ } -> Some operation | Read _ -> None)
             key.asks)
         holding
      @ [ "mark:delete" ])
  in
  let reaches key state =
    step [ ("reaches", `String (holder key)); ("state", state) ]
  in
  let both_scopes = step [ ("scopes", `List [ var "r1"; var "r2" ]) ] in
  let steps =
    [
      backend "listen"
        [ ("channels", `List (List.map (fun r -> `String r) requests)) ];
    ]
    @ List.filter_map (fun key -> key.seeded) holding
    @ [
        step
          [
            ("driver", `String "open"); ("with", `Assoc [ ("timing", timing) ]);
          ];
        step [ ("state", `String "open") ];
      ]
    @ List.concat
        (List.mapi
           (fun index key ->
             [
               step
                 [
                   ("observe", `Assoc key.observe); ("as", `String key.observer);
                 ];
             ]
             @ asked key.asks (Printf.sprintf "first%dx" (index + 1))
             @ [
                 step
                   [
                     ("reaches", `String key.observer);
                     ( "state",
                       `Assoc
                         [ ("status", `String "ready"); ("value", key.value) ]
                     );
                   ];
               ])
           holding)
    @ [ both_scopes ] @ cause row all holding
    @ asked (refetched row holding) "again"
    @ List.map
        (fun key ->
          match List.assoc_opt key.query written with
          | Some value ->
              reaches key
                (`Assoc [ ("status", `String "ready"); ("value", value) ])
          | None ->
              failf
                "%s writes %s, which refresh_cases.ml has no written value for"
                trigger key.query)
        (acted writes)
    @ List.map
        (fun key ->
          reaches key
            (`Assoc
               [
                 ("status", `String "failed");
                 ("error", `Assoc [ ("code", `String "bus.not-found") ]);
               ]))
        (acted removes)
    @ [ both_scopes; step [ ("quiet", `Int 400) ] ]
  in
  let when_word = Option.map Refresh.when__value when_ in
  let label =
    match when_word with None -> trigger | Some w -> trigger ^ ", " ^ w
  in
  let name =
    "refresh-"
    ^ String.map (fun c -> if c = ':' then '-' else c) trigger
    ^ match when_word with None -> "" | Some w -> "-" ^ w
  in
  let about =
    label ^ ": "
    ^ String.uncapitalize_ascii (String.sub docs 0 1)
    ^ String.sub docs 1 (String.length docs - 1)
  in
  match
    Corpus.of_json ~root:"." (Protocol.spec ()) ~name
      (`Assoc
         [
           ("about", `String about);
           ("source", `String "specs/src/client/refresh.json");
           ("tier", `String "parity");
           ("steps", `List steps);
         ])
  with
  | Ok case -> case
  | Error refusals ->
      failf "the refresh case %s is no case: %s" name
        (String.concat "; " (List.map Decode.error_to_string refusals))

(* A live case for every row of the refresh table. *)
let cases () =
  let { Refresh.queries; refresh; _ } = (Protocol.tables ()).client_refresh in
  List.map (fun row -> built row refresh queries) refresh
