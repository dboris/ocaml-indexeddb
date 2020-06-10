(* Copyright (C) 2015, Thomas Leonard. See the LICENSE file for
   details. *)

open Lwt
open Js_of_ocaml
module Lwt_js = Js_of_ocaml_lwt.Lwt_js

exception AbortError

type db = Idb.database Js.t

type db_upgrader = Idb.database Js.t

type db_name = Js.js_string Js.t

let db_name = Js.string

type store_name = Js.js_string Js.t

let store_name = Js.string

let create_store (db : db) name =
  ignore (db##createObjectStore name)

let close db =
  db##close

let get_factory () =
  let factory : Idb.factory Js.t Js.Optdef.t =
    (Js.Unsafe.coerce Dom_html.window)##.indexedDB
  in
  Js.Optdef.get factory
    (fun () -> failwith "IndexedDB not available")

let delete_database db_name =
  let factory = get_factory () in
  let request = factory##deleteDatabase db_name in
  let t, set_t = Lwt.wait () in
  request##.onerror := Dom.handler (fun _event ->
      Lwt.wakeup_exn set_t
        (Failure "Error trying to delete IndexedDB database");
      Js._true
    );
  request##.onsuccess := Dom.handler (fun _event ->
      Lwt.wakeup set_t ();
      Js._true
    );
  t

let idb_error typ
    (event:Idb.request Idb.errorEvent Js.t) =
  let failure msg =
    Failure
      (Printf.sprintf "IndexedDB operation (%s) failed: %s" typ msg)
  in
  Js.Opt.case event##.target
    (fun () -> failure "(missing target on error event)")
    (fun target ->
       Js.Opt.case target##.error
         (fun () -> failure "(missing error on request)")
         (fun error ->
            let name =
              Js.Optdef.case error##.name
                (fun () -> "(no name)")
                Js.to_string
            in
            let message =
              Js.Optdef.case error##.message
                (fun () -> "(no message)")
                Js.to_string
            in
            let code = Js.Optdef.get error##.code (fun () -> 0) in
            if name = "AbortError" then
              AbortError
            else
              failure
                (Printf.sprintf "%s: %s (error code %d)"
                   name message code)))

let make db_name ~version ~init =
  let factory = get_factory () in
  let request = factory##_open db_name version in
  let t, set_t = Lwt.wait () in
  request##.onblocked := Dom.handler (fun _event ->
      print_endline
        "Waiting for other IndexedDB users to close \
         their connections before upgrading schema version.";
      Js._true
    );
  request##.onupgradeneeded := Dom.handler (fun event ->
      try
        let old_version = event##.oldVersion in
        init ~old_version request##.result;
        Js._true
      with ex ->
        (* Firefox throws the exception away and returns AbortError
           instead, so save it here. *)
        Lwt.wakeup_exn set_t ex;
        raise ex
    );
  request##.onerror := Dom.handler (fun event ->
      begin match Lwt.state t, idb_error "open" event with
        | Lwt.Fail _, AbortError ->
          () (* Already reported a better exception *)
        | _, ex ->
          Lwt.wakeup_exn set_t ex
      end;
      Js._true
    );
  request##.onsuccess := Dom.handler (fun _event ->
      Lwt.wakeup set_t request##.result;
      Js._true
    );
  t

module Unsafe = struct

  type 'a store = {
    db : db;
    store_name : store_name;

    (* We reuse transactions where possible for performance.

       This does mean that if any read fails then the others will
       hang, but we treat any read failing as a fatal error anyway. *)
    mutable ro_trans :
      ('a Idb.transaction Js.t * (exn -> unit) list ref) option;
  }

  type key = Js.js_string Js.t

  type 'a content = 'a Js.t

  type key_range = Idb.keyRange Js.t

  let key_range_bound ?lower_open ?upper_open lower upper =
    let lower_open = Option.map Js.bool lower_open
    and upper_open = Option.map Js.bool upper_open
    in
    match lower_open, upper_open with
    | Some lo, Some uo ->
      Idb.keyRange##bound_lowerAndUpperOpen lower upper lo uo
    | Some lo, None ->
      Idb.keyRange##bound_lowerOpen lower upper lo
    | None, Some uo ->
      Idb.keyRange##bound_lowerAndUpperOpen lower upper Js._false uo
    | None, None ->
      Idb.keyRange##bound lower upper

  let key_range_lower_bound ?_open lower =
    match _open with
    | Some _open ->
      Idb.keyRange##lowerBound_open lower (Js.bool _open)
    | None ->
      Idb.keyRange##lowerBound lower

  let key_range_upper_bound ?_open upper =
    match _open with
    | Some _open ->
      Idb.keyRange##upperBound_open upper (Js.bool _open)
    | None ->
      Idb.keyRange##upperBound upper

  let key_range_only key =
    Idb.keyRange##only key

  let store db store_name = { db ; store_name ; ro_trans = None }

  let rec trans_ro (t : _ store) setup =
    let r, set_r = Lwt.wait () in
    match t.ro_trans with
    | None ->
      let breakers = ref [Lwt.wakeup_exn set_r] in
      let trans =
        t.db##transaction
          (Js.array [| t.store_name |])
          (Js.string "readonly") in
      t.ro_trans <- Some (trans, breakers);
      trans##.onerror := Dom.handler (fun event ->
          t.ro_trans <- None;
          let ex = idb_error "RO" event in
          if ex = AbortError then
            print_endline
              "IndexedDB transaction failed (Safari bug?): will retry";
          !breakers |> List.iter (fun b -> b ex);
          Js._true
        );
      trans##.oncomplete := Dom.handler (fun _event ->
          t.ro_trans <- None;
          Js._true
        );
      setup (trans##objectStore (t.store_name)) set_r;
      r
    | Some (trans, breakers) ->
      (* Seems we can get here when a transaction is done but
         oncomplete hasn't been called, so retry if we get an
         error. *)
      try
        setup (trans##objectStore (t.store_name)) set_r;
        breakers := Lwt.wakeup_exn set_r :: !breakers;
        r
      with _ex ->
        t.ro_trans <- None;
        trans_ro t setup

  (* On Safari, transactions can fail unpredictably, so wrap
     [trans_ro] with auto-retry. See:

     https://github.com/talex5/cuekeeper/issues/9 *)
  let trans_ro t setup =
    let rec retry delay =
      Lwt.catch
        (fun () -> trans_ro t setup)
        (function
          | AbortError ->
            Lwt_js.sleep (Random.float delay) >>= fun () ->
            retry (delay *. 1.2)
          | ex ->
            Lwt.fail ex)
    in
    retry 1.0

  let trans_rw t setup =
    let r, set_r = Lwt.wait () in
    let trans =
      t.db##transaction
        (Js.array [| t.store_name |])
        (Js.string "readwrite")
    in
    trans##.onerror := Dom.handler (fun event ->
        Lwt.wakeup_exn set_r (idb_error "RW" event);
        Js._true
      );
    trans##.oncomplete := Dom.handler (fun _event ->
        Lwt.wakeup set_r ();
        Js._true
      );
    setup (trans##objectStore (t.store_name));
    r

  let set t key value =
    trans_rw t @@ fun store ->
    ignore (store##put value key)

  let get t key =
    trans_ro t @@ fun store set_r ->
    let request = store##get key in
    request##.onsuccess :=
      Dom.handler @@ fun _event ->
      Js.Optdef.to_option request##.result
      |> Lwt.wakeup set_r;
      Js._true

  let get_all ?query ?count t =
    trans_ro t @@ fun store set_r ->
    let request =
      match query, count with
      | Some query, Some count ->
        store##getAll_queryAndCount (Js.Opt.return query) count
      | Some query, None ->
        store##getAll_query query
      | None, Some count ->
        store##getAll_queryAndCount Js.Opt.empty count
      | None, None ->
        store##getAll
    in
    request##.onsuccess :=
      Dom.handler @@ fun _event ->
      Js.Optdef.case request##.result
        (fun () -> [])
        (fun result -> result |> Js.to_array |> Array.to_list)
      |> Lwt.wakeup set_r;
      Js._true

  let bulk_get t keys =
    trans_ro t @@ fun store set_r ->
    let results = ref []
    and count = List.length keys
    in
    let get i key =
      let request = store##get key in
      request##.onsuccess :=
        Dom.handler @@ fun _event ->
          Js.Optdef.to_option request##.result :: !results
          |> if Int.equal (i + 1) count then Lwt.wakeup set_r else (:=) results;
        Js._true
    in
    List.iteri get keys

  let remove t key =
    trans_rw t @@ fun store ->
    ignore (store##delete key)

  let compare_and_set t key ~test ~new_value =
    let result = ref None in
    (trans_rw t @@ fun store ->
     let request = store##get key in
     request##.onsuccess :=
       Dom.handler (fun _event ->
           if
             request##.result
             |> Js.Optdef.to_option
             |> test
           then (
             begin
               ignore @@ match new_value with
               | None ->
                 store##delete key
               | Some new_value ->
                 store##put new_value key
             end;
             result := Some true
           ) else (
             result := Some false
           );
           Js._true))
    >|= fun () ->
    match !result with
    | None -> failwith "Transaction completed, but no result!"
    | Some x -> x

  let fold_impl cursor_direction ?query ?unique f acc t =
    let acc = ref acc in
    trans_ro t @@ fun store set_r ->
    let request =
      match query, unique with
      | Some query, Some unique ->
        store##openCursor_queryAndDirection
          (Js.Opt.return query)
          (cursor_direction unique)
      | Some query, None ->
        store##openCursor_query query
      | None, Some unique ->
        store##openCursor_queryAndDirection
          Js.Opt.empty
          (cursor_direction unique)
      | None, None ->
        store##openCursor
    in
    request##.onsuccess :=
      Dom.handler @@ fun _event ->
      Js.Opt.case request##.result
        (fun () -> Lwt.wakeup set_r !acc)
        (fun cursor ->
           let key = cursor##.key
           and value = cursor##.value in
           acc := f !acc key value;
           cursor##continue);
      Js._true

  let fold ?query ?unique f acc t =
    let cursor_direction unique =
      Js.string (if unique then "nextunique" else "next")
    in
    fold_impl cursor_direction ?query ?unique f acc t

  let fold_right ?query ?unique f acc t =
    let cursor_direction unique =
      Js.string (if unique then "prevunique" else "prev")
    in
    fold_impl cursor_direction ?query ?unique f acc t

  let bindings t =
    let acc = []
    and f acc key value = (key, value) :: acc in
    fold f acc t

end

module Make (C : Idb_sigs.Js_string_conv) = struct

  type store = Js.js_string Unsafe.store

  let store = Unsafe.store

  type key = C.key

  type content = C.content

  type key_range = Unsafe.key_range

  let key_range_bound ?lower_open ?upper_open lower upper =
    Unsafe.key_range_bound
      ?lower_open
      ?upper_open
      (C.of_key lower)
      (C.of_key upper)

  let key_range_lower_bound ?_open lower =
    Unsafe.key_range_lower_bound ?_open (C.of_key lower)

  let key_range_upper_bound ?_open upper =
    Unsafe.key_range_upper_bound ?_open (C.of_key upper)

  let key_range_only key =
    Unsafe.key_range_only (C.of_key key)

  let set store key content =
    Unsafe.set store (C.of_key key) (C.of_content content)

  let get store key =
    C.of_key key
    |> Unsafe.get store
    |> Lwt.map (Option.map C.to_content)

  let get_all ?query ?count store =
    Unsafe.get_all ?query ?count store
    |> Lwt.map (List.map C.to_content)

  let bulk_get store keys =
    List.map C.of_key keys
    |> Unsafe.bulk_get store
    |> Lwt.map (List.map (Option.map C.to_content))

  let remove store key =
    Unsafe.remove store (C.of_key key)

  let compare_and_set store key ~test ~new_value =
    let key = C.of_key key
    and test v = test (Option.map C.to_content v)
    and new_value = Option.map C.of_content new_value in
    Unsafe.compare_and_set store key ~test ~new_value

  let fold ?query ?unique f acc store =
    let f acc key content = f acc (C.to_key key) (C.to_content content) in
    Unsafe.fold ?query ?unique f acc store

  let fold_right ?query ?unique f acc store =
    let f acc key content = f acc (C.to_key key) (C.to_content content) in
    Unsafe.fold ?query ?unique f acc store

  let bindings t =
    let acc = []
    and f acc key value = (key, value) :: acc in
    fold f acc t

end

module Json = struct

  type 'a store = Js.js_string Unsafe.store

  let store = Unsafe.store

  type key = Unsafe.key

  type 'a content = 'a

  type key_range = Unsafe.key_range

  let key_range_bound = Unsafe.key_range_bound
  let key_range_lower_bound = Unsafe.key_range_lower_bound
  let key_range_upper_bound = Unsafe.key_range_upper_bound
  let key_range_only = Unsafe.key_range_only

  let set store key content =
    Unsafe.set store key (Json.output content)

  let get store key =
    Lwt.map
      (Option.map Json.unsafe_input)
      (Unsafe.get store key)

  let get_all ?query ?count store =
    Lwt.map
      (List.map Json.unsafe_input)
      (Unsafe.get_all ?query ?count store)

  let bulk_get store keys =
    Lwt.map
      (List.map (Option.map  Json.unsafe_input))
      (Unsafe.bulk_get store keys)

  let remove = Unsafe.remove

  let compare_and_set store key ~test ~new_value =
    let test v = test (Option.map Json.unsafe_input v)
    and new_value = Option.map Json.output new_value in
    Unsafe.compare_and_set store key ~test ~new_value

  let fold ?query ?unique f acc store =
    let f acc key content = f acc key (Json.unsafe_input content) in
    Unsafe.fold ?query ?unique f acc store

  let fold_right ?query ?unique f acc store =
    let f acc key content = f acc key (Json.unsafe_input content) in
    Unsafe.fold ?query ?unique f acc store

  let bindings t =
    let acc = []
    and f acc key value = (key, value) :: acc in
    fold f acc t

end
