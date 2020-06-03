open Js_of_ocaml
open Webtest.Suite

open Indexeddb

module Log = Firebug

let get_factory () =
    let factory : Idb_js_api.factory Js.t Js.Optdef.t =
        (Js.Unsafe.coerce Dom_html.window)##.indexedDB
    in
    Js.Optdef.get factory
        (fun () -> failwith "IndexedDB not available")

let db_name = Js.string "test-db"

let test_fail () = assert_true false
let test_success () = assert_true true

let test_createObjectStore_withOptions wrapper =
    let store_name = Js.string "test-store"
    and keyPath = Js.string "x"
    and factory = get_factory () in
    let request = factory##_open db_name 2 in

    request##.onupgradeneeded := Dom.handler (fun _ ->
        let opts = Idb_js_api.createObjectStoreOptions () in
        opts##.keyPath := keyPath;
        request##.result##createObjectStore_withOptions store_name opts
        |> ignore;
        Js._true);

    request##.onerror := Dom.handler (fun _ -> wrapper test_fail; Js._true);

    request##.onsuccess := Dom.handler (fun _ ->
        let trans =
            request##.result##transaction
                (Js.array [| store_name |])
                (Js.string "readonly")
        in
        let store = trans##objectStore store_name in
        wrapper (fun () ->
            assert_equal store##.keyPath keyPath;
            assert_true (not (Js.to_bool store##.autoIncrement)));
        Js._true)

let test_put_object wrapper =
    let store_name = Js.string "test-store"
    and factory = get_factory () in
    let request = factory##_open db_name 2 in

    request##.onerror := Dom.handler (fun _ -> wrapper test_fail; Js._true);

    request##.onsuccess := Dom.handler (fun _ ->
        let trans =
            request##.result##transaction
                (Js.array [| store_name |])
                (Js.string "readwrite")
        in
        let store = trans##objectStore store_name in
        let key = Js.string "hola" in
        let obj = Js.Unsafe.obj [| "x", Js.Unsafe.inject key |] in
        let put_request = store##put_object obj in

        put_request##.onerror := Dom.handler (fun _ ->
            wrapper test_fail; Js._true);

        put_request##.onsuccess := Dom.handler (fun _ ->
            Js.Optdef.case
                put_request##.result
                (fun () -> wrapper test_fail)
                (fun k -> wrapper (fun () -> assert_equal k key));
            Js._true);
        Js._true)

let test_keyRange_constr () =
    let lower = Js.string "a"
    and upper = Js.string "z" in
    let range = Idb_js_api.keyRange##bound lower upper in
    assert_equal range##.lower lower;
    assert_equal range##.upper upper;
    assert_true (Js.to_bool (range##includes (Js.string "p")));
    assert_true (not (Js.to_bool (range##includes (Js.string "?"))))

let js_api_tests =
    "js_api" >::: [
        (* "test_fail" >:: test_fail; *)
        (* "test_success" >:: test_success; *)
        "test_createObjectStore_withOptions" >:~
            test_createObjectStore_withOptions;
        "test_put_object" >:~ test_put_object;
        "test_keyRange_constr" >:: test_keyRange_constr;
    ]