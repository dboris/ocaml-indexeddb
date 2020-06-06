(lang dune 2.5)
(name indexeddb)

(generate_opam_files true)
(authors "Thomas Leonard <talex5@gmail.com>, Vasilis Papavasileiou, Boris D. <borisd@gmx.com>")
(maintainers "Ocsigen Team <dev@ocsigen.org>")
(source (github ocsigen/ocaml-indexeddb))
(license ISC)

(package
  (name indexeddb)
  (synopsis "OCaml bindings for IndexedDB")
  (depends
    (ocaml (>= 4.08.0))
    (dune (and :build (>= 2.5)))
    (webtest (and :with-test (>= 2.0)))
    (webtest-js (and :with-test (>= 2.0)))
    (lwt (>= 5.2))
    (js_of_ocaml (> 3.4.0))
    (js_of_ocaml-lwt (> 3.4.0))))