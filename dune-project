(lang dune 2.5)
(name indexeddb)

(generate_opam_files true)
(authors "Thomas Leonard <talex5@gmail.com>, Vasilis Papavasileiou")
(maintainers "Ocsigen Team <dev@ocsigen.org>, Boris D. <borisd@gmx.com>")
(source (github ocsigen/ocaml-indexeddb))
(license ISC)

(package
  (name indexeddb)
  (synopsis "OCaml bindings for IndexedDB")
  (depends
    (lwt (>= 5.2))
    (js_of_ocaml (> 3.4.0))
    (js_of_ocaml-lwt (> 3.4.0))))