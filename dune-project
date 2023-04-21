(lang dune 3.7)

(name te)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Žiga Leber")

(maintainers "mail+te@zigaleber.com")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name te)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune stdlib ppx_deriving (alcotest :with-test) fmt js_of_ocaml js_of_ocaml-ppx)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
