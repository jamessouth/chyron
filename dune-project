(lang dune 3.22)

(name chyron)

(generate_opam_files true)

(source
 (github jamessouth/chyron))

(authors "James D. South")

(maintainers "James D. South")

(license MIT)

(documentation https://github.com/jamessouth/chyron/blob/master/README.md)

(package
 (name chyron)
 (synopsis "a text scroller/split-flap/chyron")
 (description "a text scroller/split-flap/chyron")
 (depends
  ocaml
  dune
  core
  ppx_jane
  core_unix
  ppx_inline_test
  ppx_deriving
  ppx_assert
  qcheck-core
  uuseg)
 (tags ("scroll split-flap chyron")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
; (depends ocaml dune base core ppx_jane core_unix ocaml-lsp-server ocamlformat odoc ppx_inline_test utop ppx_deriving ppx_assert qcheck-core)
