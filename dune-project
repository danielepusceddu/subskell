(lang dune 3.5)

(name subskell)

(generate_opam_files true)

(source
 (github danielepusceddu/subskell))

(authors "Daniele Pusceddu")

(license LICENSE)

(package
 (name subskell)
 (synopsis "lip22 project")
 (description "Implementation of an infinitesimal subset of the haskell language")
 (depends ocaml dune)
)

(using menhir 2.1)

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
