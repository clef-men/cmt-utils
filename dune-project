(lang dune 3.0)

(name cmt-utils)

(generate_opam_files)

(authors
  "Clément Allain"
)
(maintainers
  "clement.allain@inria.fr"
)

(source (github clef-men/cmt-utils))

(package
  (name cmt-utils)
  (synopsis "Utilities for .cmt files")
  (depends
    ocaml-compiler-libs
    fmt
    cmdliner
  )
)
