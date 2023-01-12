(lang dune 3.5)

(name maybe)

(generate_opam_files true)

(source
 (github accorrenson/Maybe))

(authors "Arthur Correnson")

(maintainers "Arthur Correnson")

(license LICENSE)

(documentation https://github.com/acorrenson/Maybe)

(package
 (name maybe)
 (synopsis "A mini probabilistic language")
 (description
  "Maybe is a minimalist functionnal probabilist programming language")
 (depends ocaml dune)
 (tags
  (topics "language probabilist" your project)))
