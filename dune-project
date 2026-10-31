(lang dune 3.0)
(name ppx_include_functor)
(generate_opam_files true)
(source (github sim642/ppx_include_functor))
(authors "Simmo Saan <simmo.saan@gmail.com>")
(maintainers "Simmo Saan <simmo.saan@gmail.com>")
(license MIT)

(package
 (name ppx_include_functor)
 (synopsis "include functor as PPX")
 (description "Transformation for include functor in standard OCaml. Attempts to imitate OxCaml include functor.")
 (depends
  ppxlib))
