# ppx_include_functor

[![ci workflow status](https://github.com/sim642/ppx_include_functor/actions/workflows/ci.yml/badge.svg)](https://github.com/sim642/ppx_include_functor/actions/workflows/ci.yml)
[![GitHub release status](https://img.shields.io/github/v/release/sim642/ppx_include_functor)](https://github.com/sim642/ppx_include_functor/releases)
[![opam package status](https://badgen.net/opam/v/ppx_include_functor)](https://opam.ocaml.org/packages/ppx_include_functor)

Transformation for `include functor` in standard OCaml.
_Attempts to_ imitate [OxCaml `include functor`][oxcaml-include-functor].

[rfc-include-functor]: https://github.com/ocaml/RFCs/pull/43
[oxcaml-include-functor]: https://oxcaml.org/documentation/miscellaneous-extensions/include-functor/


## Installation
```console
opam pin git+https://github.com/sim642/ppx_include_functor.git
```

## Usage
In dune:
```
(preprocess (pps ppx_include_functor))
```

## Syntax
Use `[%%functor include Comparable.Make]` as a module item instead of `include functor Comparable.Make`.

For example,
```ocaml
module M = struct
  type t = ...
  [@@deriving compare, sexp]

  [%%functor include Comparable.Make]
end
```
is transformed (roughly) into
```ocaml
module M = struct
  module T = struct
    type t = ...
    [@@deriving compare, sexp]
  end

  include T
  include Comparable.Make(T)
end
```

### Caveats
Unlike in [OxCaml][oxcaml-include-functor]:
- Auxiliary module `Include_functor_*` (like `T`) is exposed.
- Generative functors are not supported.
- Not supported in module signatures.
