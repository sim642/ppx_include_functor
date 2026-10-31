(* From https://github.com/OlivierNicole/ocaml/blob/011df8e6202161dbdbea4d4efaac4eb082177dd7/testsuite/tests/typing-modules/include_functor.ml.
   With expects and errors removed. *)

(* Test 1: Basic usage in structs *)
module type S = sig
  type t
  val x : t
end

module F1 (X : S) = struct
  let y = X.x
end

module M1 = struct
  type t = int
  let x = 5

  [%%functor include F1]
end

let () = assert Int.(equal M1.y 5);;

(* Test 5: Include functor in signature *)
module type T = sig
  type s
  val f : s -> bool
end

module type F5 = functor (X : S) -> T with type s = X.t

module type M5_sig = sig
  type t
  val x : t

  include T with type s = t
end

module M5_impl : M5_sig = struct
  type t = int
  type s = t

  let x = 5
  let f s = x = s
end
let () = assert (M5_impl.f M5_impl.x);;

(* Test 6: Nested module names work *)
module type Eq9 = sig
  type t
  val z : t
  val equal : t -> t -> bool
end

module type S9 = sig
  module Foo : Eq9
end

module F9 (X : S9) = struct
  let eq_z = X.Foo.equal X.Foo.z
end

module Int9 : sig
  type t
  val equal : t -> t -> bool
  val of_int : int -> t
end = struct
  include Int
  let of_int t = t
end

module M9 = struct
  module Foo : Eq9 = struct
    include Int9
    let z = of_int 7
  end
  [%%functor include F9]
end

let () = assert (M9.eq_z M9.Foo.z);;

module M9' = struct
  module Foo = struct
    include Int9
    let z = of_int 6
  end
  [%%functor include F9]
end

let () = assert (not (M9'.eq_z (M9'.Foo.of_int 5)))
let () = assert (M9'.eq_z (M9'.Foo.of_int 6));;


(* Test 8: Include functor should work at the toplevel (and check shadowing). *)
type t = int
let x : t = 3
let x : t = 5
[%%functor include F1]

let () = assert (Int.(equal y 5));;

(* Test 9: Check that things get marked used appropriately when they are
   used by include functor.  (And that we're getting the warnings we expect
   to see if they weren't). *)
module M12_1 : sig val y : int list end = struct
  module Bar = struct
    type t = int
    let x = 5
  end

  module F (G :
    sig
      module T_sub : sig type t val x : t end
                  -> sig type t val x : t end
    end) = struct
    module Foo = G.T_sub(Bar)
    let y = Foo.x
  end

  module T_sub (X : sig type t val x : t end) = struct
    type t = X.t list
    let x = [X.x]
  end
  [%%functor include F]
end;;

module M12_2 : sig val y : int list end = struct
  module Bar = struct
    type t = int
    let x = 5
    let q = 42
  end

  module F (G :
    sig
      module T_sub : sig type t val x : t end
                  -> sig type t val x : t end
    end) = struct
    module Foo = G.T_sub(Bar)
    let y = Foo.x
  end

  module T_sub (X : sig type t val x : t end) = struct
    type t = X.t list
    let x = [X.x]
    let z = "something"
  end
  [%%functor include F]
end;;


(* Test 11: Make sure we're extracting functor return types appropriately *)
module type S15 = sig val x : int end
module type S15' = S15

module F15 (X : sig end) : S15' =
struct
  let x = 42
end

[%%functor include F15]


(* Test 14: Effects happen when they should *)
let r19 = ref 0

module F19 (X : sig val x : int end) = struct
  let () = r19 := X.x
end

let () = assert (Int.equal 0 !r19)

module M19 = struct
  let x = 42
  let () = assert (Int.equal 0 !r19)

  [%%functor include F19]

  let () = assert (Int.equal 42 !r19)
end

let () = assert (Int.equal 42 !r19);;


(* Test 15: Shadowed types *)
module I20 = struct
  type t = int
end

module F20 (M : sig
    type t = string
  end) =
struct
  let go (arg : M.t) = print_endline arg
end

module M20 = struct
  include I20

  type t = string

  [%%functor include F20]
end

(* Test 16: Check that scraping of result type happens in environment expanded
   with parameter type. *)
module M21 = struct
  module F (_ : sig end) = struct
    module type S = sig end
  end

  module P = struct
    module Make (M : sig end) : F(M).S = struct end
  end

  [%%functor include P.Make]
end;;
