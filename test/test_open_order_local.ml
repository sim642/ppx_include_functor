module Outer =
struct
  module Inner =
  struct
    let x = 42
  end
end

open Outer (* TODO: don't drop Outer for maintaining the open *)
open Inner

module F (_: sig end) =
struct

end
(* [%%functor include F] *) (* TODO: add back *)

let y = x
