open Open_nested
open Outer
open Inner

module F (_: sig end) =
struct

end
[%%functor include F]

let y = x
