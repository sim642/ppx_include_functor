open Int

type t = int


module F (_: sig end) =
struct

end
[%%functor include F]

let show: t -> string = to_string
