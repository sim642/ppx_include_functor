module F (X: sig val x: int end) =
struct
  let y = X.x
end

let x = 42
[%%functor include F]
