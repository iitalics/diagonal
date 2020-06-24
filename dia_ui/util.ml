module Applicative(A: Intf.Applicative_S) =
  struct
    let ( <*> ) af ax = A.map2 (@@) af ax
    let ( >>| ) ax f = A.map f ax
    let both ax ay = A.map2 (fun x y -> x, y) ax ay

    let rec all = function
      | []      -> A.const []
      | x :: xs -> A.map2 List.cons x (all xs)
  end

module Option = struct
  module A0 = struct
    type 'a t = 'a option
    let const x = Some(x)
    let map = Option.map
    let map2 f ox oy = match ox with
      | None -> None
      | Some(x) -> match oy with
                   | None -> None
                   | Some(y) -> Some(f x y)
  end
  module A1 = Applicative(A0)
  include Option
  include A0
  include A1
end

module List = struct
  let rev_split xys =
    let rec loop xs ys = function
      | (x, y) :: xys ->
         loop (x :: xs) (y :: ys) xys
      | [] ->
         (xs, ys)
    in
    loop [] [] xys

  include List
end

let clamp lo hi x =
  if      x < lo then lo
  else if x > hi then hi
  else                x

let int_lerp t x0 x1 =
  x0 + int_of_float (float_of_int (x1 - x0) *. t)

let aabb_fill_vertices (x0, y0, x1, y1) =
  [| x0; x1; x1; x0 |], [| y0; y0; y1; y1 |]

let aabb_strip_vertices (x0, y0, x1, y1) =
  [| x0; x1; x1; x0; x0 |], [| y0; y0; y1; y1; y0 |]
