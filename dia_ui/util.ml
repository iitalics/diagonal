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
