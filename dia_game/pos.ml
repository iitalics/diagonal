type t = int * int

let compare (x0, y0) (x1, y1) =
  if x0 = x1 then y0 - y1 else x0 - x1
