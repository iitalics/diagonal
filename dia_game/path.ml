type t =
  { pos: int * int;
    dir: cardinal;
    rev: revolution;
    s_dis: int;
    d_dis: int }

and cardinal = S | N | E | W
and revolution = CW | CCW
and axis = X | Y

let sgn x = if x < 0 then -1 else 1

let from_origin ~pos ~dx ~dy =
  let lx, ly = abs dx, abs dy in
  let dir, rev =
    if lx > ly then
      (if dx < 0 then W else E),
      (if sgn dx = sgn dy then CW else CCW)
    else
      (if dy < 0 then N else S),
      (if sgn dx = sgn dy then CCW else CW)
  in
  { pos; dir; rev;
    s_dis = abs (lx - ly);
    d_dis = min lx ly }

let[@ocaml.inline] from_points ~src:(x0, y0) ~tgt:(x1, y1) =
  from_origin
    ~pos:(x0, y0)
    ~dx:(x1 - x0)
    ~dy:(y1 - y0)

let string_of_cardinal = function
  | S -> "S" | N -> "N" | E -> "E" | W -> "W" [@@ocaml.inline]

let string_of_revolution = function
  | CW -> "CW" | CCW -> "CCW" [@@ocaml.inline]

let cardinal_axis = function
  | S | N -> Y
  | E | W -> X [@@ocaml.inline]

let cardinal_sign = function
  | S | E -> +1
  | N | W -> -1 [@@ocaml.inline]

let revolution_sign ~dir rev = match dir with
  | N | E -> (match rev with CW -> +1 | CCW -> -1)
  | S | W -> (match rev with CW -> -1 | CCW -> +1)

let source { pos; _ } =
  pos

let target { pos; dir; rev; s_dis; d_dis } =
  let s_sgn = dir |> cardinal_sign in
  let d_sgn = rev |> revolution_sign ~dir in
  let (x0, y0) = pos in
  let (s, d1, d2) = (s_dis * s_sgn, d_dis * s_sgn, d_dis * d_sgn) in
  match dir |> cardinal_axis with
  | X -> x0 + s + d1, y0 + d2
  | Y -> x0 + d2, y0 + s + d1

let sqrt_2 = 1.4142135623730951

let length { s_dis; d_dis; _ } =
  float_of_int s_dis
  +. float_of_int d_dis *. sqrt_2

let[@ocaml.inline] null ~src =
  from_origin ~pos:src ~dx:0 ~dy:0

let is_null { s_dis; d_dis; _ } =
  s_dis = 0 && d_dis = 0
