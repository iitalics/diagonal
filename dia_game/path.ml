type t =
  { pos: Pos.t;
    s_dis: int;
    d_dis: int;
    x_sgn: int;
    y_sgn: int;
    axis: axis }

and axis = X | Y

(* helpers *)

let sqrt_2 = 1.4142135623730951

let sgn x = if x < 0 then -1 else +1

(* accessors *)

let source p = p.pos

let target { pos = (x0, y0); s_dis; d_dis; x_sgn; y_sgn; axis } =
  match axis with
  | X -> x0 + x_sgn * (d_dis + s_dis), y0 + y_sgn * d_dis
  | Y -> x0 + x_sgn * d_dis,           y0 + y_sgn * (d_dis + s_dis)

let length { s_dis; d_dis; _ } =
  float_of_int s_dis
  +. float_of_int d_dis *. sqrt_2

let is_null { s_dis; d_dis; _ } =
  s_dis = 0 && d_dis = 0

(* constructors *)

let from_origin ~pos ~dx ~dy =
  let x_sgn, y_sgn = sgn dx, sgn dy in
  let x_len, y_len = abs dx, abs dy in
  let axis, s_dis, d_dis =
    if x_len > y_len then
      X, x_len - y_len, y_len
    else
      Y, y_len - x_len, x_len
  in
  { pos; s_dis; d_dis; x_sgn; y_sgn; axis }

let[@ocaml.inline] from_points ~src:(x0, y0) ~tgt:(x1, y1) =
  from_origin
    ~pos:(x0, y0)
    ~dx:(x1 - x0)
    ~dy:(y1 - y0)

let[@ocaml.inline] null ~src =
  from_origin ~pos:src ~dx:0 ~dy:0
