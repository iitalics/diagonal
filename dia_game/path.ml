type t =
  { pos: int * int;
    axis: axis;
    s_dis: int;
    d_dis: int }

and axis = X | Y

let sgn x = if x < 0 then -1 else 1

let from_origin ~pos ~dx ~dy =
  let axis, s_dis, d_dis =
    if abs dx > abs dy then
      X, dx - sgn dx * abs dy, dy
    else
      Y, dy - sgn dy * abs dx, dx
  in
  { pos; axis; s_dis; d_dis }

let[@ocaml.inline] from_points ~src ~tgt =
  let (x0, y0), (x1, y1) = src, tgt in
  from_origin
    ~pos:src
    ~dx:(x1 - x0)
    ~dy:(y1 - y0)
