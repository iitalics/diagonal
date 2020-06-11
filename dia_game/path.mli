type t =
  { pos: int * int;
    axis: axis;
    s_dis: int;
    d_dis: int }

and axis = X | Y

val from_origin: pos:int * int -> dx:int -> dy:int -> t
val from_points: src:int * int -> tgt:int * int -> t
