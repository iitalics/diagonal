type t =
  { pos: int * int;
    s_dis: int;
    d_dis: int;
    x_sgn: int;
    y_sgn: int;
    axis: axis }

and axis = X | Y

val from_origin: pos:int * int -> dx:int -> dy:int -> t
val from_points: src:int * int -> tgt:int * int -> t
val null: src:int * int -> t

val source: t -> int * int
val target: t -> int * int
val length: t -> float
val is_null: t -> bool
