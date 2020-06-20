type t =
  { pos: Pos.t;
    s_dis: int;
    d_dis: int;
    x_sgn: int;
    y_sgn: int;
    axis: axis }

and axis = X | Y

val from_origin: pos:Pos.t -> dx:int -> dy:int -> t
val from_points: src:Pos.t -> tgt:Pos.t -> t
val null: src:Pos.t -> t

val source: t -> Pos.t
val target: t -> Pos.t
val length: t -> float
val points: t -> Pos.t list
val is_null: t -> bool
