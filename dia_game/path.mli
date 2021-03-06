type t =
  { pos: Pos.t;
    s_dis: int;
    d_dis: int;
    x_sgn: int;
    y_sgn: int;
    axis: axis }

and axis = X | Y

type point_type =
  | Crit
  | Attk
  | Vuln

val from_origin: pos:Pos.t -> dx:int -> dy:int -> t
val from_points: src:Pos.t -> tgt:Pos.t -> t
val null: src:Pos.t -> t

val source: t -> Pos.t
val target: t -> Pos.t
val length: t -> float
val is_null: t -> bool

val num_points: t -> int
val points: t -> (Pos.t * point_type) list
val points_mem: Pos.t -> t -> bool
val knee: t -> Pos.t option
val straight: t -> Pos.t list
val diagonal: t -> Pos.t list
