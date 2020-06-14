type t =
  { pos: int * int;
    dir: cardinal;
    rev: revolution;
    s_dis: int;
    d_dis: int }

and cardinal = S | N | E | W
and revolution = CW | CCW
and axis = X | Y

val from_origin: pos:int * int -> dx:int -> dy:int -> t
val from_points: src:int * int -> tgt:int * int -> t
val null: src:int * int -> t

val source: t -> int * int
val target: t -> int * int
val length: t -> float
val is_null: t -> bool

val string_of_cardinal: cardinal -> string
val string_of_revolution: revolution -> string
val cardinal_axis: cardinal -> axis
val cardinal_sign: cardinal -> int
val revolution_sign: dir:cardinal -> revolution -> int
