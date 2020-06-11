type t =
  { start: int * int;
    axis: axis;
    s_dis: int;
    d_dis: int }

and axis = X | Y

val make: pos0:int * int -> pos1:int * int -> t
