type t =
  { start: int * int;
    axis: axis;
    s_dis: int;
    d_dis: int }

and axis = X | Y

let make ~pos0 ~pos1 =
  ignore (pos0, pos1);
  failwith "Path.make: not implemented"
