module M0 = struct

  type t = int * int

  let compare (x0, y0) (x1, y1) =
    if x0 = x1 then y0 - y1 else x0 - x1

  let hash (x, y) =
    x * Rules.grid_cols + y

  let equal : t -> t -> bool =
    fun (x0, y0) (x1, y1) ->
    x0 = x1 && y0 = y1

end

include M0

module Tbl = Hashtbl.Make(M0)
