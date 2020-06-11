type t =
  int * int

let make () = (0, 0)

let move dx dy (x, y) =
  (max 0 @@ min 7 @@ x + dx,
   max 0 @@ min 7 @@ y + dy)

let tick t = t

let key_dn k t = match k with
  | Input.Key.Left  -> t |> move (-1) 0
  | Input.Key.Right -> t |> move (+1) 0
  | Input.Key.Up    -> t |> move 0 (-1)
  | Input.Key.Down  -> t |> move 0 (+1)

let key_up _ t = t

let cursor (x, y) = (x, y)
