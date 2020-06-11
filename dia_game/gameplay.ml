type t =
  { cx: int;
    cy: int;
    tn: int;
    tf: int;
    pa: Path.t option }

let make () =
  { cx = 0; cy = 0; tn = 0; tf = 0; pa = None }

let grid_clamp v = v |> max 0 |> min (Rules.grid_cols - 1)

let move dx dy t =
  let cx, cy = grid_clamp (t.cx + dx), grid_clamp (t.cy + dy) in
  let pa = Path.from_points
             ~src:(3, 3)
             ~tgt:(cx, cy) in
  let pa = Some pa in
  { t with cx; cy; pa }

let tick t =
  let tf' = t.tf + 1 in
  if tf' >= Rules.turn_frames then
    { t with tf = 0; tn = t.tn + 1 }
  else
    { t with tf = tf' }

let key_dn k t = match k with
  | Input.Key.Left  -> t |> move (-1) 0
  | Input.Key.Right -> t |> move (+1) 0
  | Input.Key.Up    -> t |> move 0 (-1)
  | Input.Key.Down  -> t |> move 0 (+1)

let key_up _ t = t

let cursor t = (t.cx, t.cy)
let turn_num t = t.tn
let turn_frame t = t.tf
let path t = t.pa
