type t =
  { cx: int;
    cy: int;
    tn: int;
    tf: int;
    pa: Path.t }

let ox, oy = 3, 3

let make () =
  { cx = ox; cy = oy; tn = 0; tf = 0;
    pa = Path.from_origin ~pos:(ox, oy) ~dx:0 ~dy:0 }

let grid_clamp v = v |> max 0 |> min (Rules.grid_cols - 1)

let move_cursor_to cx cy t =
  let pa = Path.from_points
             ~src:(ox, oy)
             ~tgt:(cx, cy) in
  { t with cx; cy; pa }

let move_cursor_by dx dy t =
  t |> move_cursor_to
         (grid_clamp (t.cx + dx))
         (grid_clamp (t.cy + dy))

let tick t =
  let tf' = t.tf + 1 in
  if tf' >= Rules.turn_frames then
    { t with tf = 0; tn = t.tn + 1 }
  else
    { t with tf = tf' }

let key_dn k t = match k with
  | Input.Key.Left  -> t |> move_cursor_by (-1) 0
  | Input.Key.Right -> t |> move_cursor_by (+1) 0
  | Input.Key.Up    -> t |> move_cursor_by 0 (-1)
  | Input.Key.Down  -> t |> move_cursor_by 0 (+1)
  | Input.Key.Esc   -> t |> move_cursor_to ox oy

let key_up _ t = t

let is_cursor_active t =
  t.cx <> ox || t.cy <> oy

let cursor t = if t |> is_cursor_active then Some(t.cx, t.cy) else None
let path t = if t |> is_cursor_active then Some(t.pa) else None
let turn_num t = t.tn
let turn_frame t = t.tf
