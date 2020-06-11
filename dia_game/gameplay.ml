type t =
  { tn: turn;
    pl0: player;
    pl1: player;
    cx: int;
    cy: int }

and turn =
  { tn_num: int;
    tn_frame: int }

and player =
  { pl_x: int;
    pl_y: int }

let make () =
  { tn = { tn_num = 0; tn_frame = 0; };
    pl0 = { pl_x = 3; pl_y = 3 };
    pl1 = { pl_x = 6; pl_y = 7 };
    cx = 3; cy = 3 }

let grid_clamp v = v |> max 0 |> min (Rules.grid_cols - 1)

(* player *)

let player_pos { pl_x; pl_y } = (pl_x, pl_y)
let player_0 t = t.pl0
let player_1 t = t.pl1

(* cursor *)

let move_cursor_to cx cy t =
  { t with cx; cy }

let move_cursor_by dx dy t =
  t |> move_cursor_to
         (grid_clamp (t.cx + dx))
         (grid_clamp (t.cy + dy))

let reset_cursor t =
  let { pl_x; pl_y } = t.pl0 in
  t |> move_cursor_to pl_x pl_y

let cursor' t =
  (t.cx, t.cy)

let path' t =
  Path.from_points
    ~src:(t.pl0 |> player_pos)
    ~tgt:(t.cx, t.cy)

let is_cursor_active t =
  (t |> cursor') <> (t.pl0 |> player_pos)

let cursor t = if t |> is_cursor_active then Some(t |> cursor') else None
let path t = if t |> is_cursor_active then Some(t |> path') else None

(* turn *)

let tick_turn { tn_num = n; tn_frame = f } =
  let f' = f + 1 in
  if f' >= Rules.turn_frames then
    { tn_num = n + 1; tn_frame = 0 }, true
  else
    { tn_num = n; tn_frame = f' }, false

let end_turn t =
  let (p_x, p_y) = t |> path' |> Path.target in
  let t = { t with pl0 = { pl_x = p_x; pl_y = p_y } } in
  t |> reset_cursor

let turn t = t.tn

(* tick *)

let tick t =
  let tn, ended = t.tn |> tick_turn in
  let t = { t with tn } in
  if ended then
    t |> end_turn
  else
    t

(* key events *)

let key_dn k t = match k with
  | Input.Key.Left  -> t |> move_cursor_by (-1) 0
  | Input.Key.Right -> t |> move_cursor_by (+1) 0
  | Input.Key.Up    -> t |> move_cursor_by 0 (-1)
  | Input.Key.Down  -> t |> move_cursor_by 0 (+1)
  | Input.Key.Esc   -> t |> reset_cursor

let key_up _ t = t
