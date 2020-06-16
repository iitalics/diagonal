module Turn = struct
  type t = { num: int; frame: int }
end

module Player = struct
  type t = { pos: Pos.t;
             anim: anim }

  and anim =
    | No_anim
    | Moving of Path.t

  let make x y =
    { pos = (x, y);
      anim = No_anim }

  let move_anim path pl =
    { pl with anim = Moving(path) }

  let move_to pos _pl =
    { pos; anim = No_anim }
end

type t =
  { pl0: Player.t;
    pl1: Player.t;
    turn_num: int;
    phase: phase;
    f: int }

and phase =
  | Turn of { cu: Pos.t }
  | Move of { pa0: Path.t }

let make () =
  { pl0 = Player.make 3 3;
    pl1 = Player.make 6 7;
    turn_num = 1;
    phase = Turn { cu = (3, 3) };
    f = 0 }

(* players *)

let player_0 t = t.pl0
let player_1 t = t.pl1

(* phases, turns *)

type phase_action =
  | No_op
  | Animate_player_0 of Path.t
  | Move_players of Pos.t * Pos.t

let path_anim_frames path =
  int_of_float @@
    ceil ((path |> Path.length) *. Rules.move_rate)

let update_phase t = match t.phase with
  | Turn { cu } when (t.f >= Rules.turn_frames) ->
     let path = Path.from_points
                  ~src:t.pl0.pos
                  ~tgt:cu in
     Move { pa0 = path },
     Animate_player_0(path)

  | Move { pa0 } when (t.f >= (pa0 |> path_anim_frames)) ->
     let tgt0 = pa0 |> Path.target in
     let tgt1 = t.pl1.pos in
     Turn { cu = tgt0 },
     Move_players(tgt0, tgt1)

  | phase ->
     phase, No_op

let apply_phase_action t = function
  | No_op -> t
  | Animate_player_0(pa) ->
     { t with
       f = 0;
       pl0 = t.pl0 |> Player.move_anim pa }
  | Move_players(pos0, pos1) ->
     { t with
       turn_num = t.turn_num + 1;
       f = 0;
       pl0 = t.pl0 |> Player.move_to pos0;
       pl1 = t.pl1 |> Player.move_to pos1 }

let turn t =
  let num = t.turn_num in
  let frame = match t.phase with
    | Turn _ -> t.f
    | Move _ -> Rules.turn_frames in
  Turn.{ num; frame }

(* cursor *)

let cursor t =
  match t.phase with
  | Turn { cu } -> Some(cu)
  | Move _ -> None

let paths t =
  match t.phase with
  | Turn { cu }  -> [ Path.from_points ~src:t.pl0.pos ~tgt:cu ]
  | Move { pa0 } -> [ pa0 ]

let grid_clamp x =
  x |> max 0 |> min (Rules.grid_cols - 1)

let[@ocaml.inline] move_cursor_by dx dy t =
  match t.phase with
  | Turn { cu = (cx, cy) } ->
     let cu' = (grid_clamp (cx + dx),
                grid_clamp (cy + dy)) in
     { t with phase = Turn { cu = cu' } }
  | Move _ ->
     t

let[@ocaml.inline] reset_cursor t =
  match t.phase with
  | Turn { cu = _ } ->
     { t with phase = Turn { cu = t.pl0.pos } }
  | Move _ ->
     t

(* events *)

let tick t =
  let t = { t with f = t.f + 1 } in
  let phase, action = update_phase t in
  action |> apply_phase_action { t with phase }

let key_dn : Input.Key.t -> _ = function
  | Up    -> move_cursor_by 0 (-1)
  | Down  -> move_cursor_by 0 (+1)
  | Left  -> move_cursor_by (-1) 0
  | Right -> move_cursor_by (+1) 0
  | Esc   -> reset_cursor

let key_up _k t =
  (* TODO: DAS'ing *)
  t
