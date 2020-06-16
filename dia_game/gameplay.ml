module Turn = struct
  type t = { num: int; frame: int }
end

type t =
  { pl0: Player.t;
    pl1: Player.t;
    pc0: Player_controller.t;
    pc1: Player_controller.t;
    turn_num: int;
    phase: phase;
    f: int }

and phase =
  | Turn of { cu: Pos.t }
  | Moving of { frames: int }

let player_0_spawn = (3, 3)
let player_1_spawn = (6, 7)

let make ~player_ctrl_0:pc0 ~player_ctrl_1:pc1 =
  { pl0 = Player.make player_0_spawn;
    pl1 = Player.make player_1_spawn;
    pc0; pc1;
    turn_num = 1;
    phase = Turn { cu = player_0_spawn };
    f = 0 }

(* players *)

let player_0 t = t.pl0
let player_1 t = t.pl1

(* phases, turns *)

let phase_frames = function
  | Turn _            -> Rules.turn_frames
  | Moving { frames } -> frames

let end_phase t = function
  | Turn { cu } ->
     let pl0, pc0 = t.pc0
                    |> Player_controller.set_cursor cu
                    |> Player_controller.commit_turn t.pl0 in
     let pl1, pc1 = t.pc1
                    |> Player_controller.commit_turn t.pl1 in
     let frames = max
                    (pl0.anim |> Player.anim_frames)
                    (pl1.anim |> Player.anim_frames) in
     { t with
       pl0; pl1; pc0; pc1;
       phase = Moving { frames } }
  | Moving _ ->
     let pl0 = t.pl0 |> Player.stop_moving in
     let pl1 = t.pl1 |> Player.stop_moving in
     let cu = pl0.pos in
     { t with
       pl0; pl1;
       turn_num = t.turn_num + 1;
       phase = Turn { cu } }

let turn t =
  let num = t.turn_num in
  let frame = match t.phase with
    | Turn _   -> t.f
    | Moving _ -> Rules.turn_frames in
  Turn.{ num; frame }

(* cursor, paths *)

type path_type =
  | Select_path
  | Player_path

let paths t =
  let player_anim_path an k = match an with
    | Player.No_anim   -> k
    | Player.Moving pa -> (pa, Player_path) :: k
  in
  let phase_path ph k = match ph with
    | Turn { cu } -> (Path.from_points ~src:t.pl0.pos ~tgt:cu,
                      Select_path)
                     :: k
    | Moving _    -> k
  in
  phase_path t.phase
    (player_anim_path t.pl0.anim
       (player_anim_path t.pl1.anim []))

let grid_clamp x =
  x |> max 0 |> min (Rules.grid_cols - 1)

let[@ocaml.inline] move_cursor_by dx dy t =
  match t.phase with
  | Turn { cu = (cx, cy) } ->
     let cu = (grid_clamp (cx + dx),
               grid_clamp (cy + dy)) in
     { t with phase = Turn { cu } }
  | Moving _ ->
     t

let[@ocaml.inline] reset_cursor t =
  match t.phase with
  | Turn { cu = _ } ->
     { t with phase = Turn { cu = t.pl0.pos } }
  | Moving _ ->
     t

let cursor t =
  match t.phase with
  | Turn { cu } -> Some(cu)
  | Moving _    -> None

(* events *)

let tick t =
  let rec loop t =
    if t.f >= phase_frames t.phase then
      loop (end_phase { t with f = 0 } t.phase)
    else
      t
  in
  loop { t with f = t.f + 1 }

let key_dn : Input.Key.t -> _ = function
  | Up    -> move_cursor_by 0 (-1)
  | Down  -> move_cursor_by 0 (+1)
  | Left  -> move_cursor_by (-1) 0
  | Right -> move_cursor_by (+1) 0
  | Esc   -> reset_cursor

let key_up _k t =
  (* TODO: DAS'ing *)
  t
