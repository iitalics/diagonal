type t =
  { pl0: Player.t;
    pl1: Player.t;
    pc0: Player_controller.t;
    pc1: Player_controller.t;
    turn_num: int;
    phase: phase }

and phase =
  | Turn of { cu: Pos.t }
  | Moving of { t: float }
  | Damage of { hits: hits }

and hits =
  { hits_player_0: hit list;
    hits_player_1: hit list }

and hit =
  { hit_pos: Pos.t;
    hit_mark: Path.mark }

let player_0_spawn = (3, 3)
let player_1_spawn = (6, 7)

let make ~player_ctrl_0:pc0 ~player_ctrl_1:pc1 =
  { pl0 = Player.make player_0_spawn;
    pl1 = Player.make player_1_spawn;
    pc0; pc1;
    turn_num = 1;
    phase = Turn { cu = player_0_spawn } }

(* players *)

let player_0 g = g.pl0
let player_1 g = g.pl1

(* attacks and damage *)

let compare_hit_pos { hit_pos = p1; _ } { hit_pos = p2; _ } =
  Pos.compare p1 p2

let player_hit_list (pl: Player.t) =
  let path =
    match pl.anim with
    | Player.No_anim -> Path.null ~src:pl.pos
    | Player.Moving pa -> pa
  in
  List.rev_map2
    (fun hit_pos hit_mark -> { hit_pos; hit_mark })
    (path |> Path.points)
    (path |> Path.marks)
  |> List.sort compare_hit_pos

let no_hits =
  { hits_player_0 = [];
    hits_player_1 = [] }

let beats m1 m2 =
  match m1 with
  | Path.M_crit -> m2 <> Path.M_crit
  | Path.M_attk -> m2 <> Path.M_crit && m2 <> Path.M_attk
  | _ -> false

 (* let defends m1 m2 =
  (m1 = m2 && (m1 = Path.M_crit || m1 = Path.M_attk))
  || m1 = Path.M_dfnd
  || m2 = Path.M_dfnd *)

let collision_hits (hs0: hit list) (hs1: hit list) : hits =
  let cons0 h { hits_player_0; hits_player_1 } =
    { hits_player_0 = h :: hits_player_0; hits_player_1 } in
  let cons1 h { hits_player_0; hits_player_1 } =
    { hits_player_0; hits_player_1 = h :: hits_player_1 } in
  Util.List.merge_fold
    ~compare:compare_hit_pos
    (fun acc _     -> acc)
    (fun acc    _   -> acc)
    (fun acc h0 h1 ->
      if beats h0.hit_mark h1.hit_mark then
        cons0 h0 acc
      else if beats h1.hit_mark h0.hit_mark then
        cons1 h1 acc
      else
        acc)
    no_hits
    hs0
    hs1


let hits t =
  match t.phase with
  | Damage { hits } -> hits
  | Turn _ | Moving _ -> no_hits

(* phases, turns *)

let turn g = g.turn_num

let inc_turn g =
  { g with turn_num = g.turn_num + 1 }

let phase_duration g = match g.phase with
  | Turn _       -> Rules.turn_duration
  | Moving { t } -> t
  | Damage _     -> 3.

let to_moving_phase g =
  let t0 = g.pl0.anim |> Player.anim_duration in
  let t1 = g.pl1.anim |> Player.anim_duration in
  { g with phase = Moving { t = max t0 t1 } }

let to_turn_phase g =
  let cu = g.pl0.pos in
  { g with phase = Turn { cu } }
  |> inc_turn

let to_damage_phase ~hits g =
  { g with phase = Damage { hits } }

let end_phase g =
  match g.phase with
  | Turn { cu = cursor } ->
     let pl0, pc0 = g.pc0 |> Player_controller.commit_turn g.pl0 ~cursor in
     let pl1, pc1 = g.pc1 |> Player_controller.commit_turn g.pl1 in
     to_moving_phase
       { g with pl0; pl1; pc0; pc1 }

  | Moving _ ->
     let hits = collision_hits
                  (g.pl0 |> player_hit_list)
                  (g.pl1 |> player_hit_list) in
     let pl0 = g.pl0 |> Player.stop_moving in
     let pl1 = g.pl1 |> Player.stop_moving in
     to_damage_phase
       { g with pl0; pl1 }
       ~hits

  | Damage _ ->
     to_turn_phase g

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
    | Turn { cu } -> (Path.from_points ~src:t.pl0.pos ~tgt:cu, Select_path) :: k
    | Moving _    -> k
    | Damage _    -> k
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
  | Moving _ | Damage _ ->
     t

let[@ocaml.inline] reset_cursor t =
  match t.phase with
  | Turn { cu = _ } ->
     { t with phase = Turn { cu = t.pl0.pos } }
  | Moving _ | Damage _ ->
     t

let cursor t =
  match t.phase with
  | Turn { cu } -> Some(cu)
  | Moving _ | Damage _ -> None

(* events *)

let key_dn : Input.Key.t -> _ = function
  | Up    -> move_cursor_by 0 (-1)
  | Down  -> move_cursor_by 0 (+1)
  | Left  -> move_cursor_by (-1) 0
  | Right -> move_cursor_by (+1) 0
  | Esc   -> reset_cursor

let key_up _k t =
  (* TODO: DAS'ing *)
  t
