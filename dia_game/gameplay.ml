module List = Util.List

type t =
  { pl0: Player.t;
    pl1: Player.t;
    pc0: Player_controller.t;
    pc1: Player_controller.t;
    turn_num: int;
    phase: phase }

and phase =
  | Turn of { idle: idle; cu: Pos.t }
  | Moving of { path0: Path.t; path1: Path.t }
  | Damage of { idle: idle; hits: hits }

and idle =
  { pos0: Pos.t;
    pos1: Pos.t }

and hits =
  { hits_player_0: hit list;
    hits_player_1: hit list }

and hit =
  { hit_pos: Pos.t;
    hit_type: hit_type }

and hit_type =
  | Crit
  | Attk

type point_type = Path.point_type

(* attacks and damage *)

let no_hits =
  { hits_player_0 = [];
    hits_player_1 = [] }

let hit_cons0 h { hits_player_0; hits_player_1 } =
  { hits_player_0 = h :: hits_player_0; hits_player_1 }

let hit_cons1 h { hits_player_0; hits_player_1 } =
  { hits_player_0; hits_player_1 = h :: hits_player_1 }

let collision_hit_type (typ0: point_type) (typ1: point_type) =
  match typ0, typ1 with
  | Crit, Vuln -> `P0 Crit
  | Vuln, Crit -> `P1 Crit
  | Crit, Attk | Attk, Vuln -> `P0 Attk
  | Attk, Crit | Vuln, Attk -> `P1 Attk
  | Crit, Crit | Attk, Attk | Vuln, Vuln -> `No

let collision_hits path0 path1 =
  let compare (pos1, _) (pos2, _) = Pos.compare pos1 pos2 in
  List.merge_fold ~compare
    (fun acc _ -> acc)
    (fun acc _ -> acc)
    (fun acc (hit_pos, typ0) (_, typ1) ->
      match collision_hit_type typ0 typ1 with
      | `P0 hit_type -> hit_cons0 { hit_pos; hit_type } acc
      | `P1 hit_type -> hit_cons1 { hit_pos; hit_type } acc
      | `No          -> acc)
    no_hits
    (path0 |> Path.points |> List.sort compare)
    (path1 |> Path.points |> List.sort compare)

let hits g =
  match g.phase with
  | Damage { hits; _ } -> hits
  | Turn _ | Moving _ -> no_hits

let damage_of_hit_type = function
  | Crit -> 2
  | Attk -> 1

let damage_of_hits hs =
  let doh { hit_type; _ } = hit_type |> damage_of_hit_type in
  (hs.hits_player_0 |> List.sum_by doh,
   hs.hits_player_1 |> List.sum_by doh)

(* phases, turns *)

let turn g = g.turn_num
let inc_turn g = { g with turn_num = g.turn_num + 1 }

let phase_duration g = match g.phase with
  | Turn _ -> Rules.turn_duration
  | Damage _ -> 3.
  | Moving { path0; path1 } ->
     max (Path.length path0 /. Rules.move_vel)
       (Path.length path1 /. Rules.move_vel)

let idle_turn idle =
  Turn { idle; cu = idle.pos0 }

let end_phase g =
  match g.phase with
  | Turn { idle = { pos0; pos1 }; cu } ->
     let pos0', pc0 = g.pc0 |> Player_controller.pick_move
                                 { pos = pos0; opp_pos = pos1; cursor = Some cu } in
     let pos1', pc1 = g.pc1 |> Player_controller.pick_move
                                 { pos = pos1; opp_pos = pos0; cursor = None } in
     { g with
       pc0; pc1;
       phase = Moving { path0 = Path.from_points ~src:pos0 ~tgt:pos0';
                        path1 = Path.from_points ~src:pos1 ~tgt:pos1' } }

  | Moving { path0; path1 } ->
     let hits = collision_hits path0 path1 in
     let (dmg0, dmg1) = hits |> damage_of_hits in
     let pl0 = g.pl0 |> Player.take_damage dmg1 in
     let pl1 = g.pl1 |> Player.take_damage dmg0 in
     { g with
       pl0; pl1;
       phase = Damage { hits;
                        idle = { pos0 = path0 |> Path.target;
                                 pos1 = path1 |> Path.target } } }

  | Damage { idle; _ } ->
     { g with phase = idle_turn idle }
     |> inc_turn

(* cursor, paths *)

type path_type =
  | Select_path
  | Player_path

let paths t =
  match t.phase with
  | Turn { idle = { pos0; _ }; cu } ->
     [ Path.from_points ~src:pos0 ~tgt:cu, Select_path ]
  | Moving { path0; path1 } ->
     [ (path0, Player_path); (path1, Player_path) ]
  | Damage _ ->
     []

let grid_clamp x =
  x |> max 0 |> min (Rules.grid_cols - 1)

let modify_cursor f t =
  match t.phase with
  | Turn { idle; cu } ->
     { t with phase = Turn { idle; cu = f idle cu } }
  | Moving _ | Damage _ ->
     t

let move_cursor_by dx dy t =
  t |> modify_cursor
         (fun _ (x, y) ->
           (grid_clamp (x + dx),
            grid_clamp (y + dy)))

let reset_cursor t =
  t |> modify_cursor
         (fun { pos0; _ } _ ->
           pos0)

let cursor t =
  match t.phase with
  | Turn { cu; _ } -> Some(cu)
  | Moving _ | Damage _ -> None

(* players, entities *)

let player_0 g = g.pl0
let player_1 g = g.pl1

let player_entities pl0 pl1 = function
  | Turn { idle; _ } | Damage { idle; _ } ->
     let { pos0; pos1 } = idle in
     [ Entity.{ id = 0; typ = Blob_idle (pl0, pos0) };
       Entity.{ id = 1; typ = Blob_idle (pl1, pos1) } ]
  | Moving { path0; path1 } ->
     [ Entity.{ id = 0; typ = Blob_moving (pl0, path0) };
       Entity.{ id = 1; typ = Blob_moving (pl1, path1) } ]

let entities t =
  player_entities t.pl0 t.pl1 t.phase @
    [ Entity.{ id = 2; typ = Item (Item_type.Dagger, (2, 1)) };
      Entity.{ id = 3; typ = Item (Item_type.Rapier, (2, 3)) } ]

(* init *)

let spawn =
  { pos0 = (3, 3);
    pos1 = (6, 7) }

let make ~player_ctrl_0:pc0 ~player_ctrl_1:pc1 =
  { pl0 = Player.make ~color:0;
    pl1 = Player.make ~color:1;
    pc0; pc1;
    turn_num = 1;
    phase = idle_turn spawn }

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
