open Dia_util

type t =
  { (* turn / phase *)
    turn_num: int;
    phase: phase;
    (* players *)
    pl0: Player.t;
    pl1: Player.t;
    pc0: Player_controller.t;
    pc1: Player_controller.t;
    (* map entities *)
    next_id: int;
    map_items: item list;
    map_obs: ob list }

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

and item =
  { it_id: Entity.Id.t;
    it_pos: Pos.t;
    it_typ : Item_type.t }

and ob =
  { ob_id: Entity.Id.t;
    ob_pos: Pos.t;
    ob_typ: Spell_type.t }

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

let damage_of_hit ~player:Player.{ weapon; _ } =
  function
  | { hit_type = Crit; _ } ->
     (weapon |> Weapon_type.atk) + (weapon |> Weapon_type.crit_bonus)
  | { hit_type = Attk; _ } ->
     (weapon |> Weapon_type.atk)

let damage_of_hits pl0 pl1 hs =
  (hs.hits_player_0 |> List.sum_by (damage_of_hit ~player:pl0),
   hs.hits_player_1 |> List.sum_by (damage_of_hit ~player:pl1))

(* players *)

let player_0 g = g.pl0
let player_1 g = g.pl1

let player_collect_item items pos pl =
  match items |> List.find_opt (fun { it_pos; _ } -> Pos.equal pos it_pos) with
  | Some { it_typ; _ } ->
     pl |> Player.pick_up it_typ
  | None ->
     pl

let player_entities_of_phase pl0 pl1 = function
  | Turn { idle; _ } | Damage { idle; _ } ->
     let { pos0; pos1 } = idle in
     [ Entity.{ id = 0; typ = Blob_idle (pl0, pos0) };
       Entity.{ id = 1; typ = Blob_idle (pl1, pos1) } ]
  | Moving { path0; path1 } ->
     [ Entity.{ id = 0; typ = Blob_moving (pl0, path0) };
       Entity.{ id = 1; typ = Blob_moving (pl1, path1) } ]

(* items *)

let pick_up_items pick_up items =
  items |> List.filter
             (fun { it_pos; _ } ->
               not (pick_up |> List.exists (Pos.equal it_pos) ))

let spawn_items items next_id descs =
  descs |> List.fold_left
             (fun (items, id) (typ, pos) ->
               { it_id = id;
                 it_typ = typ;
                 it_pos = pos } :: items,
               id + 1)
             (items, next_id)

let entity_of_item { it_id; it_pos; it_typ } =
  Entity.{ id = it_id; typ = Item (it_typ, it_pos) }

(* obstacles *)

let spawn_obs obs next_id descs =
  descs |> List.fold_left
             (fun (obs, id) (typ, pos) ->
               { ob_id = id;
                 ob_typ = typ;
                 ob_pos = pos } :: obs,
               id + 1)
             (obs, next_id)

let cast_spell path obs next_id spell =
  match spell, Path.knee path with
  | Some typ, Some pos ->
     [ typ, pos ] |> spawn_obs obs next_id
  | _, _ ->
     obs, next_id

let entity_of_ob { ob_id; ob_pos; ob_typ } =
  Entity.{ id = ob_id; typ = Obstacle (ob_typ, ob_pos) }

(* entities *)

let entities t =
  (t.phase |> player_entities_of_phase t.pl0 t.pl1)
  @ (t.map_items |> List.rev_map entity_of_item)
  @ (t.map_obs |> List.rev_map entity_of_ob)

(* phases, turns *)

let phase_duration g = match g.phase with
  | Turn _ -> Rules.turn_duration
  | Damage _ -> 1.
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
     let { pl0; pl1; map_items; map_obs; next_id; _ } = g in
     (* cast spells *)
     let s0, pl0 = pl0 |> Player.use_spell_cast
     and s1, pl1 = pl1 |> Player.use_spell_cast in
     let map_obs, next_id = s0 |> cast_spell path0 map_obs next_id in
     let map_obs, next_id = s1 |> cast_spell path1 map_obs next_id in
     (* take damage *)
     let hits = collision_hits path0 path1 in
     let (dmg0, dmg1) = hits |> damage_of_hits pl0 pl1 in
     let (pos0, pos1) = (path0 |> Path.target, path1 |> Path.target) in
     (* pick up items *)
     let pl0 = pl0 |> Player.take_damage dmg1 |>
                 player_collect_item g.map_items pos0
     and pl1 = pl1 |> Player.take_damage dmg0 |>
                 player_collect_item g.map_items pos1 in
     let map_items = map_items |> pick_up_items [ pos0; pos1 ] in
     { g with
       pl0; pl1; map_items; map_obs; next_id;
       phase = Damage { hits; idle = { pos0; pos1 } } }

  | Damage { idle; _ } ->
     { g with
       phase = idle_turn idle;
       turn_num = g.turn_num + 1 }

let turn_num { turn_num; _ } =
  turn_num

let turn_duration { phase; _ } =
  match phase with
  | Turn _ -> Some Rules.turn_duration
  | Moving _ | Damage _ -> None

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

(* init *)

let make ~player_ctrl_0:pc0 ~player_ctrl_1:pc1 =
  let map_items, map_obs, next_id = [], [], 2 in
  let map_items, next_id =
    [ Item_type.Weapon Staff,  (3, 7);
      Item_type.Weapon Rapier, (4, 0);
      Item_type.Spell Fire,    (1, 1);
      Item_type.Spell Ice,     (6, 0) ]
    |> spawn_items map_items next_id
  in
  let map_obs, next_id =
    List.init 4 (fun i -> Spell_type.Fire, (2 + i, 2))
    |> spawn_obs map_obs next_id
  in
  { turn_num = 1;
    phase = idle_turn { pos0 = (3, 3);
                        pos1 = (6, 7) };
    pl0 = Player.make ~color:0;
    pl1 = Player.make ~color:1;
    pc0; pc1;
    next_id;
    map_items;
    map_obs }

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
