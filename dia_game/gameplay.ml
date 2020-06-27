open Dia_util

type t =
  { (* turn / phase *)
    turn_num: int;
    phase: phase;
    (* players *)
    pl0: Player.t;
    pl1: Player.t;
    path0: Path.t;
    path1: Path.t;
    pc0: Player_controller.t;
    pc1: Player_controller.t;
    (* map entities *)
    next_id: int;
    map_items: item list;
    map_obs: ob list }

and phase =
  | Main of { cu: Pos.t }
  | Moving of { time: float }
  | Damage of { hits: hits }

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
  | Main _ | Moving _ -> no_hits

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

let player_entities_of_phase pl0 path0 pl1 path1 phase =
  let typ0, typ1 =
    match phase with
    | Main _ | Damage _ ->
       Entity.Blob_idle (pl0, path0 |> Path.target),
       Entity.Blob_idle (pl1, path1 |> Path.target)
    | Moving _ ->
       Entity.Blob_moving (pl0, path0),
       Entity.Blob_moving (pl1, path1)
  in
  [ Entity.{ id = 0; typ = typ0 };
    Entity.{ id = 1; typ = typ1 } ]

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

let obs_occupied_cells obs =
  let tbl = Pos.Tbl.create (List.length obs * 2) in
  obs |> List.iter
           (fun { ob_pos; ob_id; _ } ->
             Pos.Tbl.add tbl ob_pos ob_id);
  tbl

let spawn_obs obs next_id typ pos_list =
  let occupied = obs |> obs_occupied_cells in
  let obs, id, rem_ids =
    pos_list |> List.fold_left
                  (fun (obs, id, rem) pos ->
                    ({ ob_id = id;
                       ob_typ = typ;
                       ob_pos = pos } :: obs),
                    (id + 1),
                    (match Pos.Tbl.find_opt occupied pos with
                     | None    -> rem
                     | Some id -> rem |> Entity.Id_set.add id))
                  (obs, next_id, Entity.Id_set.empty)
  in
  let obs =
    obs |> List.filter
             (fun { ob_id; _ } ->
               not @@ Entity.Id_set.mem ob_id rem_ids)
  in
  obs, id

let cast_spell obs id pl path =
  match pl |> Player.use_spell_cast with
  | None          -> obs, id, pl
  | Some (s, pl') -> match s |> Spell_type.cast_points path with
                     | []       -> obs,  id,  pl
                     | pos_list -> let (obs', id') = pos_list |> spawn_obs obs id s in
                                   obs', id', pl'

let entity_of_ob { ob_id; ob_pos; ob_typ } =
  Entity.{ id = ob_id; typ = Obstacle (ob_typ, ob_pos) }

(* entities *)

let entities g =
  List.rev_map_append entity_of_ob g.map_obs
  @@ List.rev_map_append entity_of_item g.map_items
  @@ player_entities_of_phase g.pl0 g.path0 g.pl1 g.path1 g.phase

(* phases, turns *)

let phase_duration g = match g.phase with
  | Main _ -> Rules.turn_duration
  | Damage _ -> 1.
  | Moving { time } -> time

let main_phase path0 path1 =
  ignore path1;
  Main { cu = path0 |> Path.target }

let moving_phase path0 path1 =
  Moving { time = max (Path.length path0 /. Rules.move_vel)
                    (Path.length path1 /. Rules.move_vel) }

let damage_phase hits =
  Damage { hits }

let end_turn g =
  let path0 = Path.null ~src:(g.path0 |> Path.target)
  and path1 = Path.null ~src:(g.path1 |> Path.target)
  and turn_num = g.turn_num + 1 in
  { g with
    path0; path1; turn_num;
    phase = main_phase path0 path1 }

let end_phase g =
  match g.phase with
  | Main { cu } ->
     let pos0 = g.path0 |> Path.target
     and pos1 = g.path1 |> Path.target in
     let pos0', pc0 = g.pc0 |> Player_controller.pick_move
                                 { pos = pos0; opp_pos = pos1; cursor = Some cu } in
     let pos1', pc1 = g.pc1 |> Player_controller.pick_move
                                 { pos = pos1; opp_pos = pos0; cursor = None } in
     let path0 = Path.from_points ~src:pos0 ~tgt:pos0'
     and path1 = Path.from_points ~src:pos1 ~tgt:pos1'  in
     { g with
       pc0; pc1; path0; path1;
       phase = moving_phase path0 path1  }

  | Moving _ ->
     let { pl0; pl1; _ } = g in
     (* take damage *)
     let hits = collision_hits g.path0 g.path1 in
     let (dmg0, dmg1) = hits |> damage_of_hits pl0 pl1 in
     let pl0 = pl0 |> Player.take_damage dmg1
     and pl1 = pl1 |> Player.take_damage dmg0 in
     (* cast spells *)
     let map_obs, next_id, pl0 = cast_spell g.map_obs g.next_id pl0 g.path0 in
     let map_obs, next_id, pl1 = cast_spell map_obs   next_id   pl1 g.path1 in
     (* pick up items *)
     let pos0, pos1 = (g.path0 |> Path.target, g.path1 |> Path.target) in
     let pl0 = pl0 |> player_collect_item g.map_items pos0
     and pl1 = pl1 |> player_collect_item g.map_items pos1
     and map_items = g.map_items |> pick_up_items [ pos0; pos1 ] in
     (* *)
     { g with
       pl0; pl1; map_items; map_obs; next_id;
       phase = damage_phase hits }

  | Damage _ ->
     end_turn g

let turn_num { turn_num; _ } =
  turn_num

let turn_duration { phase; _ } =
  match phase with
  | Main _ -> Some Rules.turn_duration
  | Moving _ | Damage _ -> None

(* cursor, paths *)

type path_type =
  | Select_path
  | Player_path

let paths g =
  match g.phase with
  | Main { cu } ->
     let pos0 = g.path0 |> Path.target in
     [ Path.from_points ~src:pos0 ~tgt:cu, Select_path ]
  | Moving _ ->
     [ (g.path0, Player_path);
       (g.path1, Player_path) ]
  | Damage _ ->
     []

let grid_clamp x =
  x |> max 0 |> min (Rules.grid_cols - 1)

let modify_cursor f g =
  match g.phase with
  | Main { cu } ->
     let pos0 = g.path0 |> Path.target
     and pos1 = g.path1 |> Path.target in
     { g with phase = Main { cu = f pos0 pos1 cu } }
  | Moving _ | Damage _ ->
     g

let move_cursor_by dx dy =
  modify_cursor
    (fun _ _ (x, y) ->
      (grid_clamp (x + dx),
       grid_clamp (y + dy)))

let reset_cursor =
  modify_cursor
    (fun pos0 _ _ ->
      pos0)

let cursor g =
  match g.phase with
  | Main { cu } -> Some(cu)
  | Moving _ | Damage _ -> None

(* init *)

let make ~player_ctrl_0:pc0 ~player_ctrl_1:pc1 =
  let map_items, map_obs, next_id = [], [], 2 in
  let map_items, next_id =
    [ Item_type.Weapon Staff,  (3, 7);
      Item_type.Weapon Rapier, (4, 0);
      Item_type.Spell Fire,    (1, 1);
      Item_type.Spell Life,    (0, 1);
      Item_type.Spell Ice,     (6, 0) ]
    |> spawn_items map_items next_id
  in
  let map_obs, next_id =
    List.init 4 (fun i -> (2 + i, 2))
    |> spawn_obs map_obs next_id Spell_type.Fire
  in
  let path0 = Path.null ~src:(3, 3)
  and path1 = Path.null ~src:(6, 7) in
  { turn_num = 1;
    phase = main_phase path0 path1;
    pl0 = Player.make ~color:0;
    pl1 = Player.make ~color:1;
    path0; path1;
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
