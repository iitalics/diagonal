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
  | Damage of { atks: Attack.set }
  | Cast
  | Pick_up

and item =
  { it_id: Entity.Id.t;
    it_pos: Pos.t;
    it_typ : Item_type.t }

and ob =
  { ob_id: Entity.Id.t;
    ob_pos: Pos.t;
    ob_typ: Spell_type.t;
    ob_turns_left: int }

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
    | Main _ | Damage _ | Cast | Pick_up ->
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

let default_ob_turns = 3

let obs_occupied_cells obs =
  let tbl = Pos.Tbl.create (List.length obs * 2) in
  obs |> List.iter
           (fun { ob_pos; ob_id; _ } ->
             Pos.Tbl.add tbl ob_pos ob_id);
  tbl

let decay_obs obs =
  obs |> List.rev_filter_map
           (fun ob ->
             let tl = ob.ob_turns_left - 1 in
             if tl <= 0 then
               None
             else
               Some { ob with ob_turns_left = tl })

let spawn_obs obs next_id typ turns pos_list =
  let occupied = obs |> obs_occupied_cells in
  let obs, id, rem_ids =
    pos_list |> List.fold_left
                  (fun (obs, id, rem) pos ->
                    ({ ob_id = id;
                       ob_pos = pos;
                       ob_typ = typ;
                       ob_turns_left = turns } :: obs),
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
                     | pos_list -> let (obs', id') =
                                     pos_list |> spawn_obs obs id s default_ob_turns
                                   in
                                   obs', id', pl'

let attacks_from_obs typ atk_typ path obs =
  obs |> List.rev_filter_map
           (function
            | { ob_typ; ob_pos; _ }
                 when ob_typ = typ && path |> Path.points_mem ob_pos
              -> Some Attack.{ pos = ob_pos; typ = atk_typ }
            | _
              -> None)

let entity_of_ob { ob_id; ob_pos; ob_typ; ob_turns_left } =
  Entity.{ id = ob_id; typ = Obstacle (ob_typ, ob_pos, ob_turns_left = 1) }

(* entities *)

let entities g =
  List.rev_map_append entity_of_ob g.map_obs
  @@ List.rev_map_append entity_of_item g.map_items
  @@ player_entities_of_phase g.pl0 g.path0 g.pl1 g.path1 g.phase

(* attacks and damage *)

let burns obs path0 path1 =
  Attack.{ player_0 = obs |> attacks_from_obs Fire Burn path0;
           player_1 = obs |> attacks_from_obs Fire Burn path1 }

let heals obs path0 path1 =
  Attack.{ player_0 = obs |> attacks_from_obs Life Heal path0;
           player_1 = obs |> attacks_from_obs Life Heal path1 }

let attacks g =
  match g.phase with
  | Damage { atks } -> atks
  | Main _ | Moving _ | Cast | Pick_up -> Attack.empty_set

(* phases, turns *)

let phase_duration g = match g.phase with
  | Main _          -> Rules.turn_duration
  | Damage _        -> 1.
  | Cast            -> 1.
  | Pick_up         -> 1.
  | Moving { time } -> time

let main_phase path0 path1 =
  ignore path1;
  Main { cu = path0 |> Path.target }

let goto_main_phase pos0 pos1 turn_num =
  let path0 = Path.null ~src:pos0
  and path1 = Path.null ~src:pos1
  and turn_num = turn_num + 1 in
  fun g -> { g with
             path0; path1; turn_num;
             phase = main_phase path0 path1 }

let path_collide_with_ice obs path =
  let src = path |> Path.source in
  List.fold_left
    (fun path -> function
      | { ob_typ = Ice; ob_pos = pos; _ }
           when not (Pos.equal pos src) &&
                  (path |> Path.points_mem pos)
        ->
         Path.from_points ~src ~tgt:pos
      | _
        -> path)
    path
    obs

let goto_moving_phase pos0 pc0 pos1 pc1 map_obs cu =
  let pos0', pc0 = pc0 |> Player_controller.pick_move
                            { pos = pos0; opp_pos = pos1; cursor = Some cu } in
  let pos1', pc1 = pc1 |> Player_controller.pick_move
                            { pos = pos1; opp_pos = pos0; cursor = None } in
  let path0 = Path.from_points ~src:pos0 ~tgt:pos0'
  and path1 = Path.from_points ~src:pos1 ~tgt:pos1' in
  let path0 = path0 |> path_collide_with_ice map_obs
  and path1 = path1 |> path_collide_with_ice map_obs in
  let time = max (Path.length path0 /. Rules.move_vel)
               (Path.length path1 /. Rules.move_vel) in
  fun g -> { g with
             pc0; pc1; path0; path1;
             phase = Moving { time } }

let goto_damage_phase path0 pl0 path1 pl1 map_obs =
  let atks =
    Attack.set_concat
      [ Attack.path_collision path0 path1;
        burns map_obs path0 path1;
        heals map_obs path0 path1 ]
  in
  let (dmg0, dmg1) = atks |> Attack.damage_of_set pl0 pl1 in
  let pl0 = pl0 |> Player.take_damage dmg0
  and pl1 = pl1 |> Player.take_damage dmg1 in
  fun g -> { g with
             pl0; pl1;
             phase = Damage { atks } }

let goto_cast_phase path0 pl0 path1 pl1 map_obs next_id =
  let map_obs = decay_obs map_obs in
  let map_obs, next_id, pl0 = cast_spell map_obs next_id pl0 path0 in
  let map_obs, next_id, pl1 = cast_spell map_obs next_id pl1 path1 in
  fun g -> { g with
             pl0; pl1; map_obs; next_id;
             phase = Cast }

let goto_pickup_phase pos0 pl0 pos1 pl1 map_items =
  let pl0 = pl0 |> player_collect_item map_items pos0
  and pl1 = pl1 |> player_collect_item map_items pos1
  and map_items = map_items |> pick_up_items [ pos0; pos1 ] in
  fun g -> { g with
             pl0; pl1; map_items;
             phase = Pick_up }

let end_phase g =
  match g.phase with
  | Main { cu } ->
     g |> goto_moving_phase
            (g.path0 |> Path.source) g.pc0
            (g.path1 |> Path.source) g.pc1
            g.map_obs
            cu

  | Moving _ ->
     g |> goto_damage_phase
            g.path0 g.pl0
            g.path1 g.pl1
            g.map_obs

  | Damage _ ->
     g |> goto_cast_phase
            g.path0 g.pl0
            g.path1 g.pl1
            g.map_obs g.next_id

  | Cast ->
     g |> goto_pickup_phase
            (g.path0 |> Path.target) g.pl0
            (g.path1 |> Path.target) g.pl1
            g.map_items

  | Pick_up ->
     g |> goto_main_phase
            (g.path0 |> Path.target)
            (g.path1 |> Path.target)
            g.turn_num

let turn_num { turn_num; _ } =
  turn_num

let turn_duration { phase; _ } =
  match phase with
  | Main _ -> Some Rules.turn_duration
  | Moving _ | Damage _ | Cast | Pick_up -> None

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
  | Damage _ | Cast | Pick_up ->
     []

let grid_clamp x =
  x |> max 0 |> min (Rules.grid_cols - 1)

let modify_cursor f g =
  match g.phase with
  | Main { cu } ->
     let pos0 = g.path0 |> Path.target
     and pos1 = g.path1 |> Path.target in
     { g with phase = Main { cu = f pos0 pos1 cu } }
  | Moving _ | Damage _ | Cast | Pick_up ->
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
  | Moving _ | Damage _ | Cast | Pick_up -> None

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
    |> spawn_obs map_obs next_id Spell_type.Ice 2
  in
  let path0 = Path.null ~src:(3, 5)
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
