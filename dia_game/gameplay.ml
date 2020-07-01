open Dia_util

type t =
  { rng: Prng.t;
    (* turn / phase *)
    turn_num: int;
    phase: phase;
    (* players *)
    pl0: player;
    pl1: player;
    (* map entities *)
    next_id: int;
    map_items: item list;
    map_obs: ob list }

and phase =
  | Main of
      { cu: Pos.t }
  | Moving of
      { paths: paths;
        int_paths: paths }
  | Bouncing of
      { paths: paths;
        orig_pos: Pos.t }
  | Damage of
      { paths: paths;
        atks: Attack.set }
  | Cast
  | Pick_up

and paths =
  { path0: Path.t;
    path1: Path.t }

and player =
  { pl_stats: Player.t;
    pl_pos: Pos.t;
    pl_ctl: Player_controller.t }

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

let player_0 g = g.pl0.pl_stats
let player_1 g = g.pl1.pl_stats

let player_collect_item items pl =
  let { pl_pos; pl_stats; _ } = pl in
  match items |> List.find_opt (fun { it_pos; _ } -> Pos.equal pl_pos it_pos) with
  | Some { it_typ; _ } ->
     { pl with pl_stats = pl_stats |> Player.pick_up it_typ }
  | None ->
     pl

let player_entities_of_phase pl0 pl1 phase =
  let typ0, typ1 =
    match phase with
    | Main _ | Damage _ | Cast | Pick_up ->
       Entity.Blob_idle (pl0.pl_stats, pl0.pl_pos),
       Entity.Blob_idle (pl1.pl_stats, pl1.pl_pos)
    | Moving { paths = { path0; path1; _ }; _ } ->
       Entity.Blob_moving (pl0.pl_stats, path0),
       Entity.Blob_moving (pl1.pl_stats, path1)
    | Bouncing { orig_pos; _ } ->
       Entity.Blob_bounce (pl0.pl_stats, orig_pos, pl0.pl_pos),
       Entity.Blob_bounce (pl1.pl_stats, orig_pos, pl1.pl_pos)
  in
  [ Entity.{ id = 0; typ = typ0 };
    Entity.{ id = 1; typ = typ1 } ]

(* items *)

let pick_up_items pl0 pl1 items =
  items |> List.filter
             (fun { it_pos; _ } ->
               not (Pos.equal pl0.pl_pos it_pos
                    || Pos.equal pl1.pl_pos it_pos))

let spawn_items items next_id pos_list typ_list =
  List.fold_left2
    (fun (items, id) pos typ ->
      { it_id = id;
        it_typ = typ;
        it_pos = pos } :: items,
      id + 1)
    (items, next_id)
    pos_list
    typ_list

let item_spawn_formations_a =
  (* . . . 5 6 . . .
     . . . . . . . .
     . . . . . . . .
     2 . . 0 1 . . 3
     3 . . 1 0 . . 2
     . . . . . . . .
     . . . . . . . .
     . . . 6 5 . . . *)
  [| [ (3, 3); (4, 4) ];
     [ (4, 3); (3, 4) ];
     [ (0, 3); (7, 4) ];
     [ (0, 4); (7, 3) ];
     [ (3, 0); (4, 7) ];
     [ (4, 0); (3, 7) ] |]

let spawn_items_random rng items next_id =
  let rng = rng |> Prng.copy in
  let pos_list = rng |> Prng.rand_pick item_spawn_formations_a in
  let typ_dist = rng |> Prng.rand_pick_weighted Item_type.dist_dist in
  let typ_list = pos_list |> List.map (fun _ -> rng |> Prng.rand_pick_weighted typ_dist) in
  let items, next_id =
    spawn_items items next_id
      pos_list
      typ_list
  in
  rng, items, next_id

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
  match pl.pl_stats |> Player.use_spell_cast with
  | None -> obs, id, pl
  | Some (s, pl_stats') ->
     match s |> Spell_type.cast_points path with
     | []       -> obs,  id,  pl
     | pos_list -> let (obs', id') =
                     pos_list |> spawn_obs obs id s default_ob_turns
                   in
                   obs', id', { pl with pl_stats = pl_stats' }

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
  @@ player_entities_of_phase g.pl0 g.pl1 g.phase

(* attacks and damage *)

let burns obs path0 path1 =
  Attack.{ player_0 = obs |> attacks_from_obs Fire Burn path0;
           player_1 = obs |> attacks_from_obs Fire Burn path1 }

let heals obs path0 path1 =
  Attack.{ player_0 = obs |> attacks_from_obs Life Heal path0;
           player_1 = obs |> attacks_from_obs Life Heal path1 }

let attacks g =
  match g.phase with
  | Damage { atks; _ } -> atks
  | _ -> Attack.empty_set

(* phases, turns *)

let phase_duration g =
  match g.phase with
  | Main _          -> Rules.turn_duration
  | Damage _        -> 1.0
  | Cast            -> 1.0
  | Pick_up         -> 1.0
  | Bouncing _      -> Rules.bounce_time
  | Moving { paths = { path0; path1 }; _ } ->
     max (Path.length path0 /. Rules.move_vel)
       (Path.length path1 /. Rules.move_vel)

let main_phase pl0 pl1 =
  ignore pl1;
  Main { cu = pl0.pl_pos }

let goto_main_phase pl0 pl1 turn_num =
  let turn_num = turn_num + 1 in
  fun g -> { g with
             turn_num;
             phase = main_phase pl0 pl1 }

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

let goto_moving_phase pl0 pl1 map_obs cu =
  (* move via controllers *)
  let pos0, pos1 = pl0.pl_pos, pl1.pl_pos in
  let pos0', ctl0 = pl0.pl_ctl |>
                      Player_controller.pick_move
                        { pos = pos0; opp_pos = pos1; cursor = Some cu }
  and pos1', ctl1 = pl1.pl_ctl |>
                      Player_controller.pick_move
                        { pos = pos1; opp_pos = pos0; cursor = None } in
  (* calculate paths, maybe adjusting from collisions *)
  let int_paths =
    { path0 = Path.from_points ~src:pos0 ~tgt:pos0';
      path1 = Path.from_points ~src:pos1 ~tgt:pos1' } in
  let paths =
    { path0 = int_paths.path0 |> path_collide_with_ice map_obs;
      path1 = int_paths.path1 |> path_collide_with_ice map_obs } in
  (* update player data *)
  let pl0 = { pl0 with pl_ctl = ctl0; pl_pos = paths.path0 |> Path.target }
  and pl1 = { pl1 with pl_ctl = ctl1; pl_pos = paths.path1 |> Path.target } in
  fun g -> { g with
             pl0; pl1;
             phase = Moving { paths; int_paths } }

let all_directions =
  [ -1,-1; 0,-1; 1,-1;
    -1, 0;       1, 0;
    -1, 1; 0, 1; 1, 1 ]

let bounce_positions (x0, y0) =
  all_directions
  |> List.rev_filter_map
       (fun (dx, dy) ->
         let x, y = (x0 + dx, y0 + dy) in
         if x < 0 || y < 0 || x >= Rules.grid_cols || y >= Rules.grid_cols then
           None
         else
           Some((x, y)))

let maybe_goto_bouncing_phase pl0 pl1 paths rng =
  if not @@ Pos.equal pl0.pl_pos pl1.pl_pos then
    None
  else
    let orig_pos = pl0.pl_pos in
    let candidates = bounce_positions orig_pos |> Array.of_list in
    let rng = rng |> Prng.copy in rng |> Prng.rand_shuffle candidates;
    let pl0 = { pl0 with pl_pos = candidates.(0) }
    and pl1 = { pl1 with pl_pos = candidates.(1) } in
    Some(fun g -> { g with
                    rng; pl0; pl1;
                    phase = Bouncing { paths; orig_pos } })

let goto_damage_phase pl0 pl1 paths map_obs =
  (* calculate attacks from path & map collisions *)
  let { path0; path1 } = paths in
  let atks =
    Attack.set_concat
      [ Attack.path_collision path0 path1;
        burns map_obs path0 path1;
        heals map_obs path0 path1 ]
  in
  (* calculate damage *)
  let pl_stats0, pl_stats1 = pl0.pl_stats, pl1.pl_stats in
  let (dmg0, dmg1) = atks |> Attack.damage_of_set pl_stats0 pl_stats1 in
  (* apply damage to update players *)
  let pl0 = { pl0 with pl_stats = pl_stats0 |> Player.take_damage dmg0 }
  and pl1 = { pl1 with pl_stats = pl_stats1 |> Player.take_damage dmg1 } in
  fun g -> { g with
             pl0; pl1;
             phase = Damage { paths; atks } }

let goto_cast_phase pl0 pl1 paths map_obs next_id =
  let map_obs = decay_obs map_obs in
  let map_obs, next_id, pl0 = cast_spell map_obs next_id pl0 paths.path0 in
  let map_obs, next_id, pl1 = cast_spell map_obs next_id pl1 paths.path1 in
  fun g -> { g with
             pl0; pl1; map_obs; next_id;
             phase = Cast }

let goto_pickup_phase pl0 pl1 map_items =
  let pl0 = pl0 |> player_collect_item map_items
  and pl1 = pl1 |> player_collect_item map_items
  and map_items = map_items |> pick_up_items pl0 pl1 in
  fun g -> { g with
             pl0; pl1; map_items;
             phase = Pick_up }

let end_phase g =
  match g.phase with
  | Main { cu } ->
     g |> goto_moving_phase
            g.pl0 g.pl1 g.map_obs cu

  | Moving { paths; _ } ->
     g |> Option.value
            (maybe_goto_bouncing_phase g.pl0 g.pl1 paths g.rng)
            ~default:(goto_damage_phase g.pl0 g.pl1 paths g.map_obs)

  | Bouncing { paths; _ } ->
     g |> goto_damage_phase
            g.pl0 g.pl1 paths g.map_obs

  | Damage { paths; _ } ->
     g |> goto_cast_phase
            g.pl0 g.pl1 paths g.map_obs g.next_id

  | Cast ->
     g |> goto_pickup_phase
            g.pl0 g.pl1 g.map_items

  | Pick_up ->
     g |> goto_main_phase
            g.pl0 g.pl1 g.turn_num

let turn_num { turn_num; _ } =
  turn_num

let turn_duration { phase; _ } =
  match phase with
  | Main _ -> Some Rules.turn_duration
  | _ -> None

(* cursor, paths *)

type path_type =
  | Select_path
  | Player_path

let paths g =
  match g.phase with
  | Main { cu } ->
     let pos0 = g.pl0.pl_pos in
     [ Path.from_points ~src:pos0 ~tgt:cu, Select_path ]
  | Moving { int_paths = { path0; path1; }; _ } ->
     [ (path0, Player_path);
       (path1, Player_path) ]
  | Damage _ | Bouncing _ | Cast | Pick_up ->
     (* TODO: another path type for some of these phases *)
     []

let grid_clamp x =
  x |> max 0 |> min (Rules.grid_cols - 1)

let modify_cursor f g =
  match g.phase with
  | Main { cu } ->
     { g with phase = Main { cu = f g.pl0.pl_pos cu } }
  | _ ->
     g

let cursor g =
  match g.phase with
  | Main { cu } -> Some(cu)
  | _ -> None

let move_cursor_by dx dy =
  modify_cursor
    (fun _ (x, y) ->
      (grid_clamp (x + dx),
       grid_clamp (y + dy)))

let reset_cursor =
  modify_cursor
    (fun pos _ ->
      pos)

(* init *)

let make ~player_ctrl_0:ctl0 ~player_ctrl_1:ctl1 =
  let rng = Prng.make ~seed:14
  and map_items, map_obs, next_id = [], [], 2 in
  let rng, map_items, next_id = spawn_items_random rng map_items next_id in
  let rng, map_items, next_id = spawn_items_random rng map_items next_id in
  let pl0 = { pl_stats = Player.make ~color:0;
              pl_pos = (3, 5);
              pl_ctl = ctl0 }
  and pl1 = { pl_stats = Player.make ~color:1;
              pl_pos = (6, 7);
              pl_ctl = ctl1 } in

  { rng;
    turn_num = 1;
    phase = main_phase pl0 pl1;
    pl0; pl1;
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
