open Dia_util

type t =
  { pos: Pos.t;
    typ: typ }

and typ =
  | Crit
  | Attk
  | Burn
  | Heal

type set =
  (* player_i = attacks that are dealing damage *to* player 'i' *)
  { player_0: t list;
    player_1: t list }

let empty_set =
  { player_0 = [];
    player_1 = [] }

let collision pt0 pt1 =
  let one_sided (pt0: Path.point_type) (pt1: Path.point_type) f k =
    match pt0, pt1 with
    | Crit, Vuln              -> f Crit
    | Crit, Attk | Attk, Vuln -> f Attk
    | _                       -> k
  in
  `No
  |> one_sided pt0 pt1 (fun x -> `L x)
  |> one_sided pt1 pt0 (fun x -> `R x)

let damage_of_typ pl typ =
  let w = pl.Player.prm.weapon in
  match typ with
  | Crit -> Weapon_type.atk w + Weapon_type.crit_bonus w
  | Attk -> Weapon_type.atk w
  | Burn -> Spell_type.burn_hp
  | Heal -> - Spell_type.heal_hp

let path_collision path0 path1 =
  let cons_0 a { player_0; player_1 } = { player_0 = a :: player_0; player_1 } in
  let cons_1 a { player_0; player_1 } = { player_0; player_1 = a :: player_1 } in
  let compare (pos1, _) (pos2, _) = Pos.compare pos1 pos2 in
    List.merge_fold ~compare
      (fun acc _ -> acc)
      (fun acc _ -> acc)
      (fun acc (pos, typ0) (_, typ1) ->
        match collision typ0 typ1 with
        | `L typ -> cons_1 { pos; typ } acc
        | `R typ -> cons_0 { pos; typ } acc
        | `No          -> acc)
      empty_set
      (path0 |> Path.points |> List.sort compare)
      (path1 |> Path.points |> List.sort compare)

let damage_of_set pl0 pl1 s =
  let dmg pl ts = ts |> List.sum_by (fun { typ; _ } -> damage_of_typ pl typ) in
  (dmg pl0 s.player_0,
   dmg pl1 s.player_1)

let set_concat = function
  | []       -> empty_set
  | s0 :: ss -> List.fold_left
                  (fun
                     { player_0 = p0;  player_1 = p1 }
                     { player_0 = p0'; player_1 = p1' }
                   ->
                    { player_0 = List.rev_append p0' p0;
                      player_1 = List.rev_append p1' p1 })
                  s0 ss
