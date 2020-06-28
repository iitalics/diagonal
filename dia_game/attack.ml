open Dia_util

type t =
  { pos: Pos.t;
    typ: typ }

and typ =
  | Crit
  | Attk

type set =
  { player_0: t list;
    player_1: t list }

let empty_set =
  { player_0 = [];
    player_1 = [] }

let collision (typ0: Path.point_type) (typ1: Path.point_type) =
  match typ0, typ1 with
  | Crit, Vuln -> `P0 Crit
  | Vuln, Crit -> `P1 Crit
  | Crit, Attk | Attk, Vuln -> `P0 Attk
  | Attk, Crit | Vuln, Attk -> `P1 Attk
  | Crit, Crit | Attk, Attk | Vuln, Vuln -> `No

let damage Player.{ weapon; _ } = function
  | Crit -> (weapon |> Weapon_type.atk) + (weapon |> Weapon_type.crit_bonus)
  | Attk -> (weapon |> Weapon_type.atk)

let path_collision path0 path1 =
  let cons_0 a { player_0; player_1 } = { player_0 = a :: player_0; player_1 } in
  let cons_1 a { player_0; player_1 } = { player_0; player_1 = a :: player_1 } in
  let compare (pos1, _) (pos2, _) = Pos.compare pos1 pos2 in
    List.merge_fold ~compare
      (fun acc _ -> acc)
      (fun acc _ -> acc)
      (fun acc (pos, typ0) (_, typ1) ->
        match collision typ0 typ1 with
        | `P0 typ -> cons_0 { pos; typ } acc
        | `P1 typ -> cons_1 { pos; typ } acc
        | `No          -> acc)
      empty_set
      (path0 |> Path.points |> List.sort compare)
      (path1 |> Path.points |> List.sort compare)

let damage_of_set pl0 pl1 s =
  let dmg pl ts = ts |> List.sum_by (fun { typ; _ } -> typ |> damage pl) in
  (dmg pl0 s.player_0,
   dmg pl1 s.player_1)
