type t =
  | Weapon of Weapon_type.t
  | Spell of Spell_type.t

let dist_dist =
  [ List.map (fun (x, f) -> Weapon x, f) Weapon_type.dist, 1;
    List.map (fun (x, f) -> Spell x, f) Spell_type.dist,   1 ]
