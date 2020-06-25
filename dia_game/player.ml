type t =
  { hp: int;
    color: int;
    weapon: Weapon_type.t }

let make ~color =
  { color;
    hp = Rules.max_hp;
    weapon = Dagger }

let take_damage dmg pl =
  { pl with hp = max 0 (pl.hp - dmg) }

let pick_up_weapon weapon pl =
  { pl with weapon }
