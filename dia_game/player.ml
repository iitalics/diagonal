type t =
  { hp: int;
    color: int;
    weapon: Weapon_type.t;
    spell: Spell_type.t option }

let make ~color =
  { color;
    hp = Rules.max_hp;
    weapon = Dagger;
    spell = (match color with
             | 0 -> Some Life
             | 1 -> Some Ice
             | _ -> None) }

let take_damage dmg pl =
  { pl with hp = max 0 (pl.hp - dmg) }

let pick_up_weapon weapon pl =
  { pl with weapon }
