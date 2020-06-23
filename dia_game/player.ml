type t =
  { hp: int;
    color: int }

let make ~color =
  { color;
    hp = Rules.max_hp }

let take_damage dmg pl =
  { pl with hp = max 0 (pl.hp - dmg) }
