type t =
  { hp: int;
    color: int;
    item: Item_type.t }

let make ~color =
  { color;
    hp = Rules.max_hp;
    item = Dagger }

let take_damage dmg pl =
  { pl with hp = max 0 (pl.hp - dmg) }
