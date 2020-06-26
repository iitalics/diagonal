type t =
  { hp: int;
    color: int;
    weapon: Weapon_type.t;
    spell: Spell_type.t option;
    casts: int }

let make ~color =
  { color;
    hp = Rules.max_hp;
    weapon = Dagger;
    spell = None;
    casts = 0 }

let take_damage dmg pl =
  { pl with hp = max 0 (pl.hp - dmg) }

let pick_up item pl =
  match item with
  | Item_type.Weapon w -> { pl with weapon = w }
  | Item_type.Spell s  -> { pl with spell = Some s; casts = 0 }

let use_spell_cast pl =
  match pl.spell with
  | None   -> None, pl
  | Some s -> Some s,
              (let casts = pl.casts + 1 in
               if casts >= Rules.max_casts then
                 { pl with spell = None }
               else
                 { pl with casts })
