module Equip = struct
  type t =
    { weapon: Weapon_type.t;
      spell: Spell_type.t option;
      casts_left: int }

  let default =
    { weapon = Dagger;
      spell = None;
      casts_left = 0 }

  let normalize t =
    if t.casts_left < 0
       || (t.casts_left = 0 && not (Weapon_type.extra_spell_cast t.weapon))
    then
      { t with spell = None }
    else
      t

  let set_spell ?(casts=Rules.max_casts) s t =
    if Option.is_some s then
      { t with spell = s; casts_left = casts }
    else
      t

  let set_weapon w t =
    normalize { t with weapon = w }

  let use_cast t =
    normalize { t with casts_left = t.casts_left - 1 }
end

type t =
  { hp: int;
    color: int;
    prm: Equip.t;
    alt: Equip.t }

let make ~color =
  { color;
    hp = Rules.max_hp;
    prm = Equip.default;
    alt = Equip.default }

let take_damage dmg pl =
  { pl with hp = max 0 (pl.hp - dmg) }

let pick_up item pl =
  match item with
  | Item_type.Weapon w ->
     { pl with prm = pl.prm |> Equip.set_weapon w;
               alt = pl.alt |> Equip.set_weapon pl.prm.weapon }
  | Item_type.Spell s  ->
     { pl with prm = pl.prm |> Equip.set_spell (Some s);
               alt = pl.alt |> Equip.set_spell pl.prm.spell
                                 ~casts:pl.prm.casts_left }

let use_spell_cast pl =
  match pl.prm.spell with
  | Some s -> Some (s, { pl with prm = pl.prm |> Equip.use_cast })
  | None   -> None
