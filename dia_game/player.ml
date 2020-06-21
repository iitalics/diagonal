type anim =
  | No_anim
  | Moving of Path.t

type t =
  { pos: Pos.t;
    hp: int;
    anim: anim }

let make pos =
  { pos;
    hp = Rules.max_hp;
    anim = No_anim }

let stop_moving pl =
  match pl.anim with
  | No_anim   -> pl
  | Moving pa -> make (pa |> Path.target)

let move_to pos' pl =
  let pl = stop_moving pl in
  { pl with
    anim = Moving (Path.from_points
                     ~src:pl.pos
                     ~tgt:pos') }

let take_damage dmg pl =
  { pl with hp = max 0 (pl.hp - dmg) }

let path pl =
  match pl.anim with
  | No_anim -> Path.null ~src:pl.pos
  | Moving pa -> pa
