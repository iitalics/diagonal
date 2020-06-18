type anim =
  | No_anim
  | Moving of Path.t

let anim_duration = function
  | No_anim -> 0.
  | Moving path -> (path |> Path.length)
                   /. Rules.move_vel

type t =
  { pos: Pos.t;
    anim: anim }

let make pos =
  { pos; anim = No_anim }

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
