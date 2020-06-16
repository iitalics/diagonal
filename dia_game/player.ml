type t = { pos: Pos.t;
           anim: anim }

and anim =
  | No_anim
  | Moving of Path.t

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

let anim_frames an =
  match an with
  | No_anim -> 0
  | Moving pa -> int_of_float @@
                   ceil (Path.length pa *. Rules.move_rate)
