type anim =
  | No_anim
  | Moving of Path.t

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

let path pl =
  match pl.anim with
  | No_anim -> Path.null ~src:pl.pos
  | Moving pa -> pa
