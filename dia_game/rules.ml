let fps = 20                    (* unit: F/s *)
let fps_fl = float_of_int fps   (* unit: F/s *)
let frame_time = 1. /. fps_fl   (* unit: s/F *)

let turn_frames = fps * 2       (* unit: F *)
let grid_cols = 8               (* unit: cells *)

let move_rate = 5.              (* unit: F/cells *)
let move_vel  = 1. /. move_rate (* unit: cells/F *)
