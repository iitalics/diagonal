let fps = 20                    (* unit: F/s *)
let fps_fl = float_of_int fps   (* unit: F/s *)
let frame_time = 1. /. fps_fl   (* unit: s/F *)

let turn_frames = fps * 2       (* unit: F *)
let grid_cols = 8               (* unit: cell *)

let move_rate = 4.              (* unit: F/cell *)
