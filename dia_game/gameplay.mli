type t

type path_type =
  | Select_path
  | Player_path

type hits =
  { hits_player_0: hit list;
    hits_player_1: hit list }

and hit =
  { hit_pos: Pos.t;
    hit_mark: Path.mark }

val make: player_ctrl_0:Player_controller.t
          -> player_ctrl_1:Player_controller.t
          -> t

val turn: t -> int
val player_0: t -> Player.t
val player_1: t -> Player.t
val cursor: t -> Pos.t option
val paths: t -> (Path.t * path_type) list
val hits: t -> hits

val phase_duration: t -> float
val end_phase: t -> t

val key_dn: Input.Key.t -> t -> t
val key_up: Input.Key.t -> t -> t
