module Turn: sig
  type t = { num: int; frame: int }
end

type t

type path_type =
  | Select_path
  | Player_path

val make: player_ctrl_0:Player_controller.t
          -> player_ctrl_1:Player_controller.t
          -> t

val tick: t -> t
val key_dn: Input.Key.t -> t -> t
val key_up: Input.Key.t -> t -> t

val turn: t -> Turn.t
val player_0: t -> Player.t
val player_1: t -> Player.t
val cursor: t -> Pos.t option
val paths: t -> (Path.t * path_type) list
