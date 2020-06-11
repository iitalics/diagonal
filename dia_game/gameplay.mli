type t

type turn =
  { tn_num: int;
    tn_frame: int }

type player =
  { pl_x: int;
    pl_y: int }

val make: unit -> t

val tick: t -> t
val key_dn: Input.Key.t -> t -> t
val key_up: Input.Key.t -> t -> t

val cursor: t -> (int * int) option
val path: t -> Path.t option
val turn: t -> turn
val player_0: t -> player
val player_1: t -> player
