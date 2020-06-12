module Turn: sig
  type t = { num: int; frame: int }
end

module Pos: sig
  type t = int * int
end

module Player: sig
  type t = { pos: Pos.t;
             anim: anim }

  and anim =
    | No_anim
    | Moving of Path.t
end

type t

val make: unit -> t

val tick: t -> t
val key_dn: Input.Key.t -> t -> t
val key_up: Input.Key.t -> t -> t

val turn: t -> Turn.t
val player_0: t -> Player.t
val player_1: t -> Player.t
val cursor: t -> Pos.t option
val path: t -> Path.t option
