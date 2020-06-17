module type S =
  sig
    type t
    val set_cursor: Pos.t -> t -> t
    val commit_turn: Player.t -> t -> (Player.t * t)
  end

include S

type 'a s = (module S with type t = 'a)
val make: 'a s -> 'a -> t

val user_ctrl: t
val no_ctrl: t
val bot_ctrl: Prng.t -> t