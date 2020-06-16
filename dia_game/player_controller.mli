module type S =
  sig
    type t
    val set_cursor: Pos.t -> t -> t
    val commit_turn: Player.t -> t -> (Player.t * t)
  end

include S

type 'a s = (module S with type t = 'a)
val make: 'a s -> 'a -> t

module User_ctrl: S
module No_ctrl: S

val user_ctrl: t
val no_ctrl: t
