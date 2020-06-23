type input =
  { pos: Pos.t;
    opp_pos: Pos.t;
    cursor: Pos.t option }

module type S =
  sig
    type t
    val pick_move: input -> t -> Pos.t * t
  end

include S

type 'a s = (module S with type t = 'a)
val make: 'a s -> 'a -> t

val user_ctrl: t
val no_ctrl: t
val bot_ctrl: Prng.t -> t
val auto_ctrl: Pos.t list -> t
