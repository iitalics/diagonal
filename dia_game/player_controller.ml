module type S =
  sig
    type t
    val set_cursor: Pos.t -> t -> t
    val commit_turn: Player.t -> t -> (Player.t * t)
  end

(* abstract controller *)

type 'a s = (module S with type t = 'a)

type t = PC: 'a s * 'a -> t

let make s c = PC(s, c)

let set_cursor cu (PC((module C), c)) =
  let c' = c |> C.set_cursor cu in
  PC((module C), c')

let commit_turn pl (PC((module C), c)) =
  let pl', c' = c |> C.commit_turn pl in
  pl', PC((module C), c')

(* controllers *)

module User_ctrl = struct
  type t = Pos.t option

  let set_cursor cu _ = Some(cu)

  let commit_turn pl = function
    | None     -> pl,                      None
    | Some(cu) -> pl |> Player.move_to cu, None
end

let user_ctrl = make (module User_ctrl) None

module No_ctrl = struct
  type t = unit
  let set_cursor _ _ = ()
  let commit_turn pl _ = pl, ()
end

let no_ctrl = make (module No_ctrl) ()
