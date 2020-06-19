module type S =
  sig
    type t
    val commit_turn: ?cursor:Pos.t -> Player.t -> t -> (Player.t * t)
  end

(* abstract controller *)

type 'a s = (module S with type t = 'a)

type t = PC: 'a s * 'a -> t

let make s c = PC(s, c)

let commit_turn ?cursor pl (PC((module C), c)) =
  let pl', c' = c |> C.commit_turn pl ?cursor in
  pl', PC((module C), c')

(* controllers *)

module User_ctrl = struct
  type t = unit
  let commit_turn ?cursor pl () =
    match cursor with
    | None     -> pl,                      ()
    | Some(cu) -> pl |> Player.move_to cu, ()
end

let user_ctrl = make (module User_ctrl) ()

module No_ctrl = struct
  type t = unit
  let commit_turn ?cursor:_ pl _ = pl, ()
end

let no_ctrl = make (module No_ctrl) ()

module Bot_ctrl = struct
  type t = Prng.t
  let commit_turn ?cursor:_ pl rng =
    let rng = rng |> Prng.copy in
    let x = rng |> Prng.rand_int ~max:Rules.grid_cols in
    let y = rng |> Prng.rand_int ~max:Rules.grid_cols in
    pl |> Player.move_to (x, y), rng

end

let bot_ctrl rng = make (module Bot_ctrl) rng
