type input =
  { pos: Pos.t;
    opp_pos: Pos.t;
    cursor: Pos.t option }

module type S =
  sig
    type t
    val pick_move: input -> t -> Pos.t * t
  end

(* abstract controller *)

type 'a s = (module S with type t = 'a)

type t = PC: 'a s * 'a -> t

let make s c = PC(s, c)

let pick_move inp (PC((module C), c)) =
  let (pos, c) = c |> C.pick_move inp in
  pos, PC((module C), c)

(* controllers *)

module User_ctrl = struct
  type t = unit
  let pick_move { pos; cursor; _ } () =
    match cursor with
    | None     -> pos, ()
    | Some(cu) -> cu,  ()
end

let user_ctrl = make (module User_ctrl) ()

module No_ctrl = struct
  type t = unit
  let pick_move { pos; _ } () = pos, ()
end

let no_ctrl = make (module No_ctrl) ()

module Bot_ctrl = struct
  type t = Prng.t
  let pick_move _ rng =
    let rng = rng |> Prng.copy in
    let x = rng |> Prng.rand_int ~max:Rules.grid_cols in
    let y = rng |> Prng.rand_int ~max:Rules.grid_cols in
    (x, y), rng
end

let bot_ctrl rng = make (module Bot_ctrl) rng

module Auto_ctrl = struct
  type t =
    { queue: Pos.t list;
      steps: Pos.t list }

  let pick_move _ = function
    | ({ queue = p :: xs; _ } |
         { steps = p :: xs; _ }) as t
      ->
       p, { t with queue = xs }
    | { steps = []; _ }
      -> failwith "Auto_ctrl.pick_move: steps is empty!"
end

let auto_ctrl steps = make (module Auto_ctrl) { queue = []; steps }
