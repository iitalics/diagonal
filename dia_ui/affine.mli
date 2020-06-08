type t

type 'a f6_fn = float -> float -> float -> float -> float -> float -> 'a
type 'a f2_fn = float -> float -> 'a

val make: t option -> t
val iter: unit f6_fn -> t -> unit
val iter': unit f6_fn -> t option -> unit
(*val to_matrix: t -> float * float * float * float * float * float *)

val reset: t -> unit
val transform: (t -> unit) f6_fn
val scale: (t -> unit) f2_fn
val translate: (t -> unit) f2_fn
val rotate: float -> t -> unit

val apply: (t -> float * float) f2_fn
val ( |> ): float * float -> t -> float * float