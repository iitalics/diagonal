type t

type 'a f6_fn = float -> float -> float -> float -> float -> float -> 'a
type 'a f2_fn = float -> float -> 'a
type 'a f1_fn = float -> 'a
type 'a i2_fn = int -> int -> 'a
type 'a i1_fn = int -> 'a

(* constructors *)

val make:   unit -> t
val extend: t    -> t

(* iterators / accessors *)

val iter:  unit f6_fn -> t        -> unit
val iter': unit f6_fn -> t option -> unit
(*val to_matrix: t -> float * float * float * float * float * float *)

val apply: (t -> float * float) f2_fn
val ( |> ): float * float -> t -> float * float

(* mutators *)

val reset:        t -> unit
val zero:         t -> unit
val transform:    (t -> unit) f6_fn
val scale:        (t -> unit) f2_fn
val scale':       (t -> unit) f1_fn
val translate:    (t -> unit) f2_fn
val rotate:       (t -> unit) f1_fn

val translate_i: (t -> unit) i2_fn
val rotate_deg:  (t -> unit) i2_fn
