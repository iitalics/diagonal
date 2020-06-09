type t
val make: int -> t
val n_samples: t -> int
val average: t -> float option
val push: float -> t -> unit
