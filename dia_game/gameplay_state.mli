type t
val make: unit -> t

val tick: t -> t
val key_dn: Input.Key.t -> t -> t
val key_up: Input.Key.t -> t -> t

val cursor: t -> int * int
