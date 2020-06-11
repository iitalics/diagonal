type t
val make: unit -> t

val tick: t -> t
val key_dn: Input.Key.t -> t -> t
val key_up: Input.Key.t -> t -> t

val cursor: t -> int * int
val path: t -> Path.t option
val turn_num: t -> int
val turn_frame: t -> int
