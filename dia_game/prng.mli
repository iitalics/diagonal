type t
type 'a gen = t -> 'a

val make: seed:int -> t
val copy: t -> t

val rand_int: max:int -> int gen
val rand_pick: 'a array -> 'a gen
val rand_pick_weighted: ('a * int) list -> 'a gen
val rand_shuffle: 'a array -> unit gen
