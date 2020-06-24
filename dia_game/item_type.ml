type t =
  | Shovel
  | Dagger
  | Sword
  | Rapier
  | Staff

let to_int = function
  | Shovel -> 0
  | Dagger -> 1
  | Sword  -> 2
  | Rapier -> 3
  | Staff  -> 4
