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

let name = function
  | Shovel -> "shovel"
  | Dagger -> "dagger"
  | Sword  -> "sword"
  | Rapier -> "rapier"
  | Staff  -> "staff"

let atk = function
  | Shovel -> 1
  | Dagger -> 2
  | Sword  -> 3
  | Rapier -> 5
  | Staff  -> 2

let crit_bonus = function
  | Shovel
    | Dagger
    | Sword
    | Rapier -> 1
  | Staff    -> 2
