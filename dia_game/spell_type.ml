type t =
  | Fire
  | Ice
  | Life

let to_int = function
  | Fire -> 0
  | Ice  -> 1
  | Life -> 2

let cast_points path = function
  | Fire -> path |> Path.diagonal
  | Ice  -> path |> Path.straight
  | Life -> path |> Path.knee |> Option.to_list

let dist =
  [ Fire, 3;
    Ice,  2;
    Life, 2 ]

let burn_hp = 1
let heal_hp = 3
