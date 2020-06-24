type t =
  | Fire
  | Ice
  | Life

let to_int = function
  | Fire -> 0
  | Ice  -> 1
  | Life -> 2
