module Id = struct
  type nonrec t = int
  let compare = Int.compare
end

module Id_set = Set.Make(Id)

type typ =
  | Blob_idle of Player.t * Pos.t
  | Blob_moving of Player.t * Path.t
  | Item of Item_type.t * Pos.t
  | Obstacle of Spell_type.t * Pos.t

module Typ = struct
  type nonrec t = typ
end

type t =
  { id: Id.t;
    typ: typ }

let compare_id { id = i; _ } { id = j; _ } =
  Id.compare i j
