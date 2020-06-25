type t =
  { id: id;
    typ: typ }

and id = int

and typ =
  | Blob_idle of Player.t * Pos.t
  | Blob_moving of Player.t * Path.t
  | Item of Weapon_type.t * Pos.t

module Id = struct
  type nonrec t = id
  let compare = Int.compare
end

module Typ = struct
  type nonrec t = typ
end

let compare_id { id = i; _ } { id = j; _ } =
  Id.compare i j
