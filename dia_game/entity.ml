module Id = struct
  type nonrec t = int
  let compare = Int.compare
end

module Id_set = Set.Make(Id)

type t =
  { id: Id.t;
    typ: typ }

and typ =
  | Blob_idle of Player.t * Pos.t
  | Blob_moving of Player.t * Path.t
  | Blob_bounce of Player.t * Pos.t * Pos.t
  | Item of Item_type.t * Pos.t
  | Obstacle of Spell_type.t * Pos.t * bool

let compare_id { id = i; _ } { id = j; _ } =
  Id.compare i j
