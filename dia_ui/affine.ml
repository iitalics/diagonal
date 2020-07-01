type 'a f6_fn = float -> float -> float -> float -> float -> float -> 'a
type 'a f2_fn = float -> float -> 'a
type 'a f1_fn = float -> 'a
type 'a i2_fn = int -> int -> 'a
type 'a i1_fn = int -> 'a

type t =
  { mutable m11: float; mutable m12: float; mutable m13: float;
    mutable m21: float; mutable m22: float; mutable m23: float;
    parent: t option }

let make' parent =
  { m11 = 1.; m12 = 0.; m13 = 0.;
    m21 = 0.; m22 = 1.; m23 = 0.;
    parent }

let[@ocaml.inline] make   _ = make' None
let[@ocaml.inline] extend t = make' (Some t)

let rec iter' f = function
  | None -> ()
  | Some t ->
     iter' f t.parent;
     f t.m11 t.m12 t.m13
       t.m21 t.m22 t.m23

let[@ocaml.inline] iter f t =
  iter' f (Some t)

let[@ocaml.inline] reset t =
  t.m11 <- 1.; t.m12 <- 0.; t.m13 <- 0.;
  t.m21 <- 0.; t.m22 <- 1.; t.m23 <- 0.

let[@ocaml.inline] zero t =
  t.m11 <- 0.; t.m12 <- 0.; t.m13 <- 0.;
  t.m21 <- 0.; t.m22 <- 0.; t.m23 <- 0.

let[@ocaml.inline] transform n11 n12 n13 n21 n22 n23 t =
  (*
    [ m11 m12 m13 ]   [ n11 n12 n13 ]
    [ m21 m22 m23 ] x [ n21 n22 n23 ]
    [   0   0   1 ]   [   0   0   1 ]
   *)
  let p11 = t.m11*.n11 +. t.m12*.n21 in
  let p12 = t.m11*.n12 +. t.m12*.n22 in
  let p13 = t.m11*.n13 +. t.m12*.n23 +. t.m13 in
  t.m11 <- p11; t.m12 <- p12; t.m13 <- p13;
  let p21 = t.m21*.n11 +. t.m22*.n21 in
  let p22 = t.m21*.n12 +. t.m22*.n22 in
  let p23 = t.m21*.n13 +. t.m22*.n23 +. t.m23 in
  t.m21 <- p21; t.m22 <- p22; t.m23 <- p23

let[@ocaml.inline] scale sx sy t =
  t |> transform
         sx 0. 0.
         0. sy 0.

let[@ocaml.inline] scale' s t =
  t |> transform
         s  0. 0.
         0. s  0.

let[@ocaml.inline] translate dx dy t =
  t |> transform
         1. 0. dx
         0. 1. dy

let[@ocaml.inline] rotate r t =
  let sn, cs = sin r, cos r in
  t |> transform
         cs (-. sn) 0.
         sn      cs 0.

let[@ocaml.inline] translate_i dx dy t =
  t |> translate (float_of_int dx) (float_of_int dy)

let rotate_deg _r _t =
  failwith "Affine.rotate_deg: unimplemented"

let[@ocaml.inline] apply x y t =
  (x *. t.m11 +. y *. t.m12 +. t.m13,
   x *. t.m21 +. y *. t.m22 +. t.m23)

  (*
let to_matrix t =
  let t' = make () in
  t |> iter (fun n11 n12 n13 n21 n22 n23 -> t' |> transform n11 n12 n13 n21 n22 n23);
  (t'.m11, t'.m12, t'.m13, t'.m21, t'.m22, t'.m23)
   *)

let[@ocaml.inline] ( |> ) (x, y) t =
  apply x y t
