module Dom = Js_of_ocaml.Dom
module Dom_html = Js_of_ocaml.Dom_html
module Js = Js_of_ocaml.Js
module Affine = Dia_ui.Affine

let _ALPHABETIC = Js.string "alphabetic"
let _BLACK      = Js.string "#000"
let _LEFT       = Js.string "left"
let _TOP        = Js.string "top"

let dummy_ctxt =
  lazy ((Dom_html.createCanvas Dom_html.document)
          ##getContext Dom_html._2d_)

(* colors *)

module Color = struct
  type t =
    { rgb: Js.js_string Js.t;
      a: float }

  let none: t =
    { rgb = _BLACK; a = 0. }

  let of_rgb_s (s: string): t =
    { rgb = Js.string s; a = 1. }

  let[@ocaml.inline] with_alpha a' { rgb; a } =
    { rgb; a = a *. a' }
end

(* fonts *)

module Font = struct
  type t = Js.js_string Js.t

  let make ~fam ~size =
    Js.string @@ Printf.sprintf "%dpx %s" size fam

  let measure str (font: t) =
    let cx = Lazy.force dummy_ctxt in
    cx##.font := font;
    cx##.textBaseline := _ALPHABETIC;
    let mes = cx##measureText (Js.string str) |> Js.Unsafe.coerce in
    (int_of_float mes##.width, int_of_float mes##.actualBoundingBoxAscent)
end

(* images *)

module Image = struct
  type t =
    Dom_html.imageElement Js.t

  let size (im: t) =
    (im##.width, im##.height)
end

(* drawing context *)

module Ctxt = struct
  type t = Dom_html.canvasRenderingContext2D Js.t

  let transform t (cx: t) =
    t |> Affine.iter'
           (fun a b c d e f ->
             (* NOTE: notice order is different :S *)
             cx##transform a d b e c f)

  let reset (cx: t) =
    cx##setTransform 1. 0. 0. 1. 0. 0.;
    cx##.globalAlpha := 1.

  let size (cx: t) =
    (cx##.canvas##.width, cx##.canvas##.height)

  let clear ~c (cx: t) =
    let (w, h) = cx |> size in
    cx##.fillStyle := c.Color.rgb;
    cx##fillRect 0. 0. (float_of_int w) (float_of_int h)

  type op = ?t:Affine.t -> t -> unit

  let[@ocaml.inline] op f : op =
    fun ?t cx ->
    cx |> transform t; f cx; cx |> reset

  let set_fill c cx =
    cx##.fillStyle := c.Color.rgb;
    cx##.globalAlpha := c.Color.a

  let set_stroke c cx =
    cx##.strokeStyle := c.Color.rgb;
    cx##.globalAlpha := c.Color.a

  let text ~x ~y ~font ~c str =
    op @@ fun cx ->
          cx |> set_fill c;
          cx##.font := font;
          cx##.textAlign := _LEFT;
          cx##.textBaseline := _TOP;
          cx##fillText (Js.string str) (float_of_int x) (float_of_int y)

  let image ~x ~y ~sx ~sy ~w ~h elem =
    op @@ fun cx ->
          cx##drawImage_full elem
            (float_of_int sx) (float_of_int sy) (float_of_int w) (float_of_int h)
            (float_of_int x) (float_of_int y) (float_of_int w) (float_of_int h)

  let vertices ~xs ~ys ~c mode =
    op @@ fun cx ->
          let is_fill = (mode = `Fill) in
          if not is_fill then cx##translate 0.5 0.5;
          cx##beginPath;
          xs |> Array.iteri (fun i x ->
                    let x, y = float_of_int x, float_of_int ys.(i) in
                    if (i = 0) || (mode = `Lines && (i mod 2) = 0) then
                      cx##moveTo x y
                    else
                      cx##lineTo x y);
          if is_fill then
            ( cx |> set_fill c; cx##fill )
          else
            ( cx |> set_stroke c; cx##stroke )
end
