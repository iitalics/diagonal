module Dom_html = Js_of_ocaml.Dom_html
module Js = Js_of_ocaml.Js

let _LEFT = Js.string "left"
let _TOP = Js.string "top"
let _ALPHABETIC = Js.string "alphabetic"

let dummy_ctxt =
  lazy ((Dom_html.createCanvas Dom_html.document)
          ##getContext Dom_html._2d_)

module Color = struct
  type t = Js.js_string Js.t Js.opt
  let none: t = Js.null
  let of_rgb_s (s: string): t = Js.some (Js.string s)
end

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

module Ctxt = struct
  type t = Dom_html.canvasRenderingContext2D Js.t

  let size (cx: t) =
    (cx##.canvas##.width, cx##.canvas##.height)

  let clear ~f (cx: t) =
    let (w, h) = cx |> size in
    Js.Opt.case f
      (fun () -> cx##clearRect 0. 0. (float_of_int w) (float_of_int h))
      (fun f -> cx##.fillStyle := f; cx##fillRect 0. 0. (float_of_int w) (float_of_int h))

  let text ~x ~y ~font ~f str (cx: t) =
    Js.Opt.iter f
      (fun f ->
        cx##.fillStyle := f;
        cx##.font := font;
        cx##.textAlign := _LEFT;
        cx##.textBaseline := _TOP;
        cx##fillText (Js.string str) (float_of_int x) (float_of_int y))
end
