module Dom = Js_of_ocaml.Dom
module Dom_html = Js_of_ocaml.Dom_html
module Js = Js_of_ocaml.Js

let _LEFT = Js.string "left"
let _TOP = Js.string "top"
let _ALPHABETIC = Js.string "alphabetic"

let dummy_ctxt =
  lazy ((Dom_html.createCanvas Dom_html.document)
          ##getContext Dom_html._2d_)

(* colors *)

module Color = struct
  type t = Js.js_string Js.t Js.opt
  let none: t = Js.null
  let of_rgb_s (s: string): t = Js.some (Js.string s)
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
    { elem: Dom_html.imageElement Js.t;
      sx: int; sy: int; sw: int; sh: int }

  let make elem =
    { elem;
      sx = 0;
      sy = 0;
      sw = elem##.width;
      sh = elem##.height }

  let clip ~x ~y ~w ~h { elem; sx; sy; sw; sh } =
    { elem;
      sx = sx + x;
      sy = sy + y;
      sw = min (sw - x) w;
      sh = min (sh - y) h }
end

(* drawing context *)

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

  let image ~x ~y ~w ~h { Image.elem; sx; sy; sw; sh } (cx: t) =
    cx##drawImage_full elem
      (float_of_int x) (float_of_int y) (float_of_int w) (float_of_int h)
      (float_of_int sx) (float_of_int sy) (float_of_int sw) (float_of_int sh)
end

(* resources *)

type 'a rsrc_stat =
  [ `Loading | `Done of 'a | `Error of string ]

module Rsrc_data = struct
  type t =
    | Img of Dom_html.imageElement Js.t * unit rsrc_stat ref
    | Font of Js.js_string Js.t

  type sink = t list ref

  let img ~path =
    let elem = Dom_html.createImg Dom_html.document in
    let stat = ref `Loading in
    ignore @@
      Dom.addEventListener elem
        Dom_html.Event.load
        (Dom.handler (fun _ -> stat := `Done (); Js._true))
        Js._false;
    ignore @@
      Dom.addEventListener elem
        Dom_html.Event.error
        (Dom.handler (fun _ -> stat := `Error path; Js._true))
        Js._false;
    elem##.src := Js.string path;
    Img(elem, stat)

  let font ~family ~size =
    Font(Js.string @@ Printf.sprintf "%dpx %S" size family)

  let is_done = function
    | Img (_, stat) -> !stat <> `Loading
    | Font _        -> true (* TODO: check if font is loaded *)

  let make_sink () = ref []
  let push v vs = vs := v :: !vs
  let pop vs = match !vs with
    | v :: vs' -> vs := vs'; v
    | [] -> failwith "sink empty"

  let all_done vs =
    List.for_all is_done !vs
end

module Rsrc_spec = struct
  type t =
    | Img of string
    | Font of string * int

  let init = function
    | Img(path)          -> Rsrc_data.img ~path
    | Font(family, size) -> Rsrc_data.font ~family ~size

  let to_string = function
    | Img(p)     -> Printf.sprintf "image %S" p
    | Font(f, s) -> Printf.sprintf "font %s:%d" f s
end

module Rsrc_cache = struct
  type t = (Rsrc_spec.t, Rsrc_data.t) Hashtbl.t

  let make (): t = Hashtbl.create 60

  let get rs c =
    match Hashtbl.find_opt c rs with
    | Some(rd) -> rd
    | None     ->
       let rd = rs |> Rsrc_spec.init in
       Printf.printf "== loading %s ==\n" (rs |> Rsrc_spec.to_string);
       Hashtbl.add c rs rd; rd
end

module Rsrc = struct
  class type ['a] t =
    object
      method init: Rsrc_cache.t -> Rsrc_data.sink -> unit
      method build: Rsrc_data.sink -> 'a
    end

  let const (x: 'a) : 'a t = object
      method init _ _ = ()
      method build _ = x
    end

  let map f (r: 'a t) : 'b t = object
      method init c s = r#init c s
      method build s = f (r#build s)
    end

  let map2 f (rx: 'a t) (ry: 'b t) : 'c t = object
      method init c s =
        rx#init c s;
        ry#init c s
      method build s =
        let y = ry#build s in
        let x = rx#build s in
        f x y
    end

  let of_spec spec f : _ t = object
      method init c s = s |> Rsrc_data.push (c |> Rsrc_cache.get spec)
      method build s = s |> Rsrc_data.pop |> f
    end

  type image = Image.t
  let image ~path =
    of_spec (Img(path)) (function
        | Img(elem, _) -> Image.make elem
        | _ -> failwith "wrong resource data")

  type font = Font.t
  let font ~family ~size =
    of_spec (Font(family, size)) (function
        | Font(f) -> f
        | _ -> failwith "wrong resource data")
end

module Loader = struct
  type t = Rsrc_cache.t
  type 'a rsrc = 'a Rsrc.t

  let make () = Rsrc_cache.make ()

  let load r c =
    let s = Rsrc_data.make_sink () in
    r#init c s;
    if s |> Rsrc_data.all_done then
      (* TODO: handle errors *)
      `Done (r#build s)
    else
      `Loading
end
