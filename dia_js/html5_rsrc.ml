module Dom = Js_of_ocaml.Dom
module Dom_html = Js_of_ocaml.Dom_html
module Js = Js_of_ocaml.Js

type rsrc_stat =
  [ `Loading | `Done | `Error of string ]

module Rsrc_data = struct
  type t =
    | Img of Dom_html.imageElement Js.t * rsrc_stat ref
    | Font of Js.js_string Js.t

  let img ~path =
    let elem = Dom_html.createImg Dom_html.document in
    let stat = ref `Loading in
    let listen dom_ev r =
      ignore (Dom.addEventListener elem
                dom_ev
                (Dom.handler (fun _ -> stat := r; Js._true))
                Js._false) in
    listen Dom_html.Event.load `Done;
    listen Dom_html.Event.error @@ `Error (Printf.sprintf "failed to load image %S" path);
    elem##.src := Js.string @@ Printf.sprintf "res/%s.png" path;
    Img(elem, stat)

  let font ~family ~size =
    Font(Js.string @@ Printf.sprintf "%dpx %S" size family)

  let stat = function
    | Font _        -> `Done
    | Img (_, stat) -> !stat
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

module Rsrc_stack = struct
  type t = Rsrc_data.t list ref

  let make () = ref []
  let push rd s = s := rd :: !s
  let pop s = match !s with
    | rd :: s' -> s := s'; rd
    | [] -> failwith "sink empty"

  let stat (s: t) =
    !s |> List.map Rsrc_data.stat
    |> List.find_opt ((<>) `Done)
    |> Option.value ~default:`Done
end

module Cache = struct
  type t = (Rsrc_spec.t, Rsrc_data.t) Hashtbl.t

  class type ['a] rsrc =
    object
      method init: t -> Rsrc_stack.t -> unit
      method build: Rsrc_stack.t -> 'a
    end

  let make (): t = Hashtbl.create 60

  let get rs c =
    match Hashtbl.find_opt c rs with
    | Some(rd) -> rd
    | None     ->
       let rd = rs |> Rsrc_spec.init in
       Printf.printf "Loading %s\n" (rs |> Rsrc_spec.to_string);
       Hashtbl.add c rs rd; rd

  let load r (c: t) =
    let s = Rsrc_stack.make () in
    r#init c s;
    match s |> Rsrc_stack.stat with
    | `Done                      -> `Done (r#build s)
    | (`Error _ | `Loading) as x -> x
end

type 'a t = 'a Cache.rsrc

let const (x: 'a) : 'a t = object
    method init _ _ = ()
    method build _ = x
  end

let map f (r: 'a t) : 'b t = object
    method init c s = r#init c s
    method build s = f (r#build s)
  end

let map2 f (rx: 'a t) (ry: 'b t) : 'c t = object
    method init c s = rx#init c s; ry#init c s
    method build s =
      let y = ry#build s in
      let x = rx#build s in
      f x y
  end

let of_spec spec f : _ t = object
    method init c s = s |> Rsrc_stack.push (c |> Cache.get spec)
    method build s = s |> Rsrc_stack.pop |> f
  end

type image = Html5.Image.t
let image ~path =
  of_spec (Img(path)) (function
      | Img(elem, _) -> Html5.Image.make elem
      | _            -> failwith "wrong resource data")

type font = Html5.Font.t
let font ~family ~size =
  of_spec (Font(family, size)) (function
      | Font(f) -> f
      | _       -> failwith "wrong resource data")
