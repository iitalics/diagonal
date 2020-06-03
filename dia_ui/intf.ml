(* drawing primitives *)

module type Draw_S = sig
  module Color: sig
    type t
    val none: t
    val of_rgb_s: string -> t
  end

  module Font: sig
    type t
    val measure: string -> t -> int * int
  end

  module Image: sig
    type t
    val clip: x:int -> y:int -> w:int -> h:int -> t -> t
  end

  module Ctxt: sig
    type t
    val size: t -> int * int
    val clear: f:Color.t -> t -> unit
    val text: x:int -> y:int -> font:Font.t -> f:Color.t -> string -> t -> unit
    val image: x:int -> y:int -> w:int -> h:int -> Image.t -> t -> unit
  end
end

(* resources *)

module type Rsrc_S = sig
  type 'a t
  val const: 'a -> 'a t
  val zip: ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t

  type image
  type font
  val image_rsrc: path:string -> image t
  val font_rsrc: family:string -> size:int -> font t
end

module type Loader_S = sig
  type t
  type 'a rsrc
  val load: 'a rsrc -> t -> [ `Loading | `Done of 'a | `Error of string ]
end

module type Has_assets_S = sig
  type assets
  type 'a rsrc
  val rsrc: assets rsrc
end

module Rsrc_ext(Rsrc: Rsrc_S) = struct
  include Rsrc
  let zip3 f x y z = zip (@@) (zip f x y) z
end

module No_assets(Rsrc: Rsrc_S) = struct
  type assets = unit
  type 'a rsrc = 'a Rsrc.t
  let rsrc = Rsrc.const ()
end

(* generic view interface *)

module type Renderable_S = sig
  type t
  type draw_ctxt
  val render: draw_ctxt -> t -> unit
end

module type Key_input_S = sig
  type t
  val key_dn: string -> t -> unit
  val key_up: string -> t -> unit
end

module type View_S = sig
  type t
  include Renderable_S with type t := t
  include Key_input_S with type t := t
  include Has_assets_S
end
