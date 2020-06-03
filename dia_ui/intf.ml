module type Draw_S = sig
  module Color: sig
    type t
    val none: t
    val of_rgb_s: string -> t
  end

  module Font: sig
    type t
    val make: fam:string -> size:int -> t
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
