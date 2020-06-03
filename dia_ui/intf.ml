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

  module Ctxt: sig
    type t
    val size: t -> int * int
    val clear: f:Color.t -> t -> unit
    val text: x:int -> y:int -> font:Font.t -> f:Color.t -> string -> t -> unit
  end
end

module type Renderable_S = sig
  type t
  type draw_ctxt
  val render: draw_ctxt -> t -> unit
end
