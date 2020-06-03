module type Draw_S = sig
  module Color: sig
    type t
    val none: t
    val of_rgb_s: string -> t
  end

  module Ctxt: sig
    type t
    val size: t -> int * int
    val clear: f:Color.t -> t -> unit
  end
end

module type Renderable_S = sig
  type t
  type draw_ctxt
  val render: draw_ctxt -> t -> unit
end
