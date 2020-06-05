(* misc. *)

module type S0 = sig type t end

module type Applicative_S = sig
  type 'a t
  val const: 'a -> 'a t
  val zip: ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
end

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
  include Applicative_S
  type image
  val image: path:string -> image t
  type font
  val font: family:string -> size:int -> font t
end

module type Loader_S = sig
  type t
  type 'a rsrc
  val make: unit -> t
  val load: 'a rsrc -> t -> [ `Loading | `Done of 'a | `Error of string ]
end

module No_assets(Rsrc: Rsrc_S) = struct
  type assets = unit
  type 'a rsrc = 'a Rsrc.t
  let assets_rsrc = Rsrc.const ()
end
