(* misc. *)

module type S0 = sig type t end

module type Applicative_S = sig
  type 'a t
  val const: 'a -> 'a t
  val map: ('a -> 'b) -> 'a t -> 'b t
  val map2: ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
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
    val size: t -> int * int
    val clip: x:int -> y:int -> w:int -> h:int -> t -> t
  end

  module Ctxt: sig
    type t
    val size: t -> int * int

    (** [cx |> clear ~f] clears the display with color [f]. *)
    val clear: f:Color.t -> t -> unit

    (** [cx |> text ~x ~y ~font ~f t] draws text [t] with color [f], font [font] at
       position [x, y]. *)
    (* TODO: add transformations *)
    val text: x:int -> y:int -> font:Font.t -> f:Color.t -> ?t:Affine.t -> string -> t -> unit

    (** [cx |> image ~x ~y ~t img] draws image [img] at position [x, y], optionally with
       affine transformation [t]. *)
    val image: x:int -> y:int -> ?t:Affine.t -> Image.t -> t -> unit
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
