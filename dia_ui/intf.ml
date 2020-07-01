open Dia_util

(* drawing primitives *)

type vertices =
  [ `Lines | `Strip | `Fill ]

module type Draw_S = sig
  module Color: sig
    type t
    val none: t
    val of_rgb_s: string -> t
    val with_alpha: float -> t -> t
  end

  module Font: sig
    type t
    val measure: string -> t -> int * int
  end

  module Image: sig
    type t
    val size: t -> int * int
  end

  module Ctxt: sig
    type t
    type op = ?t:Affine.t -> t -> unit

    val size: t -> int * int

    (** [cx |> clear ~c] fills the entire display with color [c]. *)
    val clear: c:Color.t -> t -> unit

    (** [cx |> text ~x ~y ~font ~c t] draws text [t] with color [c], font [font] at
       position [x, y]. *)
    val text: x:int -> y:int -> font:Font.t -> c:Color.t -> string -> op

    (** [cx |> image ~x ~y ~sx ~sy ~w ~h img] draws image [img] at position [x, y].
        with source rect origin [sx, sy] and size [w, h]. *)
    val image: x:int -> y:int -> sx:int -> sy:int -> w:int -> h:int -> Image.t -> op

    (** [cx |> lines ~xs ~ys ~c mode] draws vertices with color [c], x-coordinates [xs],
       and y-coordinates [ys]. [mode] determines if pairs of vertices form disjoint lines
       ([`Lines]), if each adjacent vertex is connected in order ([`Strip]), or if the
       vertices represent the points of a filled polygon ([`Fill]). *)
    val vertices: xs:int array -> ys:int array -> c:Color.t -> vertices -> op
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
