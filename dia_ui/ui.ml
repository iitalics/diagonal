module type S = sig
  type t
  val make: unit -> t
  include Intf.Renderable_S with type t := t
end

module Make
         (Draw: Intf.Draw_S)
       : S with type draw_ctxt = Draw.Ctxt.t
  =
  struct
    type nonrec draw_ctxt = Draw.Ctxt.t

    type t = T
    let make () = T

    let _BG_F = Draw.Color.of_rgb_s "#00f"

    let render c T =
      c |> Draw.Ctxt.clear ~f:_BG_F
  end
