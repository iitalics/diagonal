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
    open Draw
    type nonrec draw_ctxt = Ctxt.t

    type t = T
    let make () = T

    let bg_f = Color.of_rgb_s "#8cf"
    let render cx T = cx |> Ctxt.clear ~f:bg_f
  end
