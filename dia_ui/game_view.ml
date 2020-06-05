module type S = sig
  type t
  include Intf.View_S
          with type t := t
           and type init = unit
end

module Make
         (Draw: Intf.Draw_S)
         (Rsrc: Intf.Rsrc_S)
       : S with type draw_ctxt = Draw.Ctxt.t
  =
  struct
    open Draw
    type draw_ctxt = Ctxt.t
    type init = unit
    include Intf.No_assets(Rsrc)

    type t = T
    let make _assets () = T

    let bg_f = Color.of_rgb_s "#8cf"
    let render cx T = cx |> Ctxt.clear ~f:bg_f
    let handle_evt _ T = ()
  end
