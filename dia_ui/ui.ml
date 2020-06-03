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

    let _BG_F = Color.of_rgb_s "#020202"
    let _TITLE_FONT = Font.make ~fam:"Roundor" ~size:150
    let _TITLE_F = Color.of_rgb_s "#fff"

    let render c T : unit =
      begin
        c |> Ctxt.clear ~f:_BG_F;
        c |> Ctxt.text "Diag"
               ~x:10 ~y:10 ~font:_TITLE_FONT ~f:_TITLE_F;
      end
  end
