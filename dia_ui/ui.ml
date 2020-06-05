module Make_loading_view
         (Draw: Intf.Draw_S)
         (Rsrc: Intf.Rsrc_S with type font = Draw.Font.t)
         (Loader: Intf.Loader_S with type 'a rsrc = 'a Rsrc.t)
       : Intf.View_S with type assets = unit
                      and type init = Loader.t
                      and type 'a rsrc = 'a Rsrc.t
                      and type draw_ctxt = Draw.Ctxt.t
  =
  struct
    include Intf.No_assets(Rsrc)
    type draw_ctxt = Draw.Ctxt.t
    type init = Loader.t

    type t =
      { loader: Loader.t;
        mutable font: Draw.Font.t option }

    let make () loader =
      { loader; font = None }

    let bg_f = Draw.Color.of_rgb_s "#111111"
    let font_rsrc = Rsrc.font ~family:"nunito" ~size:30
    let loading_text = "Loading..."
    let loading_f = Draw.Color.of_rgb_s "#eeeeee"

    let iter_font f (v: t) =
      match v.font with
      | Some(fnt) -> f fnt
      | None -> match v.loader |> Loader.load font_rsrc with
                | `Done(fnt) -> v.font <- Some(fnt); f fnt
                | _ -> ()

    let render_text cx font =
      let (cx_w, cx_h) = cx |> Draw.Ctxt.size in
      let (mes_w, mes_h) = font |> Draw.Font.measure loading_text in
      cx |> Draw.Ctxt.text loading_text
              ~x:((cx_w - mes_w) / 2)
              ~y:((cx_h - mes_h) / 2)
              ~f:loading_f ~font

    let render cx (v: t) =
      cx |> Draw.Ctxt.clear ~f:bg_f;
      v |> iter_font (render_text cx)

    let handle_evt _ _ = ()
  end
