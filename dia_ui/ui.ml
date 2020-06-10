module Gameplay_state = Dia_game.Gameplay_state

module Make_views
         (Draw: Intf.Draw_S)
         (Rsrc: Intf.Rsrc_S with type font = Draw.Font.t
                             and type image = Draw.Image.t)
         (View_disp: View.Dispatcher_S with type draw_ctxt = Draw.Ctxt.t
                                        and type 'a rsrc = 'a Rsrc.t)
  =
  struct
    module type View_S = View_disp.View_S

    module Game_view = Game_view.Make(Draw)(Rsrc)(View_disp)

    (*** assets common to menu screens ***)

    type menu_assets =
      { title_font: Draw.Font.t;
        item_font: Draw.Font.t }

    module Menu_assets = struct
      type assets = menu_assets
      let assets_rsrc =
        Rsrc.map2 (fun title_font item_font -> { title_font; item_font })
          (Rsrc.font ~family:"roundor" ~size:100)
          (Rsrc.font ~family:"nunito" ~size:30)
    end

    (*** main menu! ***)

    module Main_menu: View_S with type init = unit and type assets = menu_assets =
      struct
        type view_disp = View_disp.t
        type draw_ctxt = Draw.Ctxt.t
        type 'a rsrc = 'a Rsrc.t
        include Menu_assets

        (* init *)

        type t =
          { assets: assets;
            mutable hov: int;
            mutable sel: bool }

        type init = unit
        let make assets _init =
          { assets;
            hov = 0;
            sel = false }

        (* event handling *)

        let update _time _v = ()

        let handle_evt ev v = match (ev : View.Evt.t) with
          | Key_dn "ArrowDown" -> v.hov <- v.hov + 1
          | Key_dn "ArrowUp"   -> v.hov <- v.hov - 1
          | Key_dn "Enter"     -> v.sel <- true
          | _ -> ()

        let switch disp v =
          if v.sel then
            ( v.sel <- false;
              match v.hov with
              | 0 -> disp |> View_disp.push_view (module Game_view)
                               ~init:(Gameplay_state.make ())
              | _ -> () )

        (* rendering *)

        let bg_c       = Draw.Color.of_rgb_s "#8df"
        let title_c    = Draw.Color.of_rgb_s "#000"
        let item_c     = Draw.Color.of_rgb_s "#fff"
        let item_hov_c = Draw.Color.of_rgb_s "#064"

        let title_ypad = 20
        let item_ypad = 8

        let title_text = "Diag"
        let item_text = [ "Single player mode";
                          "Change Controls";
                          "Change Character" ]

        let render_title assets (w, h) cx =
          let (mes_w, mes_h) = assets.title_font |> Draw.Font.measure title_text in
          let x, y = (w - mes_w) / 2, (h - mes_h) / 2 in
          cx |> Draw.Ctxt.text title_text
                  ~x ~y ~c:title_c ~font:assets.title_font;
          let y = y + mes_h + title_ypad in
          y

        let render_item assets hov_i (w, _) cx (i, y) text =
          let (mes_w, mes_h) = assets.item_font |> Draw.Font.measure text in
          let x = (w - mes_w) / 2 in
          cx |> Draw.Ctxt.text text
                  ~x ~y ~font:assets.item_font
                  ~c:(if i = hov_i then item_hov_c else item_c);
          let y = y + mes_h + item_ypad in
          (i + 1, y)

        let render cx v =
          let size = cx |> Draw.Ctxt.size in
          cx |> Draw.Ctxt.clear ~c:bg_c;
          let y = cx |> render_title v.assets size in
          let y = List.fold_left (render_item v.assets v.hov size cx) (0, y) item_text in
          ignore y
      end
  end
