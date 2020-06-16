module Gameplay = Dia_game.Gameplay
module Player_controller = Dia_game.Player_controller
module Prng = Dia_game.Prng

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
        open Draw

        type t =
          { assets: assets;
            geom: geom;
            mutable rng_seed: int;
            mutable hov: int;
            mutable sel: bool }

        and geom =
          { height: int;
            main_t: Affine.t;
            title_t: Affine.t;
            item_ts: Affine.t array }

        (* labels *)

        let title_text = "Diag"
        let item_text = [ "Single player mode";
                          "Change Controls";
                          "Change Character" ]

        (* event handling *)

        let update time v =
          v.rng_seed <- int_of_float (time *. 1000000.)

        let handle_evt ev v = match (ev : View.Evt.t) with
          | Key_dn "ArrowDown" -> v.hov <- min (v.hov + 1) (List.length item_text - 1)
          | Key_dn "ArrowUp"   -> v.hov <- max (v.hov - 1) 0
          | Key_dn "Enter"     -> v.sel <- true
          | _ -> ()

        let switch disp v =
          if v.sel then
            ( v.sel <- false;
              match v.hov with
              | 0 -> let init = Gameplay.make
                                  ~player_ctrl_0:Player_controller.user_ctrl
                                  ~player_ctrl_1:(Player_controller.bot_ctrl @@
                                                    Prng.make ~seed:v.rng_seed) in
                     disp |> View_disp.push_view (module Game_view)
                               ~init
              | _ -> () )

        (* geometry *)

        let title_ypad = 20
        let item_ypad = 8

        let make_geom ~assets =
          let main_t = Affine.make () in
          (* title transform *)
          let (title_t, y) =
            let t = main_t |> Affine.extend in
            let (mes_w, mes_h) = assets.title_font |> Font.measure title_text in
            t |> Affine.translate_i (- mes_w / 2) 0;
            (t, mes_h + title_ypad)
          in
          (* items transform *)
          let (item_ts_rev, y) =
            List.fold_left
              (fun (ts, y) text ->
                let t = main_t |> Affine.extend in
                let (mes_w, mes_h) = assets.item_font |> Draw.Font.measure text in
                t |> Affine.translate_i (- mes_w / 2) y;
                (t :: ts, y + mes_h + item_ypad))
              ([], y)
              item_text
          in
          let height = y - item_ypad in
          { height; main_t; title_t;
            item_ts = Array.of_list (List.rev item_ts_rev) }

        let update_geom (cx_w, cx_h) ge =
          let t = ge.main_t in
          t |> Affine.reset;
          t |> Affine.translate_i
                 (cx_w / 2)
                 ((cx_h - ge.height) / 2)

        (* rendering *)

        let bg_c       = Color.of_rgb_s "#8df"
        let title_c    = Color.of_rgb_s "#000"
        let item_c     = Color.of_rgb_s "#fff"
        let item_hov_c = Color.of_rgb_s "#064"

        let render_title ~assets ~t cx =
          cx |> Ctxt.text title_text
                  ~x:0 ~y:0 ~t
                  ~font:assets.title_font ~c:title_c

        let render_item ~assets ~t cx text is_hov =
          cx |> Ctxt.text text
                  ~x:0 ~y:0 ~t
                  ~font:assets.item_font
                  ~c:(if is_hov then item_hov_c else item_c)

        let render cx
              { assets; geom; hov; _ }
          =
          begin
            geom |> update_geom (cx |> Ctxt.size);
            cx |> Ctxt.clear ~c:bg_c;
            render_title ~assets ~t:geom.title_t cx;
            item_text |> List.iteri
                           (fun i text ->
                             let t = geom.item_ts.(i) in
                             render_item ~assets ~t cx text (hov = i));
          end

        (* init *)

        type init = unit
        let make assets _init =
          { assets;
            hov = 0;
            sel = false;
            rng_seed = 0;
            geom = make_geom ~assets }
      end
  end
