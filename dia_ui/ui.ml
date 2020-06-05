module Make_views
         (Draw: Intf.Draw_S)
         (Rsrc: Intf.Rsrc_S with type font = Draw.Font.t)
         (View_disp: View.Dispatcher_S with type draw_ctxt = Draw.Ctxt.t
                                        and type 'a rsrc = 'a Rsrc.t)
  =
  struct
    module type View_S = View_disp.View_S

    module Green_screen: View_S with type init = unit =
      struct
        type view_disp = View_disp.t
        type draw_ctxt = Draw.Ctxt.t
        type 'a rsrc = 'a Rsrc.t

        (* assets *)

        type assets = unit
        let assets_rsrc = Rsrc.const ()

        (* init *)

        type t = bool ref
        type init = unit
        let make _assets _init = ref false

        (* event handling *)

        let handle_evt (evt: View.Evt.t) v = match evt with
          | Key_dn("Escape") -> v := true
          | Key_dn(x) -> Printf.printf "key dn: %S\n" x
          | _ -> ()

        let switch vd v = if !v then vd |> View_disp.pop_view

        (* rendering *)

        let bg_f = Draw.Color.of_rgb_s "#8f0"
        let render cx _v = cx |> Draw.Ctxt.clear ~f:bg_f
      end


    module Main_menu: View_S with type init = unit =
      struct
        type view_disp = View_disp.t
        type draw_ctxt = Draw.Ctxt.t
        type 'a rsrc = 'a Rsrc.t

        (* assets *)

        type assets =
          { title_font: Draw.Font.t;
            item_font: Draw.Font.t }

        let assets_rsrc =
          Rsrc.zip (fun title_font item_font -> { title_font; item_font })
            (Rsrc.font ~family:"roundor" ~size:100)
            (Rsrc.font ~family:"roundor" ~size:30)

        (* init *)

        type t = { assets: assets; mutable switch: bool }
        type init = unit
        let make assets _init = { assets; switch = false }

        (* event handling *)

        let handle_evt (evt: View.Evt.t) (v: t) = match evt with
          | Key_dn("g") -> v.switch <- true
          | _ -> ()

        let switch vd (v: t) =
          if v.switch then
            ( v.switch <- false;
              vd |> View_disp.push_view
                      (module Green_screen)
                      ~init:() )

        (* rendering *)

        let bg_f = Draw.Color.of_rgb_s "#8df"
        let render cx _v =
          cx |> Draw.Ctxt.clear ~f:bg_f
      end

  end
