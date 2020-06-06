module type S =
  View.S with type init = unit

module Make
         (Draw: Intf.Draw_S)
         (Rsrc: Intf.Rsrc_S with type font = Draw.Font.t
                             and type image = Draw.Image.t)
         (View_disp: View.Dispatcher_S with type draw_ctxt = Draw.Ctxt.t
                                        and type 'a rsrc = 'a Rsrc.t)
       : S with type 'a rsrc = 'a Rsrc.t
            and type view_disp = View_disp.t
            and type draw_ctxt = Draw.Ctxt.t
  =
  struct
    type 'a rsrc = 'a Rsrc.t
    type view_disp = View_disp.t
    type draw_ctxt = Draw.Ctxt.t

    (*** assets ***)

    type assets =
      { sprites: Draw.Image.t;
        map: Draw.Image.t }

    let assets_rsrc =
      Rsrc.map2 (fun sprites map -> { sprites; map })
        (Rsrc.image ~path:"sprites")
        (Rsrc.image ~path:"map_stone")

    (*** init ***)

    type t =
      { assets: assets } [@@ocaml.unboxed]

    type init = unit
    let make assets _init =
      { assets }

    (*** event handling ***)

    let handle_evt _ _ = ()
    let switch _disp _v = ()

    (*** rendering ***)

    let bg_f = Draw.Color.of_rgb_s "#5cf"
    let render cx v =
      cx |> Draw.Ctxt.clear ~f:bg_f;

      let map_img =
        v.assets.map |> Draw.Image.clip
                          ~x:0   ~y:0
                          ~w:640 ~h:640
      in
      cx |> Draw.Ctxt.image map_img ~x:0 ~y:0
  end
