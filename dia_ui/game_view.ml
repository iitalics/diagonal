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

    (* -- rendering the map -- *)

    let map_w = 512
    let map_img_ox, map_img_oy = 64, 64
    let map_img assets =
      assets.map |> Draw.Image.clip ~x:0 ~y:0 ~w:640 ~h:640

    let render_map ~t cx v =
      cx |> Draw.Ctxt.image (v.assets |> map_img)
              ~t ~x:(- map_img_ox) ~y:(- map_img_oy)

    let blob_img ~color ~face assets =
      assets.sprites |> Draw.Image.clip
                          ~x:(0 + 64 * face)
                          ~y:(0 + 64 * color)
                          ~w:64 ~h:64

    (* -- rendering players -- *)

    let render_blob ~t ~i cx v =
      let t = Affine.make @@ Some t in
      t |> Affine.translate
             (float_of_int ((i mod 8) * 64))
             (float_of_int ((i   / 8) * 64));
      cx |> Draw.Ctxt.image
              (v.assets |> blob_img
                             ~color:(i mod 4)
                             ~face:(i mod 5))
              ~t ~x:0 ~y:0

    (* -- main entry point -- *)

    let render cx v =
      let (cx_w, cx_h) = cx |> Draw.Ctxt.size in
      cx |> Draw.Ctxt.clear ~f:bg_f;

      (* main transform for everything on the map *)
      let map_t = Affine.make None in
      map_t |> Affine.translate
                 ((cx_w - map_w) / 2 |> float_of_int)
                 ((cx_h - map_w) / 2 |> float_of_int);

      (* draw stuff *)
      v |> render_map ~t:map_t cx;
      for i = 0 to 63 do v |> render_blob ~t:map_t ~i cx done
  end
