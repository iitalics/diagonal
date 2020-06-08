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

    let bg_c   = Draw.Color.of_rgb_s "#5cf"
    let grid_c = Draw.Color.of_rgb_s "#ccc"

    (* -- rendering the map -- *)

    let cell_w = 64
    let cells = 8
    let map_w = cell_w * cells

    let map_img_ox, map_img_oy = 64, 64
    let map_img assets =
      assets.map |> Draw.Image.clip ~x:0 ~y:0 ~w:640 ~h:640

    let render_map ~t cx v =
      cx |> Draw.Ctxt.image (v.assets |> map_img)
              ~t ~x:(- map_img_ox) ~y:(- map_img_oy)

    let grid_xs, grid_ys =
      let rad = 8 in
      let outer1 i = (i   / 2 mod 9    ) * cell_w in
      let outer2 i = (i   / 2   / 9    ) * (map_w - rad) + (i mod 2) * rad in
      let inner1 i = (i   / 2 mod 7 + 1) * cell_w - rad  + (i mod 2) * rad * 2 in
      let inner2 i = (i   / 2   / 7    ) * map_w in
      let inner3 i = (i   / 2   / 7 + 1) * cell_w in
      Array.init 324 (fun i -> if      i < 36  then outer1 i
                               else if i < 72  then outer2 (i - 36)
                               else if i < 100 then inner1 (i - 72)
                               else if i < 128 then inner2 (i - 100)
                               else if i < 226 then inner1 (i - 128)
                               else                 inner3 (i - 226)),
      Array.init 324 (fun i -> if      i < 36  then outer2 i
                               else if i < 72  then outer1 (i - 36)
                               else if i < 100 then inner2 (i - 72)
                               else if i < 128 then inner1 (i - 100)
                               else if i < 226 then inner3 (i - 128)
                               else                 inner1 (i - 226))

    let render_grid ~t cx v =
      ignore v;
      cx |> Draw.Ctxt.lines `Lines
              ~t ~c:grid_c ~xs:grid_xs ~ys:grid_ys

    (* -- rendering players -- *)

    let blob_img ~color ~face assets =
      assets.sprites |> Draw.Image.clip
                          ~x:(0 + 64 * face)
                          ~y:(0 + 64 * color)
                          ~w:64 ~h:64

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
      cx |> Draw.Ctxt.clear ~c:bg_c;

      (* main transform for everything on the map *)
      let map_t = Affine.make None in
      map_t |> Affine.translate
                 ((cx_w - map_w) / 2 |> float_of_int)
                 ((cx_h - map_w) / 2 |> float_of_int);

      (* draw stuff *)
      v |> render_map ~t:map_t cx;
      for i = 0 to 3 do v |> render_blob ~t:map_t ~i cx done;
      v |> render_grid ~t:map_t cx
  end
