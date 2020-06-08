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

    module Rsrc = struct
      include Rsrc
      include Util.Applicative(Rsrc)
    end

    open Draw

    (*** assets ***)

    type assets =
      { sprites: Image.t;
        map:     Image.t;
        hud_pname: Font.t }

    let assets_rsrc =
      Rsrc.(map2 (fun [ sprites; map ] [ hud_pname ] -> { sprites; map; hud_pname })
              (all [ image ~path:"sprites";
                     image ~path:"map_stone" ])
              (all [ font ~family:"space_mono" ~size:24 ]))
        [@@ocaml.warning "-8"]

    (*** init ***)

    type t =
      { assets: assets;
        p0: player;
        p1: player }

    and player =
      { name: string;
        hp: int }

    let max_hp = 16

    type init = unit
    let make assets _init =
      { assets;
        p0 = { name = "Player One"; hp = 16 };
        p1 = { name = "Player Two"; hp = 4 } }

    (*** event handling ***)

    let handle_evt _ _ = ()
    let switch _disp _v = ()

    (*** rendering ***)

    let bg_c   = Color.of_rgb_s "#5cf"

    (* -- rendering the HUD -- *)

    let hud_bg_c = Color.of_rgb_s "#000" |> Color.with_alpha 0.5
    let hud_c    = Color.of_rgb_s "#fff"
    let hud_w = 800
    let hud_h = 68
    let hud_y = 12
    let hud_left = 9
    let hud_top = 8
    let hud_hpbar_fill_c = Color.[| of_rgb_s "#04f"; of_rgb_s "#f04" |]
    let hud_hpbar_y = 36
    let hud_hpbar_w = 386
    let hud_hpbar_h = 20

    (* TODO:
       [x] player names
       [x] hp bars
       [ ] items
       [ ] turn timer
     *)

    let render_hud_bg ~t cx =
      cx |> Ctxt.vertices `Fill
              ~t ~c:hud_bg_c
              ~xs:[| 0; hud_w; hud_w; 0     |]
              ~ys:[| 0; 0;     hud_h; hud_h |]

    let render_hud_players ~t cx assets p0 p1 =
      let font = assets.hud_pname in
      let pname p i =
        let (mes_w, _) = font |> Font.measure p.name in
        let x, y = hud_left + i * (hud_w - mes_w - hud_left * 2), hud_top in
        cx |> Ctxt.text p.name ~t ~c:hud_c ~font ~x ~y
      in
      let hp_bar p i =
        let x0 = hud_left + i * (hud_w - hud_hpbar_w - hud_left * 2) in
        let x1 = x0 + hud_hpbar_w in
        let x1' = x0 + hud_hpbar_w * p.hp / max_hp in
        let y0 = hud_hpbar_y in
        let y1 = y0 + hud_hpbar_h in
        cx |> Ctxt.vertices `Fill
                ~t ~c:hud_hpbar_fill_c.(i)
                ~xs:[| x0; x1'; x1'; x0 |]
                ~ys:[| y0; y0; y1; y1 |];
        cx |> Ctxt.vertices `Strip
                ~t ~c:hud_c
                ~xs:[| x0; x1; x1; x0; x0 |]
                ~ys:[| y0; y0; y1; y1; y0 |]
      in
      pname p0 0; hp_bar p0 0;
      pname p1 1; hp_bar p1 1

    let render_hud ~t cx v =
      render_hud_bg ~t cx;
      render_hud_players ~t cx v.assets v.p0 v.p1

    (* -- rendering the map -- *)

    let grid_c = Color.of_rgb_s "#ccc"

    let cell_w = 64
    let cells = 8
    let map_w = cell_w * cells

    let map_img_ox, map_img_oy = 64, 64
    let map_img assets =
      assets.map |> Image.clip ~x:0 ~y:0 ~w:640 ~h:640

    let render_map ~t cx v =
      cx |> Ctxt.image (v.assets |> map_img)
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
      cx |> Ctxt.vertices `Lines
              ~t ~c:grid_c ~xs:grid_xs ~ys:grid_ys

    (* -- rendering players -- *)

    let blob_img ~color ~face assets =
      assets.sprites |> Image.clip
                          ~x:(0 + 64 * face)
                          ~y:(0 + 64 * color)
                          ~w:64 ~h:64

    let render_blob ~t ~i cx v =
      let t = Affine.extend t in
      t |> Affine.translate
             (float_of_int ((i mod 8) * 64))
             (float_of_int ((i   / 8) * 64));
      cx |> Ctxt.image
              (v.assets |> blob_img
                             ~color:(i mod 4)
                             ~face:(i mod 5))
              ~t ~x:0 ~y:0

    (* -- main entry point -- *)

    let render cx v =
      let (cx_w, cx_h) = cx |> Ctxt.size in
      cx |> Ctxt.clear ~c:bg_c;

      (* transform for everything on the map *)
      let map_t = Affine.make () in
      map_t |> Affine.translate
                 ((cx_w - map_w) / 2 |> float_of_int)
                 ((cx_h - map_w) / 2 |> float_of_int);

      (* transform for the HUD *)
      let hud_t = Affine.make () in
      hud_t |> Affine.translate
                 ((cx_w - hud_w) / 2 |> float_of_int)
                 (float_of_int hud_y);

      (* draw stuff *)
      begin
        v |> render_map ~t:map_t cx;
        for i = 0 to 3 do v |> render_blob ~t:map_t ~i cx done;
        v |> render_grid ~t:map_t cx;
        v |> render_hud ~t:hud_t cx;
      end
  end
