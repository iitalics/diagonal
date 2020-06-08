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
        hud_p_name:   Font.t;
        hud_act_item: Font.t;
        hud_turn:     Font.t }

    let assets_rsrc =
      let images =
        Rsrc.(all [ image ~path:"sprites";
                    image ~path:"map_stone" ])
      in
      let fonts =
        Rsrc.(all [ font ~family:"space_mono" ~size:24;
                    font ~family:"space_mono" ~size:15;
                    font ~family:"space_mono_bold" ~size:16 ])
      in
      Rsrc.map2
        (fun[@ocaml.warning "-8"]
            [ sprites; map ]
            [ hud_p_name; hud_act_item; hud_turn ]
         ->
          { sprites; map; hud_p_name; hud_act_item; hud_turn })
        images
        fonts

    (*** init ***)

    type t =
      { assets: assets;
        p0: player;
        p1: player;
        turn_num: int;
        turn_timer_f: int  }

    and player =
      { name: string;
        hp: int;
        item: item;
        alt_item: item option }

    and item = [ `S | `F | `P | `H ]

    let max_hp = 16
    let fps = 60
    let turn_total_f = fps * 3

    type init = unit
    let make assets _init =
      { assets;
        p0 = { name = "Player One"; hp = 16; item = `S; alt_item = Some `P };
        p1 = { name = "Player Two"; hp = 4;  item = `F; alt_item = Some `S };
        turn_num = 3;
        turn_timer_f = 160 }

    (*** event handling ***)

    let handle_evt _ _ = ()
    let switch _disp _v = ()

    (*** rendering ***)

    let bg_c   = Color.of_rgb_s "#5cf"

    (* -- rendering the HUD -- *)

    let hud_bg_c = Color.(of_rgb_s "#000" |> with_alpha 0.5)
    let hud_c    = Color.of_rgb_s "#fff"
    let hud_w = 800
    let hud_h = 128
    let hud_y = 30
    let hud_left = 9
    let hud_top = 8
    let hud_hpbar_fill_c = Color.[| of_rgb_s "#04f"; of_rgb_s "#f04" |]
    let hud_hpbar_y = 36
    let hud_hpbar_w = 386
    let hud_hpbar_h = 20
    let hud_item_x  = 56 (* 32 *)
    let hud_item_y  = 84
    let hud_item_scale = 0.8
    let hud_item_alt_dx = 110 (* 80 *)
    let hud_item_alt_dy = 0
    let hud_item_alt_scale = 0.65
    let hud_item_act_text = "active item"
    let hud_item_act_text_c = Color.of_rgb_s "#ff0"
    let hud_item_act_text_dy = 24
    let hud_item_alt_text = "secondary"
    let hud_item_alt_text_c = Color.of_rgb_s "#111"
    let hud_item_alt_text_dy = 20
    let hud_turn_x = hud_w / 2
    let hud_turn_y = 82
    let hud_turn_bar_w = 160
    let hud_turn_bar_h = 12
    let hud_turn_bar_dy = 20
    let hud_turn_bar_fill_c = Color.(of_rgb_s "#fff" |> with_alpha 0.6)

    let item_icon_img (it: item) assets =
      let row = match it with `S -> 0 | `F -> 1 | `P -> 2 | `H -> 3 in
      assets.sprites |> Image.clip ~x:384 ~y:(row * 64) ~w:64 ~h:64

    let item_icon_w = 64

    let render_hud_bg ~t cx =
      cx |> Ctxt.vertices `Fill
              ~t ~c:hud_bg_c
              ~xs:[| 0; hud_w; hud_w; 0     |]
              ~ys:[| 0; 0;     hud_h; hud_h |]

    let render_hud_players ~t cx assets p0 p1 =
      let p_name p i =
        let font = assets.hud_p_name in
        let (mes_w, _) = font |> Font.measure p.name in
        let t = Affine.extend t in
        t |> Affine.translate_i
               ((1 - i) * hud_left
                +     i * (hud_w - hud_left))
               hud_top;
        cx |> Ctxt.text p.name ~t ~c:hud_c ~font
                ~x:(i * -mes_w)
                ~y:0
      in

      let hp_bar p i =
        let t = Affine.extend t in
        t |> Affine.translate_i
               ((1 - i) * hud_left
                +     i * (hud_w - hud_hpbar_w - hud_left))
               hud_hpbar_y;
        let w  = hud_hpbar_w in
        let w' = hud_hpbar_w * p.hp / max_hp in
        let h  = hud_hpbar_h in
        cx |> Ctxt.vertices `Fill
                ~t ~c:hud_hpbar_fill_c.(i)
                ~xs:[| 0; w'; w'; 0 |]
                ~ys:[| 0; 0; h; h |];
        cx |> Ctxt.vertices `Strip
                ~t ~c:hud_c
                ~xs:[| 0; w; w; 0; 0 |]
                ~ys:[| 0; 0; h; h; 0 |]
      in

      let items p i =
        let t = Affine.extend t in
        t |> Affine.translate_i
               ((1 - i) * hud_item_x
                +     i * (hud_w - hud_item_x))
               hud_item_y;

        (* primary item *)
        (let item = p.item in
         let t = Affine.extend t in
         t |> Affine.scale
                hud_item_scale hud_item_scale;
         cx |> Ctxt.image (assets |> item_icon_img item) ~t
                 ~x:(-item_icon_w / 2) ~y:(-item_icon_w / 2));

        (* active item text *)
        (let font = assets.hud_act_item in
         let (mes_w, _) = font |> Font.measure hud_item_act_text in
         cx |> Ctxt.text hud_item_act_text
                 ~t ~font ~c:hud_item_act_text_c
                 ~x:(-mes_w / 2)
                 ~y:hud_item_act_text_dy);

        (* alt item text *)
        (let font = assets.hud_act_item in
         let (mes_w, _) = font |> Font.measure hud_item_alt_text in
         cx |> Ctxt.text hud_item_alt_text
                 ~t ~font ~c:hud_item_alt_text_c
                 ~x:((1 - 2 * i) * hud_item_alt_dx - mes_w / 2)
                 ~y:(hud_item_alt_text_dy));

        (* alt item *)
        p.alt_item |>
          Option.iter (fun item ->
              let t = Affine.extend t in
              t |> Affine.translate_i
                     ((1 - 2 * i) * hud_item_alt_dx)
                     hud_item_alt_dy;
              t |> Affine.scale
                     hud_item_alt_scale hud_item_alt_scale;
              cx |> Ctxt.image (assets |> item_icon_img item) ~t
                      ~x:(-item_icon_w / 2) ~y:(-item_icon_w / 2));
      in

      p_name p0 0; hp_bar p0 0; items p0 0;
      p_name p1 1; hp_bar p1 1; items p1 1

    let render_hud_turn ~t cx assets turn_num timer_f =
      let t = Affine.extend t in
      t |> Affine.translate_i hud_turn_x hud_turn_y;
      (* timer text *)
      (let text = Printf.sprintf "turn %d (%.1fs)"
                    turn_num
                    (float_of_int timer_f /. float_of_int fps) in
       let font = assets.hud_turn in
       let (mes_w, _) = font |> Font.measure text in
       cx |> Ctxt.text text
               ~t ~font ~c:hud_c
               ~x:(-mes_w / 2) ~y:0);
      (* timer bar *)
      (let x0, x1 = -hud_turn_bar_w / 2, hud_turn_bar_w / 2 in
       let y0, y1 = hud_turn_bar_dy, hud_turn_bar_dy + hud_turn_bar_h in
       let x1' = x0 + hud_turn_bar_w * timer_f / turn_total_f in
       cx |> Ctxt.vertices `Strip
               ~t ~c:hud_c
               ~xs:[| x0; x1; x1; x0; x0 |]
               ~ys:[| y0; y0; y1; y1; y0 |];
       cx |> Ctxt.vertices `Fill
               ~t ~c:hud_turn_bar_fill_c
               ~xs:[| x0; x1'; x1'; x0 |]
               ~ys:[| y0;  y0;  y1; y1 |])

    let render_hud ~t cx v : unit =
      begin
        render_hud_bg ~t cx;
        render_hud_players ~t cx v.assets v.p0 v.p1;
        render_hud_turn ~t cx v.assets v.turn_num v.turn_timer_f;
      end

    (* -- rendering the map -- *)

    let grid_c = Color.of_rgb_s "#ccc"

    let cell_w = 64
    let cells = 8
    let map_w = cell_w * cells
    let map_y = 260

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
      let (cx_w, _) = cx |> Ctxt.size in
      cx |> Ctxt.clear ~c:bg_c;

      (* transform for everything on the map *)
      let map_t = Affine.make () in
      map_t |> Affine.translate_i
                 ((cx_w - map_w) / 2)
                 map_y;

      (* transform for the HUD *)
      let hud_t = Affine.make () in
      hud_t |> Affine.translate_i
                 ((cx_w - hud_w) / 2)
                 hud_y;

      (* draw stuff *)
      begin
        v |> render_map ~t:map_t cx;
        for i = 0 to 3 do v |> render_blob ~t:map_t ~i cx done;
        v |> render_grid ~t:map_t cx;
        v |> render_hud ~t:hud_t cx;
      end
  end
