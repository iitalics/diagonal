module Gameplay = Dia_game.Gameplay
module Turn = Dia_game.Gameplay.Turn
module Rules = Dia_game.Rules
module Item_type = Dia_game.Item_type

module type S = sig
  type assets
  type 'a rsrc
  val assets_rsrc: assets rsrc

  type t
  val make: assets -> Gameplay.t -> t
  val update_time: float -> t -> unit
  val update_game: tick_time:float -> Gameplay.t -> t -> unit

  type draw_ctxt
  val render: draw_ctxt -> t -> unit
end

module Make
         (Draw: Intf.Draw_S)
         (Rsrc: Intf.Rsrc_S with type font = Draw.Font.t
                             and type image = Draw.Image.t)
       : S with type 'a rsrc = 'a Rsrc.t
            and type draw_ctxt = Draw.Ctxt.t
  =
  struct
    type draw_ctxt = Draw.Ctxt.t
    type 'a rsrc = 'a Rsrc.t

    module Rsrc = struct
      include Rsrc
      include Util.Applicative(Rsrc)
    end

    module Ctxt = Draw.Ctxt
    module Color = Draw.Color
    module Font = Draw.Font
    module Image = Draw.Image

    (*** assets ***)

    type assets =
      { sprites:       Image.t;
        p_name_font:   Font.t;
        act_item_font: Font.t;
        turn_font:     Font.t }

    let assets_rsrc : assets Rsrc.t =
      let fonts =
        Rsrc.(all [ font ~family:"space_mono" ~size:24;
                    font ~family:"space_mono" ~size:15;
                    font ~family:"space_mono_bold" ~size:16 ])
      in
      let images =
        Rsrc.image ~path:"sprites"
      in
      Rsrc.map2 (fun[@ocaml.warning "-8"]
                    [ p_name_font; act_item_font; turn_font ]
                    sprites ->
          { sprites; p_name_font; act_item_font; turn_font })
        fonts
        images

    (*** data ***)

    type t =
      { assets: assets;
        mutable time: float;
        mutable player_0: player_data;
        mutable player_1: player_data;
        mutable turn_data: turn_data }

    and player_data =
      { pl_name: string;
        pl_hp: int;
        pl_item: Item_type.t;
        pl_alt_item: Item_type.t option }

    and turn_data =
      { tn_num: int;
        tn_time: float;
        (* amt(t) = amt0 + t * amt_v *)
        tn_amt0: float;
        tn_amt_v: float }

    let max_hp = 16

    (*** processing game state data ***)

    let turn_frames_fl = float_of_int Rules.turn_frames
    let turn_amt_v = ~-. Rules.fps_fl /. turn_frames_fl (* 1/sec *)

    let turn_data_of_turn ~t0 (tn: Turn.t) =
      let rem_f = Rules.turn_frames - tn.frame in
      let time  = float_of_int rem_f /. Rules.fps_fl in
      let amt   = float_of_int rem_f /. turn_frames_fl in
      (* amt(t) = amt0 + t * amt_v  ==>  amt0 = amt(t) - t * amt_v *)
      { tn_num = tn.num; tn_time = time;
        tn_amt0 = amt -. t0 *. turn_amt_v;
        tn_amt_v = turn_amt_v }

    let update_time time hud =
      hud.time <- time

    let update_game ~tick_time:t0 game hud =
      hud.turn_data <- game |> Gameplay.turn |> turn_data_of_turn ~t0

    (*** init ***)

    let default_player name =
      { pl_name = name;
        pl_hp = 16;
        pl_item = `S;
        pl_alt_item = None }

    let make assets _game =
      { assets;
        time = 0.;
        player_0 = default_player "Player One";
        player_1 = default_player "Player Two";
        turn_data = { tn_num = -1; tn_time = 0.; tn_amt0 = 0.; tn_amt_v = 0. } }

    (*** rendering ***)

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
    let hud_turn_y = 78
    let hud_turn_bar_w = 160
    let hud_turn_bar_h = 12
    let hud_turn_bar_dy = 20
    let hud_turn_bar_fill_c = Color.(of_rgb_s "#fff" |> with_alpha 0.6)

    let render_icon_img ~assets ?t cx ty =
      let row = match ty with `S -> 0 | `F -> 1 | `P -> 2 | `H -> 3 in
      cx |> Ctxt.image assets.sprites
              ~x:(-32) ~y:(-32) ?t
              ~sx:384 ~sy:(row * 64) ~w:64 ~h:64

    let render_bg ~t cx =
      cx |> Ctxt.vertices `Fill
              ~t ~c:hud_bg_c
              ~xs:[| 0; hud_w; hud_w; 0     |]
              ~ys:[| 0; 0;     hud_h; hud_h |]

    let render_player ~assets ~t cx i
          { pl_name; pl_hp; pl_item; pl_alt_item; _ }
      =
      (* player name *)
      (let font = assets.p_name_font in
       let (mes_w, _) = font |> Font.measure pl_name in
       let t = Affine.extend t in
       t |> Affine.translate_i
              ((1 - i) * hud_left
               +     i * (hud_w - hud_left))
              hud_top;
       cx |> Ctxt.text pl_name ~t ~c:hud_c ~font
               ~x:(i * -mes_w)
               ~y:0);

      (* hp bar *)
      (let t = Affine.extend t in
       t |> Affine.translate_i
              ((1 - i) * hud_left
               +     i * (hud_w - hud_hpbar_w - hud_left))
              hud_hpbar_y;
       let w  = hud_hpbar_w in
       let w' = hud_hpbar_w * pl_hp / max_hp in
       let h  = hud_hpbar_h in
       cx |> Ctxt.vertices `Fill
               ~t ~c:hud_hpbar_fill_c.(i)
               ~xs:[| 0; w'; w'; 0 |]
               ~ys:[| 0; 0; h; h |];
       cx |> Ctxt.vertices `Strip
               ~t ~c:hud_c
               ~xs:[| 0; w; w; 0; 0 |]
               ~ys:[| 0; 0; h; h; 0 |]);

      (* items *)
      (let t = Affine.extend t in
       t |> Affine.translate_i
              ((1 - i) * hud_item_x
               +     i * (hud_w - hud_item_x))
              hud_item_y;

       (* primary item *)
       (let item = pl_item in
        let t = Affine.extend t in
        t |> Affine.scale hud_item_scale hud_item_scale;
        render_icon_img ~assets ~t cx item);

       (* active item text *)
       (let font = assets.act_item_font in
        let (mes_w, _) = font |> Font.measure hud_item_act_text in
        cx |> Ctxt.text hud_item_act_text
                ~t ~font ~c:hud_item_act_text_c
                ~x:(-mes_w / 2)
                ~y:hud_item_act_text_dy);

       pl_alt_item |>
         Option.iter (fun item ->
             (* alt item *)
             (let t = Affine.extend t in
              t |> Affine.translate_i
                     ((1 - 2 * i) * hud_item_alt_dx)
                     hud_item_alt_dy;
              t |> Affine.scale
                     hud_item_alt_scale hud_item_alt_scale;
              render_icon_img ~assets ~t cx item);

             (* alt item text *)
             (let font = assets.act_item_font in
              let (mes_w, _) = font |> Font.measure hud_item_alt_text in
              cx |> Ctxt.text hud_item_alt_text
                      ~t ~font ~c:hud_item_alt_text_c
                      ~x:((1 - 2 * i) * hud_item_alt_dx - mes_w / 2)
                      ~y:(hud_item_alt_text_dy))))

    let render_turn_indicator ~assets ~time ~t cx
          { tn_num; tn_time; tn_amt0; tn_amt_v }
      =
      let t = Affine.extend t in
      t |> Affine.translate_i hud_turn_x hud_turn_y;

      (* timer text *)
      (let text = Printf.sprintf "turn %d (%.1fs)" tn_num tn_time in
       let font = assets.turn_font in
       let (mes_w, _) = font |> Font.measure text in
       cx |> Ctxt.text text
               ~t ~font ~c:hud_c
               ~x:(-mes_w / 2) ~y:0);

      (* timer bar *)
      (let amt = max 0. (tn_amt0 +. time *. tn_amt_v) in
       let x0, x1 = -hud_turn_bar_w / 2, hud_turn_bar_w / 2 in
       let y0, y1 = hud_turn_bar_dy, hud_turn_bar_dy + hud_turn_bar_h in
       let x1' = x0 + int_of_float (float_of_int hud_turn_bar_w *. amt) in
       cx |> Ctxt.vertices `Fill
               ~t ~c:hud_turn_bar_fill_c
               ~xs:[| x0; x1'; x1'; x0 |]
               ~ys:[| y0;  y0;  y1; y1 |];
       cx |> Ctxt.vertices `Strip
               ~t ~c:hud_c
               ~xs:[| x0; x1; x1; x0; x0 |]
               ~ys:[| y0; y0; y1; y1; y0 |])

    let render cx { assets; player_0; player_1; turn_data; time } =
      let (cx_w, _) = cx |> Ctxt.size in
      let t = Affine.make () in
      t |> Affine.translate_i ((cx_w - hud_w) / 2) hud_y;
      begin
        render_bg ~t cx;
        render_player ~assets ~t cx 0 player_0;
        render_player ~assets ~t cx 1 player_1;
        render_turn_indicator ~assets ~time ~t cx turn_data;
        ()
      end
  end
