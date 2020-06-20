module Gameplay = Dia_game.Gameplay
module Rules = Dia_game.Rules
module Item_type = Dia_game.Item_type

module type S = sig
  type assets
  type 'a rsrc
  val assets_rsrc: assets rsrc

  type t
  val make: assets -> Gameplay.t -> t
  val update: float -> t -> unit
  val update_game: float -> Gameplay.t -> t -> unit

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
        base_tf: Affine.t;
        mutable player_0: player_data;
        mutable player_1: player_data;
        mutable turn_data: turn_data }

    and player_data =
      { pl_idx: int;
        pl_name: string;
        pl_hp: int;
        pl_item: Item_type.t;
        pl_name_tf: Affine.t;
        pl_hpbar_tf: Affine.t;
        pl_items_tf: Affine.t;
        pl_item_icon_tf: Affine.t }

    and turn_data =
      { tn_tf: Affine.t;
        mutable tn_num: int;
        mutable tn_dur: float;
        (* amt(t) = amt0 + t * amt_v *)
        mutable tn_amt0: float;
        mutable tn_amt_v: float;
        mutable tn_text: string;
        tn_bar_o: int array * int array;
        tn_bar_f: int array * int array }

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
    let hud_item_act_text = "active item"
    let hud_item_act_text_c = Color.of_rgb_s "#ff0"
    let hud_item_act_text_dy = 24
    let hud_turn_x = hud_w / 2
    let hud_turn_y = 78
    let hud_turn_bar_w = 160
    let hud_turn_bar_h = 12
    let hud_turn_bar_dy = 20
    let hud_turn_bar_fill_c = Color.(of_rgb_s "#fff" |> with_alpha 0.6)

    (* background *)

    let update_base_tf (w, _) tf =
      tf |> Affine.reset;
      tf |> Affine.translate_i ((w - hud_w) / 2) hud_y

    let bg_xs, bg_ys = [| 0; hud_w; hud_w; 0     |],
                       [| 0; 0;     hud_h; hud_h |]

    let render_bg cx tf =
      cx |> Ctxt.vertices `Fill
              ~t:tf ~c:hud_bg_c
              ~xs:bg_xs ~ys:bg_ys

    (* player info *)

    let make_player_data ~name ~hp ~item base_tf i =
      let name_tf = base_tf |> Affine.extend in
      let hpbar_tf = base_tf |> Affine.extend in
      let items_tf = base_tf |> Affine.extend in
      let item_icon_tf = items_tf |> Affine.extend in
      name_tf |> Affine.translate_i
                   ((1 - i) * hud_left
                    +     i * (hud_w - hud_left))
                   hud_top;
      hpbar_tf |> Affine.translate_i
                    ((1 - i) * hud_left
                     +     i * (hud_w - hud_hpbar_w - hud_left))
                    hud_hpbar_y;
      items_tf |> Affine.translate_i
                    ((1 - i) * hud_item_x
                     +     i * (hud_w - hud_item_x))
                    hud_item_y;
      item_icon_tf |> Affine.scale
                        hud_item_scale hud_item_scale;
      { pl_idx = i;
        pl_name = name;
        pl_hp = hp;
        pl_item = item;
        pl_name_tf = name_tf;
        pl_hpbar_tf = hpbar_tf;
        pl_items_tf = items_tf;
        pl_item_icon_tf = item_icon_tf }

    let render_icon_img ~assets cx ty tf =
      let row = match ty with `S -> 0 | `F -> 1 | `P -> 2 | `H -> 3 in
      cx |> Ctxt.image assets.sprites
              ~x:(-32) ~y:(-32) ~t:tf
              ~sx:384 ~sy:(row * 64) ~w:64 ~h:64

    let render_player ~assets cx
          { pl_idx; pl_name; pl_hp; pl_item;
            pl_name_tf; pl_hpbar_tf; pl_items_tf; pl_item_icon_tf }
      =
      (* player name *)
      (let font = assets.p_name_font in
       let (mes_w, _) = font |> Font.measure pl_name in
       cx |> Ctxt.text pl_name
               ~c:hud_c ~font ~t:pl_name_tf
               ~x:(pl_idx * -mes_w) ~y:0 );

      (* hp bar *)
      (let w  = hud_hpbar_w in
       let w' = hud_hpbar_w * pl_hp / Rules.max_hp in
       let h  = hud_hpbar_h in
       cx |> Ctxt.vertices `Fill
               ~c:hud_hpbar_fill_c.(pl_idx) ~t:pl_hpbar_tf
               ~xs:[| 0; w'; w'; 0 |]
               ~ys:[| 0; 0; h; h |];
       cx |> Ctxt.vertices `Strip
               ~c:hud_c ~t:pl_hpbar_tf
               ~xs:[| 0; w; w; 0; 0 |]
               ~ys:[| 0; 0; h; h; 0 |]);

      (* items *)
      (pl_item_icon_tf |> render_icon_img ~assets cx pl_item;
       let font = assets.act_item_font in
       let (mes_w, _) = font |> Font.measure hud_item_act_text in
       cx |> Ctxt.text hud_item_act_text
               ~font ~c:hud_item_act_text_c ~t:pl_items_tf
               ~x:(-mes_w / 2)
               ~y:hud_item_act_text_dy)

    (* turn data *)

    let turn_bar_coords amt =
      let y0, y1 = hud_turn_bar_dy, hud_turn_bar_dy + hud_turn_bar_h in
      let x0 = -hud_turn_bar_w / 2 in
      let x1 = x0 + int_of_float (float_of_int hud_turn_bar_w *. amt) in
      x0, y0, x1, y1

    let make_turn_data base_tf =
      let tf = base_tf |> Affine.extend in
      tf |> Affine.translate_i
              hud_turn_x
              hud_turn_y;
      let x0, y0, x1, y1 = turn_bar_coords 1. in
      { tn_tf = tf;
        tn_num = 0; tn_dur = 0.;
        tn_amt0 = 0.; tn_amt_v = 0.;
        tn_text = "";
        tn_bar_o = [| x0; x1; x1; x0; x0 |], [| y0; y0; y1; y1; y0 |];
        tn_bar_f = [| x0; x1; x1; x0 |],     [| y0; y0; y1; y1 |] }

    let start_turn time0 num tn =
      (* amt(t0)      = 1
         amt(t0 + dt) = 0 *)
      tn.tn_num <- num;
      tn.tn_dur <- Rules.turn_duration;
      tn.tn_amt_v <- -1. /. Rules.turn_duration;
      tn.tn_amt0 <- 1. -. time0 *. tn.tn_amt_v

    let update_turn time
          ({ tn_num; tn_dur; tn_amt0; tn_amt_v; _ } as tn)
      =
      let tn_amt = max 0. (tn_amt0 +. time *. tn_amt_v) in
      let tn_time = tn_amt *. tn_dur in
      let fill_xs, _ = tn.tn_bar_f in
      let _, _, fill_x1, _ = turn_bar_coords tn_amt in
      tn.tn_text <- Printf.sprintf "turn %d (%.1fs)" tn_num tn_time;
      fill_xs.(1) <- fill_x1;
      fill_xs.(2) <- fill_x1

    let render_turn_indicator ~assets cx
          { tn_tf; tn_text; tn_bar_o; tn_bar_f; _ }
      =
      (let font = assets.turn_font in
       let (mes_w, _) = font |> Font.measure tn_text in
       cx |> Ctxt.text tn_text
               ~font ~c:hud_c ~t:tn_tf
               ~x:(-mes_w / 2) ~y:0);

      (let outl_xs, outl_ys = tn_bar_o in
       let fill_xs, fill_ys = tn_bar_f in
       cx |> Ctxt.vertices `Fill
               ~c:hud_turn_bar_fill_c ~t:tn_tf
               ~xs:fill_xs ~ys:fill_ys;
       cx |> Ctxt.vertices `Strip
               ~c:hud_c ~t:tn_tf
               ~xs:outl_xs ~ys:outl_ys)

    let render cx { assets; base_tf; player_0; player_1; turn_data; _ } : unit =
      begin
        base_tf |> update_base_tf (cx |> Ctxt.size);
        base_tf |> render_bg cx;
        player_0 |> render_player ~assets cx;
        player_1 |> render_player ~assets cx;
        turn_data |> render_turn_indicator ~assets cx;
      end

    (*** processing game state data ***)

    let update time hud =
      hud.turn_data |> update_turn time

    let update_game time0 game hud =
      let tn_num = game |> Gameplay.turn in
      if tn_num <> hud.turn_data.tn_num then
        hud.turn_data |> start_turn time0 tn_num

    (*** init ***)

    let make assets _game =
      let base_tf = Affine.make () in
      { assets;
        base_tf;
        player_0 = make_player_data base_tf 0
                     ~name:"Player One" ~hp:Rules.max_hp ~item:`S;
        player_1 = make_player_data base_tf 1
                     ~name:"Player Two" ~hp:Rules.max_hp ~item:`H;
        turn_data = make_turn_data base_tf }
  end
