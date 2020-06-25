module Gameplay = Dia_game.Gameplay
module Player = Dia_game.Player
module Rules = Dia_game.Rules
module Spell_type = Dia_game.Spell_type
module Weapon_type = Dia_game.Weapon_type
open Util

module type S = sig
  type assets
  type 'a rsrc
  val assets_rsrc: assets rsrc

  type t
  val make: assets -> Gameplay.t -> t
  val update: float -> t -> unit
  val set_game_data: float -> Gameplay.t -> t -> unit

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

    module Color = Draw.Color
    module Ctxt = Draw.Ctxt
    module Font = Draw.Font
    module Image = Draw.Image

    module Rsrc = struct
      include Applicative(Rsrc)
      include Rsrc
    end

    (*** assets ***)

    type assets =
      { sprites: Image.t;
        turn_font: Font.t;
        hp_font: Font.t;
        stats_font: Font.t }

    let assets_rsrc : assets rsrc =
      let images =
        Rsrc.image ~path:"sprites"
      in
      let fonts =
        Rsrc.(all
                [ font ~family:"space_mono" ~size:14;
                  font ~family:"space_mono_bold" ~size:12;
                  font ~family:"space_mono_bold" ~size:14 ])
      in
      Rsrc.map2
        (fun[@ocaml.warning "-8"]
            sprites
            [ turn_font; hp_font; stats_font ]
         ->
          { sprites; turn_font; hp_font; stats_font })
        images
        fonts

    (*** rendering ***)

    let bg_c = Color.(of_rgb_s "#000" |> with_alpha 0.56)

    (* HUD origin *)

    let base_y = 32
    let padding = 12
    let outl_c = Color.of_rgb_s "#fff"

    let update_base_tf (w, _) tf =
      tf |> Affine.reset;
      tf |> Affine.translate_i (w / 2) base_y

    (* players *)

    let pl_bbox_w = 420
    let pl_bbox_h = 104
    let pl_bbox_xs, pl_bbox_ys = aabb_fill_vertices (0, 0, pl_bbox_w, pl_bbox_h)

    let pl_icon_x = 8
    let pl_icon_y = 8
    let pl_icon_w = 32

    let pl_hpbar_x = 48
    let pl_hpbar_y = 14
    let pl_hpbar_w = 358 - 1
    let pl_hpbar_h = 20 - 1
    let pl_hpbar_hp_c = Color.of_rgb_s "#e00"
    let pl_hpbar_bg_c = Color.(of_rgb_s "#fff" |> with_alpha 0.2)

    let pl_hp_text ~hp = Printf.sprintf "%d HP" hp
    let pl_hp_text_c = Color.of_rgb_s "#900"
    let pl_hp_text_x0 = pl_hpbar_x + pl_hpbar_w - 5
    let pl_hp_text_y0 = pl_hpbar_y + pl_hpbar_h / 2

    let pl_weap_x = 8
    let pl_weap_y = 48
    let pl_weap_w = 48
    let pl_weap_outl_xs, pl_weap_outl_ys =
      aabb_strip_vertices (pl_weap_x, pl_weap_y,
                           pl_weap_x + pl_weap_w, pl_weap_y + pl_weap_w)

    let pl_spell_x = 64
    let pl_spell_y = 52
    let pl_spell_w = 40
    let pl_spell_outl_xs, pl_spell_outl_ys =
      aabb_strip_vertices (pl_spell_x, pl_spell_y,
                           pl_spell_x + pl_spell_w, pl_spell_y + pl_spell_w)

    let pl_stats_x = 112
    let pl_stats_y1 = 54
    let pl_stats_y2 = 78
    let pl_stats_text_c = Color.of_rgb_s "#fff"
    let pl_stats_text ~atk ~def =
      (Printf.sprintf "ATK: %d" atk,
       Printf.sprintf "DEF: %d" def)

    type player_data =
      { pl_tf: Affine.t;
        (* icon *)
        pl_icon_tf: Affine.t;
        pl_color: int;
        (* hp text *)
        mutable pl_hp_text: string;
        mutable pl_hp_text_x: int;
        mutable pl_hp_text_y: int;
        (* hp bar *)
        pl_hpbar_ol: int array * int array;
        pl_hpbar_bg: int array * int array;
        pl_hpbar_fi: int array * int array;
        (* item *)
        pl_weap_tf: Affine.t;
        pl_spell_tf: Affine.t;
        mutable pl_weap: Weapon_type.t;
        pl_spell: Spell_type.t option;
        (* stats *)
        mutable pl_stats_text: string * string }

    let[@ocaml.inline] hpbar_coords amt =
      pl_hpbar_x, pl_hpbar_y,
      int_lerp amt pl_hpbar_x (pl_hpbar_x + pl_hpbar_w),
      pl_hpbar_y + pl_hpbar_h

    let make_player_data base_tf idx (pl: Player.t) =
      (* base tf *)
      let tf = base_tf |> Affine.extend in
      tf |> Affine.translate_i
              (padding / 2
               + (idx - 1) * (pl_bbox_w + padding))
              0;
      (* icon tf *)
      let icon_tf = tf |> Affine.extend in
      icon_tf |> Affine.translate_i
                   (pl_icon_x + pl_icon_w / 2)
                   (pl_icon_y + pl_icon_w / 2);
      (let s = float_of_int pl_icon_w /. 64. in
       icon_tf |> Affine.scale s s);
      (* weapon tf *)
      let weap_tf = tf |> Affine.extend in
      weap_tf |> Affine.translate_i
                   (pl_weap_x + pl_weap_w / 2)
                   (pl_weap_y + pl_weap_w / 2);
      (let s = float_of_int pl_weap_w /. 64. in
       weap_tf |> Affine.scale s s);
      (* spell tf *)
      let spell_tf = tf |> Affine.extend in
      spell_tf |> Affine.translate_i
                    (pl_spell_x + pl_spell_w / 2)
                    (pl_spell_y + pl_spell_w / 2);
      (let s = float_of_int pl_spell_w /. 64. in
       spell_tf |> Affine.scale s s);
      (* *)
      { pl_tf = tf;
        pl_icon_tf = icon_tf;
        pl_color = pl.color;
        pl_hp_text = ""; pl_hp_text_x = 0; pl_hp_text_y = 0;
        pl_hpbar_ol = hpbar_coords 1. |> aabb_strip_vertices;
        pl_hpbar_bg = hpbar_coords 1. |> aabb_fill_vertices;
        pl_hpbar_fi = hpbar_coords 1. |> aabb_fill_vertices;
        pl_weap_tf = weap_tf;
        pl_spell_tf = spell_tf;
        pl_weap = pl.weapon;
        pl_spell = (match pl.color with
                    | 0 -> Some Fire
                    | 1 -> Some Ice
                    | _ -> None);
        pl_stats_text = ("", "") }

    let set_player_data ~assets _time0 (pl: Player.t) player =
      (* hp text *)
      player.pl_hp_text <- pl_hp_text ~hp:pl.hp;
      let (hp_mes_w, hp_mes_h) = assets.hp_font |> Font.measure player.pl_hp_text in
      player.pl_hp_text_x <- pl_hp_text_x0 - hp_mes_w;
      player.pl_hp_text_y <- pl_hp_text_y0 - hp_mes_h / 2;
      (* hp bar *)
      let _, _, hp_x1, _ =
        hpbar_coords
          (float_of_int pl.hp /. float_of_int Rules.max_hp)
      in
      let (fill_xs, _) = player.pl_hpbar_fi in
      fill_xs.(1) <- hp_x1;
      fill_xs.(2) <- hp_x1;
      (* item *)
      player.pl_weap <- pl.weapon;
      (* stats *)
      player.pl_stats_text <- pl_stats_text
                                ~atk:(pl.weapon |> Weapon_type.atk)
                                ~def:0

    let render_player_icon ~assets ~cx tf color =
      let sx = 0 in
      let sy = color * 64 in
      cx |> Ctxt.image assets.sprites
              ~t:tf ~x:(-32) ~y:(-32)
              ~sx ~sy ~w:64 ~h:64

    let render_weap_icon ~assets ~cx tf typ =
      let sx = 352 + Weapon_type.to_int typ * 64 in
      let sy = 0 in
      cx |> Ctxt.image assets.sprites
              ~t:tf ~x:(-32) ~y:(-32)
              ~sx ~sy ~w:64 ~h:64

    let render_spell_icon ~assets ~cx tf typ =
      let sx = 416 + Spell_type.to_int typ * 64 in
      let sy = 64 in
      cx |> Ctxt.image assets.sprites
              ~t:tf ~x:(-32) ~y:(-32)
              ~sx ~sy ~w:64 ~h:64

    let render_player_data ~assets ~cx
          { pl_tf;
            pl_icon_tf; pl_color;
            pl_hp_text; pl_hp_text_x; pl_hp_text_y;
            pl_hpbar_ol = (hp_ol_xs, hp_ol_ys);
            pl_hpbar_bg = (hp_bg_xs, hp_bg_ys);
            pl_hpbar_fi = (hp_fi_xs, hp_fi_ys);
            pl_weap_tf; pl_spell_tf; pl_weap; pl_spell;
            pl_stats_text = (stats_text1, stats_text2) }
      =
      (* bounding box *)
      cx |> Ctxt.vertices `Fill
              ~t:pl_tf ~c:bg_c
              ~xs:pl_bbox_xs ~ys:pl_bbox_ys;
      (* icon *)
      pl_color |> render_player_icon ~assets ~cx pl_icon_tf;
      (* hp bar *)
      cx |> Ctxt.vertices `Fill
              ~t:pl_tf ~c:pl_hpbar_bg_c
              ~xs:hp_bg_xs ~ys:hp_bg_ys;
      cx |> Ctxt.vertices `Fill
              ~t:pl_tf ~c:pl_hpbar_hp_c
              ~xs:hp_fi_xs ~ys:hp_fi_ys;
      cx |> Ctxt.vertices `Strip
              ~t:pl_tf ~c:outl_c
              ~xs:hp_ol_xs ~ys:hp_ol_ys;
      (* hp text *)
      cx |> Ctxt.text pl_hp_text
              ~t:pl_tf ~c:pl_hp_text_c ~font:assets.hp_font
              ~x:pl_hp_text_x ~y:pl_hp_text_y;
      (* item & spell *)
      cx |> Ctxt.vertices `Strip
              ~t:pl_tf ~c:outl_c
              ~xs:pl_weap_outl_xs ~ys:pl_weap_outl_ys;
      cx |> Ctxt.vertices `Strip
              ~t:pl_tf ~c:outl_c
              ~xs:pl_spell_outl_xs ~ys:pl_spell_outl_ys;
      pl_weap |> render_weap_icon ~assets ~cx pl_weap_tf;
      pl_spell |> Option.iter (render_spell_icon ~assets ~cx pl_spell_tf);
      (* stats *)
      cx |> Ctxt.text stats_text1
              ~t:pl_tf ~c:pl_stats_text_c ~font:assets.stats_font
              ~x:pl_stats_x ~y:pl_stats_y1;
      cx |> Ctxt.text stats_text2
              ~t:pl_tf ~c:pl_stats_text_c ~font:assets.stats_font
              ~x:pl_stats_x ~y:pl_stats_y2

    (* turn *)

    let tn_bbox_w = 176
    let tn_bbox_h = 52
    let tn_bbox_xs, tn_bbox_ys = aabb_fill_vertices (0, 0, tn_bbox_w, tn_bbox_h)

    let tn_amt_x = 8
    let tn_amt_y = 32
    let tn_amt_w = 160 - 1
    let tn_amt_h = 12 - 1
    let tn_fill_c = Color.(of_rgb_s "#fff" |> with_alpha 0.56)

    let tn_text_y = 10
    let tn_text_x0 = tn_bbox_w / 2
    let tn_text_c = Color.of_rgb_s "#fff"
    let tn_text ~num ~tim = Printf.sprintf "Turn %d (%.1fs)" num tim

    type turn_data =
      { tn_tf: Affine.t;
        tn_bar_ol: int array * int array;
        tn_bar_fi: int array * int array;
        mutable tn_amt0: float;          (* amt(t) = amt0 + amt_vel * t *)
        mutable tn_amt_vel: float;
        mutable tn_num: int;
        mutable tn_dur: float;
        mutable tn_text: string;
        mutable tn_text_x: int }

    let turn_amt_coords amt =
      tn_amt_x, tn_amt_y,
      int_lerp amt tn_amt_x (tn_amt_x + tn_amt_w),
      (tn_amt_y + tn_amt_h)

    let make_turn_data base_tf =
      let tf = base_tf |> Affine.extend in
      tf |> Affine.translate_i
              (-tn_bbox_w / 2)
              (pl_bbox_h + padding);
      { tn_tf = tf;
        tn_bar_ol = turn_amt_coords 1. |> aabb_strip_vertices;
        tn_bar_fi = turn_amt_coords 1. |> aabb_fill_vertices;
        tn_amt0 = 1.0; tn_amt_vel = 0.; tn_num = 0; tn_dur = 1.;
        tn_text = "";
        tn_text_x = 0 }

    let set_turn_data time0 ~num ~dur turn =
      let (amt0, vel, dur) =
        match dur with
        | None -> (0., 0., 1.)
        | Some(dur) ->
           (* amt0 + vel * t0         = 1.0
              amt0 + vel * (t0 + dur) = 0.0 *)
           let vel = -1. /. dur in
           let amt0 = 1.0 -. vel *. time0 in
           (amt0, vel, dur)
      in
      turn.tn_num <- num;
      turn.tn_amt0 <- amt0;
      turn.tn_amt_vel <- vel;
      turn.tn_dur <- dur

    let update_turn_data ~assets time turn =
      let amt = clamp 0. 1. (turn.tn_amt0 +. turn.tn_amt_vel *. time) in
      (* update text *)
      turn.tn_text <- tn_text
                        ~num:turn.tn_num
                        ~tim:(turn.tn_dur *. amt);
      turn.tn_text_x <- (let (mes_w, _) = assets.turn_font |> Font.measure turn.tn_text in
                         tn_text_x0 - mes_w / 2);
      (* update progress bar *)
      let _, _, amt_x1, _ = turn_amt_coords amt in
      let (fill_xs, _) = turn.tn_bar_fi in
      fill_xs.(1) <- amt_x1;
      fill_xs.(2) <- amt_x1

    let render_turn_data ~assets ~cx
          { tn_tf; tn_text; tn_text_x;
            tn_bar_ol = (outl_xs, outl_ys);
            tn_bar_fi = (fill_xs, fill_ys); _ }
      =
      cx |> Ctxt.vertices `Fill
              ~t:tn_tf ~c:bg_c
              ~xs:tn_bbox_xs ~ys:tn_bbox_ys;
      cx |> Ctxt.vertices `Fill
              ~t:tn_tf ~c:tn_fill_c
              ~xs:fill_xs ~ys:fill_ys;
      cx |> Ctxt.vertices `Strip
              ~t:tn_tf ~c:outl_c
              ~xs:outl_xs ~ys:outl_ys;
      cx |> Ctxt.text tn_text
              ~font:assets.turn_font
              ~t:tn_tf ~c:tn_text_c
              ~x:tn_text_x ~y:tn_text_y

    (* entrypoint *)

    type t =
      { assets: assets;
        base_tf: Affine.t;
        player_0: player_data;
        player_1: player_data;
        turn: turn_data }

    let render cx
          { assets; base_tf; player_0; player_1; turn }
      =
      begin
        base_tf |> update_base_tf (cx |> Ctxt.size);
        player_0 |> render_player_data ~assets ~cx;
        player_1 |> render_player_data ~assets ~cx;
        turn |> render_turn_data ~assets ~cx;
      end

    (*** processing game state ***)

    let update time
          { assets; turn; _ }
      =
      turn |> update_turn_data ~assets time

    let set_game_data time0 g
          { assets; turn; player_0; player_1; _ }
      =
      turn |> set_turn_data time0
                ~num:(g |> Gameplay.turn_num)
                ~dur:(g |> Gameplay.turn_duration);
      player_0 |> set_player_data ~assets time0
                    (g |> Gameplay.player_0);
      player_1 |> set_player_data ~assets time0
                    (g |> Gameplay.player_1)

    (*** init ***)

    let make assets g =
      let base_tf = Affine.make () in
      { assets; base_tf;
        player_0 = g |> Gameplay.player_0 |> make_player_data base_tf 0;
        player_1 = g |> Gameplay.player_1 |> make_player_data base_tf 1;
        turn = make_turn_data base_tf }
  end
