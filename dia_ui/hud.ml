module Gameplay = Dia_game.Gameplay

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

    module Color = Draw.Color
    module Ctxt = Draw.Ctxt

    (*** assets ***)

    type assets = A
    let assets_rsrc = Rsrc.const A

    (*** rendering ***)

    let bg_c = Color.(of_rgb_s "#000" |> with_alpha 0.56)

    (* HUD origin *)

    let base_y = 20
    let padding = 12

    let update_base_tf (w, _) tf =
      tf |> Affine.reset;
      tf |> Affine.translate_i (w / 2) base_y

    (* players *)

    type player_data =
      { pl_tf: Affine.t }

    let pl_bbox_w = 420
    let pl_bbox_h = 104
    let pl_bbox_xs, pl_bbox_ys =
      [| 0; pl_bbox_w; pl_bbox_w; 0 |],
      [| 0; 0; pl_bbox_h; pl_bbox_h |]

    let make_player_data base_tf idx =
      let tf = base_tf |> Affine.extend in
      tf |> Affine.translate_i
              (padding / 2
               + (idx - 1) * (pl_bbox_w + padding))
              0;
      { pl_tf = tf }

    let render_player_data ~cx { pl_tf } =
      cx |> Ctxt.vertices `Fill
              ~t:pl_tf ~c:bg_c
              ~xs:pl_bbox_xs ~ys:pl_bbox_ys

    (* turn *)

    let tn_bbox_w = 176
    let tn_bbox_h = 52
    let tn_bbox_xs, tn_bbox_ys =
      [| 0; tn_bbox_w; tn_bbox_w; 0 |],
      [| 0; 0; tn_bbox_h; tn_bbox_h |]

    let tn_amt_x = 8
    let tn_amt_y = 32
    let tn_amt_w = 160 - 1
    let tn_amt_h = 12 - 1
    let tn_outl_c = Color.(of_rgb_s "#fff")
    let tn_fill_c = Color.(of_rgb_s "#fff" |> with_alpha 0.56)

    let tn_amt_coords amt =
      let x0, y0 = tn_amt_x, tn_amt_y in
      let x1 = x0 + int_of_float (float_of_int tn_amt_w *. amt) in
      let y1 = y0 + tn_amt_h in
      x0, y0, x1, y1

    type turn_data =
      { tn_tf: Affine.t;
        tn_bar_ol: int array * int array;
        tn_bar_fi: int array * int array }

    let make_turn_data base_tf =
      let tf = base_tf |> Affine.extend in
      tf |> Affine.translate_i
              (-tn_bbox_w / 2)
              (pl_bbox_h + padding);
      let a_x0, a_y0, a_x1, a_y1 = tn_amt_coords 1. in
      { tn_tf = tf;
        tn_bar_ol = [| a_x0; a_x1; a_x1; a_x0; a_x0 |],
                    [| a_y0; a_y0; a_y1; a_y1; a_y0 |];
        tn_bar_fi = [| a_x0; a_x1; a_x1; a_x0 |],
                    [| a_y0; a_y0; a_y1; a_y1 |] }

    let render_turn_data ~cx
          { tn_tf;
            tn_bar_ol = (outl_xs, outl_ys);
            tn_bar_fi = (fill_xs, fill_ys) }
      =
      cx |> Ctxt.vertices `Fill
              ~t:tn_tf ~c:bg_c
              ~xs:tn_bbox_xs ~ys:tn_bbox_ys;
      cx |> Ctxt.vertices `Fill
              ~t:tn_tf ~c:tn_fill_c
              ~xs:fill_xs ~ys:fill_ys;
      cx |> Ctxt.vertices `Strip
              ~t:tn_tf ~c:tn_outl_c
              ~xs:outl_xs ~ys:outl_ys

    (* entrypoint *)

    type t =
      { base_tf: Affine.t;
        player_0: player_data;
        player_1: player_data;
        turn: turn_data }

    let render cx
          { base_tf; player_0; player_1; turn }
      =
      begin
        base_tf |> update_base_tf (cx |> Ctxt.size);
        player_0 |> render_player_data ~cx;
        player_1 |> render_player_data ~cx;
        turn |> render_turn_data ~cx;
      end

    (*** processing game state ***)

    let update _time _hud = ()
    let update_game _time0 _g _hud = ()

    (*** init ***)

    let make _assets _g =
      let base_tf = Affine.make () in
      { base_tf;
        player_0 = make_player_data base_tf 0;
        player_1 = make_player_data base_tf 1;
        turn = make_turn_data base_tf }
  end
