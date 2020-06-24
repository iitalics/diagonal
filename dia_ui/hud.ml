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

    (*** type defs ***)

    type t =
      { base_tf: Affine.t;
        player_0: player_data;
        player_1: player_data }

    and player_data =
      { pl_tf: Affine.t }

    (*** rendering ***)

    let bg_c = Color.(of_rgb_s "#000" |> with_alpha 0.56)

    (* HUD origin *)

    let base_y = 20

    let update_base_tf (w, _) tf =
      tf |> Affine.reset;
      tf |> Affine.translate_i (w / 2) base_y

    (* players *)

    let pl_bbox_w = 420
    let pl_bbox_h = 104
    let pl_bbox_pad = 12
    let pl_bbox_xs, pl_bbox_ys =
      [| 0; pl_bbox_w; pl_bbox_w; 0 |],
      [| 0; 0; pl_bbox_h; pl_bbox_h |]

    let make_player_data base_tf idx =
      let tf = base_tf |> Affine.extend in
      tf |> Affine.translate_i
              (pl_bbox_pad / 2
               + (idx - 1) * (pl_bbox_w + pl_bbox_pad))
              0;
      { pl_tf = tf }

    let render_player_data ~cx { pl_tf } =
      cx |> Ctxt.vertices `Fill
              ~t:pl_tf ~c:bg_c
              ~xs:pl_bbox_xs ~ys:pl_bbox_ys

    let render cx
          { base_tf; player_0; player_1 }
      =
      begin
        base_tf |> update_base_tf (cx |> Ctxt.size);
        player_0 |> render_player_data ~cx;
        player_1 |> render_player_data ~cx;
      end

    (*** processing game state ***)

    let update _time _hud = ()
    let update_game _time0 _g _hud = ()

    (*** init ***)

    let make _assets _g =
      let base_tf = Affine.make () in
      { base_tf;
        player_0 = make_player_data base_tf 0;
        player_1 = make_player_data base_tf 1 }
  end
