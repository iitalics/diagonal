module Entity = Dia_game.Entity
module Evt = View.Evt
module Gameplay = Dia_game.Gameplay
module Input = Dia_game.Input
module Item_type = Dia_game.Item_type
module Path = Dia_game.Path
module Player = Dia_game.Player
module Pos = Dia_game.Pos
module Rules = Dia_game.Rules
open Util

module type S =
  View.S with type init = Gameplay.t

module Make
         (Draw: Intf.Draw_S)
         (Rsrc: Intf.Rsrc_S with type font = Draw.Font.t
                             and type image = Draw.Image.t)
         (View_disp: View.Dispatcher_S with type draw_ctxt = Draw.Ctxt.t
                                        and type 'a rsrc = 'a Rsrc.t)
(*
       : S with type 'a rsrc = 'a Rsrc.t
            and type view_disp = View_disp.t
            and type draw_ctxt = Draw.Ctxt.t
 *)
  =
  struct
    type 'a rsrc = 'a Rsrc.t
    type view_disp = View_disp.t
    type draw_ctxt = Draw.Ctxt.t

    module Rsrc = struct
      include Rsrc
      include Applicative(Rsrc)
    end

    module Ctxt = Draw.Ctxt
    module Color = Draw.Color
    module Font = Draw.Font
    module Image = Draw.Image

    module HUD = Hud.Make(Draw)(Rsrc)

    (*** assets ***)

    type game_assets =
      { sprites: Image.t;
        map:     Image.t;
        dmg_font: Font.t }

    let game_assets_rsrc =
      let images =
        Rsrc.(all [ image ~path:"sprites";
                    image ~path:"map_stone" ])
      in
      let fonts =
        Rsrc.font ~family:"nunito" ~size:16
      in
      Rsrc.map2
        (fun[@ocaml.warning "-8"]
            [ sprites; map ]
            dmg_font
         ->
          { sprites; map; dmg_font })
        images
        fonts

    type assets = game_assets * HUD.assets
    let assets_rsrc = Rsrc.both game_assets_rsrc HUD.assets_rsrc

    (*** init ***)

    type t =
      { assets: game_assets;
        hud: HUD.t;
        mutable game: Gameplay.t;
        mutable phase_start_time: float;
        (* map, entities *)
        map_tf: Affine.t;
        mutable ents: entity_data array;
        (* cursor, path *)
        cursor_tf: Affine.t;
        mutable path_data: path_data array;
        mutable hit_marks: hit_type array;
        mutable hit_mark_tfs: Affine.t array }

    and entity_data =
      { en_id: Entity.id;
        en_look: entity_look;
        en_tf: Affine.t;
        mutable en_z_order: float }

    and entity_look =
      | Hidden
      | Blob of
          { color: int; face: int;
            x: float; y: float;
            path: path_anim option }
      | Item of
          { typ: int;
            x: float; y: float }

    and path_anim =
      { dis0: float; vel: float;
        s_dis: float; d_dis: float; len: float;
        x_sgn: float; y_sgn: float;
        axis: Path.axis }

    and path_data =
      { pa_tf: Affine.t;
        pa_type: Gameplay.path_type;
        pa_xs: int array;
        pa_ys: int array }

    and hit_type =
      Gameplay.hit_type

    (*** rendering ***)

    (* map & background *)

    let bg_c = Color.of_rgb_s "#5cf"
    let map_y = 260
    let map_w = 512

    let update_map_tf (w, _) tf =
      tf |> Affine.reset;
      tf |> Affine.translate_i
                 ((w - map_w) / 2)
                 map_y

    let render_map_and_bg ~cx { assets; map_tf; _ } =
      cx |> Ctxt.clear ~c:bg_c;
      cx |> Ctxt.image assets.map
              ~x:(-64) ~y:(-64) ~t:map_tf
              ~sx:0 ~sy:0 ~w:640 ~h:640

    (* grid *)

    let cell_w = 64
    let cell_w_fl = float_of_int cell_w

    let[@ocaml.inline] translate_to_grid_center_fl (col, row) tf =
      tf |> Affine.translate
              ((col +. 0.5) *. cell_w_fl)
              ((row +. 0.5) *. cell_w_fl)

    let[@ocaml.inline] translate_to_grid_center (col, row) tf =
      tf |> translate_to_grid_center_fl
              (float_of_int col, float_of_int row)

    let grid_c = Color.of_rgb_s "#ccc"

    let grid_xs, grid_ys =
      let map_w = cell_w * Rules.grid_cols in
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

    let render_grid ~cx tf =
      cx |> Ctxt.vertices `Lines
              ~t:tf ~c:grid_c ~xs:grid_xs ~ys:grid_ys

    (* path *)

    let path_sel_c = Color.(of_rgb_s "#fff" |> with_alpha 0.3)
    let path_pl_c = Color.(of_rgb_s "#fff" |> with_alpha 0.4)
    let path_c = function
      | Gameplay.Select_path -> path_sel_c
      | Gameplay.Player_path -> path_pl_c

    let path_rad = 16

    let bent_line_vertices s_len d_len x_sgn y_sgn axis =
      let r, r', r'' =
        path_rad,
        path_rad * 707 / 1000, (* ~ r * sin(45 deg) *)
        path_rad * 414 / 1000  (* ~ r * tan(22.5 deg) *)
      in
      (*
         0--------------1          + = ( 0, 0)
         +            *  \         * = (l1, 0)
         5-----------4    \        @ = (l2,l2)
                      \    \
                       3-@--2

                       2--@-3
                      /    /
         0-----------1    /
         +            *  /
         5--------------4
       *)
      let l1, l2, l3 = s_len, s_len + d_len, d_len in
      let maj i = [|     0; i*(l1 + r''); i*(l2 + r'); i*(l2 - r'); i*(l1 - r'');   0 |] in
      let min i = [| i* -r; i*     -r   ; i*(l3 - r'); i*(l3 + r'); i*      r   ; i*r |] in
      match axis with
      | Path.X -> maj x_sgn, min y_sgn
      | Path.Y -> min x_sgn, maj y_sgn

    let make_path_data base_tf path typ =
      let Path.{ pos; axis; s_dis; d_dis; x_sgn; y_sgn; _ } = path in
      let tf = base_tf |> Affine.extend in
      tf |> translate_to_grid_center pos;
      let xs, ys = bent_line_vertices
                     (s_dis * cell_w)
                     (d_dis * cell_w)
                     x_sgn y_sgn axis in
      { pa_tf = tf;
        pa_type = typ;
        pa_xs = xs; pa_ys = ys }

    let make_path_data_array base_tf (path_list: (Path.t * Gameplay.path_type) list) =
      path_list
      |> List.rev_map (fun (pa, pt) -> make_path_data base_tf pa pt)
      |> Array.of_list

    let render_path ~cx { pa_tf; pa_type; pa_xs; pa_ys } =
      cx |> Ctxt.vertices `Fill
              ~t:pa_tf ~c:(path_c pa_type)
              ~xs:pa_xs ~ys:pa_ys

    (* cursor, hit marks *)

    let dmg_text_c = Color.of_rgb_s "#f00"

    let update_cursor_tf (cur: Pos.t option) tf =
      tf |> Affine.reset;
      match cur with
      | Some(pos) -> tf |> translate_to_grid_center pos
      | None      -> tf |> Affine.scale 0. 0.

    let render_cursor ~assets ~cx tf =
      cx |> Ctxt.image assets.sprites
              ~x:(-32) ~y:(-32) ~t:tf
              ~sx:768 ~sy:160 ~w:64 ~h:64

    let make_hit_mark_array base_tf (h: Gameplay.hits) =
      let tfs, mks =
        (h.hits_player_0 @ h.hits_player_1)
        |> List.rev_map
             (fun Gameplay.{ hit_pos; hit_type } ->
               let tf = base_tf |> Affine.extend in
               tf |> translate_to_grid_center hit_pos;
               (tf, hit_type))
        |> List.rev_split
      in
      Array.of_list mks, Array.of_list tfs

    let render_hit_mark ~assets ~cx i (typ: hit_type) tf =
      let sx, sy = match typ with
        | Attk -> 832, 160
        | Crit -> 896, 160
        (* | Dfnd -> 832, 224
           | Vuln -> 896, 224 *)
      in
      cx |> Ctxt.image assets.sprites
              ~x:(-32) ~y:(-32) ~t:tf
              ~sx ~sy ~w:64 ~h:64;
      ignore (dmg_text_c, i)
      (*
      cx |> Ctxt.text (Printf.sprintf "(%d)" i)
              ~x:32 ~y:(-10) ~t:tf
              ~font:assets.dmg_font ~c:dmg_text_c *)

    let render_grid_elements_below ~cx
          { map_tf; path_data; _ }
      =
      begin
        map_tf |> render_grid ~cx;
        path_data |> Array.iter (render_path ~cx);
      end

    let render_grid_elements_above ~cx
          { assets; cursor_tf; hit_marks; hit_mark_tfs; _ }
      =
      begin
        cursor_tf |> render_cursor ~assets ~cx;
        hit_marks |> Array.iteri
                       (fun i typ ->
                         hit_mark_tfs.(i) |> render_hit_mark ~assets ~cx i typ);
      end

    (* entities *)

    let sqrt2_2 = 0.7071067 (* ~ sqrt(2)/2 *)

    let make_path_anim time0 (pa: Path.t) =
      let vel = Rules.move_vel in
      { dis0  = ~-. time0 *. vel;
        vel;
        s_dis = float_of_int pa.s_dis;
        d_dis = float_of_int pa.d_dis;
        len   = pa |> Path.length;
        x_sgn = float_of_int pa.x_sgn;
        y_sgn = float_of_int pa.y_sgn;
        axis  = pa.axis }

    let subcell_off_of_path_anim time
          { dis0; vel; s_dis; d_dis; len; x_sgn; y_sgn; axis; _ }
      =
      let d = dis0 +. time *. vel in
      let d' = (d -. s_dis) *. sqrt2_2 in
      let s_off, d_off = if      d <= s_dis then d, 0.
                         else if d <= len   then s_dis +. d', d'
                         else                    s_dis +. d_dis, d_dis in
      match axis with
      | X -> (s_off *. x_sgn, d_off *. y_sgn)
      | Y -> (d_off *. x_sgn, s_off *. y_sgn)

    let subcell_pos_of_entity_look time = function
      | Hidden ->
         (0., 0.)
      | Blob { x; y; path = None; _ } ->
         (x, y)
      | Blob { x; y; path = Some a; _ } ->
         let (dx, dy) = a |> subcell_off_of_path_anim time in
         (x +. dx, y +. dy)
      | Item { x; y; _ } ->
         (x, y)

    let blob_entity_look Player.{ color; _ } (x, y) path =
      let face = [| 0; 3 |].(color) in
      Blob { color; face; path;
             x = float_of_int x;
             y = float_of_int y }

    let item_entity_look typ (x, y) =
      Item { typ = Item_type.to_int typ;
             x = float_of_int x;
             y = float_of_int y }

    let make_entity_data time0 base_tf (en: Entity.t) =
      let look =
        match en.typ with
        | Entity.Blob_idle (pl, pos) ->
           blob_entity_look pl pos None;
        | Entity.Blob_moving (pl, pa) ->
           blob_entity_look pl (pa |> Path.source) (Some (pa |> make_path_anim time0))
        | Entity.Item (typ, pos) ->
           item_entity_look typ pos
      in
      { en_id = en.id;
        en_look = look;
        en_tf = base_tf |> Affine.extend;
        en_z_order = 0. }

    let update_entity_tf time (e: entity_data) =
      let (cx, cy) = subcell_pos_of_entity_look time e.en_look in
      e.en_z_order <- cy;
      e.en_tf |> Affine.reset;
      e.en_tf |> translate_to_grid_center_fl (cx, cy)

    let render_blob_img ~assets ~cx color face tf =
      cx |> Ctxt.image assets.sprites
              ~x:(-32) ~y:(-32) ~t:tf
              ~sx:(0 + 64 * face) ~sy:(0 + 64 * color) ~w:64 ~h:64

    let entity_look_sprite_clip = function
      | Hidden ->
         (0, 0, 0, 0, 0, 0)
      | Blob { color; face; _ } ->
         let sx, sy = 64 * face, 64 * color in
         (-32, -32, sx, sy, 64, 64)
      | Item { typ; _ } ->
         let sx = 352 + typ * 64 in
         (-32, -32, sx, 0, 64, 64)

    let render_entity ~assets ~cx { en_look; en_tf; _ } =
      let (x, y, sx, sy, w, h) = en_look |> entity_look_sprite_clip in
      cx |> Ctxt.image assets.sprites
              ~x ~y ~sx ~sy ~w ~h ~t:en_tf

    let make_entity_array time base_tf (ens: Entity.t list) =
      ens
      |> List.rev_map (make_entity_data time base_tf)
      |> Array.of_list

    let update_map_elements time { ents; _ } =
      let compare { en_z_order = z1; _ } { en_z_order = z2; _ } =
        Float.compare z1 z2
      in
      ents |> Array.sort compare;
      ents |> Array.iter (update_entity_tf time)

    let render_map_elements ~cx { assets; ents; _ } =
      ents |> Array.iter (render_entity ~assets ~cx)

    (* -- main entry point -- *)

    let render cx v : unit
      =
      begin
        v.map_tf |> update_map_tf (cx |> Ctxt.size);
        v |> render_map_and_bg ~cx;
        v |> render_grid_elements_below ~cx;
        v |> render_map_elements ~cx;
        v |> render_grid_elements_above ~cx;
        v.hud |> HUD.render cx;
        ()
      end

    (*** processing game state data ***)

    let update_game time0 (game: Gameplay.t) (v: t) =
      (* entities *)
      v.ents <- make_entity_array time0 v.map_tf (game |> Gameplay.entities);
      (* cursor *)
      v.cursor_tf |> update_cursor_tf (game |> Gameplay.cursor);
      (* hit marks *)
      let hit_mks, hit_tfs = game |> Gameplay.hits |> make_hit_mark_array v.map_tf in
      v.hit_marks <- hit_mks;
      v.hit_mark_tfs <- hit_tfs;
      (* paths *)
      v.path_data <- game |> Gameplay.paths |> make_path_data_array v.map_tf;
      (* HUD *)
      v.hud |> HUD.update_game time0 game;
      v.game <- game

    (*** event handling ***)

    let update time v =
      (* update game phase *)
      let rec advance_phase_loop start_time =
        let dur = v.game |> Gameplay.phase_duration in
        let end_time = start_time +. dur in
        if time >= end_time then
          ( v |> update_game end_time (v.game |> Gameplay.end_phase);
            advance_phase_loop end_time )
        else
          v.phase_start_time <- start_time
      in
      advance_phase_loop v.phase_start_time;
      (* update animations *)
      v.hud |> HUD.update time;
      v |> update_map_elements time

    let handle_evt evt v =
      let update_by f =
        v |> update_game v.phase_start_time (f v.game)
      in
      let input_of_key_code = function
        | "ArrowLeft"  -> Some Input.Key.Left
        | "ArrowRight" -> Some Input.Key.Right
        | "ArrowUp"    -> Some Input.Key.Up
        | "ArrowDown"  -> Some Input.Key.Down
        | "Escape"     -> Some Input.Key.Esc
        | _            -> None
      in
      match evt with
      | Evt.Key_dn kc ->
         input_of_key_code kc |>
           Option.iter (fun i -> update_by (Gameplay.key_dn i))
      | Evt.Key_up kc ->
         input_of_key_code kc |>
           Option.iter (fun i -> update_by (Gameplay.key_up i))

    let switch _disp _v = ()

    (*** init ***)

    type init = Gameplay.t

    let make (assets, hud_assets) game =
      let map_tf = Affine.make () in
      let v0 =
        { assets;
          game;
          hud = HUD.make hud_assets game;
          phase_start_time = 0.;
          (* map, entities *)
          map_tf;
          ents = [||];
          (* cursor, path *)
          cursor_tf = map_tf |> Affine.extend;
          path_data = [||];
          hit_marks = [||];
          hit_mark_tfs = [||] }
      in
      v0 |> update_game 0. game;
      v0
  end
