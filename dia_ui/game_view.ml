module Evt = View.Evt
module Gameplay = Dia_game.Gameplay
module Input = Dia_game.Input
module Item_type = Dia_game.Item_type
module Path = Dia_game.Path
module Player = Dia_game.Player
module Pos = Dia_game.Pos
module Rules = Dia_game.Rules
module Turn = Dia_game.Gameplay.Turn

module type S =
  View.S with type init = Gameplay.t

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

    module Ctxt = Draw.Ctxt
    module Color = Draw.Color
    module Font = Draw.Font
    module Image = Draw.Image

    module HUD = Hud.Make(Draw)(Rsrc)

    (*** assets ***)

    type game_assets =
      { sprites: Image.t;
        map:     Image.t }

    let game_assets_rsrc =
      let images =
        Rsrc.(all [ image ~path:"sprites";
                    image ~path:"map_stone" ])
      in
      Rsrc.map
        (fun[@ocaml.warning "-8"]
            [ sprites; map ]
         ->
          { sprites; map })
        images

    type assets = game_assets * HUD.assets
    let assets_rsrc = Rsrc.both game_assets_rsrc HUD.assets_rsrc

    (*** init ***)

    type t =
      { assets: game_assets;
        hud: HUD.t;
        mutable game: Gameplay.t;
        (* animations *)
        mutable anim_time: float;
        mutable last_tick_time: float;
        (* players, map *)
        mutable player_0: player_data;
        mutable player_1: player_data;
        (* cursor, path *)
        mutable cursor: Pos.t option;
        mutable path_data: (Path.t * path_type) list }

    and player_data =
      { (* user info *)
        pl_color: int;
        pl_face: int;
        pl_name: string;
        (* map state *)
        pl_pos: Pos.t;
        pl_anim: player_anim }

    and player_anim =
      | Player_idle
      | Player_moving of
          { dis0: float; vel: float;
            s_dis: float; d_dis: float; len: float;
            x_sgn: float; y_sgn: float;
            axis: Path.axis }

    and path_type =
      Gameplay.path_type

    (*** processing game state data ***)

    let path_vel_fl =
      Rules.fps_fl /. Rules.move_rate (* cell/s *)

    let update_player_data ~tick_time:t0 (pl: Player.t) (pl': Player.t) (pd: player_data) =
      let pl_anim =
        if pl'.anim = pl.anim then
          pd.pl_anim
        else
          match pl'.anim with
          | Player.No_anim -> Player_idle
          | Player.Moving p when Path.is_null p -> Player_idle
          | Player.Moving ({ s_dis; d_dis; x_sgn; y_sgn; axis; _ } as pa) ->
             Player_moving
               { dis0  = ~-. t0 *. path_vel_fl;
                 vel   = path_vel_fl;
                 s_dis = float_of_int s_dis;
                 d_dis = float_of_int d_dis;
                 len   = pa |> Path.length;
                 x_sgn = float_of_int x_sgn;
                 y_sgn = float_of_int y_sgn;
                 axis  = axis }
      in
      { pd with pl_pos = pl'.pos; pl_anim }

    let update_from_game game v =
      let tick_time = v.last_tick_time in
      begin
        (* hud *)
        v.hud |> HUD.update_game ~tick_time game;
        (* players, map *)
        v.player_0 <- v.player_0 |> update_player_data ~tick_time
                                      (v.game |> Gameplay.player_0)
                                      (game |> Gameplay.player_0);
        v.player_1 <- v.player_1 |> update_player_data ~tick_time
                                      (v.game |> Gameplay.player_1)
                                      (game |> Gameplay.player_1);
        (* cursor, path *)
        v.cursor <- game |> Gameplay.cursor;
        v.path_data <- game |> Gameplay.paths;
        (* *)
        v.game <- game;
      end

    let update_game f v =
      v |> update_from_game (f v.game)

    (* init *)

    type init = Gameplay.t

    let default_player color face name =
      { pl_color = color;
        pl_face = face;
        pl_name = name;
        pl_pos = (0, 0);
        pl_anim = Player_idle }

    let make (assets, hud_assets) game =
      let v0 =
        { assets;
          game;
          hud = HUD.make hud_assets game;
          (* animations *)
          anim_time = 0.;
          last_tick_time = 0.;
          (* players, map *)
          player_0 = default_player 0 0 "Player One";
          player_1 = default_player 3 2 "Player Two";
          (* cursor, path *)
          cursor = None;
          path_data = [] }
      in
      v0 |> update_from_game game;
      v0

    (*** event handling ***)

    let f_dt = Rules.frame_time

    let update t v =
      v.hud |> HUD.update_time t;
      v.anim_time <- t;
      let tick_t = v.last_tick_time +. f_dt in
      if t > tick_t then
        ( v.last_tick_time <- tick_t;
          v |> update_game Gameplay.tick )

    let handle_evt evt v =
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
           Option.iter (fun i -> v |> update_game (Gameplay.key_dn i))
      | Evt.Key_up kc ->
         input_of_key_code kc |>
           Option.iter (fun i -> v |> update_game (Gameplay.key_up i))

    let switch _disp _v = ()

    (*** rendering ***)

    let bg_c   = Color.of_rgb_s "#5cf"

    (* -- rendering the map -- *)

    let grid_c = Color.of_rgb_s "#ccc"

    let cell_w = 64
    let cell_w_fl = float_of_int cell_w
    let map_w = cell_w * Rules.grid_cols
    let map_y = 260

    let render_map ~t cx { assets; _ } =
      cx |> Ctxt.image assets.map
              ~x:(-64) ~y:(-64) ~t
              ~sx:0 ~sy:0 ~w:640 ~h:640

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

    let render_grid ~t cx =
      cx |> Ctxt.vertices `Lines
              ~t ~c:grid_c ~xs:grid_xs ~ys:grid_ys

    let translate_to_grid_center (col, row) t =
      t |> Affine.translate_i
             (col * cell_w + cell_w / 2)
             (row * cell_w + cell_w / 2)

    let path_sel_c = Color.(of_rgb_s "#fff" |> with_alpha 0.3)
    let path_pl_c = Color.(of_rgb_s "#fff" |> with_alpha 0.4)
    let path_c = function
      | Gameplay.Select_path -> path_sel_c
      | Gameplay.Player_path -> path_pl_c

    let path_rad = 16

    let bent_line_coords s_len d_len x_sgn y_sgn axis =
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

    let render_path ~t cx c Path.{ pos; axis; s_dis; d_dis; x_sgn; y_sgn } =
      let t = Affine.extend t in
      t |> translate_to_grid_center pos;
      let xs, ys = bent_line_coords
                     (s_dis * cell_w)
                     (d_dis * cell_w)
                     x_sgn y_sgn axis in
      cx |> Ctxt.vertices `Fill
              ~t ~c ~xs ~ys

    let cursor_c = Color.of_rgb_s "#fff"
    let cursor_coords =
      let a, b, c = 8, 26, 29 in
      let elbow i =
        let x = i mod 2 in
        let y = i   / 2 in
        let dx = x * 2 - 1 in
        let dy = y * 2 - 1 in
        (*
           c 0-------------1
             |             |
           b 5--------4    |
                      |    |
                      |    |
                      |    |
           a          3----2
             a        b    c
         *)
        [| x+a*dx; x+c*dx; x+c*dx; x+b*dx; x+b*dx; x+a*dx |],
        [| y+c*dy; y+c*dy; y+a*dy; y+a*dy; y+b*dy; y+b*dy |]
      in
      Array.init 4 elbow

    let render_cursor ~t cx pos =
      let t = Affine.extend t in
      t |> translate_to_grid_center pos;
      cursor_coords |> Array.iter
                         (fun (xs, ys) ->
                           cx |> Ctxt.vertices `Fill ~t ~c:cursor_c ~xs ~ys)

    let render_grid_elements ~t cx v =
      begin
        render_grid ~t cx;
        v.path_data |> List.iter (fun (pa, pt) -> render_path ~t cx (path_c pt) pa);
        v.cursor |> Option.iter (render_cursor ~t cx);
      end

    (* -- rendering map elements (players, items) -- *)

    let render_blob_img ~assets ?t cx color face =
      cx |> Ctxt.image assets.sprites
              ~x:(-32) ~y:(-32) ?t
              ~sx:(0 + 64 * face) ~sy:(0 + 64 * color) ~w:64 ~h:64

    let sqrt2_2 = 0.7071067 (* ~ sqrt(2)/2 *)

    let apply_player_anim ~anim_time ~t = function
      | Player_idle ->
         ()

      | Player_moving { dis0; vel; s_dis; d_dis; len; x_sgn; y_sgn; axis } ->
         let d = dis0 +. anim_time *. vel in
         let d' = (d -. s_dis) *. sqrt2_2 in
         let s_off, d_off = if      d <= 0.    then 0., 0.
                            else if d <= s_dis then d, 0.
                            else if d <= len   then s_dis +. d', d'
                            else                    s_dis +. d_dis, d_dis in
         let x_off, y_off = (match axis with
                             | X -> s_off, d_off
                             | Y -> d_off, s_off) in
         t |> Affine.translate
                (x_off *. x_sgn *. cell_w_fl)
                (y_off *. y_sgn *. cell_w_fl)

    let make_player_transform ~t ~anim_time { pl_pos; pl_anim; _ } =
      let t = Affine.extend t in
      t |> translate_to_grid_center pl_pos;
      pl_anim |> apply_player_anim ~anim_time ~t;
      t

    let render_player ~assets ~t cx { pl_color; pl_face; _ } =
      render_blob_img ~assets ~t cx pl_color pl_face

    let render_map_elements ~t cx
          { assets; anim_time; player_0; player_1; _ }
      =
      begin
        let pl0_t = player_0 |> make_player_transform ~t ~anim_time in
        let pl1_t = player_1 |> make_player_transform ~t ~anim_time in
        render_player ~assets ~t:pl0_t cx player_0;
        render_player ~assets ~t:pl1_t cx player_1;
      end

    (* -- main entry point -- *)

    let render cx v : unit =
      let (cx_w, _) = cx |> Ctxt.size in
      cx |> Ctxt.clear ~c:bg_c;

      (* transform for everything on the map *)
      let map_t = Affine.make () in
      map_t |> Affine.translate_i
                 ((cx_w - map_w) / 2)
                 map_y;

      (* draw stuff *)
      begin
        v |> render_map ~t:map_t cx;
        v |> render_grid_elements ~t:map_t cx;
        v |> render_map_elements ~t:map_t cx;
        v.hud |> HUD.render cx;
        ()
      end
  end
