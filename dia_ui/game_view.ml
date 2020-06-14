module Evt = View.Evt
module Gameplay = Dia_game.Gameplay
module Input = Dia_game.Input
module Path = Dia_game.Path
module Player = Dia_game.Gameplay.Player
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

    type pos = int * int
    type item_type = [ `S | `F | `P | `H ]

    type t =
      { assets: assets;
        mutable game: Gameplay.t;
        (* animations *)
        mutable anim_time: float;
        mutable last_tick_time: float;
        (* players, map *)
        mutable player_0: player_data;
        mutable player_1: player_data;
        (* cursor, path *)
        mutable cursor: pos option;
        mutable path_data: path_data option;
        (* turn *)
        mutable turn_data: turn_data }

    and player_data =
      { (* user info *)
        pl_color: int;
        pl_face: int;
        pl_name: string;
        (* player state *)
        pl_hp: int;
        pl_item: item_type;
        pl_alt_item: item_type option;
        pl_switching: bool;
        (* map state *)
        pl_pos: pos;
        pl_anim: player_anim }

    and player_anim =
      | Player_idle
      | Player_move of
          (* x(t) = x0 + t * x_v *)
          (* y(t) = y0 + t * y_v *)
          { x0: float; y0: float;
            x_v: float; y_v: float }

(*
    and item =
      { it_type: item_type;
        it_pos: pos }
 *)

    and path_data =
      { pa_pos: pos;
        pa_s_dis: int;
        pa_d_dis: int;
        pa_s_sgn: int;
        pa_d_sgn: int;
        pa_axis: Path.axis }

    and turn_data =
      { tn_num: int;
        tn_time: float;
        (* amt(t) = amt0 + t * amt_v *)
        tn_amt0: float;
        tn_amt_v: float }

    let max_hp = 16

    (*** processing game state data ***)

    let update_player_data ~time:t0 (pl: Player.t) (pl': Player.t) (pd: player_data) =
      let pl_pos = pl'.pos in
      let pl_anim =
        if pl'.anim = pl.anim then
          pd.pl_anim
        else
          match pl'.anim with
          | Player.No_anim -> Player_idle
          | Player.Moving np when np |> Path.is_null -> Player_idle
          | Player.Moving path ->
             let (sx, sy) = path |> Path.source in
             let (tx, ty) = path |> Path.target in
             let (dx, dy) = (tx - sx), (ty - sy) in
             let dt = (path |> Path.length) *. Rules.move_rate /. Rules.fps_fl in
             (* x(t)       = x0 + t * x_v
                x(t0)      = x0 + t0 * x_v         = 0
                x(t0 + dt) = x0 + (t0 + dt) * x_v  = dx  *)
             let x_v, y_v = float_of_int dx /. dt, float_of_int dy /. dt in
             let x0, y0 = ~-. t0 *. x_v, ~-. t0 *. y_v in
             Player_move { x0; y0; x_v; y_v }
      in
      { pd with pl_pos; pl_anim }

    let path_data_of_path Path.{ pos; dir; rev; s_dis; d_dis } =
      { pa_pos = pos;
        pa_s_dis = s_dis;
        pa_d_dis = d_dis;
        pa_s_sgn = dir |> Path.cardinal_sign;
        pa_d_sgn = rev |> Path.revolution_sign ~dir;
        pa_axis = dir |> Path.cardinal_axis }

    let turn_frames_fl = float_of_int Rules.turn_frames
    let turn_amt_v = ~-. Rules.fps_fl /. turn_frames_fl (* 1/sec *)

    let turn_data_of_turn ~time:t0 (tn: Turn.t) =
      let rem_f = Rules.turn_frames - tn.frame in
      let time  = float_of_int rem_f /. Rules.fps_fl in
      let amt   = float_of_int rem_f /. turn_frames_fl in
      (* amt(t) = amt0 + t * amt_v  ==>  amt0 = amt(t) - t * amt_v *)
      { tn_num = tn.num; tn_time = time;
        tn_amt0 = amt -. t0 *. turn_amt_v;
        tn_amt_v = turn_amt_v }

    let update_from_game game v =
      let time = v.last_tick_time in
      begin
        (* players, map *)
        v.player_0 <- v.player_0 |> update_player_data ~time
                                      (v.game |> Gameplay.player_0)
                                      (game |> Gameplay.player_0);
        v.player_1 <- v.player_1 |> update_player_data ~time
                                      (v.game |> Gameplay.player_1)
                                      (game |> Gameplay.player_1);
        (* cursor, path *)
        v.cursor <- game |> Gameplay.cursor;
        v.path_data <- game |> Gameplay.path |> Option.map path_data_of_path;
        (* turn *)
        v.turn_data <- game |> Gameplay.turn |> turn_data_of_turn ~time;
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
        pl_hp = 16;
        pl_item = `S;
        pl_alt_item = None;
        pl_switching = false;
        pl_pos = (0, 0);
        pl_anim = Player_idle }

    let make assets game =
      let v0 =
        { assets;
          game;
          (* animations *)
          anim_time = 0.;
          last_tick_time = 0.;
          (* players, map *)
          player_0 = default_player 0 0 "Player One";
          player_1 = default_player 3 2 "Player Two";
          (* cursor, path *)
          cursor = None;
          path_data = None;
          (* turn *)
          turn_data = { tn_num = -1; tn_time = 0.; tn_amt0 = 0.; tn_amt_v = 0. } }
      in
      v0 |> update_from_game game;
      v0

    (*** event handling ***)

    let f_dt = Rules.frame_time

    let update t v =
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
    let hud_turn_y = 78
    let hud_turn_bar_w = 160
    let hud_turn_bar_h = 12
    let hud_turn_bar_dy = 20
    let hud_turn_bar_fill_c = Color.(of_rgb_s "#fff" |> with_alpha 0.6)

    let item_icon_img ty assets =
      let row = match ty with `S -> 0 | `F -> 1 | `P -> 2 | `H -> 3 in
      assets.sprites |> Image.clip ~x:384 ~y:(row * 64) ~w:64 ~h:64

    let item_icon_w = 64

    let render_hud_bg ~t cx =
      cx |> Ctxt.vertices `Fill
              ~t ~c:hud_bg_c
              ~xs:[| 0; hud_w; hud_w; 0     |]
              ~ys:[| 0; 0;     hud_h; hud_h |]

    let render_hud_player ~assets ~t cx i
          { pl_name; pl_hp; pl_item; pl_alt_item; _ }
      =
      (* player name *)
      (let font = assets.hud_p_name in
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

       pl_alt_item |>
         Option.iter (fun item ->
             (* alt item *)
             (let t = Affine.extend t in
              t |> Affine.translate_i
                     ((1 - 2 * i) * hud_item_alt_dx)
                     hud_item_alt_dy;
              t |> Affine.scale
                     hud_item_alt_scale hud_item_alt_scale;
              cx |> Ctxt.image (assets |> item_icon_img item) ~t
                      ~x:(-item_icon_w / 2) ~y:(-item_icon_w / 2));

             (* alt item text *)
             (let font = assets.hud_act_item in
              let (mes_w, _) = font |> Font.measure hud_item_alt_text in
              cx |> Ctxt.text hud_item_alt_text
                      ~t ~font ~c:hud_item_alt_text_c
                      ~x:((1 - 2 * i) * hud_item_alt_dx - mes_w / 2)
                      ~y:(hud_item_alt_text_dy))))

    let render_hud_turn ~assets ~anim_time ~t cx
          { tn_num; tn_time; tn_amt0; tn_amt_v }
      =
      let t = Affine.extend t in
      t |> Affine.translate_i hud_turn_x hud_turn_y;

      (* timer text *)
      (let text = Printf.sprintf "turn %d (%.1fs)" tn_num tn_time in
       let font = assets.hud_turn in
       let (mes_w, _) = font |> Font.measure text in
       cx |> Ctxt.text text
               ~t ~font ~c:hud_c
               ~x:(-mes_w / 2) ~y:0);

      (* timer bar *)
      (let amt = max 0. (tn_amt0 +. anim_time *. tn_amt_v) in
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

    let render_hud ~t cx
          { assets; player_0; player_1; turn_data; _ }
      =
      begin
        render_hud_bg ~t cx;
        render_hud_player ~assets ~t cx 0 player_0;
        render_hud_player ~assets ~t cx 1 player_1;
        render_hud_turn ~assets ~t cx turn_data
      end

    (* -- rendering the map -- *)

    let grid_c = Color.of_rgb_s "#ccc"

    let cell_w = 64
    let cell_w_fl = float_of_int cell_w
    let map_w = cell_w * Rules.grid_cols
    let map_y = 260

    let map_img_ox, map_img_oy = 64, 64
    let map_img assets =
      assets.map |> Image.clip ~x:0 ~y:0 ~w:640 ~h:640

    let render_map ~t cx
          { assets; _ }
      =
      cx |> Ctxt.image (assets |> map_img)
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

    let render_grid ~t cx =
      cx |> Ctxt.vertices `Lines
              ~t ~c:grid_c ~xs:grid_xs ~ys:grid_ys

    let translate_to_grid_center (col, row) t =
      t |> Affine.translate_i
             (col * cell_w + cell_w / 2)
             (row * cell_w + cell_w / 2)

    let path_c = Color.(of_rgb_s "#fff" |> with_alpha 0.3)
    let path_rad = 16

    let bent_line_coords s_len d_len s_sgn d_sgn axis =
      let r, r', r'' =
        path_rad,
        path_rad * 707 / 1000, (* ~ r * sin(45 deg) *)
        path_rad * 414 / 1000  (* ~ r * tan(22.5 deg) *)
      in
      let v1 = s_len * s_sgn in
      let v2 = d_len * s_sgn + v1 in
      let v3 = d_len * d_sgn in
      let i  = s_sgn * d_sgn in
      match axis with
      | Path.X ->
         let x1, x2, y2 = v1, v2, v3 in
         (*
            0--------------1          + = ( 0, 0)
            +            *  \         * = (x1, 0)
            5-----------4    \        @ = (x2,y2)
                         \    \
                          3-@--2

                          2--@-3
                         /    /
            0-----------1    /
            +            *  /
            5--------------4
          *)
         [|  0; x1 + r''*i; x2 + r'*i; x2 - r'*i; x1 - r''*i; 0 |],
         [| -r;     -r    ; y2 - r'  ; y2 + r'  ;      r    ; r |]
      | Path.Y ->
         let y1, y2, x2 = v1, v2, v3 in
         [| -r;     -r    ; x2 - r'  ; x2 + r'  ;      r    ; r |],
         [|  0; y1 + r''*i; y2 + r'*i; y2 - r'*i; y1 - r''*i; 0 |]

    let render_path ~t cx
          { pa_pos; pa_s_dis; pa_d_dis; pa_s_sgn; pa_d_sgn; pa_axis }
      =
      let t = Affine.extend t in
      t |> translate_to_grid_center pa_pos;
      let xs, ys = bent_line_coords
                     (pa_s_dis * cell_w)
                     (pa_d_dis * cell_w)
                     pa_s_sgn pa_d_sgn pa_axis in
      cx |> Ctxt.vertices `Fill
              ~t ~c:path_c ~xs ~ys

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
        v.path_data |> Option.iter (render_path ~t cx);
        v.cursor |> Option.iter (render_cursor ~t cx);
      end

    (* -- rendering map elements (players, items) -- *)

    let blob_img_ox, blob_img_oy = 32, 32
    let blob_img color face assets =
      assets.sprites |> Image.clip
                          ~x:(0 + 64 * face)
                          ~y:(0 + 64 * color)
                          ~w:64 ~h:64

    let swop_img_ox, swop_img_oy = 32, 32
    let swop_dx, swop_dy = 0, -56
    let swop_img assets =
      assets.sprites |> Image.clip ~x:704 ~y:160 ~w:64 ~h:64

    (* let item_img = item_icon_img *)

    let render_switching_above_player ~assets ~t cx =
      let t = Affine.extend t in
      t |> Affine.translate_i swop_dx swop_dy;
      cx |> Ctxt.image (assets |> swop_img)
              ~t ~x:(- swop_img_ox) ~y:(- swop_img_oy)

    let make_player_transform ~t ~anim_time { pl_pos; pl_anim; _ } =
      let t = Affine.extend t in
      t |> translate_to_grid_center pl_pos;
      ( match pl_anim with
        | Player_idle -> ()
        | Player_move { x0; y0; x_v; y_v } ->
           t |> Affine.translate
                  ((x0 +. anim_time *. x_v) *. cell_w_fl)
                  ((y0 +. anim_time *. y_v) *. cell_w_fl) );
      t

    let render_player_z0 ~assets ~t cx { pl_color; pl_face; _ } =
      cx |> Ctxt.image (assets |> blob_img pl_color pl_face)
              ~t ~x:(- blob_img_ox) ~y:(- blob_img_oy)

    let render_player_z1 ~assets ~t cx { pl_switching; _ } =
      if pl_switching then
        render_switching_above_player ~assets ~t cx

      (*
    let render_item ~assets ~t cx
          { it_type; it_pos }
      =
      let t = Affine.extend t in
      t |> translate_to_grid_center it_pos;
      cx |> Ctxt.image (assets |> item_img it_type)
              ~t ~x:(-cell_w / 2) ~y:(-cell_w / 2)
       *)

    let render_map_elements ~t cx
          { assets; anim_time; player_0; player_1; _ }
      =
      begin
        let pl0_t = player_0 |> make_player_transform ~t ~anim_time in
        let pl1_t = player_1 |> make_player_transform ~t ~anim_time in
        render_player_z0 ~assets ~t:pl0_t cx player_0;
        render_player_z0 ~assets ~t:pl1_t cx player_1;
        (* List.iter (render_item ~assets ~t cx) items; *)
        render_player_z1 ~assets ~t:pl0_t cx player_0;
        render_player_z1 ~assets ~t:pl1_t cx player_1;
      end

    (* -- main entry point -- *)

    let render cx v : unit =
      let anim_time = v.anim_time in

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
        v |> render_grid_elements ~t:map_t cx;
        v |> render_map_elements ~t:map_t cx;
        v |> render_hud ~anim_time ~t:hud_t cx;
      end
  end
