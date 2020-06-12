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
        mutable p0: player;
        mutable p1: player;
        (* cursor, path *)
        mutable cursor: pos option;
        mutable path_data: path_data option;
        (* turn *)
        mutable turn_data: turn_data }

    and player =
      { pl_color: int;
        pl_face: int;
        pl_pos: pos;
        pl_name: string;
        pl_hp: int;
        pl_item: item_type;
        pl_alt_item: item_type option;
        pl_switching: bool }

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
        tn_time_left: float;
        tn_amt_left: float }

    let max_hp = 16

    (*** processing game state data ***)

    let path_data_of_path Path.{ pos; dir; rev; s_dis; d_dis } =
      { pa_pos = pos;
        pa_s_dis = s_dis;
        pa_d_dis = d_dis;
        pa_s_sgn = dir |> Path.cardinal_sign;
        pa_d_sgn = rev |> Path.revolution_sign ~dir;
        pa_axis = dir |> Path.cardinal_axis }

    let turn_data_of_turn Turn.{ num; frame } =
      { tn_num = num;
        tn_time_left = float_of_int (Rules.turn_frames - frame)
                       /. Rules.fps_fl;
        tn_amt_left  = float_of_int (Rules.turn_frames - frame)
                       /. float_of_int Rules.turn_frames }

    let update_from_game game v =
      begin
        (* players, map *)
        let pl0, pl1 = (game |> Gameplay.player_0), (game |> Gameplay.player_1) in
        v.p0 <- { v.p0 with pl_pos = pl0.pos };
        v.p1 <- { v.p1 with pl_pos = pl1.pos };
        (* cursor, path *)
        v.cursor <- game |> Gameplay.cursor;
        v.path_data <- game |> Gameplay.path |> Option.map path_data_of_path;
        (* turn *)
        v.turn_data <- game |> Gameplay.turn |> turn_data_of_turn;
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
        pl_pos = (0, 0);
        pl_switching = false }

    let make assets game =
      let v0 =
        { assets;
          game;
          (* animations *)
          anim_time = 0.;
          last_tick_time = 0.;
          (* players, map *)
          p0 = default_player 0 0 "Player One";
          p1 = default_player 3 2 "Player Two";
          (* cursor, path *)
          cursor = None;
          path_data = None;
          (* turn *)
          turn_data = { tn_num = 0; tn_time_left = 0.; tn_amt_left = 0. } }
      in
      v0 |> update_from_game game; v0

    (*** event handling ***)

    let f_dt = Rules.frame_time

    let update time v =
      let rec tick time0 =
        if time > time0 +. f_dt then
          ( v |> update_game Gameplay.tick;
            tick (time0 +. f_dt) )
        else
          v.last_tick_time <- time0
      in
      v.anim_time <- time;
      tick v.last_tick_time

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

    let render_hud_turn ~assets ~t cx
          { tn_num; tn_time_left; tn_amt_left }
      =
      let t = Affine.extend t in
      t |> Affine.translate_i hud_turn_x hud_turn_y;

      (* timer text *)
      (let text = Printf.sprintf "turn %d (%.1fs)" tn_num tn_time_left in
       let font = assets.hud_turn in
       let (mes_w, _) = font |> Font.measure text in
       cx |> Ctxt.text text
               ~t ~font ~c:hud_c
               ~x:(-mes_w / 2) ~y:0);

      (* timer bar *)
      (let x0, x1 = -hud_turn_bar_w / 2, hud_turn_bar_w / 2 in
       let y0, y1 = hud_turn_bar_dy, hud_turn_bar_dy + hud_turn_bar_h in
       let x1' = x0 + int_of_float (float_of_int hud_turn_bar_w *. tn_amt_left) in
       cx |> Ctxt.vertices `Fill
               ~t ~c:hud_turn_bar_fill_c
               ~xs:[| x0; x1'; x1'; x0 |]
               ~ys:[| y0;  y0;  y1; y1 |];
       cx |> Ctxt.vertices `Strip
               ~t ~c:hud_c
               ~xs:[| x0; x1; x1; x0; x0 |]
               ~ys:[| y0; y0; y1; y1; y0 |])

    let render_hud ~t cx
          { assets; p0; p1; turn_data; _ }
      =
      begin
        render_hud_bg ~t cx;
        render_hud_player ~assets ~t cx 0 p0;
        render_hud_player ~assets ~t cx 1 p1;
        render_hud_turn ~assets ~t cx turn_data
      end

    (* -- rendering the map -- *)

    let grid_c = Color.of_rgb_s "#ccc"

    let cell_w = 64
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

    let render_player_z0 ~assets ~t cx
          { pl_color; pl_face; pl_pos; _ }
      =
      let t = Affine.extend t in
      t |> translate_to_grid_center pl_pos;
      cx |> Ctxt.image (assets |> blob_img pl_color pl_face)
              ~t ~x:(- blob_img_ox) ~y:(- blob_img_oy)

    let render_player_z1 ~assets ~t cx
          { pl_pos; pl_switching; _ }
      =
      let t = Affine.extend t in
      t |> translate_to_grid_center pl_pos;
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
          { assets; p0; p1; _ }
      =
      begin
        render_player_z0 ~assets ~t cx p0;
        render_player_z0 ~assets ~t cx p1;
        (* List.iter (render_item ~assets ~t cx) items; *)
        render_player_z1 ~assets ~t cx p0;
        render_player_z1 ~assets ~t cx p1;
      end

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
        v |> render_grid_elements ~t:map_t cx;
        v |> render_map_elements ~t:map_t cx;
        v |> render_hud ~t:hud_t cx;
      end
  end
