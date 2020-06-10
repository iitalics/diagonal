module Dom = Js_of_ocaml.Dom
module Dom_html = Js_of_ocaml.Dom_html
module Js = Js_of_ocaml.Js

module View_disp =
  Dia_ui.View.Make_dispatcher(Html5)(Html5_rsrc)(Html5_rsrc.Cache)

module Ui_views =
  Dia_ui.Ui.Make_views(Html5)(Html5_rsrc)(View_disp)

module Overlay =
  Dia_ui.Fps_counter_overlay.Make(Html5)(Html5_rsrc)(Html5_rsrc.Cache)

let the_view_disp =
  View_disp.make (module Ui_views.Main_menu)
    ~init:()

let the_overlay =
  Overlay.make ()

let the_canvas = Dom_html.createCanvas Dom_html.document
let the_ctxt   = the_canvas##getContext Dom_html._2d_

let () =
  begin
    (* add canvas to document *)
    ignore @@
      Dom_html.document##.body##appendChild
        (Js.Unsafe.coerce the_canvas);

    (* always fit canvas to window size *)
    ( let resize () =
        the_canvas##.width  := Js.Optdef.get Dom_html.window##.innerWidth (fun () -> 0);
        the_canvas##.height := Js.Optdef.get Dom_html.window##.innerHeight (fun () -> 0);
      in
      ignore @@
        Dom.addEventListener
          Dom_html.window
          Dom_html.Event.resize
          (Dom.handler (fun _ -> resize (); Js._true))
          Js._false;
      resize () );

    (* listen for key events *)
    ( let handle ev =
        the_view_disp |> View_disp.handle_evt ev;
        the_overlay   |> Overlay.handle_evt ev;
      in
      let on_key dom_ev mk_evt =
        ignore @@
          Dom.addEventListener
            Dom_html.document##.body
            dom_ev
            (Dom.handler
               (fun e ->
                 Js.Optdef.iter e##.key
                   (fun kc -> handle @@ mk_evt (Js.to_string kc));
                 Js._true))
            Js._false
      in
      on_key Dom_html.Event.keydown (fun k -> Key_dn k);
      on_key Dom_html.Event.keyup   (fun k -> Key_up k) );

    (* draw every frame *)
    ( let rec render_loop time_ms =
        (* update time *)
        let time = time_ms *. 0.001 in
        the_view_disp |> View_disp.update time;
        the_overlay   |> Overlay.update time;
        (* render *)
        the_view_disp |> View_disp.render the_ctxt;
        the_overlay   |> Overlay.render the_ctxt;
        ignore @@
          Dom_html.window##requestAnimationFrame
            (Js.wrap_callback render_loop);
      in
      render_loop @@
        Js.Unsafe.global##.performance##now )
  end
