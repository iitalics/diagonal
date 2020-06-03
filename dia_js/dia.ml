module Dom = Js_of_ocaml.Dom
module Dom_html = Js_of_ocaml.Dom_html
module Js = Js_of_ocaml.Js
module Ui = Dia_ui.Ui.Make(Canvas_draw)

let the_canvas = Dom_html.createCanvas Dom_html.document
let the_ctxt   = the_canvas##getContext Dom_html._2d_

let the_ui = Ui.make ()

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

    (* draw every frame *)
    ( let rec loop () =
        the_ui |> Ui.render the_ctxt;
        ignore @@
          Dom_html.window##requestAnimationFrame
            (Js.wrap_callback (fun _t -> loop ()))
      in
      loop () );

    (* listen for key events *)
    ( let on_key ev handle =
        Dom.addEventListener
          Dom_html.document##.body
          ev
          (Dom.handler
             (fun e ->
               Js.Optdef.iter e##.key (fun kc -> handle (Js.to_string kc) the_ui);
               Js._true))
          Js._false
      in
      ignore @@ on_key Dom_html.Event.keydown Ui.key_dn;
      ignore @@ on_key Dom_html.Event.keyup   Ui.key_up );
  end
