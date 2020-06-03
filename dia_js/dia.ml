module Dom = Js_of_ocaml.Dom
module Dom_html = Js_of_ocaml.Dom_html
module Js = Js_of_ocaml.Js
module Ui = Dia_ui.Ui.Make(Canvas_draw)

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
          (Dom.Event.make "resize")
          (Dom.handler (fun _ -> resize (); Js._false))
          Js._false;
      resize () );

    (* draw every frame *)
    ( let ui = Ui.make () in
      let rec loop () =
        ui |> Ui.render the_ctxt;
        ignore @@
          Dom_html.window##requestAnimationFrame
            (Js.wrap_callback (fun _t -> loop ()))
      in
      loop () );
  end
