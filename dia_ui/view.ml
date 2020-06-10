(*** events ***)

module Evt = struct
  type t =
    | Key_dn of string
    | Key_up of string
end

(*** view interface ***)

module type Basic_S = sig
  include Intf.S0

  (* event handling *)
  val update: float -> t -> unit
  val handle_evt: Evt.t -> t -> unit

  (* rendering *)
  type draw_ctxt
  val render: draw_ctxt -> t -> unit
end

module type S = sig
  include Basic_S

  (* asset dependencies & loading *)
  type assets
  type 'a rsrc
  val assets_rsrc: assets rsrc

  (* generic way to init a view *)
  type init
  val make: assets -> init -> t

  (* switching views *)
  type view_disp
  val switch: view_disp -> t -> unit
end

(*** loading view ***)

module Make_loading_view
         (Draw: Intf.Draw_S)
         (Rsrc: Intf.Rsrc_S with type font = Draw.Font.t)
  =
  struct
    type assets = Draw.Font.t
    let assets_rsrc = Rsrc.font ~family:"nunito" ~size:30

    let bg_c = Draw.Color.of_rgb_s "#111111"
    let loading_text = "Loading..."
    let loading_c = Draw.Color.of_rgb_s "#eeeeee"

    let render_text cx font =
      let (cx_w, cx_h) = cx |> Draw.Ctxt.size in
      let (mes_w, mes_h) = font |> Draw.Font.measure loading_text in
      cx |> Draw.Ctxt.text loading_text
              ~x:((cx_w - mes_w) / 2)
              ~y:((cx_h - mes_h) / 2)
              ~c:loading_c ~font

    let render cx assets =
      cx |> Draw.Ctxt.clear ~c:bg_c;
      assets |> Option.iter (render_text cx)
  end

(*** view dispatcher ***)

module type Dispatcher_S = sig
  include Basic_S
  type 'a rsrc

  module type View_S =
    S with type view_disp = t
       and type draw_ctxt = draw_ctxt
       and type 'a rsrc   = 'a rsrc

  val make: ?assets:'a -> init:'i
            -> (module View_S with type assets = 'a and type init = 'i)
            -> t

  val push_view: ?assets:'a -> init:'i
                 -> (module View_S with type assets = 'a and type init = 'i)
                 -> t
                 -> unit

  val pop_view: t -> unit
end

module Make_dispatcher
         (Draw: Intf.Draw_S)
         (Rsrc: Intf.Rsrc_S     with type font = Draw.Font.t
                                 and type image = Draw.Image.t)
         (Loader: Intf.Loader_S with type 'a rsrc = 'a Rsrc.t)
       : Dispatcher_S with type draw_ctxt = Draw.Ctxt.t
                       and type 'a rsrc   = 'a Rsrc.t
  =
  struct
    module Loading_view = Make_loading_view(Draw)(Rsrc)

    (* the recursion in t and view_m makes this extra "View_S'" definition awkward but
       necessary... *)

    module type View_S' =
      S with type draw_ctxt = Draw.Ctxt.t
         and type 'a rsrc   = 'a Rsrc.t

    (* type definitions *)

    type t =
      { mutable lva: loading_view_assets;
        mutable stack: view list;
        mutable time: float }

    and loading_view_assets =
      | LV_loading of Loader.t
      | LV_loaded of Loading_view.assets

    and view =
      | Loading: 'i * Loader.t * (_, _, 'i) view_m -> view
      | View:    'v * float    * ('v, _, _) view_m -> view

    and ('v, 'a, 'i) view_m =
      (module View_S' with type t         = 'v
                       and type assets    = 'a
                       and type init      = 'i
                       and type view_disp = t)

    type 'a rsrc = 'a Rsrc.t
    type draw_ctxt = Draw.Ctxt.t

    module type View_S =
      S with type view_disp = t
         and type draw_ctxt = draw_ctxt
         and type 'a rsrc   = 'a rsrc

    (* initialization *)

    let make_view (type a i) (assets: a option) (init: i) (time: float)
          (module V: View_S with type init = i and type assets = a)
      =
      match assets with
      | Some(assets) -> View(V.make assets init, time, (module V))
      | None         -> Loading(init, Loader.make (), (module V))

    let make ?assets ~init v_m =
      let time = 0. in
      { lva = LV_loading(Loader.make ());
        stack = [ make_view assets init time v_m ];
        time }

    (* manipulating the view-stack *)

    let update_stack (vd: t) =
      let rec loop height =
        let height' = List.length vd.stack in
        if height <> height' then
          match vd.stack with
          | [] ->
             failwith "stack empty!"

          | View(v, _t0, (module V)) :: _stack ->
             begin
               v |> V.switch vd;
               loop height'
             end

          | Loading(init, loader, (module V)) :: stack ->
             match loader |> Loader.load V.assets_rsrc with
             | `Done(assets) ->
                begin
                  vd.stack <- make_view (Some assets) init vd.time (module V) :: stack;
                  loop 0
                end
             | `Loading -> ()
             | `Error(e) -> failwith e
      in
      loop 0

    let push_view ?assets ~init v_m (vd: t) =
      vd.stack <- make_view assets init vd.time v_m :: vd.stack

    let pop_view (vd: t) = match vd.stack with
      | _ :: vs -> vd.stack <- vs
      | []      -> failwith "stack empty!"

    let top_of_stack (vd: t) = match vd.stack with
      | v :: _ -> v
      | []     -> failwith "stack empty!"

    (* rendering & event handling *)

    let loading_view_assets vd = match vd.lva with
      | LV_loaded(a) -> Some(a)
      | LV_loading(loader) ->
         match loader |> Loader.load Loading_view.assets_rsrc with
         | `Done(a) -> ( vd.lva <- LV_loaded(a); Some(a) )
         | `Loading -> None
         | `Error(e) -> failwith e

    let render cx (vd: t) =
      update_stack vd;
      match top_of_stack vd with
      | View(v, _t, (module V)) ->
         v |> V.render cx
      | Loading _ ->
         vd |> loading_view_assets |> Loading_view.render cx

    let update time vd =
      vd.time <- time;
      match top_of_stack vd with
      | View(v, time0, (module V)) ->
         v |> V.update (time -. time0)
      | Loading _ ->
         ()

    let handle_evt evt (vd: t) =
      match top_of_stack vd with
      | View(v, _t0, (module V)) ->
         v |> V.handle_evt evt
      | Loading _ ->
         ()
  end
