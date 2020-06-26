open Dia_util

module type S =
  sig
    include View.Basic_S
    val make: unit -> t
  end

module Make
         (Draw: Intf.Draw_S)
         (Rsrc: Intf.Rsrc_S with type font = Draw.Font.t)
         (Loader: Intf.Loader_S with type 'a rsrc = 'a Rsrc.t)
       : S with type draw_ctxt = Draw.Ctxt.t
  =
  struct
    open Draw
    type draw_ctxt = Ctxt.t

    (* init *)

    type t =
      { dt_avg: Running_average.t;
        mutable prev_time: float option;
        mutable font: [ `Loading of Loader.t
                      | `Done of Font.t ] }

    let make () =
      { dt_avg = Running_average.make 30;
        prev_time = None;
        font = `Loading(Loader.make ()) }

    (* event handling *)

    let update time t =
      t.prev_time |> Option.iter
                       (fun time' ->
                         t.dt_avg |> Running_average.push (time -. time'));
      t.prev_time <- Some(time)

    let handle_evt _ _ = ()

    (* resources *)

    let font_rsrc =
      Rsrc.font ~family:"nunito" ~size:16

    let load_font t =
      match t.font with
      | `Done(f) -> Some(f)
      | `Loading(loader) ->
         match loader |> Loader.load font_rsrc with
         | `Done(f)            -> ( t.font <- `Done(f); Some(f) )
         | `Error _ | `Loading -> None

    (* rendering *)

    let fps_x, fps_y = 4, 4
    let fps_c = Color.of_rgb_s "#000"

    let render_overlay cx font avg_dt =
      let text = Printf.sprintf "%.1f FPS (%.2f ms)"
                   (1. /. avg_dt)
                   (avg_dt *. 1000.) in
      let (cx_w, _) = cx |> Ctxt.size in
      let (mes_w, _) = font |> Font.measure text in
      cx |> Ctxt.text text
              ~font ~c:fps_c
              ~x:(cx_w - mes_w - fps_x)
              ~y:fps_y

    let render cx t =
      ignore @@
        Option.map2
          (render_overlay cx)
          (t |> load_font)
          (t.dt_avg |> Running_average.average)
  end
