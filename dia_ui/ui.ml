module type S = sig
  include Intf.View_S
  val make: assets -> t
end

module Make
         (Draw: Intf.Draw_S)
         (Rsrc: Intf.Rsrc_S with type font = Draw.Font.t)
       : S with type draw_ctxt = Draw.Ctxt.t
            and type 'a rsrc = 'a Rsrc.t
  =
  struct
    open Draw
    type nonrec draw_ctxt = Ctxt.t
    type nonrec 'a rsrc = 'a Rsrc.t

    (* init *)

    type t =
      { assets: assets;
        items: string list;
        mutable sel: int }

    and assets =
      { title_font: Font.t;
        item_font: Font.t }

    let make assets =
      { assets;
        items = [ "Singleplayer";
                  "Tutorial";
                  "Change Character";
                  "Change Controls" ];
        sel = 0 }

    let rsrc =
      Rsrc.zip (fun title_font item_font -> { title_font; item_font })
        (Rsrc.font_rsrc ~family:"roundor" ~size:150)
        (Rsrc.font_rsrc ~family:"nunito" ~size:40)

    (* key handlers *)

    let key_dn key v =
      let delta = match key with
        | "ArrowUp"   -> -1
        | "ArrowDown" -> 1
        | _           -> 0 in
      let n = List.length v.items in
      v.sel <- (v.sel + n + delta) mod n

    let key_up _ _ = ()

    (* rendering *)

    let _BG_F = Color.of_rgb_s "#020202"

    let render_bg cx =
      cx |> Ctxt.clear ~f:_BG_F

    let _TITLE = "Diag"
    let _TITLE_F = Color.of_rgb_s "#fff"

    let _MIDDLE_PAD = 10

    let render_title ~assets cx =
      let (cx_w, cx_h) = cx |> Ctxt.size in
      let (mes_w, mes_h) = assets.title_font |> Font.measure _TITLE in
      cx |> Ctxt.text _TITLE
              ~font:assets.title_font
              ~x:(cx_w / 2 - mes_w - _MIDDLE_PAD)
              ~y:((cx_h - mes_h) / 2)
              ~f:_TITLE_F

    let _ITEM_F = Color.of_rgb_s "#eee"
    let _ITEM_SEL_F = Color.of_rgb_s "#f04"
    let _ITEM_SEP = 40
    let _ITEM_SEL_SEP = 60

    let render_items ~assets items sel_idx cx =
      let (cx_w, cx_h) = cx |> Ctxt.size in
      let tot_w = List.fold_left
                    (fun tot_w it ->
                      let (mes_w, _) = assets.item_font |> Font.measure it in
                      max tot_w mes_w)
                    0
                    items in
      let tot_h = (List.length items - 1) * _ITEM_SEP + _ITEM_SEL_SEP in
      let x0, y0 = cx_w / 2 + tot_w, (cx_h - tot_h) / 2 in
      ignore @@
        List.fold_left
          (fun (y, idx) it ->
            let (mes_w, mes_h) = assets.item_font |> Font.measure it in
            let h = if idx = sel_idx then _ITEM_SEL_SEP else _ITEM_SEP in
            cx |> Ctxt.text it
                    ~font:assets.item_font
                    ~x:(x0 - mes_w + _MIDDLE_PAD)
                    ~y:(y + (h -  mes_h) / 2)
                    ~f:(if idx = sel_idx then _ITEM_SEL_F else _ITEM_F);
            (y + h, idx + 1))
          (y0, 0)
          items

    let render cx { assets; items; sel } =
      begin
        cx |> render_bg;
        cx |> render_title ~assets;
        cx |> render_items ~assets items sel;
      end
  end
