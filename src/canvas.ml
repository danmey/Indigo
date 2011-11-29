open Gtk_react
  (* Redraw the screen from the backing pixmap *)

let create ~pane =
  let width = 200 in
  let height = 200 in
  let window = GMisc.drawing_area ~width ~height ~packing:pane#add () in
  let module E = (val Gtk_react.create ~window : S) in

  let expose {window; event} =
    let area = GdkEvent.Expose.area event in
    let x = Gdk.Rectangle.x area in
    let y = Gdk.Rectangle.y area in
    let width = Gdk.Rectangle.width area in
    let height = Gdk.Rectangle.height area in
    let drawing =
      window#misc#realize ();
      new GDraw.drawable (window#misc#window)
    in
      (* drawing#put_pixmap ~x ~y ~xsrc:x ~ysrc:y ~width ~height !backing#pixmap; *)
    let x,y = 0,0 in
    let rect = window # misc # allocation in
    let update_rect = Gdk.Rectangle.create ~x ~y ~width ~height in
    window#misc#draw (Some update_rect);
    window
  in

  let surface (window : GMisc.drawing_area) =
    let area = window # misc # allocation in
    let width = area.Gtk.width  in
    let height = area.Gtk.height  in
    let pixmap = GDraw.pixmap ~width ~height ~window () in
    pixmap#set_foreground `BLACK;
    pixmap#rectangle ~x:0 ~y:0 ~width ~height ~filled:true ();
    pixmap in

  let window = React.E.map expose E.exposed in
  let surface = React.E.map surface window in
  ()
