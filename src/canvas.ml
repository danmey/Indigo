open Gtk_react
(* React is based on weak references so we need to prevent GC of reclaiming it *)
let canvas = ref None
let create ~pane =
  let width = 200 in
  let height = 200 in
  let window = GMisc.drawing_area ~width ~height ~packing:pane#add () in
  let module E = (val Gtk_react.create ~window : S) in

  let module E2 = struct
    include E
    let pixmap = ref (GDraw.pixmap ~width ~height ~window ())
      
    let configure ({ window; event } as a) =
      let width = GdkEvent.Configure.width event in
      let height = GdkEvent.Configure.height event in
      pixmap := GDraw.pixmap ~width ~height ~window ();
      !pixmap#set_foreground `BLACK;
      !pixmap#rectangle ~x:0 ~y:0 ~width ~height ~filled:true ();
      a

    (* Redraw the screen from the backing pixmap *)
    let expose ({ window; event } as a) =
      let area = GdkEvent.Expose.area event in
      let x = Gdk.Rectangle.x area in
      let y = Gdk.Rectangle.y area in
      let width = Gdk.Rectangle.width area in
      let height = Gdk.Rectangle.height area in
      let drawing =
        window#misc#realize ();
        new GDraw.drawable (window#misc#window)
      in
      drawing#put_pixmap ~x ~y ~xsrc:x ~ysrc:y ~width ~height !pixmap#pixmap;
      a

    let configured = React.E.map configure E.configured
    let exposed = React.E.map expose E.exposed
  end 
  in
  canvas := Some (module E2 : S);
  (module E2 : S)
    
