let new_surface ~width ~height ~window =
  let pixmap = GDraw.pixmap ~width ~height ~window () in
  pixmap#set_foreground `BLACK;
  pixmap#rectangle ~x:0 ~y:0 ~width ~height ~filled:true ();
  pixmap
