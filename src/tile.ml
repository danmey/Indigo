class tile ~x ~y = object  (self:'self)
    
  val mutable dragged = false
  val mutable pos_x = x
  val mutable pos_y = y
  val mutable width = 0
  val mutable height = 0
  val mutable drag_x = 0
  val mutable drag_y = 0

  method draw (pixmap : GDraw.pixmap) = ()
  method click = ()

  method is_in ~x ~y =     
    let xt, yt = self # position in
    let w, h = self # size in
    x >= xt && x < xt + w && y >= yt && y < yt + h

  method button_down ~x ~y =
    if self # is_in ~x ~y then
      begin
        self # click; dragged <- true;
        let tile_x, tile_y = x - pos_x,  y - pos_y in
        drag_x <- tile_x;
        drag_y <- tile_y
      end
  method button_up = 
    if dragged then self # drop (); dragged <- false
  method motion ~x ~y = 
    if self # is_in ~x ~y then
      begin
        if dragged 
        then begin 
          pos_x <- x-drag_x; pos_y <- y-drag_y
        end
      end
  method set_position ~x ~y = pos_x <-x; pos_x <-y;
  method position : (int * int) = pos_x, pos_y
  method size = width, height
  method drag_start () = dragged <- true
  method drag_stop () = dragged <- false
  method drop () = ()
end


let dice ~x ~y = object (self:'self)
  inherit tile ~x ~y as super
  val image = GdkPixbuf.from_file "../resources/images/g6-1.png"
  initializer
    width <- GdkPixbuf.get_width image;
    height <- GdkPixbuf.get_height image

  method draw pixmap =
    let x, y = super # position in
    let x = x - 5 in
    let y = y - 5 in
    let width = 10 in
    let height = 10 in
    let update_rect = Gdk.Rectangle.create ~x ~y ~width ~height in
    pixmap#put_pixbuf ~x ~y image;

end

