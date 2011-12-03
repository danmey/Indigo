open Common
include Widget_sig
module Make(L : LAYOUT)(P : PAINTER)(E : EVENT) : S = struct
  include Base.Make(L)(P)(E)

  let f (cairo, rect, ts) state =
    let x,y,width,height = Rect.coords rect in
    let red, green, blue = 0.0, 0.0, 0.0 in
    Cairo.set_source_rgb cairo ~red ~green ~blue;
    Cairo.rectangle cairo ~x ~y ~width ~height;
    Cairo.fill cairo

  let paint = paint f

  let put_widget { EventInfo.Mouse.Press.mouse = { EventInfo.Mouse.pos = (x,y) } } =
    let pos = Rect.rect (x,y) (100.,100.) in
    PlaceWidget ((module Note.Make(L)(P) : Wrap.Make1), pos)

  let message = React.E.map put_widget Event.press
end
