open Common
include Widget_sig
module Make(L : LAYOUT)(P : PAINTER)(E : EVENT) : S = struct
  include Base.Make(L)(P)(E)

  let f (cairo, rect, ts) state =
    let x,y,width,height = Rect.coords rect in
    let red, green, blue = 1.0, 0.5, 0.8 in
    Cairo.set_source_rgb cairo ~red ~green ~blue;
    Cairo.rectangle cairo ~x ~y ~width ~height;
    Cairo.fill cairo

  let paint = paint f
end
