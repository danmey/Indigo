open Common
include Widget_sig
module Make(L : LAYOUT)(P : PAINTER)(E : EVENT) : S = struct
  include Base
  let draw_curved_rectangle cr ~x ~y ~width ~height =
    Cairo.save cr;
    Cairo.move_to cr ~x ~y:(y+.height /. 2.);
    Cairo.curve_to cr ~x1:x ~y1:y ~x2:x ~y2:y ~x3:(x +. width /. 2.) ~y3:y;
    Cairo.curve_to cr ~x1:(x +. width) ~y1:y ~x2:(x +. width) ~y2:y ~x3:(x +. width) ~y3:(y +. height /. 2.);
    Cairo.curve_to cr ~x1:(x+. width) ~y1:(y+. height) ~x2:(x +. width) ~y2:(y +. height) ~x3:(x +. width /. 2.) ~y3:(y +. height);
    Cairo.curve_to cr ~x1:x ~y1:(y +. height) ~x2:x ~y2:(y +. height) ~x3:x ~y3:(y +. height /. 2.);
    Cairo.restore cr
      

  let rectangle cr ~pos:(x,y) ~size:(width,height) = 
    let pat = (Cairo.Pattern.create_linear ~x0:x ~y0:y ~x1:(x+.width) ~y1:(y+.height)) in
    Cairo.Pattern.add_color_stop_rgb pat ~off:0.0 ~red:0.6 ~green:0.6 ~blue:0.7;
    Cairo.Pattern.add_color_stop_rgb pat ~off:1.0 ~red:0.6 ~green:0.6 ~blue:1.0;
    Cairo.set_source cr pat;
    draw_curved_rectangle cr ~x ~y ~width ~height;
    Cairo.fill cr
end
