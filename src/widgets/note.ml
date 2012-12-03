open Common
include Widget_sig
module Make(L : LAYOUT)(P : PAINTER)(E : EVENT) : S = struct
  include Base.Make(L)(P)(E)
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
    (* Cairo.Pattern.add_color_stop_rgb pat ~ofs:0.0 ~red:0.6 ~green:0.6 ~blue:0.7; *)
    (* Cairo.Pattern.add_color_stop_rgb pat ~ofs:1.0 ~red:0.6 ~green:0.6 ~blue:1.0; *)
    Cairo.set_source cr pat;
    draw_curved_rectangle cr ~x ~y ~width ~height;
    Cairo.fill cr

  let f (cairo, rect, ts) state =
    let x,y,width,height = Rect.coords rect in
    let red, green, blue = 0.3, 0.3, 0.3 in
    (* Cairo.set_source_rgb cairo ~red ~green ~blue; *)
    rectangle cairo ~pos:(x,y) ~size:(width,height);
    Cairo.fill cairo

  let paint = paint f


  let state =
    let with_pos b { EventInfo.Mouse.Press.mouse = { EventInfo.Mouse.pos } } =
      b, pos in
    React.S.fold (fun state pressed ->
      match state,pressed with
        | _,(true,pos) -> State.Dragging pos
        | _,(false,_) -> State.Normal
        | state,_ -> state) State.initial
      (React.E.select
         [React.E.map (with_pos true) E.press;
          React.E.map (with_pos false)  E.release])

  let message =
    React.E.fmap (fun x -> x)
      (React.S.sample
      (fun (global_pos,pos) -> function
        | State.Dragging pos' -> Some (MoveWidget (Pos.sub global_pos pos'))
        | a -> None) E.motion state)

end
