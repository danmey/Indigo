include Widget_sig;;

module FreeLayout = struct
  type t
  let place r _ = r
end

module MakeDefaultPainter(G : GRAPHICS) = struct
  type t
  module Graphics = G
  type gc = G.gc
  let frame pos =
    G.Draw.rectangle ~pos:(Rect.pos pos) ~size:(Rect.size pos)
  let background col = G.Draw.background col
  let string pos str = ()
end

module State = struct
  type t = Normal|Pressed
  let initial = Normal
end


module MakeBoard(L : LAYOUT)(P : PAINTER)(E : EVENT) : S = struct
  module Layout = L
  module Painter = P
  module State = State
  module Event = E
  type gc = Painter.gc
  let pack gc rect = rect

  let change_state state { EventInfo.mouse } =
    print_endline "state changed";
    flush stdout;
    match state with 
      | State.Normal -> State.Pressed
      | State.Pressed -> State.Normal

  let state = React.E.fold change_state State.initial Event.click

  let paint = React.E.map (fun _ -> ()) Event.paint
   
end
