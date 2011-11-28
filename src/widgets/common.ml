include Widget_sig
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
  let foreground col = G.Draw.foreground col
  let string pos str = ()
end

module State(Dummy : sig end) = struct
  type t = Normal | Pressed | Start of Timestamp.t
  let initial = Start (Timestamp.get ())
end
