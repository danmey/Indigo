include Widget_sig
module FreeLayout = struct
  type t
  let place r _ = r
end

module MakeDefaultPainter(G : GRAPHICS) = struct
  type t
  let frame pos = ()
  let background col = ()
  let foreground col = ()
  let string pos str = ()
end

module State(Dummy : sig end) = struct
  type t = Normal | Pressed | Start of Timestamp.t
  let initial = Start (Timestamp.get ())
end
