module type GRAPHICS = sig
  type bitmap
  type gc
  module Draw : sig
    val bitmap : pos:(int * int) -> bitmap -> unit
    val text : pos:(int * int) -> string -> unit
    val rectangle : pos:(int*int) -> size:(int*int) -> unit
  end
  val bitmap_of_file : fn:string -> bitmap
  val size_of_bitmap : bitmap -> int*int
  val bitmap : string -> bitmap
  val load_bitmap : string -> bitmap
end

module type LAYOUT = sig
  type t
  val place : Rect.t -> t -> Rect.t
end

module type PAINTER = sig
  type t
  module Graphics : GRAPHICS
  type gc = Graphics.gc
  val frame : Rect.t -> unit
  val string : Rect.t -> string -> unit
end

module type STATE = sig
  type t
  val initial : t
end

module FreeLayout = struct
  type t
  let place r _ = r
end

module MakeDefaultPainter(G : GRAPHICS) = struct
  type t
  module Graphics = G
  type gc = G.gc
  let frame pos = G.Draw.rectangle ~pos:(Rect.pos pos) ~size:(Rect.size pos)
  let string pos str = ()
end

module DefaultState = struct
  type t = [`Normal]
  let initial = `Normal
end

module type S = sig
  module Layout : LAYOUT
  module State : STATE
  module Painter : PAINTER
  type gc = Painter.gc
  val pack : Layout.t -> Rect.t -> Rect.t
  val paint : State.t -> Rect.t -> unit
  val change : State.t -> State.t
end

module type MAKE =
  functor (Layout : LAYOUT) -> 
    functor (Painter : PAINTER) -> 
      functor (State : STATE) -> S

module MakeBoard(L : LAYOUT)(P : PAINTER)(S : STATE) : S = struct
  module Layout = L
  module Painter = P
  module State = S
  type gc = Painter.gc
  let pack gc rect = rect
  let paint _ rect = Painter.frame rect
  let change s = s
end
