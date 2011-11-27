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
  module Graphics : GRAPHICS
  type t
  type gc = Graphics.gc
  val frame : Rect.t -> unit
  val string :  Rect.t -> string -> unit
end

module type STATE = sig
  type t
  val initial : t
end

module type MAKE =
  functor (Layout : LAYOUT) -> 
    functor (Painter : PAINTER) -> 
      functor (State : STATE) -> sig
  module Layout : LAYOUT
  module State : STATE
  module Painter : PAINTER
  type gc = Painter.gc
  val pack : Layout.t -> Rect.t -> Rect.t
  val paint : State.t -> Rect.t -> unit
  val change : State.t -> State.t
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

module MakeBoard : MAKE
module DefaultState : STATE
module FreeLayout : LAYOUT
module MakeDefaultPainter(G : GRAPHICS) : PAINTER with module Graphics = G

