module type GRAPHICS = sig
  type bitmap
  type gc
  module Draw : sig
    val bitmap : pos:(int * int) -> bitmap -> unit
    val text : pos:(int * int) -> string -> unit
    val rectangle : pos:(int*int) -> size:(int*int) -> unit
    val background : int * int * int -> unit
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
  val background : int * int * int -> unit
end

module type STATE = sig
  type t = Normal|Pressed
  val initial : t
end

module type EVENT = sig
  val click : EventInfo.click React.E.t
  val paint : Rect.t React.E.t
end


module type S = sig
  module Layout : LAYOUT
  module Painter : PAINTER
  module Event : EVENT
  module State : STATE
  type gc = Painter.gc
  val pack : Layout.t -> Rect.t -> Rect.t
  val paint : unit React.E.t
  val state : State.t React.E.t
end

module type MAKE =
  functor (Layout : LAYOUT) -> 
    functor (Painter : PAINTER) -> 
        functor (Event : EVENT) -> S



