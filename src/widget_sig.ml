module type GRAPHICS = sig
  (* This will be abstracted later, after we know which features we need from Cairo
     possible port to js_of_ocaml *)
  include module type of Cairo
end

module type LAYOUT = sig
  type t
  val place : Rect.t -> t -> Rect.t
end

module type PAINTER = sig
  val frame : Rect.t -> unit
  val string :  Rect.t -> string -> unit
  val background : int * int * int -> unit
  val foreground : int * int * int -> unit
end

module type STATE = sig
  type t = Normal | Pressed | Start of Timestamp.t
  val initial : t
end

module type EVENT = sig
  val click : EventInfo.Mouse.Click.t React.E.t
  val press : EventInfo.Mouse.Press.t React.E.t
  val release : EventInfo.Mouse.Press.t React.E.t
  val paint : (Rect.t * Timestamp.t) React.E.t
  val time : Timestamp.t React.S.t
end


module type S = sig
  module Layout : LAYOUT
  module Painter : PAINTER
  module Event : EVENT
  module State : STATE
  val pack : Layout.t -> Rect.t -> Rect.t
  val paint : unit React.E.t
  val state : State.t React.S.t
end

module type MAKE =
  functor (Layout : LAYOUT) -> 
    functor (Painter : PAINTER) -> 
        functor (Event : EVENT) -> S


