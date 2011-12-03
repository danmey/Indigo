
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
  type t = Normal | Pressed | Start of Timestamp.t | Dragging of Rect.t
  val initial : t
end

module type EVENT = sig
  val press : EventInfo.Mouse.Press.t React.E.t
  val release : EventInfo.Mouse.Press.t React.E.t
  val paint : (Cairo.t * Rect.t * Timestamp.t) React.E.t
  val motion : (Pos.t * Pos.t) React.E.t
  val time : Timestamp.t React.S.t
end



module rec Wrap : sig
  module type S = sig
    module Layout : LAYOUT
    module Painter : PAINTER
    module Event : EVENT
    module State : STATE
    val pack : Layout.t -> Rect.t -> Rect.t
    val paint : unit React.E.t
    val state : State.t React.S.t
    val message : M.message React.E.t
  end 
  module type Make1 = functor (E : EVENT) -> S
end = struct
  module type S = sig
    module Layout : LAYOUT
    module Painter : PAINTER
    module Event : EVENT
    module State : STATE
    val pack : Layout.t -> Rect.t -> Rect.t
    val paint : unit React.E.t
    val state : State.t React.S.t
    val message : M.message React.E.t
  end
  module type Make1 = functor (E : EVENT) -> S
end and M : sig 
type message =
  | PlaceWidget of (module Wrap.Make1) * Rect.t
  | MoveWidget of Pos.t
  | Nil
end = struct
type message =
  | PlaceWidget of (module Wrap.Make1) * Rect.t
  | MoveWidget of Pos.t
  | Nil
end

include Wrap
include M

module type MAKE =
  functor (Layout : LAYOUT) -> 
    functor (Painter : PAINTER) -> 
        functor (Event : EVENT) -> Wrap.S
