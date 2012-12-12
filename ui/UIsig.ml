module type CLIENT = sig

  val poll_event : unit -> Event.prim option
  val is_end_session : Event.window -> bool
  val repaint_window : x:int -> y:int -> width:int -> height:int -> clip:Rect.Int.t option -> unit
  val redraw_screen : x:int -> y:int -> width:int -> height:int -> unit
  val screen_rect : unit -> (int * int) * (int * int)

  val on_event : Window.t option -> Event.window -> unit

end

type button = Left | Right | Middle
type position = int * int


module type REACT_CLIENT = sig
  val poll_event : unit -> Event.prim option
  val connect : Events.basic -> unit
  val screen_rect : unit -> (int * int) * (int * int)
end

(* module type MOUSE_EVENT_SOURCE = sig *)
(*   type button = Left | Right | Middle *)
(*   type position = int * int *)

(*   val left_button : bool React.S *)
(*   val right_button : bool React.S *)
(*   val mid_button : bool React.S *)
(*   val position : position React.S *)
(*   val press : button React.E *)
(*   val release : button React.E *)
(* end *)
