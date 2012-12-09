module type CLIENT = sig
  val poll_event : unit -> Event.prim option
  val is_end_session : Event.window -> bool
  val repaint_window : x:int -> y:int -> width:int -> height:int -> clip:Rect.Int.t option -> unit
  val on_event : Window.t option -> Event.window -> unit
  val redraw_screen : x:int -> y:int -> width:int -> height:int -> unit
  val screen_rect : unit -> (int * int) * (int * int)
end
