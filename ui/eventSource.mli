module Mouse : sig

  type button = Left | Right | Middle
  type position = int * int

  val left_button : bool React.S
  val right_button : bool React.S
  val mid_button : bool React.S
  val position : position React.S
  val press : button React.E
  val release : button React.E

end
