open Common
include Widget_sig
module Make(L : LAYOUT)(P : PAINTER)(E : EVENT) = struct
  module Layout = L
  module Painter = P
  module State = State (struct end)
  module Event = E

  let pack gc rect = rect

  let change_state state { EventInfo.Mouse.Press.mouse; EventInfo.Mouse.Press.time_stamp } =
    match state with 
      | State.Normal -> State.Pressed
      | State.Pressed -> State.Normal
      | s -> s

  let stretch_time = 0.15

  let change_with_time ts state =
    match state with
      | State.Start ts' -> if Timestamp.since ts ts' > stretch_time then State.Normal else state
      | s -> s

  let state = 
    React.S.l2 change_with_time Event.time
      (React.S.fold change_state State.initial Event.press)

  let paint f = React.S.sample f Event.paint state
end
