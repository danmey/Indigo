open Common
include Widget_sig
module Make(L : LAYOUT)(P : PAINTER)(E : EVENT) : S = struct
  module Layout = L
  module Painter = P
  module State = State (struct end)
  module Event = E
  let pack gc rect = rect


  let change_state state { EventInfo.Mouse.Click.mouse; EventInfo.Mouse.Click.time_stamp } =
    match state with 
      | State.Normal -> State.Pressed
      | State.Pressed -> State.Normal
      | s -> s

  let stretch_time = 0.15

  let change_with_time ts state =
    match state with
      | State.Start ts' -> if Timestamp.since ts ts' > stretch_time then State.Normal else state
      | s -> s


  (* let time = React.S.Float.(Event.time /. React.S.const stretch_speed) *)

  let state = 
    React.S.l2 change_with_time Event.time
      (React.S.fold change_state State.initial Event.click)
    
  let paint = React.S.sample (fun (rect, ts) state ->
    let f,b,s = match state with 
      | State.Normal -> (0,0,0), (65535,65535,65535),stretch_time
      | State.Pressed -> (65535,65535,65535), (0,0,0),stretch_time
      | State.Start ts' -> let s = Timestamp.since ts ts' in
                           (0,0,0), (65535,65535,65535), s
    in
    let s = sin (3.1415/.2. *. s /. stretch_time) in
    let rect = Rect.scale s rect in
    P.foreground f;
    P.frame rect;
    let rect = Rect.shrink rect 2 in
    P.foreground b;
    P.frame rect;
  ) Event.paint state
   
end
