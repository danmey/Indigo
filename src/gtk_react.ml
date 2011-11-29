  type 'a t = { window: GMisc.drawing_area; event: 'a }
module type S = sig
  val exposed : GdkEvent.Expose.t t React.E.t
  val configured : GdkEvent.Configure.t t React.E.t
  val notified : GdkEvent.Motion.t t React.E.t
  val pressed : GdkEvent.Button.t t React.E.t
  val released : GdkEvent.Button.t t React.E.t
end
let create ~window =
  let e = React.E.create in
  let i = ignore in
  let module E = struct
    let w f event = 
      let () = f {window; event} in true
    let exposed, send_expose = e ()
    let configured, send_configure = e ()
    let notified, send_notify = e ()
    let pressed, send_press = e ()
    let released, send_release = e ()
    let send_expose = w send_expose
    let send_configure = w send_configure
    let send_notify = w send_notify
    let send_press = w send_press
    let send_release = w send_release
  end 
  in
  i(window#event#connect#expose ~callback:E.send_expose);
  i(window#event#connect#configure ~callback:E.send_configure);
  i(window#event#connect#motion_notify ~callback:E.send_notify);
  i(window#event#connect#button_press ~callback:E.send_press);
  i(window#event#connect#button_release ~callback:E.send_release);
  window#event#add [`EXPOSURE; 
                  `LEAVE_NOTIFY; 
                  `BUTTON_PRESS; 
                  `BUTTON_RELEASE; 
                  `POINTER_MOTION; 
                  `POINTER_MOTION_HINT];
  (module E : S)
