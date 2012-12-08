
module Option = BatOption
let (-|) = BatPervasives.(-|)

module type CLIENT = sig
  val poll_event : unit -> Event.prim option
  val is_end_session : Event.window -> bool
  val repaint_window : x:int -> y:int -> width:int -> height:int -> clip:Rect.Int.t option -> unit
  val on_event : Window.t option -> Event.window -> unit
  val redraw_screen : x:int -> y:int -> width:int -> height:int -> unit
  val screen_rect : unit -> (int * int) * (int * int)
end

module Make(Client : CLIENT) = struct

  module R = Rect.Int
  let translate_event x = x

  let event_loop () =
    let (x, y), (width, height) = Client.screen_rect () in
    Client.redraw_screen ~x ~y ~width ~height;
    let screen = Manager.current_screen () in
    screen.Screen.root.Window.width <- width;
    screen.Screen.root.Window.height <- height;
    let rec loop () =
      Thread.yield();
      match Client.poll_event () with
      | Some event ->
        let event = translate_event event in
        let abs_x, abs_y = Event.position event in
        let window :: _ = Manager.pick_window ~abs_x ~abs_y in
        Client.on_event (Some window) event;
        List.iter (fun window ->
          let x,y = Window.absolute_coord ~rel_x:0 ~rel_y:0 window in
          let clip = Option.bind (Option.Monad.return -| Window.abs_rect) window.Window.parent in
          Window.(Client.repaint_window
                    ~x
                    ~y
                    ~width:window.width
                    ~height:window.height
                    ~clip
          ))
          (Manager.windows ());
        let (x, y), (width, height) = Client.screen_rect () in
        Client.redraw_screen ~x ~y ~width ~height;
        if not (Client.is_end_session event) then
          loop ()
      | None -> loop ()
    in
    loop ()


end
