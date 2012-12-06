
module type CLIENT = sig
  val poll_event : unit -> Event.prim option
  val is_end_session : Event.window -> bool
  val repaint_window : x:int -> y:int -> width:int -> height:int -> unit
  val on_event : Window.t option -> Event.window -> unit
  val redraw_screen : x:int -> y:int -> width:int -> height:int -> unit
  val screen_rect : unit -> (int * int) * (int * int)
end

module Make(Client : CLIENT) = struct

  let translate_event x = x

  let event_loop () =
    let (x, y), (width, height) = Client.screen_rect () in
    Client.redraw_screen ~x ~y ~width ~height;
    let rec loop () =
      Thread.yield();
      match Client.poll_event () with
      | Some event ->
        let event = translate_event event in
        List.iter (fun window -> Client.on_event (Some window) event)
          Manager.((current_screen ()).Screen.windows);
        List.iter (fun window ->
          Window.(Client.repaint_window
                    ~x:window.rel_x
                    ~y:window.rel_y
                    ~width:window.width
                    ~height:window.height))
          Manager.((current_screen ()).Screen.windows);
        Client.on_event None event;
        let (x, y), (width, height) = Client.screen_rect () in
        Client.redraw_screen ~x ~y ~width ~height;
        if not (Client.is_end_session event) then
          loop ()
      | None -> loop ()
    in
    loop ()

end
