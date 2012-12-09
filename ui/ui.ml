open Batteries

module O = BatOption
module R = Rect.Int

module Make(Client : Sig.CLIENT) = struct

  let event_loop () =
    let (x, y), (width, height) = Client.screen_rect () in
    let screen = Manager.current_screen () in

    Client.redraw_screen ~x ~y ~width ~height;
    Screen.set_size screen ~width ~height;

    let rec loop () =

      Thread.yield();

      match Client.poll_event () with
      | Some event ->

        let abs_x, abs_y = Event.position event in
        let window = Manager.pick_window ~abs_x ~abs_y in

        Client.on_event window event;

        List.iter (fun window ->
          let x, y = Window.absolute_coord ~rel_x:0 ~rel_y:0 window in
          let clip = O.bind (O.Monad.return -| Window.abs_rect) window.Window.parent in
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
