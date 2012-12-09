open Batteries

module O = BatOption
module R = Rect.Int
module E = Event

module Make(Client : UIsig.REACT_CLIENT) = struct

  module Mouse = struct

    type button = Left | Right | Middle
    type position = int * int

    let left_button, send_left_button = React.S.create false
    let right_button, send_right_button = React.S.create false
    let mid_button, send_mid_button = React.S.create false
    let position, send_position = React.S.create (0,0)
    let press, send_press = React.E.create ()
    let release, send_release = React.E.create ()
    let start_hover, send_start_hover = React.E.create ()
    let end_hover, send_end_hover = React.E.create ()
    let start_focus, send_start_focus = React.E.create ()
    let end_focus, send_end_focus = React.E.create ()
  end

  let event_loop () =
    let (x, y), (width, height) = Client.screen_rect () in

    let screen = Manager.current_screen () in

    let mouse = Mouse.(UIsig.(
      { left_button
      ; right_button
      ; mid_button
      ; position
      ; press
      ; release
      ; start_hover
      ; end_hover
      ; start_focus
      ; end_focus
      })) in

    Client.connect mouse;
    Client.redraw_screen ~x ~y ~width ~height;
    Screen.set_size screen ~width ~height;

    let do_event = function
    | E.MouseDown ((), position) ->
      Mouse.send_left_button true
    | E.MouseUp ((), position) ->
      Mouse.send_left_button false
    | E.MouseMove ((), position) ->
      Mouse.send_position position
    | _ -> () in

    let rec loop () =

      Thread.yield();

      match Client.poll_event () with
      | Some event ->

        let abs_x, abs_y = Event.position event in
        Manager.pick_window ~abs_x ~abs_y |> O.may (fun window -> do_event event);

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
    loop ();
    mouse

end
