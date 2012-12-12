open Batteries

module O = BatOption
module R = Rect.Int
module E = Event

module Make(Client : UIsig.REACT_CLIENT) = struct


  let event_loop () =
    let (x, y), (width, height) = Client.screen_rect () in

    let screen = Manager.current_screen () in

    let left_button, send_left_button = React.S.create false in
    let right_button, send_right_button = React.S.create false in
    let mid_button, send_mid_button = React.S.create false in
    let position, send_position = React.S.create (0,0) in
    let key, send_key = React.E.create () in
    let close, send_close = React.E.create () in

    let events = Events.(
      { left_button
      ; right_button
      ; mid_button
      ; position
      ; key
      ; send_close
      }) in

    let e = Client.connect events in
    let close = React.E.map (fun () -> exit 0) close in

    Screen.set_size screen ~width ~height;

    let do_event = function
    | E.MouseDown ((), position) ->
      send_left_button true;
    | E.MouseUp ((), position) ->
      send_left_button false;
    | E.MouseMove ((), position) ->
      send_position position;
    | _ -> () in

    let rec loop () =

      Thread.yield();

      begin
        match Client.poll_event () with
        | Some event -> do_event event
        | None -> ()
      end;
      loop ()
    in
    loop ();
    events, e, close

end
