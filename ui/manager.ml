
module type CLIENT = sig
  val poll_event : unit -> Event.prim option
  val is_end_session : Event.window -> bool
end

module Make(Client : CLIENT) = struct

  type t = { mutable screens : Screen.t list
           ; mutable current_screen : Screen.t option
           ; pending_events : unit Event.t Queue.t
           ; mutable event_receivers : EventReceiver.t list }

  let manager = { screens = []
                ; current_screen = None
                ; pending_events = Queue.create ()
                ; event_receivers = [] }

  let open_screen name =
    let screen = Screen.create name in
    manager.screens <- screen :: manager.screens;
    match manager.current_screen with
    | None -> manager.current_screen <- Some screen
    | Some _ -> ()

  let register_receiver receiver =
    manager.event_receivers <- receiver :: manager.event_receivers

  let translate_event x = x

  let event_loop () =
    let rec loop () =
      match Client.poll_event () with
      | Some event ->
        let event = translate_event event in
        if not (Client.is_end_session event) then
          loop ()
      | None ->
        loop ()
    in
    loop ()
end
