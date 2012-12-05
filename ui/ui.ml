
module type CLIENT = sig
  val poll_event : unit -> Event.prim option
  val is_end_session : Event.window -> bool
  val repaint_window : x:int -> y:int -> width:int -> height:int -> unit
  val on_event : Window.t option -> unit
end

module Make(Client : CLIENT) = struct

  let translate_event x = x

  let event_loop () =
    let rec loop () =
      match Client.poll_event () with
      | Some event ->
        let event = translate_event event in
        List.iter (fun window -> Client.on_event (Some window))
          Manager.((current_screen ()).Screen.windows);

        if not (Client.is_end_session event) then
          loop ()
      | None ->
        loop ()
    in
    loop ()

end
