module G = Graphics
module E = Event

module M = Manager

module Client = struct

  let poll_event =
    let last_status = ref None in
    fun () ->
      let status = G.wait_next_event [G.Poll] in
      match !last_status with
      | None ->
        last_status := Some status;
        begin match G.(status.button, status.keypressed) with
        | true, _ -> Some (E.(MouseDown ((), G.(status.mouse_x, status.mouse_y))))
        | false, true -> Some (E.(KeyDown ((), G.(status.mouse_x, status.mouse_y), G.(int_of_char status.key))))
        | _ -> None
        end
      | Some status' ->
        if status' <> status then
          begin
            last_status := Some status;
            let mouse_x, mouse_y = G.(status.mouse_x, status.mouse_y) in
            let code = G.(int_of_char status.key) in
            match G.(status.button, status.keypressed, status'.button, status'.keypressed) with
            | true,_,false,_ -> Some (E.(MouseUp ((), (mouse_x, mouse_y))))
            | false,_,true,_ -> Some (E.(MouseDown ((), (mouse_x, mouse_y))))
            | _,false,_,true -> Some (E.(KeyDown ((), (mouse_x, mouse_y), code)))
            | _,true,_,false -> Some (E.(KeyUp ((), (mouse_x, mouse_y), code)))
            | _ -> None
          end
        else None

  let is_end_session = function
  | E.KeyDown _ -> true
  | _ -> false

  let repaint_window ~x ~y ~width ~height =
    G.set_color G.black;
    G.draw_rect x y width height;
    G.set_color G.white;
    G.draw_rect x y width height

  let on_event window =
    ()
end

module UI = Ui.Make(Client)

let () =
  G.open_graph "";
  M.open_screen "main";
  UI.event_loop ();
  G.close_graph ()
