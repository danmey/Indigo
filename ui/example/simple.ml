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
        let mouse_x, mouse_y = G.(status.mouse_x, status.mouse_y) in
        let code = G.(int_of_char status.key) in
        let ev = match G.(status'.button, status'.keypressed, status.button, status.keypressed) with
        | false,_,true,_ -> Some (E.(MouseDown ((), (mouse_x, mouse_y))))
        | true,_,false,_ -> Some (E.(MouseUp ((), (mouse_x, mouse_y))))
        | _,false,_,true -> Some (E.(KeyDown ((), (mouse_x, mouse_y), code)))
        | _,true,_,false -> Some (E.(KeyUp ((), (mouse_x, mouse_y), code)))
        | _ -> None
        in
        if ev <> None then last_status := Some status;
        ev

  let is_end_session = function
  | E.KeyDown _ -> true
  | _ -> false

  let repaint_window ~x ~y ~width ~height =
    G.set_color G.white;
    G.draw_rect x y width height

  let on_event window = function
  | E.MouseDown (_,(abs_x, abs_y)) ->
    let windows = M.pick_window ~abs_x ~abs_y in
    let window = List.hd (List.rev windows) in
    let rel_x, rel_y = Window.relative_coord ~abs_x ~abs_y window in
    M.open_window ~rel_x ~rel_y ~w:100 ~h:100 "test" ~parent:(List.hd (List.rev windows))
  | _ -> ()

  let redraw_screen ~x ~y ~width ~height =
    G.synchronize();
    G.set_color G.black;
    G.fill_rect x y width height

  let screen_rect () =
    (0,0), (G.size_x (),G.size_y ())
end

module UI = Ui.Make(Client)

let () =
  G.open_graph "";
  G.auto_synchronize false;
  M.open_screen "main";
  G.set_color G.black;
  let _, (w,h) = (0,0), (G.size_x (),G.size_y ()) in
  G.fill_rect 0 0 w h;
  G.synchronize();
  G.synchronize();
  UI.event_loop ();
  G.close_graph ()
