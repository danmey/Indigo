module P = Pos.Int
module O = BatOption

module R = Rect.Int
module G = Graphics
module E = Event
module M = Manager

module Client = struct

  let poll_event =
    let last_button_down = ref false in
    let last_key_pressed = ref false in
    fun () ->
      let button_down = G.button_down () in
      let key_pressed = G.key_pressed () in
      if button_down <> !last_button_down then begin
        last_button_down := button_down;
        if button_down then
          Some (E.MouseDown((), G.mouse_pos ()))
        else
          Some (E.MouseUp((), G.mouse_pos ()))
      end
      else begin
        if key_pressed <> !last_key_pressed then begin
          G.read_key ();
          last_key_pressed := key_pressed;
          if key_pressed then
            Some (E.KeyDown((), G.mouse_pos (), 0))
          else
            Some (E.KeyUp((), G.mouse_pos (), 0))
        end else None
      end


  let is_end_session = function
  | E.KeyDown _ -> true
  | _ -> false

  let repaint_window ~x ~y ~width ~height ~(clip: R.t option) =
    let line (x1,y1) (x2,y2) =
      G.moveto x1 y1;
      G.lineto x2 y2
    in
    let rect (x,y,w,h) =
      line (x,y) (x+w,y);
      line (x+w,y) (x+w,y+h);
      line (x+w,y+h) (x,y+h);
      line (x,y+h) (x,y)
    in
    let w,h = width, height in
    G.set_color G.black;
    G.fill_rect (x+1) (y+1) (w-2) (h-2);
    G.set_color G.white;
    rect (x,y+h-16,16,16);
    rect (x+w-16,y+h-16,16,16);
    rect (x,y,w,h)


  let on_event window = function
  | E.MouseDown (_,(abs_x, abs_y)) ->
    let window = M.pick_window ~abs_x ~abs_y in
    let rel_x, rel_y = Window.relative_coord ~abs_x ~abs_y window in
    Format.fprintf Format.std_formatter "Picked: %a@." Window.print window;
    M.open_window ~rel_x ~rel_y ~w:100 ~h:100 "test" ~parent:window
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
  let _, (w, h) = (0,0), (G.size_x (),G.size_y ()) in
  G.fill_rect 0 0 w h;
  G.synchronize();
  G.synchronize();
  UI.event_loop ();
  G.close_graph ()
