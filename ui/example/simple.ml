open Batteries

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
    let last_mouse_pos = ref (0,0) in
    fun () ->
      let button_down = G.button_down () in
      let key_pressed = G.key_pressed () in
      let mouse_pos = G.mouse_pos () in
      if button_down <> !last_button_down then begin
        last_button_down := button_down;
        if button_down then
          Some (E.MouseDown((), mouse_pos))
        else
          Some (E.MouseUp((), mouse_pos))
      end
      else begin
        if key_pressed <> !last_key_pressed then begin
          G.read_key ();
          last_key_pressed := key_pressed;
          if key_pressed then
            Some (E.KeyDown((), mouse_pos, 0))
          else
            Some (E.KeyUp((), mouse_pos, 0))
        end else
        if mouse_pos <> !last_mouse_pos then begin
          last_mouse_pos := mouse_pos;
          Some (E.MouseMove((), mouse_pos))
        end
        else None
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

  type border =
    | Left
    | Right
    | Top
    | Bottom

  type action_type =
    | Resize of border * (int * int * int)
    | Move of (int * int)
    | Toplevel
    | Close

  type command =
    | Cresize of border
    | Cmove
    | Ctoplevel
    | Cclose

  let region_actions =
    let border = 5 in
    [(fun x y w h -> x <= border), Cresize Left;
     (fun x y w h -> x >= w-border && x <= w), Cresize Right;
     (fun x y w h -> y >= 0 && y <= border), Cresize Bottom;
     (fun x y w h -> y >= h-border && y <= h), Cresize Top;
     (fun x y w h -> x >= 0 && x <= 16 && y >= h-border), Cclose;
     (fun x y w h -> true), Cmove]

  let dispatch_action ~rel_x ~rel_y ~mpos_x ~mpos_y window =
    let x, y = rel_x, rel_y in
    let w, h = Window.size window in
    let rec dispatch = function
    | (pred, cmd) :: rest ->
      let border_coord = function
      | Left | Right -> x, mpos_y-y, w
      | Top | Bottom -> mpos_x-x, y, h
      in
      if pred x y w h then
        Some (match cmd with
        | Cresize border -> Resize (border, border_coord border)
        | Cmove -> Move (rel_x,rel_y)
        | Cclose -> Close
        | Ctoplevel -> Toplevel)
      else dispatch rest
    | [] -> None
    in
    dispatch region_actions

  type action = { window : Window.t;
                  action : action_type }
  let on_event =
    let button_down = ref false in
    let current_action = ref None in
    fun window ->
      function

      | E.MouseDown (_,(abs_x, abs_y)) ->
        button_down := true;
        let window = M.pick_window ~abs_x ~abs_y in
        let rel_x, rel_y = Window.relative_coord ~abs_x ~abs_y window in
        if Window.is_root window then
          M.open_window ~rel_x ~rel_y ~w:100 ~h:100 "test" ~parent:window
        else
          begin match dispatch_action ~rel_x ~rel_y ~mpos_x:abs_x ~mpos_y:abs_y window with
          | Some action -> current_action := Some { action; window }
          | None -> M.open_window ~rel_x ~rel_y ~w:100 ~h:100 "test" ~parent:window
          end

      | E.MouseUp (_, (abs_x, abs_y)) ->
        button_down := false;
        begin match !current_action with
        | Some {action; window} ->
          current_action := None;
          let parent = M.pick_window_skip ~abs_x ~abs_y ~skip:window in
          M.set_window_parent ~parent window
        | None -> ()
        end

      | E.MouseMove (_, (abs_x, abs_y)) ->
        if !button_down then
          match !current_action with
          | Some action -> begin match action.action with
            | Move (dx, dy) ->
              let rel_x, rel_y = abs_x - dx, abs_y - dy in
              Window.set_pos ~rel_x ~rel_y action.window
            | Resize (Left, (dx, dy, width)) ->
              let old_x, _ = Window.position action.window in
              let rel_x, rel_y = abs_x - dx, dy in
              let dw = old_x - rel_x in
              Window.set_pos ~rel_x ~rel_y action.window;
              Window.add_width action.window dw;
            | Resize (Bottom, (dx, dy, width)) ->
              let _, old_y = Window.position action.window in
              let rel_x, rel_y = dx, abs_y - dy in
              let dh = old_y - rel_y in
              Window.set_pos ~rel_x ~rel_y action.window;
              Window.add_height action.window dh;
            | Resize (Right, (dx, dy, width)) ->
              let width = abs_x - Window.x_coord action.window in
              Window.set_width action.window width;
            | Resize (Top, (dx, dy, width)) ->
              let height = abs_y - Window.y_coord action.window in
              Window.set_height action.window height;
            | _ -> ()
          end
          | _ -> ()
        else ()

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
