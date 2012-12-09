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
          ignore **> G.read_key ();
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
    [ (fun x y w h -> x <= 16 && y >= h-16), Cclose;
      (fun x y w h -> x <= border), Cresize Left;
      (fun x y w h -> x >= w-border && x <= w), Cresize Right;
      (fun x y w h -> y >= 0 && y <= border), Cresize Bottom;
      (fun x y w h -> y >= h-border && y <= h), Cresize Top;
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
        | Cmove -> Move (rel_x, rel_y)
        | Cclose -> Close
        | Ctoplevel -> Toplevel)
      else dispatch rest
    | [] -> None
    in
    dispatch region_actions

  type action = { window : Window.t;
                  action : action_type }
  open UIsig
  let connect { left_button
              ; right_button
              ; mid_button
              ; position
              ; press
              ; release } =

    ()

  let redraw_screen ~x ~y ~width ~height =
    G.synchronize();
    G.set_color G.black;
    G.fill_rect x y width height

  let screen_rect () =
    (0,0), (G.size_x (), G.size_y ())
end

module UI = ReactUI.Make(Client)

let mouse =
  G.open_graph "";
  G.auto_synchronize false;
  M.open_screen "main";
  G.set_color G.black;
  let _, (w, h) = (0,0), (G.size_x (), G.size_y ()) in
  G.fill_rect 0 0 w h;
  G.synchronize();
  G.synchronize();
  let mouse = UI.event_loop () in
  G.close_graph ();
  mouse
