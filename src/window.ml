(*----------------------------------------------------------------------------
  window.ml - window tree management
  Copyright (C) 2011 Wojciech Meyer

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
  --------------------------------------------------------------------------*)

module Make(G : Widget_sig.GRAPHICS)(C : Canvas.CANVAS)(Desktop : sig val size : unit -> (float * float) end) = struct

type window = {
  mutable pos : Rect.t;
  mutable children : window list;
  set_time : Timestamp.t -> unit;
  resize: (int * int) React.E.t;
  send_paint : Cairo.t * Rect.t * Timestamp.t -> unit;
  widget : (module Widget_sig.Wrap.S);
  message: unit React.E.t
}

let add parent window =
  parent.children <-  parent.children @ [window]

let messages = Queue.create () 

let focused_window = ref None

open Widgets
let rec make_widget () =
  let module W = 
        (Board.Make(Common.FreeLayout)
           (Common.MakeDefaultPainter(G)) : Widget_sig.Wrap.Make1) in
  (module W : Widget_sig.Wrap.Make1)

and desktop = lazy begin
  let module W = (val make_widget () : Widget_sig.Wrap.Make1) in
  widget (module W : Widget_sig.Wrap.Make1) (Rect.rect (0.,0.) (300.,300.)) 
end

and find_window pos' =
  let desktop = Lazy.force desktop in
  let rec loop rect window =
    let pos = window.pos in
    let rect = Rect.place_in pos rect in
    (if Rect.is_in rect pos' then
        window :: List.concat (List.map (loop rect) window.children)
     else 
        [])
  in
  List.rev (loop (desktop.pos) desktop)

and client_pos window global_pos = 
  Pos.sub global_pos (Rect.pos (abs_pos window))

and abs_pos window =
  let desktop = Lazy.force desktop in
  let path = window_path window in
  List.fold_left 
    (fun rect w ->
      let pos = position window in
      Rect.place_in pos rect) desktop.pos path

and widget widget rect =
  let open Widgets in
      let window = ref None in
      let module Event = struct
        let press = React.E.fmap (mouse_press window) C.pressed
        let release = React.E.fmap (mouse_release window) C.released
        let motion = React.E.fmap (mouse_motion window) C.notified
        let paint, send_paint = React.E.create ()
        let time, set_time = React.S.create (Timestamp.get ())
      end in
      let module MakeW = (val widget : Widget_sig.Wrap.Make1)  in
      let module W = MakeW(Event) in
      let message = React.E.map (message window) W.message in
      let window' = { pos = rect; 
        children = []; 
        set_time = Event.set_time;
        send_paint = Event.send_paint;
        resize = React.E.map resize C.resize;
        message = message;
        widget = (module W : Widget_sig.Wrap.S) } in
      window := Some window';
      window'

and mouse_press window {Gtk_react.event} =
    match !window with
      | Some window ->
        begin
        let x, y = GdkEvent.Button.x event, GdkEvent.Button.y event in
          match find_window (x,y) with
            | [] -> None
            | w :: xs when w == window -> 
              focused_window := Some window;
              let pos = client_pos window (x,y) in
              Some 
                { EventInfo.Mouse.Press.mouse = 
                    { EventInfo.Mouse.pos;
                      EventInfo.Mouse.button = EventInfo.Mouse.Left };
                  time_stamp = Timestamp.get () }
            | _ -> None
        end
      | None -> None
and mouse_motion window {Gtk_react.event} =
    match !window, !focused_window with
      | Some window, Some window' when window == window' ->
        let x, y = GdkEvent.Motion.x event, GdkEvent.Motion.y event in
        Some ((x,y), (client_pos window (x,y)))
      | _ -> None
and mouse_release window {Gtk_react.event} =
    match !window, !focused_window with
      | Some window, Some window' when window == window' ->
        let x, y = GdkEvent.Button.x event, GdkEvent.Button.y event in
        Some { EventInfo.Mouse.Press.mouse = 
            { EventInfo.Mouse.pos = (x,y);
              EventInfo.Mouse.button = EventInfo.Mouse.Left };
               time_stamp = Timestamp.get () }
      | _ -> None
    
and resize (width, height) =
    let desktop = Lazy.force desktop in
    desktop.pos <- Rect.rect (0.,0.) (float width, float height);
    (width, height)

and message window = function
  | Widget_sig.M.Nil -> ()
  | a ->
    match !window with
      | Some window ->
          Queue.add (window, a) messages
      | None -> ()

and window_path window =
  let desktop = Lazy.force desktop in
  let bool_of_option = function Some _ -> true | None -> false  in
  let rec find_loop path ({ children; } as window') =
    if window' == window 
    then Some path 
    else
      match children with
        | [] -> None
        | windows ->
          try List.find bool_of_option
            (List.map (fun w -> find_loop (w :: path) w) windows)
          with _ -> None
  in
  match find_loop [] desktop with
    | None -> []
    | Some path -> List.rev path
        
and position window = 
  window.pos

let with_scisor _ f = f ()

let rec draw_window window =
  let desktop = Lazy.force desktop in
  let ts = Timestamp.get () in
  let rec draw_client_window rect ({ children; widget; send_paint } as w) =
    let pos = position w in
    let client_rect = Rect.place_in pos rect in
    with_scisor rect (fun () ->
      let cr = Cairo_lablgtk.create (!C.pixmap#pixmap) in
      send_paint (cr, client_rect, ts);
      List.iter (draw_client_window (Rect.together rect client_rect)) children)
  in
  draw_client_window (position desktop) window;
  C.update()


let rec iter_window f window =
  f window;
  List.iter (iter_window f) window.children

let draw () = 
  let desktop = Lazy.force desktop in
  draw_window desktop


let relative_pos window_relative window =
  let window_relative_pos = abs_pos window_relative in
  let window_pos = abs_pos window in
  Rect.subr window_relative_pos window_pos


let iddle () =
  let desktop = Lazy.force desktop in
  let ts = Timestamp.get () in
  Queue.iter (function
    | _, Widget_sig.M.PlaceWidget (w, pos) -> 
      let w = widget w pos in
      add desktop w
  | window, Widget_sig.M.MoveWidget client_pos ->
    match window_path window with
      | window :: _ -> window.pos <- Rect.rect client_pos (Rect.size window.pos)) messages;
  Queue.clear messages;

  iter_window (fun ({ set_time } as w) -> set_time ts; draw_window w) desktop
end
