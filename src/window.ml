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
  widget : (module Widget_sig.S)
}

let rec desktop = lazy (default (Rect.rect (0.,0.) (300.,300.)))

and default rect =
  let open Widgets in
      let module Event = struct
        let press = React.E.map mouse_click C.pressed
        let release = React.E.map mouse_click C.released
        let paint, send_paint = React.E.create ()
        let time, set_time = React.S.create (Timestamp.get ())
      end
      in
      let module W = 
            (Board.Make(Common.FreeLayout)
              (Common.MakeDefaultPainter(G))
              (Event) : Widget_sig.S) in
      { pos = rect; 
        children = []; 
        set_time = Event.set_time;
        send_paint = Event.send_paint;
        resize = React.E.map resize C.resize;
        widget = (module W : Widget_sig.S) }

and mouse_click {Gtk_react.window; Gtk_react.event} =
  let x, y = GdkEvent.Button.x event, GdkEvent.Button.y event in
  { EventInfo.Mouse.Press.mouse = 
      { EventInfo.Mouse.pos = (x,y);
        EventInfo.Mouse.button = EventInfo.Mouse.Left };
    time_stamp = Timestamp.get () }

and resize (width, height) =
    let desktop = Lazy.force desktop in
    Printf.printf "resize %s" (Rect.to_string desktop.pos);
    desktop.pos <- Rect.rect (0.,0.) (float width, float height);
    (width, height)

let position window = 
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
      C.update();
      List.iter (draw_client_window (Rect.together rect client_rect)) children)
  in
  draw_client_window (position desktop) window

let rec iter_window f window =
  f window;
  List.iter (iter_window f) window.children

let draw () = 
  let desktop = Lazy.force desktop in
  draw_window desktop

let window_path window =
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

let abs_pos window =
  let desktop = Lazy.force desktop in
  let path = window_path window in
  List.fold_left 
    (fun rect w ->
      let pos = position window in
      Rect.place_in pos rect) desktop.pos path

let find_window pos' =
  let desktop = Lazy.force desktop in
  let rec loop rect window =
    let pos = position window in
    let rect = Rect.place_in pos rect in
    (if Rect.is_in rect pos' then
        window :: List.concat (List.map (loop rect) window.children)
     else 
        [])
  in
  List.rev (loop (position desktop) desktop)
      
let add parent window =
  parent.children <-  parent.children @ [window];
  ()

let relative_pos window_relative window =
  let window_relative_pos = abs_pos window_relative in
  let window_pos = abs_pos window in
  Rect.subr window_relative_pos window_pos

let client_pos window global_pos = 
  Pos.sub global_pos (Rect.pos (abs_pos window))

let iddle () =
  let desktop = Lazy.force desktop in
  let ts = Timestamp.get () in
  iter_window (fun ({ set_time } as w) -> set_time ts; draw_window w) desktop
end
