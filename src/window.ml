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

module Make(G : Widget.GRAPHICS) = struct

type window = {
  mutable pos : Rect.t;
  mutable children : window list;
  send_click : EventInfo.Mouse.Click.t -> unit;
  send_paint : Rect.t * Timestamp.t -> unit;
  send_time : Timestamp.t -> unit;
  (* painter : G.gc -> Rect.t -> unit *)
  widget : (module Widget.S)
}

(* let default_painter canvas pos =  *)
(*   G.Draw.rectangle canvas ~pos:(Rect.pos pos) ~size:(Rect.size pos) *)


let default rect =
  let module Event =
      struct 
        let click, send_click = React.E.create () 
        let paint, send_paint = React.E.create () 
        let press, send_ = React.E.create () 
        let time, send_time = React.S.create (Timestamp.get ()) 
        let release, send_ = React.E.create () 
      end
  in
  { pos = rect; 
    children = []; 
    send_click = Event.send_click;
    send_paint = Event.send_paint;
    send_time = Event.send_time;
    widget = (module Widget.MakeBoard(Widget.FreeLayout)(Widget.MakeDefaultPainter(G))(Event) : Widget.S) }

let desktop_rect () = Rect.rect (0,0) (300,300)
let desktop = default (desktop_rect())
let empty_window () = default Rect.o


let shelf rect = desktop.pos <- rect

(* let desktop_rect () = Rect.rect (0,0) (Display.display_size ()) *)
let with_scisor _ f = f ()

let rec draw_window (canvas : G.gc) window =
  let ts = Timestamp.get () in
  let rec draw_client_window rect { pos; children; widget; send_paint } =
    let client_rect = Rect.place_in pos rect in
    with_scisor rect (fun () ->
      send_paint (client_rect,ts);
      List.iter (draw_client_window (Rect.together rect client_rect)) children)
  in
  draw_client_window (desktop_rect ()) window

let rec iter_window f window =
  f window;
  List.iter (iter_window f) window.children

let draw_desktop canvas = 
  draw_window canvas desktop

let window_path window =
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
  let path = window_path window in
  List.fold_left 
    (fun rect { pos } ->
      Rect.place_in pos rect) desktop.pos path

let find_window position =
  let rec loop rect window =
    Printf.printf "rect1: %s\n" (Rect.to_string rect);
    let rect = Rect.place_in window.pos rect in
    Printf.printf "pos: %s\n" (Pos.to_string position);
    Printf.printf "rect2: %s\n" (Rect.to_string window.pos);
    flush stdout;
    (if Rect.is_in rect position then
        [window] @ List.concat (List.map (loop rect) window.children)
     else 
        [])
  in
  List.rev (loop (desktop_rect()) desktop)
      
let add parent window =
  parent.children <-  parent.children @ [window];
  ()

(* let remove parent window = *)
(*   parent.children <- BatList.remove_if ((==) window) parent.children; *)
(*   () *)

let relative_pos window_relative window =
  let window_relative_pos = abs_pos window_relative in
  let window_pos = abs_pos window in
  Rect.subr window_relative_pos window_pos

let client_pos window global_pos = 
  Pos.sub global_pos (Rect.pos (abs_pos window))

let button_pressed pos =
  match find_window pos with
    | { send_click } :: _ ->
      send_click {
        EventInfo.Mouse.Click.time_stamp = Timestamp.get ();
        EventInfo.Mouse.Click.mouse = 
          { EventInfo.Mouse.pos; 
            EventInfo.Mouse.button = EventInfo.Mouse.Left 
          } }
    | [] -> ()

let iddle () =
  let ts = Timestamp.get () in
  iter_window (fun ({ send_time } as w) -> send_time ts) desktop
end
