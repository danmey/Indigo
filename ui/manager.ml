open Batteries
module O = Option

type t = { mutable screens : Screen.t list
         ; mutable current_screen : Screen.t option
         ; pending_events : unit Event.t Queue.t }

let manager = { screens = []
              ; current_screen = None
              ; pending_events = Queue.create () }


let current_screen () =
  match manager.current_screen with
  | None -> failwith "No screen"
  | Some screen -> screen

let current_root () = current_screen () |> Screen.root

let open_screen name ~width ~height =
  let root = Window.create ~width ~height () in
  let screen = Screen.create name ~root in
  manager.screens <- screen :: manager.screens;
  (match manager.current_screen with
  | None -> manager.current_screen <- Some screen
  | Some _ -> ())

let open_window ~rel_x ~rel_y ~width ~height ?parent ?name () =
  let open Window in

  let screen = current_screen() in
  let window = Window.create ~rel_x ~rel_y ~width ~height () in
  let parent = O.default (current_root ()) parent in
  let cw, ch = parent.width, parent.height in
  let w, h = cw - rel_x, ch - rel_y in

  window.rel_x <- rel_x;
  window.rel_y <- rel_y;
  set_parent ~parent window;
  parent.children <- window :: parent.children;
  window.width <- if w >= window.width then window.width else w;
  window.height <- if h >= window.height then window.height else h;

  Screen.add_window screen parent window;
  print Format.std_formatter (Screen.root (current_screen ()))

let close_window window =
  (match window.Window.parent with
  | Some parent -> parent.Window.children <- List.filter ((!=) window) parent.Window.children
  | None -> ());
  Screen.change_zorder (current_screen ()) Zorder.remove window

let pick_window_skip ~abs_x ~abs_y ?skip =
  let screen = current_screen () in
  let rec loop = function
  | window :: rest ->
    let rel_x, rel_y = Window.relative_coord ~abs_x ~abs_y window in
    if rel_x >= 0
      && rel_x < window.Window.width
      && rel_y >= 0
      && rel_y < window.Window.height then
      match skip with
      | None -> Some window
      | Some skip when skip != window -> Some window
      | _ -> loop rest
    else loop rest
  | [] -> None
  in
  loop (Zorder.rev_order (current_screen()).Screen.zorder)

let pick_window ~abs_x ~abs_y = pick_window_skip ~abs_x ~abs_y ?skip:None

let windows () = (snd (current_screen()).Screen.zorder)

let set_window_topl window =
  window.Window.parent <- Some ((current_screen()).Screen.root);
  Screen.change_zorder (current_screen ()) Zorder.push_topl window

let set_window_parent window ~parent =
  let abs_x, abs_y = Window.absolute_coord ~rel_x:0 ~rel_y:0 window in
  let rel_x, rel_y = Window.relative_coord ~abs_x ~abs_y parent in
  window.Window.parent <- Some parent;
  Window.set_pos ~rel_x ~rel_y window;
  Screen.change_zorder (current_screen ()) (Zorder.push_after parent) window
