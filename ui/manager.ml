module type UI = sig end
type t = { mutable screens : Screen.t list
         ; mutable current_screen : Screen.t option
         ; pending_events : unit Event.t Queue.t
         ; mutable event_receivers : EventReceiver.t list }

let manager = { screens = []
              ; current_screen = None
              ; pending_events = Queue.create ()
              ; event_receivers = [] }

let current_screen () =
  match manager.current_screen with
  | None -> failwith "No screen"
  | Some screen -> screen

let open_screen name =
  let screen = Screen.create name in
  manager.screens <- screen :: manager.screens;
  match manager.current_screen with
  | None -> manager.current_screen <- Some screen
  | Some _ -> ()

let open_window ~rel_x ~rel_y ~w ~h ?parent name =
  let screen = current_screen() in
  let window = Window.create () in

  Window.(window.rel_x <- rel_x;
          window.rel_y <- rel_y;
          window.width <- w;
          window.height <- h;
          (match parent with
          | Some parent ->
            parent.children <- window :: parent.children;
            let cw, ch = parent.width, parent.height in
            let w = cw - window.rel_x in
            let h = ch - window.rel_y in
            window.width <- if w >= window.width then window.width else w;
            window.height <- if h >= window.height then window.height else h;
            Screen.add_window screen parent window
          | None -> ());
          window.parent <- parent);
  Window.print Format.std_formatter ((current_screen ()).Screen.root)

let pick_window ~abs_x ~abs_y =
  let screen = current_screen () in
  let rec loop = function
  | window :: rest ->
    let rel_x, rel_y = Window.relative_coord ~abs_x ~abs_y window in
    if rel_x >= 0
      && rel_x < window.Window.width
      && rel_y >= 0
      && rel_y < window.Window.height then
      window
    else loop rest
  | [] -> raise Not_found
  in
  loop (Zorder.rev_order (current_screen()).Screen.zorder)

let windows () = (snd (current_screen()).Screen.zorder)
