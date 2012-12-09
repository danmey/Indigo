type t = { mutable zorder : Window.t Zorder.t
         ; name : string
         ; root : Window.t }

let create name =
  let window = Window.create () in
  { zorder = Zorder.empty window;
    name;
    root = window }

let add_window screen parent_window window =
  screen.zorder <- Zorder.after parent_window window screen.zorder

let change_zorder screen f x =
  screen.zorder <- f x screen.zorder

let set_size screen ~width ~height =
  Window.set_size ~width ~height screen.root
