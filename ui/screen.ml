type t = { mutable zorder : Window.t Zorder.t
         ; name : string
         ; root : Window.t }

let create name ~root =
  { zorder = Zorder.empty root;
    name;
    root }

let add_window screen parent_window window =
  screen.zorder <- Zorder.after parent_window window screen.zorder

let change_zorder screen f x =
  screen.zorder <- f x screen.zorder

let set_size screen ~width ~height =
  Window.set_size ~width ~height screen.root

let root { root } = root

(* let set_root screen ~root = screen.root <- root *)
