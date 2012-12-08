type t = { mutable windows : Window.t Zorder.t
         ; name : string
         ; root : Window.t }

let create name =
  { windows = Zorder.empty;
    name;
    root = Window.create () }

let add_window screen window =
  screen.windows <-  [window] :: screen.windows
