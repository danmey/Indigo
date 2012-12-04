type t = { mutable windows : Window.t list
         ; name : string
         ; root : Window.t }

let create name =
  { windows = [];
    name;
    root = Window.create () }
