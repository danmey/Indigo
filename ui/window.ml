type 'a repaint = 'a -> unit

type t = { mutable rel_x : int
         ; mutable rel_y : int
         ; mutable width : int
         ; mutable height : int
         ; mutable dirty : bool
         ; mutable children : t list
         ; mutable depth : int
         ; mutable enabled : bool }

let create () =
  { rel_x = 0;
    rel_y = 0;
    width = 0;
    height = 0;
    dirty = true;
    children = [];
    depth = 0;
    enabled = true }

exception Found of t

let pick ~abs_x ~abs_y window =
  let rec visit abs_x abs_y window =
    print_int abs_x;
    print_newline ();
    print_int abs_y;
    print_newline ();
    print_int window.rel_x;
    print_newline ();
    print_int window.rel_y;
    print_newline ();
    flush stdout;
    if abs_x >= window.rel_x &&
      abs_x < window.rel_x + window.width
      && abs_y > window.rel_y &&
      abs_y < window.rel_y + window.height
    then true
    else
      try let window = List.find (visit (abs_x - window.rel_x) (abs_y - window.rel_y)) window.children
          in raise (Found window)
      with Not_found -> false
  in
  try
    if visit abs_x abs_y window
    then window
    else raise Not_found
  with Found window -> window
