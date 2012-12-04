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
