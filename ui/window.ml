type 'a repaint = 'a -> unit

type t = { mutable rel_x : int
         ; mutable rel_y : int
         ; mutable width : int
         ; mutable height : int
         ; mutable dirty : bool
         ; mutable children : t list
         ; mutable parent : t option
         ; mutable depth : int
         ; mutable enabled : bool }

let create () =
  { rel_x = 0;
    rel_y = 0;
    width = 0;
    height = 0;
    dirty = true;
    children = [];
    parent = None;
    depth = 0;
    enabled = true }


let calc_coord f x y window =
  let rec visit x y = function
  | Some window -> visit (f x window.rel_x) (f y window.rel_y) window.parent
  | None -> x, y
  in
  visit x y window

let relative_coord ~abs_x ~abs_y window =
  calc_coord (-) abs_x abs_y (Some window)

let absolute_coord ~rel_x ~rel_y window =
  calc_coord (+) rel_x rel_y (Some window)

(* let pick ~abs_x ~abs_y window = *)
(*   let rec visit acc = function *)
(*   | [] -> acc *)
(*   | window :: rest -> *)
(*     let rel_x, rel_y = relative_coord ~abs_x ~abs_y window in *)
(*     let acc = *)
(*       if rel_x >= 0 *)
(*         && rel_x < window.width *)
(*         && rel_y >= 0 *)
(*         && rel_y < window.height then *)
(*         visit (window :: acc) rest *)
(*       else acc *)
(*     in *)
(*     visit acc window.children *)
(*   in *)
(*   visit [] [window] *)

let abs_rect window =
  let x, y = absolute_coord ~rel_x:0 ~rel_y:0 window in
  Rect.Int.rect (x,y) (window.width, window.height)

let rel_rect window =
  Rect.Int.rect (window.rel_x,window.rel_y) (window.width, window.height)

let print ppf window =
  let open Format in
  let rec visit ppf ({rel_x; rel_y; width; height; children; parent} as window) =
    match children with
    | [] ->
      let abs_x,abs_y = absolute_coord ~rel_x:0 ~rel_y:0 window in
      fprintf ppf "@[<h>@[<hov 0>(%d,@ %d)@ (%d,@ %d)@ ->@ (%d,@ %d)@]%s@]@,"
        rel_x
        rel_y
        width height
        abs_x
        abs_y
        (match parent with Some _ -> "*" | _ -> "")
    | children ->
      let abs_x,abs_y = absolute_coord ~rel_x:0 ~rel_y:0 window in
      fprintf ppf "@[<v 2>@[<h>@[<hov 0>(%d,@ %d)@ (%d,@ %d)@ ->@ (%d,@ %d)@]@]%s@,%a@]@,"
        rel_x
        rel_y
        width
        height
        abs_x
        abs_y
        (match parent with Some _ -> "*" | _ -> "")
        (fun ppf lst -> List.iter (visit ppf) lst) children
  in
  visit ppf window;
  fprintf ppf "=======@."

let set_pos ~rel_x ~rel_y window =
  window.rel_x <- rel_x;
  window.rel_y <- rel_y

let set_abs_pos ~abs_x ~abs_y window =
  let rel_x, rel_y = relative_coord ~abs_x ~abs_y window in
  set_pos ~rel_x ~rel_y window

let set_parent ~parent window =
  window.parent <- Some parent

let is_root window = window.parent = None

let move ~dx ~dy window =
  window.rel_x <- window.rel_x + dx;
  window.rel_y <- window.rel_y + dy

let width window = window.width
let height window = window.height

let x_coord window = window.rel_x
let y_coord window = window.rel_y

let position window = x_coord window, y_coord window
let size window = width window, height window

let set_width window width = window.width <- width
let set_height window height = window.height <- height

let add_width window n = window.width <- window.width + n
let add_height window n = window.height <- window.height + n
