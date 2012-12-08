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

let print ppf window =
  let open Format in
  let rec visit ppf {rel_x; rel_y; width; height; children} =
    match children with
    [] -> fprintf ppf "@[<h>@[<hov 0>(%d,@ %d)@ (%d,@ %d)@]@]@," rel_x rel_y width height
    | children ->
      fprintf ppf "@[<v 2>@[<h>@[<hov 0>(%d,@ %d)@ (%d,@ %d)@]@]@,%a@]@," rel_x rel_y width height (fun ppf lst ->
        List.iter (visit ppf) lst) children;

  in
  visit ppf window;
  fprintf ppf "=======@."


let calc_coord f x y window =
  let rec visit x y = function
  | Some window -> visit (f x window.rel_x) (f y window.rel_y) window.parent
  | None -> x, y
  in
  visit x y (Some window)

let relative_coord ~abs_x ~abs_y window =
  calc_coord (-) abs_x abs_y window

let absolute_coord ~rel_x ~rel_y window =
  calc_coord (+) rel_x rel_y window

let pick ~abs_x ~abs_y window =
  let rec visit acc = function
  | [] -> acc
  | window :: rest ->
    let rel_x, rel_y = relative_coord ~abs_x ~abs_y window in
    let acc =
      if rel_x >= 0
        && rel_x < window.width
        && rel_y >= 0
        && rel_y < window.height then
        visit (window :: acc) rest
      else acc
    in
    visit acc window.children
  in
  visit [] [window]

let abs_rect window =
  let x, y = absolute_coord ~rel_x:0 ~rel_y:0 window in
  Rect.Int.rect (x,y) (window.width, window.height)
