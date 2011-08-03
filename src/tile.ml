(* TODO: This file needs to be rewritten to get rid of object orientatoen: EDIT: Progress. *)

module type GRAPHICS_BACKEND = sig
  type bitmap
  type gc
  val draw_bitmap : pos:(int * int) -> gc -> bitmap -> unit
  val bitmap_of_file : fn:string -> bitmap
  val size_of_bitmap : bitmap -> int*int
end

module Make(B : GRAPHICS_BACKEND) = struct

  type t  = { width : int;
              height : int;
              pos : (int * int);
              drag: drag option;
              graphics: graphics option;
            }

  and drag = { dragged : bool;
               drag_x : int;
               drag_y : int }

  and graphics = { bitmap: B.bitmap}

  let rec is_in t ~x ~y =     
    let xt, yt = t.pos in
    x >= xt && x < xt + t.width && y >= yt && y < yt + t.height

  and button_pressed t ~x ~y =
    if is_in t ~x ~y then
      let drag = match t.drag with
        | Some drag ->
          let pos_x, pos_y = t.pos in
          let drag_x, drag_y = x - pos_x,  y - pos_y in
          Some { dragged = true; drag_x; drag_y }
        | None -> None in
      { t with drag }
    else t

  and button_released t ~x ~y =
    if is_in t ~x ~y then
      let drag = match t.drag with
        | Some drag ->
          Some { drag with dragged = false; }
        | None -> None in
      { t with drag }
    else t
      
  and motion t ~x ~y = 
    if is_in t ~x ~y then
      match t.drag with
        | Some d ->
          if d.dragged then
            { t with pos= (x-d.drag_x, y-d.drag_y) }
          else t
        | None -> t
    else t
        
  and default_drag = { dragged = false; drag_x = 0; drag_y = 0 }

(* TODO: Move somewher else *)
  and dice ~x ~y =
    let fn = "resources/images/g6-1.png" in
    let bitmap = B.bitmap_of_file ~fn in
    let width, height = B.size_of_bitmap bitmap in
    let pos = x, y in
    { width;
      height;
      pos;
      drag = Some default_drag;
      graphics = Some { bitmap = bitmap } }

  and draw canvas t =
    match t.graphics with
      | Some g -> B.draw_bitmap ~pos:t.pos canvas g.bitmap
      |  None -> ()


end

