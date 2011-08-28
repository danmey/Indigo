(*----------------------------------------------------------------------------
  board.ml - Board objects
  Copyright (C) 2011 Wojciech Meyer 

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
  --------------------------------------------------------------------------*)

module type GRAPHICS_BACKEND = sig
  type bitmap
  type gc
  module Draw : sig
    val bitmap : pos:(int * int) -> gc -> bitmap -> unit
    val text : pos:(int * int) -> gc -> string -> unit
    val rectangle : pos:(int*int) -> size:(int*int) -> gc -> unit
  end
  val bitmap_of_file : fn:string -> bitmap
  val size_of_bitmap : bitmap -> int*int
  val bitmap : string -> bitmap
  val load_bitmap : string -> bitmap
end


module type NETWORK_BACKEND = sig
  type t
  val send : (t -> unit)
  val set_send : (t -> unit) -> unit
end


module Make(G : GRAPHICS_BACKEND) = struct
  type t  = 
      { width : int;
        height : int;
        pos : (int * int);
        drag: drag option;
        id: string;
        graphics: string;
        element: element
      }
  and drag = { dragged : bool;
               drag_x : int;
               drag_y : int }
  and element = 
    | Board
    | Dice

  let default_drag = { dragged = false; drag_x = 0; drag_y = 0 }

  module Board = struct
    let draw canvas t =
      G.Draw.rectangle canvas ~pos:t.pos ~size:(t.width, t.height)

    and board ~x ~y id =
      let width = 200 in
      let height = 200 in
      let pos = x,y in
      let fn = "" in
      { width = width;
        height = height;
        pos = pos;
        drag = Some default_drag;
        graphics = fn;
        id;
        element = Board }
  end

  module Element = struct
  (* TODO: Move somewher else *)
    let rec dice ~x ~y id =
      let fn = "resources/images/g6-1.png" in
      let bitmap = G.bitmap_of_file ~fn in
      let width, height = G.size_of_bitmap bitmap in
      let pos = x, y in
      ignore(G.load_bitmap fn);
      { width = width;
        height = height;
        pos = pos;
        drag = Some default_drag;
        graphics = fn;
        id;
        element = Dice
      }

    and draw canvas t =
      G.Draw.bitmap canvas (G.bitmap t.graphics) ~pos:t.pos;
      G.Draw.text canvas t.id ~pos:t.pos

  end

    let rec is_in t ~x ~y =     
      let xt, yt = t.pos in
      x >= xt && x < xt + t.width && y >= yt && y < yt + t.height

    and button_pressed t ~x ~y =
      if is_in t ~x ~y then
        let drag = match t.drag with
          | Some drag ->
            let pos_x, pos_y = t.pos in
            let drag_x, drag_y = x - pos_x,  y - pos_y in
            Some { dragged = true; drag_x=drag_x; drag_y=drag_y }
          | None -> None in
        { t with drag = drag }
      else t

    and button_released t ~x ~y =
      if is_in t ~x ~y then
        let drag = match t.drag with
          | Some drag ->
            Some { drag with dragged = false; }
          | None -> None in
        { t with drag = drag }
      else t
        
    and motion t ~x ~y = 
      match t.drag with
        | Some d ->
          if d.dragged then
            { t with pos = (x-d.drag_x, y-d.drag_y) }
          else t
        | None -> t

    and draw canvas t = 
      match t.element with
        | Board -> Board.draw canvas t
        | Dice -> Element.draw canvas t
    and print t =
      Printf.printf "width: %d height: %d pos: (%d %d)" t.width t.height (fst t.pos) (snd t.pos)
    and dragged t =
      match t.drag with
        | Some drag -> true
        | None -> false 

end

