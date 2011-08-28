(*----------------------------------------------------------------------------
  table.ml - Main definiton of table
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


module Make(G : Board.GRAPHICS_BACKEND) = struct
  module Board = Board.Make(G)
  module Element = Board.Element
  type t = { boards: Board.element list }

  let map_first_hit ~x ~y f t =
    let rec loop hit acc =
      function
        | [] -> hit, List.rev acc
        | el :: xs -> 
          if hit = None && Board.is_in ~x ~y el then
            loop (Some (f el)) acc xs
          else
            loop hit (el::acc) xs
    in
    match loop None [] t.boards with
      | None, boards -> { boards }
      | Some el, boards -> { boards = el :: boards }
    
    
  let rec create () = { boards = [] }

  and add canvas tile =
    { boards = tile :: canvas.boards; }

  and draw canvas gc =
    List.iter (Board.draw gc) (List.rev canvas.boards)

  and button_pressed canvas ~x ~y =
    map_first_hit ~x ~y (fun t -> Board.button_pressed ~x ~y t) canvas

  and button_released canvas ~x ~y =
    { canvas with boards = List.map (fun t -> Board.button_released t ~x ~y) canvas.boards }

  and motion canvas ~x ~y =
    { canvas with boards = List.map (fun t -> Board.motion t ~x ~y) canvas.boards }

  and print c = List.iter (fun t -> Board.print t; print_endline ""; flush stdout) c.boards
    
  and dragged canvas = List.exists Board.dragged canvas.boards

  and replace_item canvas id ~item =
    let rec loop = function
      | [] -> []
      | i :: xs ->
        if id = Board.id i then
          item :: loop xs
        else
          i :: loop xs
    in
    { canvas with boards = loop canvas.boards }

  and find_item canvas id =
    List.find (fun t -> Board.id t = id) canvas.boards
    
  (* and move_item canvas id ~x ~y = *)
  (*   let item = find_item canvas id in *)
  (*   replace_item canvas id ~item:{item with Board.pos=(x,y) } *)

end

