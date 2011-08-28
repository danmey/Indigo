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
      }
  and drag = { dragged : bool;
               drag_x : int;
               drag_y : int;
               drag_type:drag_type }
  and element = 
    | Board of t * t list
    | Element of t
        
  and drag_type = [ `Left | `Right | `Top | `Bottom | `Centre ]

  let default_drag = { drag_type=`Centre; dragged = false; drag_x = 0; drag_y = 0 }
  module Default = struct

    let rec drag drag_type t ~x ~y =
      let drag = match t.drag with
        | Some drag ->
          let pos_x, pos_y = t.pos in
          begin match drag_type with
            | `Centre | `Left | `Top ->
              let drag_x, drag_y = x - pos_x,  y - pos_y in
              Some { drag_type; dragged = true; drag_x; drag_y }
            | `Right | `Bottom ->
              let drag_x, drag_y = x - (pos_x+t.width-10),  y - (pos_y+t.height-10) in
              Some { drag_type; dragged = true; drag_x; drag_y } end
        | None -> None  
      in
      { t with drag = drag }

    and button_pressed t ~x ~y =
      if is_in t ~x ~y then
        drag `Centre t ~x ~ y
      else t

    and is_in t ~x ~y =     
      let xt, yt = t.pos in
      x >= xt && x < xt + t.width && y >= yt && y < yt + t.height
  end

  module Board = struct
    let rec draw canvas t =
      G.Draw.rectangle canvas ~pos:t.pos ~size:(t.width, t.height);
      let x, y = t.pos in
      let x, y = x + 10, y + 10 in
      let width, height = t.width - 20, t.height - 20 in
      let pos = x,y in
      G.Draw.rectangle canvas ~pos:pos ~size:(width, height)

    and board ~x ~y id =
      let width = 200 in
      let height = 200 in
      let pos = x,y in
      let fn = "" in
      Board ({ width = width;
        height = height;
        pos = pos;
        drag = Some default_drag;
        graphics = fn;
        id; }, [])
    and edge t ~x ~y =
      let tx, ty = t.pos in
      if x > tx && x < tx + 10 then Some `Left else
        if x > tx + t.width - 10 && x < tx + t.width then Some `Right else
          if y > ty && y < ty + 10 then Some `Top else
            if y > ty + t.height - 10 && y < ty + t.height then Some `Bottom else
            None
    and button_pressed t ~x ~y =
      if Default.is_in t ~x ~y then
        match edge t ~x ~y with
          | None -> Default.button_pressed t ~x ~y
          | Some `Left -> Default.drag `Left t ~x ~y
          | Some `Right -> Default.drag `Right t ~x ~y
          | Some `Top -> Default.drag `Top t ~x ~y
          | Some `Bottom -> Default.drag `Bottom t ~x ~y
      else t
  end

  module Element = struct
  (* TODO: Move somewher else *)
    let rec dice ~x ~y id =
      let fn = "resources/images/g6-1.png" in
      let bitmap = G.bitmap_of_file ~fn in
      let width, height = G.size_of_bitmap bitmap in
      let pos = x, y in
      ignore(G.load_bitmap fn);
      Element { width = width;
        height = height;
        pos = pos;
        drag = Some default_drag;
        graphics = fn;
        id;
      }

    and draw canvas t =
      G.Draw.bitmap canvas (G.bitmap t.graphics) ~pos:t.pos;
      G.Draw.text canvas t.id ~pos:t.pos

  end
    
  let lift e f =
    match e with
      | Board (b,l) -> Board (f b, l)
      | Element e -> Element (f e)

  let lift0 e f =
    match e with
      | Board (b,l) -> f b
      | Element e -> f e

  let rec button_released e ~x ~y = lift e (fun t ->
    (* if Default.is_in t ~x ~y then *)
      let drag = match t.drag with
        | Some drag ->
          Some { drag with dragged = false; }
        | None -> None in
      { t with drag = drag })
    (* else t *)
        
  and limit t =
    let x, y = t.pos in
    let x = if x < 0 then 0 else x in
    let y = if y < 0 then 0 else y in
    let min = 4*10+5 in
    let width = if t.width < min then min else t.width in
    let height = if t.height < min then min else t.height in
    { t with pos = (x,y); width; height }
      
  and motion t ~x ~y = lift t (fun t ->
      match t.drag with
        | Some d ->
          if d.dragged then
            limit (match d.drag_type with
              | `Centre -> { t with pos = (x-d.drag_x, y-d.drag_y) }
              | `Left -> 
                let dx = x - d.drag_x - fst t.pos in
                { t with pos = (x-d.drag_x, snd t.pos); width = t.width - dx }
              | `Right -> 
                let dx = x - fst t.pos in
                { t with width = dx }
              | `Top -> 
                let dy = y - d.drag_y - snd t.pos in
                { t with pos = (fst t.pos, y-d.drag_y); height = t.height - dy }
              | `Bottom ->
                let dy = y - snd t.pos in
                { t with height = dy })
          else t
        | None -> t)

    and draw canvas = function
      | Board (b,_) -> Board.draw canvas b
      | Element e -> Element.draw canvas e
        
  and print = function
    | Board (e, _) -> 
      Printf.printf "Board: width: %d height: %d pos: (%d %d)" 
        e.width 
        e.height 
        (fst e.pos) 
        (snd e.pos)
    | Element e ->
      Printf.printf "Element: width: %d height: %d pos: (%d %d)" 
            e.width 
        e.height 
        (fst e.pos) 
        (snd e.pos)
        
  and dragged e = 
    let e = match e with
      | Board (e, _)
      | Element e -> e 
    in
    match e.drag with
      | Some drag -> true
      | None -> false 
        
  and is_in ~x ~y = function
    | Board (e, _)
    | Element e -> Default.is_in ~x ~y e
      
  and button_pressed ~x ~y = function
        | Board (b, lst) -> Board (Board.button_pressed b ~x ~y, lst)
        | Element e -> Element (Default.button_pressed e ~x ~y)
          
  and id t = lift0 t (fun t -> t.id)

end

