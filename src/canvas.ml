module Make(G : Tile.GRAPHICS_BACKEND) = struct
  module T = Tile.Make(G)
  module Item = T
  type t = { tiles : T.t list; }

  let rec create () = { tiles = [] }
  and add canvas tile =
    { canvas with tiles = tile :: canvas.tiles; }

  and draw canvas gc =
    List.iter (T.draw gc) canvas.tiles

  and button_pressed canvas ~x ~y =
    { canvas with tiles = List.map (fun t -> T.button_pressed t ~x ~y) canvas.tiles }

  and button_released canvas ~x ~y =
    { canvas with tiles = List.map (fun t -> T.button_released t ~x ~y) canvas.tiles }

  and motion canvas ~x ~y =
    { canvas with tiles = List.map (fun t -> T.motion t ~x ~y) canvas.tiles }

  and print c = List.iter (fun t -> T.print t; print_endline ""; flush stdout) c.tiles
    
  and dragged canvas = List.exists T.dragged canvas.tiles

  and replace_item canvas id ~item =
    let rec loop = function
      | [] -> []
      | i :: xs ->
        if id = i.T.id then
          item :: loop xs
        else
          i :: loop xs
    in
    { canvas with tiles = loop canvas.tiles }

  and find_item canvas id =
    List.find (fun t -> t.T.id = id) canvas.tiles
    
  and move_item canvas id ~x ~y =
    let item = find_item canvas id in
    replace_item canvas id ~item:{item with T.pos=(x,y) }

end

