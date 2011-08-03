module Make(B : Tile.GRAPHICS_BACKEND) = struct
  module T = Tile.Make(B)

  type t = { tiles : T.t list; }

  let create () = { tiles = [] }
  let add canvas tile =
    { canvas with tiles = tile :: canvas.tiles }

  and draw canvas gc =
    List.iter (T.draw gc) canvas.tiles

  and button_pressed canvas ~x ~y =
    { canvas with tiles = List.map (fun t -> T.button_pressed t ~x ~y) canvas.tiles }

  and button_released canvas ~x ~y =
    { canvas with tiles = List.map (fun t -> T.button_released t ~x ~y) canvas.tiles }

  and motion canvas ~x ~y =
    { canvas with tiles = List.map (fun t -> T.motion t ~x ~y) canvas.tiles }

end

