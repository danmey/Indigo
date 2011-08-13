module Make(G : Tile.GRAPHICS_BACKEND) = struct
  module T = Tile.Make(G)
  module Item = T
  type t = { tiles : T.t list; }

  let create () = { tiles = [] }
  let add canvas tile =
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

    (* let sexp_of_t = function Canvas c -> sexp_of_t c *)
  (* let t_of_sexp t = Canvas (t_of_sexp t) *)
end

