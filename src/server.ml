module GtkBackend = struct
  type bitmap = unit
  type gc = unit

  let resources : (string, unit) Hashtbl.t = Hashtbl.create 137

  let draw_bitmap ~pos:((x:int),(y:int)) (gc:gc) (bitmap:bitmap) = ()
  let draw_text ~pos:((x:int),(y:int)) (gc:gc) string = ()

  let bitmap_of_file ~(fn:string) = ()

  let size_of_bitmap (bitmap:unit) = (0,0)

  let load_bitmap (fn:string) = ()

  let bitmap (name:string) = ()

end

module Canvas = Table.Make(GtkBackend)
module Board = Board.Make(GtkBackend)
module Protocol = Protocol.Make(Canvas)
module Connection = Connection.Make(Protocol)(struct 
  let receive (_ : Protocol.client_cmd) = () end)

lwt () =
    Connection.Server.start (int_of_string (Sys.argv.(1)))
