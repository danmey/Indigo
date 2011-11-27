(*----------------------------------------------------------------------------
  server.ml - Entry point for INDIGO server
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

module GtkBackend = struct
  type bitmap = unit
  type gc = unit

  let resources : (string, unit) Hashtbl.t = Hashtbl.create 137

  module Draw = struct
    let bitmap ~pos:((x:int),(y:int)) (bitmap:bitmap) = ()
    let text ~pos:((x:int),(y:int)) string = ()
    let rectangle ~pos:(x,y) ~size:(width,height) = ()
  end
  let bitmap_of_file ~(fn:string) = ()

  let size_of_bitmap (bitmap:unit) = (0,0)

  let load_bitmap (fn:string) = ()

  let bitmap (name:string) = ()

end

module Canvas = Table.Make(GtkBackend)
module Board = Board.Make(GtkBackend)
module Connection = Connection.Make(Protocol)(struct 
  let receive (_ : Protocol.client_cmd) = () end)

lwt () =
  Connection.Server.start (int_of_string (Sys.argv.(1)))
