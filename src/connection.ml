(*----------------------------------------------------------------------------
  server.ml - Client server code.
  Copyright (C) 2011 Wojciech Meyer, Filip Łuszczak

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


open Lwt
open Lwt_unix
open Lwt_chan

type command =
  | Quit
  | Brush of int * int
  | AddTile 
let write out_ch (cmd : command) =
    output_value out_ch cmd;
    flush out_ch

let read_val in_ch : command option Lwt.t =
      input_value in_ch >>= fun (v : command) -> return (Some v)

    
module Server = struct

  let rec restart_on_EINTR f x = 
    try f x with Unix.Unix_error (Unix.EINTR, _, _) -> restart_on_EINTR f x

  let start port =
    lwt host_name = gethostname () in
    lwt entry = gethostbyname host_name in
    let host = entry.h_addr_list.(0) in
    let addr = ADDR_INET (host, port) in
    let server_socket = socket PF_INET SOCK_STREAM 0 in
      catch (fun () ->
        bind server_socket addr;
        listen server_socket 10;
        let rec loop clients =
          choose [
            begin
              lwt (fd,_) = accept server_socket in
              let out_ch = out_channel_of_descr fd in
              let in_ch = in_channel_of_descr fd in
              loop ((in_ch, out_ch) :: clients)
            end;

            begin
              let read_clients = 
                List.map 
                  (fun (in_ch, out_ch) -> 
                    read_val in_ch) clients 
              in
              lwt cmd = Lwt.pick read_clients in
              Lwt_util.iter (fun (_,out_ch) ->
                    (* TODO: Still don't understand why the data still arrive to random sockets 
                         even if i check it was the same socket as read from *)
                (if (* s != s' *) true then 
                    begin
                      match cmd with 
                        | Some cmd ->
                          write out_ch cmd 
                        | None -> return ()
                    end
                     else return ())) clients; loop clients
            end]
        in
        loop []) (fun z -> close server_socket; fail z)

end


module Client = struct
  let connect ~port ~host ~receive =
     lwt entry = gethostbyname host in
     let host = entry.h_addr_list.(0) in
     let addr = ADDR_INET (host, port) in
     lwt in_ch, out_ch = open_connection addr in
     let rec loop () =
       lwt cmd = read_val in_ch in
       lwt () = receive cmd in
       loop ()
    in
    Lwt.ignore_result (loop ());
    return (fun cmd -> output_value out_ch cmd)
end


let main () =
  let port = int_of_string Sys.argv.(1) in
  run (Server.start port)
;;
