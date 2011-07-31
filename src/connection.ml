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

let write fd (cmd : command) =
  let out_ch = out_channel_of_descr fd in
  output_value out_ch cmd;
  flush out_ch

module Server = struct

  let rec restart_on_EINTR f x = 
    try f x with Unix.Unix_error (Unix.EINTR, _, _) -> restart_on_EINTR f x

  let read (fd,_) : command Lwt.t =
    let in_ch = in_channel_of_descr fd in
    input_value in_ch

  let start port =
      gethostname () >>= fun host_name ->
      gethostbyname host_name >>= fun entry ->
      let host = entry.h_addr_list.(0) in
      let addr = ADDR_INET (host, port) in
      let server_socket = socket PF_INET SOCK_STREAM 0 in
      catch (fun () ->
        bind server_socket addr;
        listen server_socket 10;
        let rec loop clients =
          choose [(accept server_socket >>= fun client ->
                   loop (client :: clients));
                  let read_clients = List.map read clients in
                  Lwt_unix.sleep 0.1 >>= fun () -> Lwt.pick read_clients >>= fun cmd ->
                  Lwt_util.iter (fun (s,_) ->
                      (* TODO: Still don't understand why the data still arrive to random sockets 
                         even if i check it was the same socket as read from *)
                    (if (* s != s' *) true then 
                        begin
                          print_endline (match cmd with Quit -> "Quit Server");
                          Pervasives.flush Pervasives.stdout;
                          (write s cmd)
                        end
                     else return ())) clients; loop clients]
        in
        loop []) (fun z -> close server_socket; fail z)

end


module Client = struct
  let connect port host ~receive =
    gethostbyname host >>= fun entry ->
    let host = entry.h_addr_list.(0) in
    let addr = ADDR_INET (host, port) in
    let socket = socket PF_INET SOCK_STREAM 0 in
    connect socket addr >>= fun () ->
    return ((fun () ->
      let in_ch = in_channel_of_descr socket in
      input_value in_ch >>= fun (cmd : command) ->
      print_endline "Client update";
      Pervasives.flush Pervasives.stdout;
      receive cmd),
    (fun (cmd : command) ->
      print_endline "Client send";
      Pervasives.flush Pervasives.stdout;
      write socket cmd))
end

let main () =
  let port = int_of_string Sys.argv.(1) in
  run (Server.start port)
;;
