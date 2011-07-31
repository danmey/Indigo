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


module Server = struct

  let rec restart_on_EINTR f x = 
    try f x with Unix.Unix_error (Unix.EINTR, _, _) -> restart_on_EINTR f x

  (* TODO: It's maybe a minimal server code that copies received data to stdout.
     Next step is convert it to use Lwt_unix in non blocking setting. *)

  let read (s,_) =
      let str = String.make 4096 ' ' in
      read s str 0 4096 >>= fun n ->
      match n with
        | 0 -> return None
        | n -> return (Some (s, String.sub str 0 n))

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
                  Lwt_unix.sleep 0.1 >>= fun () -> Lwt.pick read_clients >>= fun str ->
                  (match str with
                    | Some (s', str) -> Lwt_util.iter (fun (s,_) ->
                      (* TODO: Still don't understand why the data still arrive to random sockets 
                         even if i check it was the same socket as read from *)
                      (if (* s != s' *) true then 
                          begin
                            ignore(write s str 0 (String.length str));
                            return ()
                          end
                        else return ())) clients
                    | None -> return ()); loop clients]
        in
        loop []) (fun z -> close server_socket; fail z)

end


module Client = struct
end

let main () =
  let port = int_of_string Sys.argv.(1) in
  run (Server.start port)
;;
main()
