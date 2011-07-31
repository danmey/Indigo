(*----------------------------------------------------------------------------
  server.ml - Server side code.
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


open Unix


module Server = struct

  let rec restart_on_EINTR f x = 
    try f x with Unix_error (EINTR, _, _) -> restart_on_EINTR f x

  (* TODO: It's maybe a minimal server code that copies received data to stdout.
     Next step is convert it to use Lwt_unix in non blocking setting. *)
  let start port receive =
    let host_name = gethostname () in
    let entry = gethostbyname host_name in
    let host = entry.h_addr_list.(0) in
    let addr = ADDR_INET (host, port) in
    let server_socket = socket PF_INET SOCK_STREAM 0 in
    try
      bind server_socket addr;
      listen server_socket 10;
      Sys.signal Sys.sigpipe Sys.Signal_ignore;
      let rec loop () =
        Unix.sleep 1;
        let client = accept server_socket in
        receive server_socket client;
        loop () in
      loop ()
    with z -> close server_socket; raise z

end


let main () =
  let port = int_of_string Sys.argv.(1) in
  Server.start port
    (fun _ (s,_) ->
      while true do
        Unix.sleep 1;
        let str = String.make 10 ' ' in
        try
        (match read s str 0 10 with
          | 0 -> ()
          | n -> ignore(write stdout str 0 n))
        with z -> ()
      done)
;;
main()
