(*----------------------------------------------------------------------------
  server.ml - Client server code.
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


open Lwt
open Lwt_unix
open Lwt_chan


module Make(C : sig type command end)(R : sig val receive : C.command -> unit end) = struct
let write out_ch (cmd : C.command) =
  try_lwt
    lwt () = output_value out_ch cmd in
    flush out_ch
  with 
    | End_of_file -> return ()
    | Unix.Unix_error (_,_,_) -> return ()

let read_val in_ch =
  try_lwt
    input_value in_ch >>= fun (v : C.command) -> return (Some v)
  with 
    | End_of_file -> return None
    | Unix.Unix_error (_,_,_) -> return None
    
module Server = struct

  let rec restart_on_EINTR f x = 
    try f x with Unix.Unix_error (Unix.EINTR, _, _) -> restart_on_EINTR f x

  let rec start port =
    lwt host_name = gethostname () in
    lwt entry = gethostbyname host_name in
    let host = entry.h_addr_list.(0) in
    let addr = ADDR_INET (host, port) in
    let server_socket = socket PF_INET SOCK_STREAM 0 in
    let read_clients clients = 
      List.map 
        (fun (in_ch, _, fd) -> 
          lwt cmd = read_val in_ch in
          match cmd with
            | Some cmd -> return (Some (fd, cmd))
            | None -> return None)
        clients
    in
    catch (fun () ->
      bind server_socket addr;
      listen server_socket 10;
      let rec loop clients =
        choose [
          begin
            lwt (fd,_) = accept server_socket in
            let out_ch = out_channel_of_descr fd in
            let in_ch = in_channel_of_descr fd in
            loop ((in_ch, out_ch, fd) :: clients)
          end;

          match_lwt Lwt.pick (read_clients clients) with
            | Some (fd', cmd)  ->
              Lwt_util.iter 
                (fun (in_ch, out_ch, fd) ->
                  return (match cmd with 
                    cmd ->
                      (if Lwt_unix.unix_file_descr fd <> Lwt_unix.unix_file_descr fd' then 
                          Lwt.ignore_result (write out_ch cmd))
                    )) clients; loop clients
            | None -> loop clients
        ]

        in
        loop []) 
      (function 
        | z -> close server_socket; fail z)

end


module Client = struct
  let connect ~port ~host =
     lwt entry = gethostbyname host in
     let host = entry.h_addr_list.(0) in
     let addr = ADDR_INET (host, port) in
     lwt in_ch, out_ch = open_connection addr in
     let rec loop () =
       lwt cmd = read_val in_ch in
       lwt () = return (match cmd with Some cmd -> R.receive cmd | None -> ()) in
       loop ()
    in
    Lwt.ignore_result (loop ());
    return (fun cmd -> output_value out_ch cmd)
end

end
