(*----------------------------------------------------------------------------
  connection.ml - Client server code.
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


module Make(C : sig
  type client_cmd =
    | MoveElement of string * (int * int)
    | Login of string
    | BadPassword of string
    | BadUser of string
    | UserList of string list
    | NewUser of string
    | UserAlreadyLoggedIn of string
    | KickUser of string
  type server_cmd =
    | Disconnect of string
    | RequestLogin of string * string
    | RequestUserList
    | Quit of string
    | Kick of string
  type t =
    | Server of server_cmd
    | Client of client_cmd

(* include module type of Protocol *)
end)(R : sig val receive : C.client_cmd -> unit end) = struct

  let read_val_client in_ch =
    try_lwt
      input_value in_ch >>= fun (v : C.client_cmd) -> return (Some v)
    with
      | End_of_file -> return None
      | Unix.Unix_error (_,_,_) -> return None

  let read_val_server in_ch =
    try_lwt
      input_value in_ch >>= fun (v : C.t) -> return (Some v)
    with 
      | End_of_file -> return None
      | Unix.Unix_error (_,_,_) -> return None
    

module Server = struct

  type client = {
    mutable name: string option;
    in_ch: Lwt_io.input_channel;
    out_ch: Lwt_io.output_channel }

  let rec restart_on_EINTR f x = 
    try f x with Unix.Unix_error (Unix.EINTR, _, _) -> restart_on_EINTR f x

  let rec start port =
    Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
    Sys.catch_break true;
    lwt host_name = gethostname () in
    lwt entry = gethostbyname host_name in
    let host = entry.h_addr_list.(0) in
    let addr = ADDR_INET (host, port) in
    let clients = Queue.create () in
    let passwd = Config.Server.read () in

    let send_all cmd =
      return (Queue.iter 
        (fun {out_ch} -> 
          Lwt.ignore_result(output_value out_ch cmd))
        clients) in

    (* let send_others name cmd = *)
    (*   return (Queue.iter  *)
    (*     (fun (name',(_,out_ch)) ->  *)
    (*       if name <> name' then *)
    (*         Lwt.ignore_result(output_value out_ch cmd)) *)
    (*     clients) in *)
    let remove_client uname =
      let clients' = Queue.copy clients in
      Queue.clear clients;
      return begin Queue.iter 
          (function 
            | {name=Some name} as client -> 
              begin if uname <> name 
                then Queue.add client clients end
            | client -> Queue.add client clients 
          ) clients' end
    in

    let kick_client uname =
      let clients' = Queue.copy clients in
      Queue.clear clients;
      begin Queue.iter 
          (function 
            | {out_ch;name=Some name} as client -> 
              begin if uname = name then
                  Lwt.ignore_result (output_value out_ch (C.Client (C.KickUser uname))) end
            | client -> Queue.add client clients 
          ) clients' end;
      remove_client uname
    in

    let receive ({ name; in_ch; out_ch } as client) =
      try_lwt
      lwt cmd = input_value in_ch in
      lwt () = match cmd with 
        | C.Client _ as cmd  -> send_all cmd
        | C.Server cmd ->
          begin match cmd with
            | C.RequestLogin (uname, pass) ->
              let module M = Map.Make (struct type t = string let compare = String.compare end) in
              let set = Queue.fold 
                (fun set ->
                  function 
                    | {name=Some name} as el -> 
                      M.add name el set 
                    | _ -> set) M.empty clients in
              begin
              try_lwt
                let _ = M.find uname set in
                output_value out_ch (C.Client (C.UserAlreadyLoggedIn uname))
              with Not_found ->
                let pass' = Digest.to_hex (Digest.string (List.assoc uname passwd)) in
                if pass = pass' 
                then lwt () =
                    client.name <- Some uname;
                    output_value out_ch (C.Client (C.Login uname)) in
                    send_all (C.Client (C.NewUser uname))
                else output_value out_ch (C.Client (C.BadPassword uname))
              end

            | C.RequestUserList ->
              let lst = Queue.fold 
                (fun lst -> function
                  | {name=Some name} ->  name :: lst
                  | _ -> lst) [] clients in
              send_all (C.Client (C.UserList lst))

            | C.Quit uname -> remove_client uname
            | C.Kick uname -> kick_client uname
      end
                
      in
      return None
      with End_of_file -> return name
    in
    let add_client (in_ch,out_ch) = Queue.add {name=None; in_ch; out_ch} clients in
    
    let server = Lwt_io.establish_server addr add_client in
    
    at_exit (fun () -> Lwt_io.shutdown_server server);
    while_lwt true do 
    lwt () = Lwt_unix.sleep 0.01 in 
    return (Queue.iter
              begin fun client ->
              Lwt.ignore_result begin match_lwt receive client with
                | Some name -> remove_client name
                | None -> return () end end
              clients) done

end


module Client = struct
  type ('a, 'b) result = 
    | Authorised of 'a * 'b
    | BadPass
    | BadUname
    | UserAlreadyLoggedIn of string
    | Kick of string
  let connect ?(kick=false) { LoginData.port; LoginData.host; LoginData.login } disconnect =
     lwt entry = gethostbyname host in
     let host = entry.h_addr_list.(0) in
     let addr = ADDR_INET (host, port) in
     lwt in_ch, out_ch = open_connection addr in
     let rec loop () =
       match_lwt read_val_server in_ch with 
         | Some (C.Client (C.KickUser name)) -> print_endline "Kick"; Pervasives.flush Pervasives.stdout; disconnect ()
         | Some (C.Client cmd) -> R.receive cmd; loop ()
         | _ -> disconnect ()
    in
    match login with
      | Some(LoginData.FullLogin {LoginData.uname; LoginData.pass}) ->
        let rec loop2 () =
          lwt cmd = read_val_server in_ch in
          match cmd with
            | Some(C.Client (C.Login uname')) when uname = uname' ->
              loop();
              return (Authorised ((fun (cmd : C.t) -> Lwt.ignore_result(output_value out_ch cmd)), uname))
            | Some(C.Client (C.BadPassword uname')) when uname = uname' ->
                return BadPass
            | Some(C.Client (C.BadUser uname')) when uname = uname' ->
                return BadUname
            | Some(C.Client (C.UserAlreadyLoggedIn uname')) ->
                return (UserAlreadyLoggedIn uname')
            | _ ->  loop2 () in
              lwt () = if kick then output_value out_ch (C.Server (C.Kick (uname))) else return () in
              lwt () = output_value out_ch (C.Server (C.RequestLogin (uname, pass))) in
              lwt a = loop2 () in
            return a
end
end
