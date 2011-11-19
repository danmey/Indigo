module type S = sig
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
end
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
module Log = struct

  let log = function
    | Server cmd -> begin
      match cmd with
	| Disconnect str -> LOG "Client '%s' disconnected" str LEVEL DEBUG
	| RequestLogin (username, pass) -> LOG "RequestLogin from %s with password %s" username pass LEVEL DEBUG
	| _ -> ()
    end
    | _ -> ()

  let input_value ch =
    lwt v = Lwt_chan.input_value ch in
    log v;
    Lwt.return v
end
