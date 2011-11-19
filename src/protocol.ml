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
	| Disconnect str -> LOG "> Client '%s' disconnected" str LEVEL DEBUG
	| RequestLogin (username, pass) -> LOG "> RequestLogin from %s with password %s" username pass LEVEL DEBUG
        | RequestUserList -> LOG "> RequestUserList" LEVEL DEBUG
        | Quit user -> LOG "> User '%s' has quit" user LEVEL DEBUG
        | Kick user -> LOG "> User '%s' was kicked from server" user LEVEL DEBUG
	| _ -> LOG "> Unknown message" LEVEL DEBUG
    end
    | Client cmd -> begin
      match cmd with
        | BadPassword str -> LOG "< BadPassword for user '%s'" str LEVEL DEBUG
        | Login user -> LOG "< Login for user '%s'" user LEVEL DEBUG
        | MoveElement (what, (x, y)) -> LOG "Moved Element %s to (%d, %d)" what x y LEVEL DEBUG
        | BadUser user -> LOG "< Bad user '%s'" user LEVEL DEBUG
        | NewUser user -> LOG "< New user '%s'" user LEVEL DEBUG
        | UserAlreadyLoggedIn user -> LOG "< User '%s' already logged in" user LEVEL DEBUG 
        | KickUser user -> LOG "< User '%s' has been kicked" user LEVEL DEBUG
        | UserList _ -> LOG "< Users list requested" LEVEL DEBUG
        | _ -> LOG "< Unknown message" LEVEL DEBUG
    end

  let input_value ch =
    lwt v = Lwt_chan.input_value ch in
    log v;
    Lwt.return v

  let output_value ch v =
    log v;
    Lwt_chan.output_value ch v

end


