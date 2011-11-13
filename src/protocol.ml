  type client_cmd =
    | MoveElement of string * (int * int)
    | Login of string
    | BadPassword of string
    | BadUser of string
    | UserList of string list
    | NewUser of string
    | UserAlreadyLoggedIn of string
  type server_cmd =
    | Disconnect of string
    | RequestLogin of string * string
    | RequestUserList
    | Quit of string
    | Kick of string
  type t =
    | Server of server_cmd
    | Client of client_cmd
