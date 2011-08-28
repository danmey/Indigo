  type client_cmd =
    | MoveElement of string * (int * int)
    | Login of string
    | BadAuth of string
  type server_cmd =
    | Disconnect of string
    | RequestLogin of string * string
  type t =
    | Server of server_cmd
    | Client of client_cmd
