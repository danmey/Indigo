
module Make(B : sig type t end) = struct
  type t =
    | Server of server_cmd
    | Client of client_cmd
  and client_cmd =
    | State of B.t
    | MoveElement of string * (int * int)
  and server_cmd =
    | Disconnect of string
end
