
module Make(C : sig type t module Item : sig type t end end) = struct
  type t =
    | Server of server_cmd
    | Client of client_cmd
  and client_cmd =
    | State of C.t
    | MoveElement of string * (int * int)
  and server_cmd =
    | Disconnect of string
end
