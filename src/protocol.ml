type 'a server_command = Quit | Cmd of 'a

module Make(C : sig type t module Item : sig type t end end) = struct
  type command = 
    State of C.t
    | Quit
end
