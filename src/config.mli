module Client :
sig
  val read : unit -> LoginData.t
  val write : LoginData.t -> unit
  val with_profile :
    (LoginData.t -> LoginData.t option) -> LoginData.t option
end

module Server :
sig
  val read : unit -> (string * string) list
end
