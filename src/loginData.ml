open Pcre

type t = { host : string; port : int; login: login option }
and full_login = { uname: string; pass: string }
and login =
  | Uname of string
  | FullLogin of full_login


exception Parse_error of string

let err msg = raise (Parse_error msg)

let rexp n r s =
  try
    let rex = regexp  r in
    let ss = exec ~rex s in
    Some (get_substring ss n)
  with Not_found -> None

(* Taken from http://stackoverflow.com/questions/106179/regular-expression-to-match-hostname-or-ip-address *)
let validIpAddressRegex = "(([0-9]|[1-9][0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])\\.){3}([0-9]|[1-9][0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])";;
let validHostnameRegex = "(([a-zA-Z]|[a-zA-Z][a-zA-Z0-9\\-]*[a-zA-Z0-9])\\.)*([A-Za-z]|[A-Za-z][A-Za-z0-9\\-]*[A-Za-z0-9])"
let uname_chars = "[a-zA-Z0-9_\\-]"
let port_chars = "[0-9]"  
let hostname_regex = Printf.sprintf "(%s)|(%s)" validHostnameRegex validIpAddressRegex
let login_regex = Printf.sprintf "^((%s+)@)?(%s)(:(%s+))?$" uname_chars hostname_regex port_chars

let parse_login str =
  rexp 2 login_regex str,
  rexp 4 login_regex str,
  rexp 8 login_regex str,
  rexp 13 login_regex str

let of_string text =
  let uname, host, ip, port = parse_login text in
  let host = match host, ip with 
    | (Some h, None) -> Some h  
    | (None , Some h) -> Some h 
    | _ -> None in
  let err what = err (Printf.sprintf "%s needs to be specified or is incorrect!" what) in
  match host with
    | None -> err "Host or IP"
    | Some host -> begin match port with
        | None -> err "Port"
        | Some port -> 
          { login = 
              begin match uname with 
                | Some uname -> Some (Uname uname)
                | None -> None 
              end;
            host; 
            port = int_of_string port } end

let to_string_server_only { host; port } =
  host ^ ":" ^ string_of_int port

let to_string_with_password ({ host; port; login } as d) =
  begin match login with
    | Some login -> begin match login with
        | FullLogin { uname; pass } -> Printf.sprintf "%s:%s@" uname pass
        | Uname uname -> Printf.sprintf "%s@" uname
    end
    | None -> "" end ^ to_string_server_only d

let to_string ({ host; port; login } as d) =
  begin match login with
    | Some login -> begin match login with
        | FullLogin { uname; pass } -> Printf.sprintf "%s@" uname
        | Uname uname -> Printf.sprintf "%s@" uname
    end
    | None -> "" end ^ to_string_server_only d

let validate data =
  ignore(of_string (to_string data));
  data

let is_complete data =
  let _ = validate data in
  match data with
    | { login = Some (FullLogin _) } -> true
    | _ -> false

let set_auth uname pass data =
  {data with login = Some (FullLogin {uname; pass = Digest.to_hex (Digest.string pass)})}
