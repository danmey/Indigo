open Config_file

let config_file = "/home/danmey/.indigo.ml"
let default_host = "danmey.org"
let default_uname = "wojtek"
let default_port = 1234
let default_profile = "main"

let setup () =
  let group = new group in
  let profile_cp = (new list_cp string_wrappers ~group ["profiles"] [default_profile] "List of profiles.") in
  group#read config_file;
  let profile :: _ = profile_cp # get in
  let host_cp = new string_cp ~group [profile; "host"] default_host "Host name." in
  let port_cp = new int_cp ~group [profile; "port"] default_port "Port." in
  let uname_cp = new string_cp ~group [profile; "uname"] default_uname "User name." in
  let pass_cp = new string_cp ~group [profile; "pass"] "" "Password." in
  group#read config_file;
  group, profile, host_cp, port_cp, uname_cp, pass_cp

let read () =
  let group, profile, host_cp, port_cp, uname_cp, pass_cp = setup () in
  { LoginData.host = host_cp # get;
    LoginData.port = port_cp # get;
    LoginData.login = Some 
      (LoginData.FullLogin 
         { LoginData.uname = uname_cp # get;
           LoginData.pass = pass_cp # get; }) }
    
let write data =
  let group, profile, host_cp, port_cp, uname_cp, pass_cp = setup () in
  host_cp # set data.LoginData.host;
  port_cp # set data.LoginData.port;
  begin match data.LoginData.login with
    | None -> ()
    | Some login ->
      match login with
        |  (LoginData.FullLogin 
              { LoginData.uname = uname;
                LoginData.pass = pass }) ->
          uname_cp # set uname;
          pass_cp # set "ala ma kota"
        | LoginData.Uname uname ->
          uname_cp # set uname end;
  group # write config_file
    

let with_profile f = let data = read () in 
                     let data = f data in
                     write data; data
 
