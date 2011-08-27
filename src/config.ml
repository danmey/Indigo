open Config_file

let config_file = "/home/danmey/.indigo.ml"
let default_host = "danmey.org"
let default_uname = "wojtek"
let default_port = 1234
let default_profile = "main"

let read () =
  let group = new group in
  group#read config_file;
  let profiles = new list_cp string_wrappers ~group ["profiles"] [default_profile] "List of profiles." in
  let hd :: tl = 
    List.map (fun profile ->
      { LoginData.host = (new string_cp ~group [profile; "host"] default_host "Host name.") # get;
        LoginData.port = (new int_cp ~group [profile; "port"] default_port "Port.") # get;
        LoginData.login = Some 
          (LoginData.FullLogin 
             { LoginData.uname = (new string_cp ~group [profile; "uname"] default_uname "User name.") # get;
               LoginData.pass = (new string_cp ~group [profile; "pass"] "" "Password.") # get; }) }) 
      profiles # get in
  hd

let write data =
  let group = new group in
  let profile :: _ = (new list_cp string_wrappers ~group ["profiles"] [default_profile] "List of profiles.") # get in
  new string_cp ~group [profile; "host"] data.LoginData.host "Host name.";
  new int_cp ~group [profile; "port"] data.LoginData.port "Port.";
  begin match data.LoginData.login with
    | None -> ()
    | Some login ->
      match login with
        |  (LoginData.FullLogin 
              { LoginData.uname = uname;
                LoginData.pass = pass }) ->
            new string_cp ~group [profile; "uname"] uname "User name.";
            new string_cp ~group [profile; "pass"] pass "Password."; ()
        | LoginData.Uname uname ->
          new string_cp ~group [profile; "uname"] uname "User name."; () end;
  group # write
    

let with_profile f = let data = read () in 
                     let data = f data in
                     write data; data
   
  
    
    

