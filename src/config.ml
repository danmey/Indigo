open Config_file

let config_file = "/home/danmey/.indigo.ml"
let default_profile = "default"
let default_host = "danmey.org"
let default_uname = "wojtek"
let default_port = 1234

let read () =
  let group = new group in
  group#read config_file;
  let profiles = new list_cp string_wrappers ~group ["main"] [default_profile] "List of profiles." in
  List.iter (fun profile ->
    let h = new string_cp ~group ["main"; profile; "host"] "danmey.org" "Host name." in
    let h = new string_cp ~group ["main"; profile; "uname"] "wojtek" "User name." in
    let h = new int_cp ~group ["main"; profile; "port"] 1234 "Port." in
    print_endline (h#get)) profiles#get
  
    
    

