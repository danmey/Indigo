(*----------------------------------------------------------------------------
  config.ml - Config file parsing
  Copyright (C) 2011 Wojciech Meyer, Jakub Oboza 

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
  --------------------------------------------------------------------------*)

open Yojson.Safe
open Config_file
module NewClient = struct

  exception Wrong_type of string

  (* TODO add File.join / Unix.join *)
  let config_file = (Unix.getpwuid (Unix.getuid ())).Unix.pw_dir ^ "/.indigo.json"
  let config = from_file config_file
  let default_host = "danmey.org"
  let default_uname = "wojtek"
  let default_port = 1234
  let default_profile = "main"

  let int = function `Int i -> i | _ -> raise (Wrong_type "int")
  let string = function `String s -> s | _ -> raise (Wrong_type "string")

 

  let read () = 
    let config = from_file config_file in
    let get_param conversion param_name default_value = match config with
           | `Assoc lst -> conversion(List.assoc param_name lst)
           | _ -> default_value in
    let host    = get_param string "host"    default_host       in
    let uname   = get_param string "uname"   default_uname      in
    let port    = get_param int    "port"    default_port       in
    let pass    = get_param string "pass"    default_profile    in   
    { LoginData.host;
      LoginData.port;
      LoginData.login = Some 
        (LoginData.FullLogin 
           { LoginData.uname;
             LoginData.pass; }) } 

  let write data = ()

  let with_profile f = let data = read () in 
                       let data = f data  in
                       match data with 
                         | Some data -> write data; Some data 
                         | None -> data

end



module Client = struct
  let config_file =  "/.indigo.json"
  let default_host = "danmey.org"
  let default_uname = "wojtek"
  let default_port = 1234
  let default_profile = "main"
  
  let entries group profile =
    let host_cp = new string_cp ~group [profile; "host"] default_host "Host name." in
    let port_cp = new int_cp ~group [profile; "port"] default_port "Port." in
    let uname_cp = new string_cp ~group [profile; "uname"] default_uname "User name." in
    let pass_cp = new string_cp ~group [profile; "pass"] "" "Password." in
    host_cp, port_cp, uname_cp, pass_cp

  let setup () =
    let group = new group in
    let profile_cp = (new list_cp string_wrappers ~group ["profiles"] [default_profile] "List of profiles.") in
    group#read config_file;
    let profile, (host_cp, port_cp, uname_cp, pass_cp)
      = match profile_cp # get with
        | profile :: _ -> 
          profile, entries group profile
        | [] ->
          let (host_cp, port_cp, uname_cp, pass_cp as entries) = entries group default_profile in
          group # add host_cp;
          group # add port_cp; 
          group # add uname_cp;
          group # add pass_cp;
          default_profile, entries in
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
            pass_cp # set pass;
          | LoginData.Uname uname ->
            uname_cp # set uname end;
    group # write config_file
      

  let with_profile f = let data = read () in 
                       let data = f data in
                       match data with 
                         | Some data -> write data; Some data 
                         | None -> data
end


module Server = struct
  let config_file = (Unix.getpwuid (Unix.getuid ())).Unix.pw_dir ^ "/.indigo-server.ml"
  let read () =
    let group = new group in
    let auth_list_cp = 
      new list_cp (tuple2_wrappers string_wrappers string_wrappers)
        ~group ["passwd"] ["admin", "admin"] "List of users password." 
    in
    group # read config_file;
    auth_list_cp # get
      
end
