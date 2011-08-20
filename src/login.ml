(*----------------------------------------------------------------------------
  login.ml - Login form
  Copyright (C) 2011 Wojciech Meyer 

  (contains some code and reworked version of `input_widget' in gToolbox.ml 
  from Lablgtk)

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

let mOk = "Ok"
let mCancel = "Cancel"

let ldestroy w = w # destroy ()

let input_widget_ex ~widget ~event ~get_text ~bind_ok ~expand ?(actions=[])
    ?(accept=(mOk, ldestroy)) ?(cancel=(mCancel, ldestroy))
    ~title message =

  let ok_lb, f_ok = accept in
  let cancel_lb, f_cancel = cancel in

  let window = GWindow.dialog ~title ~modal:true () in

  let _ = window#connect#destroy ~callback: GMain.Main.quit in

  let main_box = window#vbox in
  let hbox_boutons = window#action_area in

  let vbox_saisie = GPack.vbox ~packing: (main_box#pack ~expand: true) () in
  
  ignore (GMisc.label ~text:message ~packing:(vbox_saisie#pack ~padding:3) ());

  vbox_saisie#pack widget ~expand ~padding: 3;
  let actions = [accept] @ actions @ [cancel] in
  let wb_ok :: _ = List.map (fun (name, cb) ->
    let b = GButton.button ~label: name
      ~packing: 
      (hbox_boutons#pack ~expand: true ~padding: 3) () in
    let _ = b # connect # clicked (fun () -> cb window) in
    b) actions  in

  wb_ok#grab_default ();

  (* the enter key is linked to the ok action *)
  (* the escape key is linked to the cancel action *)
  event#connect#key_press ~callback:
    begin fun ev -> 
      if GdkEvent.Key.keyval ev = GdkKeysyms._Return && bind_ok then f_ok window;
      if GdkEvent.Key.keyval ev = GdkKeysyms._Escape then f_cancel window;
      false
    end;

  widget#misc#grab_focus ();
  window#show ();
  GMain.Main.main ()

let login ~title ?ok ?cancel ?(text="") message =
  let vbox = GPack.vbox () in
  let we_chaine = GEdit.entry ~text ~packing:vbox#add () in
  if text <> "" then
    we_chaine#select_region 0 (we_chaine#text_length);
  input_widget_ex ~widget:vbox#coerce ~event:we_chaine#event
    ~get_text:(fun () -> we_chaine#text) ~bind_ok:true
    ~expand: false
    ~title
    ~actions:["Clipboard login",ldestroy] message

let create k =
  ignore (GMain.init ());
  login 
    ~title:"Paste your login details or click paste"
    ~ok:"Login"
    ~text:"danmey.org:1234" "INDIGO";;

create "ala";;
