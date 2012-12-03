(*----------------------------------------------------------------------------
  mainFrame.ml - Main window of INDIGO client.
  Copyright (C) 2011 Wojciech Meyer

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

open Lwt
open Lwt_chan
open Lwt_unix


module Listener = struct
  let listeners = ref []
  let add_listener f = listeners := f :: !listeners
  let receive x = List.iter (fun f -> f x) !listeners
end

module Connection = Connection.Make(Protocol)(Listener)


let motion_notify send area ev =
  let (x, y) =
    if GdkEvent.Motion.is_hint ev
	then area#misc#pointer
	else
      (int_of_float (GdkEvent.Motion.x ev), int_of_float (GdkEvent.Motion.y ev))
  in
  true

let drag_data_received context ~x ~y data ~info ~time =
    context # finish ~success:true ~del:true ~time

let drag_drop
    (area : GMisc.drawing_area)
    (src_widget : GTree.view)
    (context : GObj.drag_context) ~x ~y ~time =
  let a = src_widget#drag#get_data ~target:"INTEGER"  ~time context in
  true

module type Sig = sig
  val iddle : unit -> unit
end
let rec update_display window () =
  let module Window = (val window : Sig) in
  Window.iddle ();
  Lwt.bind (Lwt_unix.sleep 0.01) (update_display window)

let create () =
  lwt a =
    ignore (GMain.init ());

    Lwt_glib.install  ();

    let waiter, wakener = Lwt.wait () in
    let width = 200 in
    let height = 200 in
    let window = GWindow.window ~title:"Indigo" () in

    let _ = window#connect#destroy ~callback:(fun () -> wakeup wakener ()) in

  (* Create a basic tool layout *)
    let main_paned = GPack.paned `HORIZONTAL ~packing:window#add () in

    let tool_vbox = GPack.vbox  ~packing:main_paned#add () in

  (* Create the drawing area *)
    let module Canvas = (val Canvas.create ~pane:main_paned : Canvas.CANVAS) in
    let module Window =
          Window.Make
            (Cairo)
            (Canvas)
            (struct let size () = 500.,500. end) in

    let view = ObjectTree.create ~packing:tool_vbox#add () in

            (* let target_entry = { Gtk.target= "INTEGER"; Gtk.flags= []; Gtk.info=123 } in *)
            (* view#drag#source_set ~modi:[`BUTTON1] ~actions:[`COPY] [target_entry]; *)
            (* area#drag#dest_set ~flags:[`HIGHLIGHT;`MOTION] ~actions:[`COPY] [target_entry]; *)
            (* area#drag#connect#data_received ~callback:drag_data_received; *)
            (* area#drag#connect#drop ~callback:(drag_drop area view); *)
            (* let user_list, receive = UserList.create ~packing:tool_vbox#add ~canvas:area () in *)
    let rec login_loop kick login_data () =
    (match login_data with
      | None -> return ()
      | Some login_data ->
        match_lwt Connection.Client.connect ~kick login_data (fun () -> return (window # destroy ())) with
          | Connection.Client.BadPass ->
            return (Login.err "Bad password. Please try again!")
          | Connection.Client.BadUname ->
            return (Login.err "Bad password. Please try again!")
          | Connection.Client.UserAlreadyLoggedIn name ->
            begin match Login.confirm "User already logged in. Should I kick it out?" with
              | `OK -> login_loop true (Some login_data) ()
              | _ -> return () end
          | Connection.Client.Authorised (send, uname) ->
            let quit _ = send (Protocol.Server (Protocol.Quit uname)) in
            at_exit quit;
            Sys.catch_break true;
            (* Listener.add_listener receive; *)
            ignore(window#show ());

            update_display (module Window : Sig) ();
            send (Protocol.Server (Protocol.RequestUserList));
        (* Main loop: *)
            waiter);
    in
    let login_data = Login.create () in
    login_loop false login_data ()
  in
  return a
