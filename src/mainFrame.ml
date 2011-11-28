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

let font = lazy (Gdk.Font.load "-adobe-helvetica-bold-r-normal--12-120-75-75-p-70-iso8859-1")

let main_window_size = ref None
module GtkBackend = struct
  type bitmap = GdkPixbuf.pixbuf
  type gc = GDraw.pixmap
      
  let gc = ref (let px = GDraw.pixmap ~width:200 ~height:200 () in
                begin
                  px#set_foreground `BLACK ;
                  let width, height = px#size in
                  px#rectangle ~x:0 ~y:0 ~width ~height ~filled:true ()
                end;
                px)


  let resources = Hashtbl.create 137
  module Draw = struct
    let with_canvas f =
      match !gc with
        | canvas -> f canvas


    (* let bitmap ~pos:(x,y) (bitmap:bitmap) = *)
    (*   with_canvas (fun (gc : gc) ->  gc#put_pixbuf ~x ~y bitmap) *)
    let bitmap ~pos:(x,y) (bitmap:bitmap) = ()
    
    let text ~pos:(x,y) text =
      with_canvas (fun gc -> 
        
        (* let col = `RGB (0, 0, 0) in *)
        (* gc # set_foreground col; *)
        (* gc # string ~x ~y ~font:(Lazy.force font) text; *)
        ())
        
    let draw_curved_rectangle cr ~x ~y ~width ~height =
	Cairo.save cr;
	Cairo.move_to cr ~x ~y:(y+.height /. 2.);
	Cairo.curve_to cr ~x1:x ~y1:y ~x2:x ~y2:y ~x3:(x +. width /. 2.) ~y3:y;
	Cairo.curve_to cr ~x1:(x +. width) ~y1:y ~x2:(x +. width) ~y2:y ~x3:(x +. width) ~y3:(y +. height /. 2.);
	Cairo.curve_to cr ~x1:(x+. width) ~y1:(y+. height) ~x2:(x +. width) ~y2:(y +. height) ~x3:(x +. width /. 2.) ~y3:(y +. height);
	Cairo.curve_to cr ~x1:x ~y1:(y +. height) ~x2:x ~y2:(y +. height) ~x3:x ~y3:(y +. height /. 2.);
	Cairo.restore cr
 

    let rectangle ~pos:(x,y) ~size:(width,height) = 
      let x,y,width, height = float x, float y, float width, float height in
      with_canvas (fun gc ->
        let cr = Cairo_lablgtk.create (gc)#pixmap in
        let pat = (Cairo.Pattern.create_linear ~x0:x ~y0:y ~x1:(x+.width) ~y1:(y+.height)) in
        Cairo.Pattern.add_color_stop_rgb pat ~off:0.0 ~red:0.6 ~green:0.6 ~blue:0.7;
        Cairo.Pattern.add_color_stop_rgb pat ~off:1.0 ~red:0.6 ~green:0.6 ~blue:1.0;
        Cairo.set_source cr pat;
        draw_curved_rectangle cr ~x ~y ~width ~height;        (* Cairo.rectangle cr ~x ~y ~width ~height; *)
        Cairo.fill cr;
      )

        
    let background (r,g,b) =
      let red,green,blue = float r, float g, float b in
      with_canvas (fun gc -> 
        let cr = Cairo_lablgtk.create (gc)#pixmap in
        Cairo.set_source_rgb ~red ~green ~blue;
      (* let col = `RGB (r, g, b) in *)
      (* gc # set_background col *)
        ()
      )
    let foreground (r,g,b) =
      with_canvas (fun gc -> 
      (* let col = `RGB (r, g, b) in *)
      (* gc # set_foreground col *)
        ()
      )
  end
  let bitmap_of_file ~fn = GdkPixbuf.from_file fn

  let size_of_bitmap bitmap =
    GdkPixbuf.get_width bitmap, GdkPixbuf.get_height bitmap

  let load_bitmap fn =
    let bmp = bitmap_of_file ~fn in 
    Hashtbl.add resources fn bmp; 
    bmp

  let bitmap name = 
    try 
      Hashtbl.find resources name 
    with Not_found ->
      print_endline "not loaded";
      let bmp = load_bitmap name in 
      bmp

end

module Window = Window.Make(GtkBackend)
  (struct let size () = 
            match !main_window_size with
              | Some size -> size
              | None -> 0,0 end)

module Listener = struct
  let listeners = ref []
  let add_listener f = listeners := f :: !listeners
  let receive x = List.iter (fun f -> f x) !listeners
end

module Connection = Connection.Make(Protocol)(Listener)
    
(* Create a new backing pixmap of the appropriate size *)
let configure window backing ev =
  let width = GdkEvent.Configure.width ev in
  let height = GdkEvent.Configure.height ev in
  let pixmap = GDraw.pixmap ~width ~height ~window () in
  pixmap#set_foreground `BLACK;
  pixmap#rectangle ~x:0 ~y:0 ~width ~height ~filled:true ();
  backing := pixmap;
  main_window_size := (Some (width, height));
  true

(* Redraw the screen from the backing pixmap *)
let expose (drawing_area:GMisc.drawing_area) (backing:GDraw.pixmap ref) ev =
  let area = GdkEvent.Expose.area ev in
  let x = Gdk.Rectangle.x area in
  let y = Gdk.Rectangle.y area in
  let width = Gdk.Rectangle.width area in
  let height = Gdk.Rectangle.height area in
  let drawing =
    drawing_area#misc#realize ();
    new GDraw.drawable (drawing_area#misc#window)
  in
  drawing#put_pixmap ~x ~y ~xsrc:x ~ysrc:y ~width ~height !backing#pixmap;
  main_window_size := (Some (width, height));
  false

(* Draw a rectangle on the screen *)
let draw_brush (area:GMisc.drawing_area) (backing:GDraw.pixmap ref) x y =
  let x = x - 5 in
  let y = y - 5 in
  let width = 10 in
  let height = 10 in
  let update_rect = Gdk.Rectangle.create ~x ~y ~width ~height in
  !backing#set_foreground `BLACK;
  !backing#rectangle ~x ~y ~width ~height ~filled:true ();
  area#misc#draw (Some update_rect)


let button_pressed send area backing ev =
  if GdkEvent.Button.button ev = 1 then
    begin
      let x, y = (int_of_float (GdkEvent.Button.x ev)), (int_of_float (GdkEvent.Button.y ev)) in
      ()
      (* Lwt.ignore_result (table (fun c -> Table.button_pressed c ~x ~y)) *)
    end;
  let x, y = (int_of_float (GdkEvent.Button.x ev)), (int_of_float (GdkEvent.Button.y ev)) in
  Window.button_pressed (x,y);
  (* table (fun c -> send (Protocol.Client (Protocol.State c)); c); *)
  true

let button_release send area backing ev =
  if GdkEvent.Button.button ev = 1 then
    begin
      let x, y = (int_of_float (GdkEvent.Button.x ev)), (int_of_float (GdkEvent.Button.y ev)) in
      ()
    end;
  true

let motion_notify send area backing ev =
  let (x, y) =
    if GdkEvent.Motion.is_hint ev
	then area#misc#pointer
	else
      (int_of_float (GdkEvent.Motion.x ev), int_of_float (GdkEvent.Motion.y ev))
  in
  true


(* Create a scrolled text area that displays a "message" *)
let create_text packing =
  let scrolled_window = GBin.scrolled_window
    ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ~packing () in
  let view = GText.view ~packing:scrolled_window#add () in
    view, scrolled_window#coerce


let drag_data_received context ~x ~y data ~info ~time =
    context # finish ~success:true ~del:true ~time


let make_symgen () =
  let i = ref (-1) in
  (fun () -> incr(i); Printf.sprintf "dice<%d>" !i)
let gensym = make_symgen ()



let drag_drop 
    (area : GMisc.drawing_area) 
    (backing : GDraw.pixmap ref) 
    (src_widget : GTree.view) 
    (context : GObj.drag_context) ~x ~y ~time =
  let a = src_widget#drag#get_data ~target:"INTEGER"  ~time context in
  true
        

let rec update_display send (area:GMisc.drawing_area) () =
  Pervasives.flush Pervasives.stdout;
  let x,y = 0,0 in
  let rect = area # misc # allocation in
  let width, height = rect.Gtk.width, rect.Gtk.height in
  let update_rect = Gdk.Rectangle.create ~x ~y ~width ~height in

  !GtkBackend.gc#set_foreground `WHITE;
  !GtkBackend.gc#rectangle ~x:0 ~y:0 ~width ~height ~filled:true ();
  area#misc#draw (Some update_rect);
  main_window_size := (Some (width, height));
  Window.iddle ();
  Lwt.bind (Lwt_unix.sleep 0.01) (update_display send area)

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
    let area = GMisc.drawing_area ~width ~height ~packing:main_paned#add () in
    main_window_size := (Some (width, height));

            ignore(area#event#connect#expose ~callback:(expose area GtkBackend.gc));
            ignore(area#event#connect#configure ~callback:(configure window GtkBackend.gc));
            
        (* Event signals *)
            ignore(area#event#connect#motion_notify ~callback:(motion_notify send area GtkBackend.gc));
            ignore(area#event#connect#button_press ~callback:(button_pressed send area GtkBackend.gc));
            ignore(area#event#connect#button_release ~callback:(button_release send area GtkBackend.gc));
            ignore(area#event#connect#motion_notify ~callback:(motion_notify send area GtkBackend.gc));
            
            area#event#add [`EXPOSURE; 
                            `LEAVE_NOTIFY; 
                            `BUTTON_PRESS; 
                            `BUTTON_RELEASE; 
                            `POINTER_MOTION; 
                            `POINTER_MOTION_HINT];
            
            let view = ObjectTree.create ~packing:tool_vbox#add ~canvas:area () in
            
            let target_entry = { Gtk.target= "INTEGER"; Gtk.flags= []; Gtk.info=123 } in
            view#drag#source_set ~modi:[`BUTTON1] ~actions:[`COPY] [target_entry];
            area#drag#dest_set ~flags:[`HIGHLIGHT;`MOTION] ~actions:[`COPY] [target_entry];
            area#drag#connect#data_received ~callback:drag_data_received;
            area#drag#connect#drop ~callback:(drag_drop area GtkBackend.gc view);
            let user_list, receive = UserList.create ~packing:tool_vbox#add ~canvas:area () in

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
            Listener.add_listener receive;
            ignore(window#show ());
        
            update_display send area ();
            send (Protocol.Server (Protocol.RequestUserList));
        (* Main loop: *)
            waiter)
    in
    let login_data = Login.create () in
    login_loop false login_data ()
  in
  return a

 
