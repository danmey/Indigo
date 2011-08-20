(*----------------------------------------------------------------------------
canvas.ml - Generic canvas

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

module GtkBackend = struct
  type bitmap = GdkPixbuf.pixbuf
  type gc = GDraw.pixmap


  let resources = Hashtbl.create 137
  let draw_bitmap ~pos:(x,y) (gc:gc) (bitmap:bitmap) =
    gc#put_pixbuf ~x ~y bitmap; ()

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

module Canvas = Canvas.Make(GtkBackend)
module Tile = Tile.Make(GtkBackend)
module Protocol = Protocol.Make(Canvas)

(* Backing pixmap for drawing area *)
let backing = ref (GDraw.pixmap ~width:200 ~height:200 ())
let the_canvas = ref (Canvas.create ())
let canvas_mutex = Lwt_mutex.create ()
let canvas f =
  Lwt_mutex.with_lock canvas_mutex 
    (fun () ->
      the_canvas := f !the_canvas;
      return ())

let set_canvas c =
  Lwt.ignore_result (Lwt_mutex.with_lock canvas_mutex 
    (fun () ->
      return (ignore(the_canvas := c))))

module Connection = Connection.Make(Protocol)(struct 
  let receive = function
    | Protocol.Quit -> 
      print_endline "Quit"
    | Protocol.State c -> 
      set_canvas c end)
    
(* Create a new backing pixmap of the appropriate size *)
let configure window backing ev =
  let width = GdkEvent.Configure.width ev in
  let height = GdkEvent.Configure.height ev in
  let pixmap = GDraw.pixmap ~width ~height ~window () in
  pixmap#set_foreground `WHITE;
  pixmap#rectangle ~x:0 ~y:0 ~width ~height ~filled:true ();
  backing := pixmap;
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

(* let font = lazy (Gdk.Font.load "Courier New") *)

let button_pressed send area backing ev =
  if GdkEvent.Button.button ev = 1 then
    begin
      let x, y = (int_of_float (GdkEvent.Button.x ev)), (int_of_float (GdkEvent.Button.y ev)) in
      Lwt.ignore_result (canvas (fun c -> Canvas.button_pressed c ~x ~y))
    end;
  canvas (fun c -> send (Protocol.State c); c);
  true

let button_release send area backing ev =
  if GdkEvent.Button.button ev = 1 then
    begin
      let x, y = (int_of_float (GdkEvent.Button.x ev)), (int_of_float (GdkEvent.Button.y ev)) in
      Lwt.ignore_result (canvas (fun c -> Canvas.button_released c ~x ~y))
    end;
  canvas (fun c -> send (Protocol.State c); c);
  true

let motion_notify send area backing ev =
  let (x, y) =
    if GdkEvent.Motion.is_hint ev
	then area#misc#pointer
	else
      (int_of_float (GdkEvent.Motion.x ev), int_of_float (GdkEvent.Motion.y ev))
  in
  canvas (fun c -> 
    let c = Canvas.motion c ~x ~y in
    begin if Canvas.dragged c then
        Lwt.ignore_result (send (Protocol.State c))
    end;
    c);
  true


(* Create a scrolled text area that displays a "message" *)
let create_text packing =
  let scrolled_window = GBin.scrolled_window
    ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ~packing () in
  let view = GText.view ~packing:scrolled_window#add () in
    view, scrolled_window#coerce

let drag_data_received context ~x ~y data ~info ~time =
    context # finish ~success:true ~del:true ~time


let drag_drop (area:GMisc.drawing_area) (backing:GDraw.pixmap ref) (src_widget : GTree.view) (context : GObj.drag_context) ~x ~y ~time =
  let a = src_widget#drag#get_data ~target:"INTEGER"  ~time context in

      Lwt.ignore_result (canvas (fun c -> Canvas.add c (Tile.dice ~x ~y)));
      true

        
let rec update_display send (area:GMisc.drawing_area) () =
  Pervasives.flush Pervasives.stdout;
  let x,y = 0,0 in
  let rect = area # misc # allocation in
  let width, height = rect.Gtk.width, rect.Gtk.height in
  let update_rect = Gdk.Rectangle.create ~x ~y ~width ~height in

  !backing#set_foreground `WHITE;
  !backing#rectangle ~x:0 ~y:0 ~width ~height ~filled:true ();
  Lwt.ignore_result (canvas (fun c -> Canvas.draw c !backing; c));
  area#misc#draw (Some update_rect);
  Lwt.bind (Lwt_unix.sleep 0.01) (update_display send area)

lwt () =
    ignore (GMain.init ());

    Lwt_glib.install  (); 
    
    let waiter, wakener = Lwt.wait () in
    
    let port = (int_of_string (Sys.argv.(1))) in
    
    let width = 200 in
    let height = 200 in
  
    let () = Login.create () in
    let window = GWindow.window ~title:"Indigo" () in

  let _ = window#connect#destroy ~callback:(fun () -> wakeup wakener ()) in

  (* Create a basic tool layout *)
  let main_paned = GPack.paned `HORIZONTAL ~packing:window#add () in

  let tool_vbox = GPack.vbox  ~packing:main_paned#add () in

  (* Create the drawing area *)
  let area = GMisc.drawing_area ~width ~height ~packing:main_paned#add () in

  lwt send = Connection.Client.connect ~port ~host:(Sys.argv.(2)) in
  ignore(area#event#connect#expose ~callback:(expose area backing));
  ignore(area#event#connect#configure ~callback:(configure window backing));
    
    (* Event signals *)
  ignore(area#event#connect#motion_notify ~callback:(motion_notify send area backing));
  ignore(area#event#connect#button_press ~callback:(button_pressed send area backing));
  ignore(area#event#connect#button_release ~callback:(button_release send area backing));
  ignore(area#event#connect#motion_notify ~callback:(motion_notify send area backing));
    
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
  (* area#drag#connect#leave ~callback:(fun context ~time -> print_endline "Leave!"; flush stdout); *)
  (* area#drag#connect#motion ~callback:(fun context ~x ~y ~time -> print_endline "Motion!"; flush stdout; false); *)
  area#drag#connect#drop ~callback:(drag_drop area backing view);
  (* view#drag#connect#data_get  ~callback:drag_data_get; *)
  (* view#drag#connect#data_delete  ~callback:(fun context -> print_endline "Data Delete!"; flush stdout); *)
  (* view#drag#connect#beginning  ~callback:(fun context -> print_endline "Data Begining!"; flush stdout); *)
  (* view#drag#connect#ending  ~callback:(fun context -> print_endline "Data End!"; flush stdout); *)

        (* .. And a quit button *)
  let quit_button = GButton.button ~label:"Quit" ~packing:tool_vbox#add () in
    (* let _ = GMain.Idle.add (idle send area) in *)
  (* ignore(quit_button#connect#clicked  *)
  (*          ~callback:(fun () ->  *)
  (*            (ignore(canvas  *)
  (*                      (fun c -> *)
  (*                        send (Protocol.State c); *)
  (*                        let ch = open_in_bin "test.bin" in *)
  (*                        let c = input_value ch in *)
  (*                        c))))); *)

    ignore(window#show ());

    update_display send area ();
(* Main loop: *)
    waiter

 
