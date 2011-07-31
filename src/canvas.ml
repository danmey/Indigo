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


(* Backing pixmap for drawing area *)
let backing = ref (GDraw.pixmap ~width:200 ~height:200 ())

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

let button_pressed area backing ev =
  if GdkEvent.Button.button ev = 1 then (
    let x = int_of_float (GdkEvent.Button.x ev) in
    let y = int_of_float (GdkEvent.Button.y ev) in
    draw_brush area backing x y
  );
  true

let motion_notify area backing ev =
  let (x, y) =
    if GdkEvent.Motion.is_hint ev
	then area#misc#pointer
	else
      (int_of_float (GdkEvent.Motion.x ev), int_of_float (GdkEvent.Motion.y ev))
  in
  let state = GdkEvent.Motion.state ev in
  if Gdk.Convert.test_modifier `BUTTON1 state
  then draw_brush area backing x y;
  true

(* Create a scrolled text area that displays a "message" *)
let create_text packing =
  let scrolled_window = GBin.scrolled_window
    ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ~packing () in
  let view = GText.view ~packing:scrolled_window#add () in
    view, scrolled_window#coerce

let main () =

  let width = 200 in
  let height = 200 in

  let window = GWindow.window ~title:"Scribble" () in
  ignore(window#connect#destroy ~callback:GMain.Main.quit);

  (* Create a basic tool layout *)
  let main_paned = GPack.paned `HORIZONTAL ~packing:window#add () in

  let tool_vbox = GPack.vbox  ~packing:main_paned#add () in

  (* Create the drawing area *)
  let area = GMisc.drawing_area ~width ~height ~packing:main_paned#add () in

  ignore(area#event#connect#expose ~callback:(expose area backing));
  ignore(area#event#connect#configure ~callback:(configure window backing));

  (* Event signals *)
  ignore(area#event#connect#motion_notify ~callback:(motion_notify area backing));
  ignore(area#event#connect#button_press ~callback:(button_pressed area backing));

  area#event#add [`EXPOSURE; `LEAVE_NOTIFY; `BUTTON_PRESS; `POINTER_MOTION; `POINTER_MOTION_HINT];
  
  ignore(ObjectTree.create ~packing:tool_vbox#add ());

  (* .. And a quit button *)
  let quit_button = GButton.button ~label:"Quit" ~packing:tool_vbox#add () in
  ignore(quit_button#connect#clicked ~callback:window#destroy);

  window#show ();

  GMain.Main.main ()

let _ = Printexc.print main ()
