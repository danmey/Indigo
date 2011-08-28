(*----------------------------------------------------------------------------
  objectTree.ml - Selection of game objects.
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

open Gobject.Data

let cols = new GTree.column_list
let col_first_name = cols#add string
let col_last_name = cols#add string
let col_year_born = cols#add uint

let create_and_fill_model data =
  let treestore = GTree.tree_store cols in
  let append (first_name, last_name) =
    let toplevel = treestore#append () in
    treestore#set ~row:toplevel ~column:col_first_name first_name;
    treestore#set ~row:toplevel ~column:col_last_name last_name;
  in
  List.iter append [
    "Dice", "D6";
    "Board", "Board"
  ];
  treestore

let selected (view:GTree.view) =
  match view # selection # get_selected_rows with
    | row :: _ ->
      let el = view # model # get ~row:(view # model # get_iter row) ~column:col_first_name in
      Some (match el with
        | "Dice" -> `Dice
        | "Board" -> `Board)
    | _ -> None
let create ~packing ~(canvas:GMisc.drawing_area) () =
  let view = GTree.view ~packing () in

  (* Column #1 *)
  (* pack cell renderer into tree view column *)
  (* connect 'text' property of the cell renderer to
   * model column that contains the first name *)
  let col = GTree.view_column ~title:"Kind"
      ~renderer:(GTree.cell_renderer_text [], ["text", col_first_name]) () in
  (* pack tree view column into tree view *)
  view#append_column col;

  (* Column #2 *)
  (* create cell_renderer and set 'weight' property of it to
   * bold print (we want all last name in bold) *)
  let cell_renderer = GTree.cell_renderer_text [`WEIGHT `BOLD] in
  (* pack cell renderer into tree view column *)
  (* connect 'text' property of the cell renderer to
   * model column that contains the last name *)
  let col = GTree.view_column ~title:"Type"
      ~renderer:(cell_renderer, ["text", col_last_name]) () in
  (* pack tree view column into tree view *)
  view#append_column col;

  let renderer = GTree.cell_renderer_text [] in
  (* pack cell renderer into tree view column *)
  (* let col = GTree.view_column ~title:"Age" *)
  (*     ~renderer:(renderer, []) () in *)
  (* connect a cell data function *)
  (* col#set_cell_data_func renderer (age_cell_data_func renderer); *)
  (* pack tree view column into tree view *)
  view#append_column col;

  let model = create_and_fill_model () in
  view#set_model (Some (model#coerce));

  (* view#selection#set_mode `NONE; *)
  view

