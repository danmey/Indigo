type 'a t = 'a list list

let rec after item new_item zorder =
  match zorder with
  | after :: items :: rest when List.memq item after ->
    after :: (new_item :: items) :: rest
  | after :: items when List.memq item after ->
    after :: [new_item] :: items
  | after :: [] when not (List.memq item after) -> [new_item :: after]
  | items :: rest -> items :: after item new_item rest
  | [] -> [[new_item]]

let rec place item new_item zorder =
  match zorder with
  | items :: rest when List.memq item items ->
    (new_item :: items) :: rest
  | items :: rest -> items :: place item new_item rest
  | [] -> [[new_item]]

let rec before item new_item zorder =
  match zorder with
  | items :: before :: rest when List.memq item before ->
    (new_item :: items) :: before :: rest
  | before :: items when List.memq item before ->
    [new_item] :: before :: items
  | before :: [] when not (List.memq item before) -> [new_item :: before]
  | items :: rest -> items :: before item new_item rest
  | [] -> [[new_item]]

let compare first second zorder =
  let first,_ =  BatList.findi (fun _ lst -> List.memq first lst) zorder in
  let second,_ = BatList.findi (fun _ lst -> List.memq second lst) zorder in
  second - first

let remove item zorder = BatList.map (BatList.filter ((<>) item)) zorder

let push_before item before_item zorder =
  let zorder = remove before_item zorder in
  before item before_item zorder

let push_after item after_item zorder =
  let zorder = remove after_item zorder in
  after item after_item zorder

let find p zorder = List.find (List.find p) zorder

let empty = [[]]
