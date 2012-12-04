type position = int * int
type key_code = int
type 'a t =
    MouseDown of 'a * position
  | MouseUp of 'a * position
  | KeyDown of 'a * position * key_code
  | KeyUp of 'a * position * key_code

type prim = unit t
