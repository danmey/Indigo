type position = int * int
type key_code = int
type 'a t =
    MouseDown of 'a * position
  | MouseUp of 'a * position
  | MouseMove of 'a * position
  | KeyDown of 'a * position * key_code
  | KeyUp of 'a * position * key_code

type prim = unit t

type window = unit t

let position = function
| MouseDown (_, position)
| MouseUp (_, position)
| MouseMove (_, position)
| KeyDown (_, position, _)
| KeyUp (_, position, _) -> position
