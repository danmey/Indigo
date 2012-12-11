(*----------------------------------------------------------------------------
  pos.ml - 2D representation of position
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

module Make(T :

  sig
    type t
    val ( - ) : t -> t -> t
    val ( + ) : t -> t -> t

  end) = struct

  open T

  type t = T.t * T.t
  let fold2 f (x1,y1) (x2,y2) = (f x1 x2, f y1 y2)
  let sub = fold2 (-)
  let add = fold2 (+)
  let abs (x,y) = abs x, abs y
  let x = fst
  let y = snd
  let set_x p ax = (ax, y p)
  let set_y p ay = (x p, ay)
  let add_x p p' = (x p + x p', y p)
  let add_y p p' = (x p,y p + y p')
  let to_string (x,y) = Printf.sprintf "(%f, %f)" x y
  let clip (x,y,w,h) (px,py) =
    (if px >= x then (if px < x+w then px else x+w) else x),
    (if py >= y then (if py < y+h then py else y+h) else y)

end

module Int = Make(BatInt)
module Float = Make(BatFloat)
