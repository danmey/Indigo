(*----------------------------------------------------------------------------
  rect.ml - 2D representation of rectangle
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
    val ( * ) : t -> t -> t
    val ( / ) : t -> t -> t
    val zero : t
    val two : t
  end) = struct

  module Pos = Pos.Make(T)

  open T

  type t = {mutable x:T.t; mutable y:T.t; mutable w:T.t; mutable h:T.t}
  let rect (x,y) (w,h) = {x=x; y=y; w=w; h=h; }
  let size r = (r.w,r.h)
  let abs_dim r = (r.x, r.y),(r.x+r.w,r.y+r.h)
  let pos r = (r.x, r.y)
  let subr r1 r2 =
    let p1 = pos r1 in
    let p2 = pos r2 in
    rect (Pos.sub p2 p1) (size r1)
  let set_pos r (x,y) = r.x <- x; r.y <- y
  let is_in r (x,y) = x >= r.x && x < r.x + r.w && y >= r.y && y < r.y + r.h
  let together r1 r2 =
    let x, y = max r1.x r2.x, max r1.y r2.y in
    let x',y' = min (r1.x + r1.w) (r2.x + r2.w), min (r1.y + r1.h) (r2.y + r2.h) in
    rect (x,y) (x'-x, y'-y)
  let pos_in r (x,y) = (x-r.x,y-r.y)
  let fold2 f r (x,y) = {r with x=f r.x x; y=f r.y y}
  let sub = fold2 (-)
  let by = fold2 (+)
  let by_h r (x,_) = { r with x=r.x+x}
  let by_v r (_,y) = { r with y=r.y+y}
  let o = rect (zero,zero) (zero,zero)
  let place_in r1 r2 = let p = pos r2 in by r1 p
  let border n r = rect (r.x + n / two, r.y + n / two) (r.w - n, r.h - n)
  let lift4 f { x; y; w; h; } = f x, f y, f w, f h
  let lift22 f g  { x; y; w; h; } = f x, f y, g w,g h
  let coords { x; y; w; h } = x,y,w,h
  let coordsf { x; y; w; h } = x, y, w, h
  (* let to_string { x; y; w; h; } = Printf.sprintf "{ x=%s; y=%s; w=%s; h=%s }" x y w h *)
  let shrink { x; y; w; h; } pixels =
    { x= x + pixels;
      y = y + pixels;
      w = w - two * pixels;
      h = h - two * pixels }
  let scale s ({w; h} as r) =
    { r with
      w = w * s;
      h = h * s; }

  let clip_point {x;y;w;h} (px,py) =
    (if px >= x then (if px < x+w then px else x+w) else x),
    (if py >= y then (if py < y+h then py else y+h) else y)

  let clip_rect clip r =
    let px,py,w,h = coords r in
    let clip = clip_point clip in
    let x,y = clip (px, py) in
    let w,h = clip ((px + w), (py + h)) in
    rect (x,y) (w-px,h-py)

end

module Int = Make(struct include BatInt let two = 2 end)
module Float = Make(struct include BatFloat let two = 2. end)
