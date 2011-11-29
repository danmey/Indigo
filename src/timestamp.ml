type t = float
let start = Unix.gettimeofday ()
let get () = Unix.gettimeofday () -. start
let since t1 t2 = t1 -. t2

