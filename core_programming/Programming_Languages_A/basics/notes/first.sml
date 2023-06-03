(* This is a comment. This is our first program. *)

val x = 34
(* static env: x : int *)
(* dynamic env: x ---> 34 *)

val y = 17
(* static env: x : int y : int *)
(* dynamic env: x ---> 34, y ---> 17 *)

val z = (x + y) + (y + 2)
(* static env: x : int y : int z : int *)
(* dynamic env: x ---> 34, y ---> 17, z ---> 70, w -> 71 *)

val abs_of_z = if z < 0 then 0 - z else z (* bool *) (* int *)
(* static env: int *)
(* dynamic env: .... abs_of_z ---> 70 *)

val abs_of_z_simpler = abs z
