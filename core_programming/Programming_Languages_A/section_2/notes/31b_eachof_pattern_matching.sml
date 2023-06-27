
val (name, good, hello) = (1, 2, 3)
(*
* val name = 1
* val good = 2
* val hello = 3
*
* All functions take only one argument, and pattern match to produce argument
* value bindings
*)

val {first=x, middle=y, last=z} = {first="First", middle="Second", last="Third"}
(*
* val x = "First"
* val y = "Second"
* val z = "Third"
*)

fun tuple_pattern_match (x, y, z) =
  x + y + z


val tuple = (1, 2, 3)

val test = tuple_pattern_match tuple = 6

(*
*
* Tuple pattern matches and produces relevant output
*
* *)

fun pr () = 10

val new = pr()
