(* This is an example implementation *)

datatype exp = Constant of int
             | Negate of exp
             | Add of exp * exp
             | Multiply of exp * exp

fun max_constant e =
  case e of
     Constant i => i
     | Negate exp => max_constant exp
     | Add (exp1, exp2) => Int.max(max_constant exp1, max_constant exp2)
     | Multiply (exp1, exp2) => Int.max(max_constant exp1, max_constant exp2)

val test_exp = Add (Constant 100, Negate (Constant 20))
val test = max_constant test_exp = 100 (* The test worked *)
