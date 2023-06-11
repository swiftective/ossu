(* Programming Languages, Dan Grossman *)
(* Section 2: Case Expressions *)

datatype mytype = TwoInts of int * int
                | Str of string
                | Pizza

fun f x =
  case x of
    Pizza => 3
    | Str s => 8
    | TwoInts(i1,i2) => i1 + i2



val x = TwoInts (1, 2)
val y = Pizza
val z = Str "Bye"
val test1 = f x
val test2 = f y
val test3 = f z

(*    | Pizza => 4; (* redundant case: error *) *)
(*fun g x = case x of Pizza => 3 (* missing cases: warning *) *)

