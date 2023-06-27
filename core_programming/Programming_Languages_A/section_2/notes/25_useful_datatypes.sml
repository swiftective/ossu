(* Programming Languages, Dan Grossman *)
(* Section 2: Useful Datatypes *)

datatype suit = Club | Diamond | Heart | Spade

datatype rank = Jack | Queen | King | Ace | Num of int

datatype bst = Val of int | Node of int * bst * bst

val ex_bst = Node (2, Node (1, Val 1, Val 1), Val 1)

fun sum_bst node =
  case node of
       Val i => i
     | Node (i, bst1, bst2) => i + sum_bst bst1 + sum_bst bst2

val test_bst = sum_bst ex_bst = 6


datatype id = StudentNum of int
            | Name of string * (string option) * string

datatype exp = Constant of int
             | Negate of exp
             | Add of exp * exp
             | Multiply of exp * exp

fun eval e =
    case e of
        Constant i => i
      | Negate e2  => ~ (eval e2)
      | Add(e1,e2) => (eval e1) + (eval e2)
      | Multiply(e1,e2) => (eval e1) * (eval e2)

fun number_of_adds e =
    case e of
        Constant i      => 0
      | Negate e2       => number_of_adds e2
      | Add(e1,e2)      => 1 + number_of_adds e1 + number_of_adds e2
      | Multiply(e1,e2) => number_of_adds e1 + number_of_adds e2

val example_exp = Add (Constant 19, Negate (Constant 4))

val example_ans = eval example_exp

val example_addcount = number_of_adds (Multiply(example_exp,example_exp))
