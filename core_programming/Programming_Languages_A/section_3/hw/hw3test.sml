(* Homework3 Simple Test*)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

use "./hw3.sml";

val test1 = only_capitals ["A","B","C"] = ["A","B","C"]

val test2 = longest_string1 ["A","bc","C"] = "bc"
val test2a = longest_string1 ["Aaa","bcc","C"] = "Aaa"

val test3 = longest_string2 ["A","bc","C"] = "bc"
val test3a = longest_string2 ["Aaa","bcc","C"] = "bcc"

val test4a = longest_string3 ["A","bc","C"] = "bc"
val test4b = longest_string4 ["A","B","C"] = "C"

val test5 = longest_capitalized ["A","bc","C"] = "A"

val test6 = rev_string "abc" = "cba"

val test7 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4

val test8 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE
val test8a = all_answers (fn x => if x = 1 then SOME [x] else NONE) [] = SOME []

val test9a = count_wildcards Wildcard = 1

val test9ab =
  count_wildcards
    (TupleP
    [Wildcard, Wildcard, TupleP [Wildcard, UnitP], ConstP 1,ConstructorP ("var", Wildcard) ])
  = 4

val test9b = count_wild_and_variable_lengths (Variable("a")) = 1
val test9ba =
  count_wild_and_variable_lengths (TupleP
    [Wildcard, Wildcard, TupleP [Wildcard, UnitP], ConstP 1,ConstructorP ("var1",
    Wildcard), Variable("var2") ])
  = 8


val test9c = count_some_var ("x", Variable("x")) = 1

val test9ca =
  count_some_var
  ("x", TupleP
  [Wildcard, Wildcard, TupleP [Wildcard, UnitP], ConstP 1,ConstructorP
  ("var1", Wildcard), Variable("var2")])
  = 0

val test9cb =
  count_some_var
  ("good", TupleP
  [Wildcard, Wildcard, TupleP [Wildcard, UnitP], ConstP 1,ConstructorP
  ("good", Wildcard), Variable("good")])
  = 1

val test10 = check_pat (Variable("x")) = true
val test10a =
  check_pat
  (TupleP
  [Wildcard, Wildcard, Variable ("good"), TupleP [Wildcard, UnitP], ConstP 1,ConstructorP
  ("good", Wildcard), Variable("good")])
  = false

val test11 = match (Const(1), UnitP) = NONE
val test11a = match (Const (1), Variable "good") = SOME [("good", Const 1)]
val test11b = match (Unit, UnitP) = SOME []

val test12 =
  first_match Unit [UnitP]
  = SOME []

val test12a = first_match Unit [] = NONE
