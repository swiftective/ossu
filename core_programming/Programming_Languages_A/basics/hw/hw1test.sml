(* Homework1 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

use "hw1.sml";

val test1 = is_older ((1,2,3),(2,3,4)) = true
val test2 = is_older ((2,3,4), (1,2,3)) = false
val test3 = number_in_month ([(2012,2,28),(2013,12,1)],2) = 1
val test4 = number_in_month ([(2012,2,28),(2013,2,1)],2) = 2
val test5 = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = 3
val test6 = dates_in_month ([(2012,2,28),(2013,12,1)],2) = [(2012,2,28)]
val test7 = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)]
val test8 = get_nth (["hi", "there", "how", "are", "you"], 2) = "there"
val test9 = date_to_string (2013, 6, 1) = "June 1, 2013"
val test10 = number_before_reaching_sum (10, [1,2,3,4,5]) = 3
val test11 = number_before_reaching_sum (10, [8,2,3,4,5]) = 1
val test12 = what_month 70 = 3
val test13 = what_month 10 = 1
val test14 = what_month 100 = 4
val test15 = month_range (31, 34) = [1,2,2,2]
val test16 = oldest([(2012,2,28),(2011,3,31),(2011,4,28)]) = SOME (2011,3,31)
val test17 = oldest([(2012,2,28),(2001,3,31),(2011,4,28)]) = SOME (2001,3,31)
