(* Programming Languages, Dan Grossman *)
(* Section 1: List Functions *)

(* Functions taking or producing lists *)

fun sum_list (xs : int list) =
    if null xs
    then 0
    else hd(xs) + sum_list(tl(xs))

fun countdown (x : int) =
    if x=0
    then []
    else x :: countdown(x-1)

fun append (xs : int list, ys : int list) = (* part of the course logo :) *)
    if null xs
    then ys
    else hd(xs) :: append(tl(xs), ys)

(* More functions over lists, here lists of pairs of ints *)

fun sum_pair_list (xs : (int * int) list) =
    if null xs
    then 0
    else #1 (hd(xs)) + #2 (hd(xs)) + sum_pair_list(tl(xs))

fun firsts (xs : (int * int) list) =
    if null xs
    then []
    else (#1 (hd xs))::(firsts(tl xs))

fun seconds (xs : (int * int) list) =
    if null xs
    then []
    else (#2 (hd xs))::(seconds(tl xs))

fun sum_pair_list2 (xs : (int * int) list) =
    (sum_list (firsts xs)) + (sum_list (seconds xs))

(* functions I wrote *)
fun sum_list (xs : int list) =
  if null xs
  then 0
  else hd xs + sum_list(tl xs)

val test1 = (sum_list [1, 2, 3, 4, 5] = 15)

fun product_list  (xs : int list) =
  if null xs
  then 1
  else hd xs * product_list (tl xs)

val test2 = (product_list [1, 2, 3, 4, 5] = 120)

fun append_list (xs : int list, ys : int list) =
  if null xs
  then ys
  else hd xs::append_list(tl xs, ys)

val test3 = append_list([1, 2], [3, 4]) = [1, 2, 3, 4]

fun sum_pair_list (xs : (int * int) list) =
  if null xs
  then 0
  else #1 (hd xs) + #2 (hd xs) + sum_pair_list(tl xs)


val test4 = sum_pair_list[(1, 2), (3, 4), (5, 6)] = 21

fun firsts (xs : (int * int) list) =
  if null xs
  then []
  else #1 (hd xs) :: firsts (tl xs)


val test5 = firsts [(1, 2), (3, 4), (5, 6)] = [1, 3, 5]

fun seconds (xs : (int * int) list) =
  if null xs
  then []
  else #2 (hd xs) :: seconds (tl xs)

val test6 = seconds [(1, 2), (3, 4), (5, 6)] = [2, 4, 6]

fun sum_pair_list2 (xs : (int * int) list) =
  sum_list(firsts xs ) + sum_list(seconds xs)

val test7 = sum_pair_list2[(1, 2), (3, 4), (5, 6)] = 21


fun countdown (n : int) =
  if n = 0
  then []
  else n :: countdown (n - 1)


val test8 = countdown 5 = [5, 4, 3, 2, 1]

fun factorial (n : int) = product_list(countdown n)

val test9 = factorial 5 = 120

fun new (s : string) =
  let
    fun new (s : string, n : int) =
      if n = 0
      then s
      else ""
  in
    new (s, 0)
  end

val test10 = new "think" = "think"
