(* Programming Languages, Dan Grossman *)
(* Section 2: Exceptions *)

fun hd xs =
    case xs of
        []   => raise List.Empty
      | x::_ => x

exception MyUndesirableCondition

exception MyOtherException of int * int

fun mydiv (x,y) =
    if y=0
    then raise MyUndesirableCondition
    else x div y

fun maxlist (xs,ex) = (* int list * exn -> int *)
    case xs of
        [] => raise ex
      | x::[] => x
      | x::xs' => Int.max(x,maxlist(xs',ex))

val w = maxlist ([3,4,5],MyUndesirableCondition) (* 5 *)

val x = maxlist ([3,4,5],MyUndesirableCondition) (* 5 *)
	handle MyUndesirableCondition => 42

(*val y = maxlist ([],MyUndesirableCondition)*)

val z = maxlist ([],MyUndesirableCondition) (* 42 *)
	handle MyUndesirableCondition => 42


exception MyNewException
exception MyNewException2 of int

fun throw_exception n =
  if n = 0
  then raise MyNewException
  else if n = 1
  then raise MyNewException2 n
  else n * n


val new = (throw_exception 0
            handle MyNewException => 10)
            handle MyNewException2 n => (n + 4) * 2

val new1 = (throw_exception 1
            handle MyNewException => 10)
            handle MyNewException2 n => (n + 4) * 2

(*
* this code below throws a exception and halts the program
* val new2 = throw_exception 1
*)

(* handling exceptions with a function *)
fun handle_exception ex =
  case ex of
       MyNewException  => 10
     | MyNewException2 n => (n + 4)
     | ex => raise ex

val new3 = throw_exception 1
            handle ex => handle_exception ex
