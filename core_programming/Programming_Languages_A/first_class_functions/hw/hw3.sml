(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let
	val r = g f1 f2
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

val only_capitals = List.filter (fn s => Char.isUpper(String.sub(s, 0)))

val longest_string1 = List.foldl
                      (fn (curr, prev) => if String.size curr > String.size prev
                                          then curr
                                          else prev)
                      ""

val longest_string2 = List.foldl
                      (fn (curr, prev) => if String.size curr >= String.size prev
                                          then curr
                                          else prev)
                      ""

fun longest_string_helper f = List.foldl
                              (fn (curr, prev) => if f (String.size curr, String.size prev)
                                                  then curr
                                                  else prev)
                              ""

val longest_string3 = longest_string_helper (fn (a, b) => a > b)

val longest_string4 = longest_string_helper (fn (a, b) => a >= b)

val longest_capitalized = longest_string3 o only_capitals

val rev_string = String.implode o List.rev o String.explode

fun first_answer f xs =
  case xs of
       [] => raise NoAnswer
     | x::xs' => case f x of
                    NONE => first_answer f xs' | SOME l => l

fun all_answers f xs =
  let fun all_answers (xs, acc) =
        case xs of
             [] => SOME acc
           | x::xs' => case f x of
                            NONE => NONE
                          | SOME l => all_answers (xs', acc @ l)
  in all_answers (xs, []) end

val count_wildcards = g (fn _ => 1) (fn _ => 0)

val count_wild_and_variable_lengths = g (fn _ => 1) (fn s => String.size s)

fun count_some_var (s1, p) =
  g (fn _ => 0) (fn s2 => if s2 = s1 then 1 else 0) p


fun check_pat p =
  let fun get_all_vars p =
        case p of
             Wildcard          => []
           | Variable x        => [x]
           | TupleP ps         => List.foldl (fn (c,p) => p @ get_all_vars c) [] ps
           | ConstructorP(_,p) => get_all_vars p
           | _                 => []

    fun uniq acc lst =
      case lst of
           [] => true
         | x::xs' => not (List.exists (fn a => a = x) acc)
                     andalso uniq (x::acc) xs'
  in (uniq [] o get_all_vars) p  end


fun match (v_p : valu * pattern) =
  case v_p of
       (_ ,Wildcard) => SOME []
     | (v, Variable s) => SOME [(s, v)]
     | (Unit, UnitP) => SOME []
     | (Const i, ConstP j) => if i = j then SOME [] else NONE
     | (Tuple vs, TupleP ps) => if List.length vs = List.length ps
                              then all_answers match (ListPair.zip (vs, ps))
                              else NONE
     | (Constructor (s1, v), ConstructorP (s2, p)) =>
         if s1 = s2
         then match (v, p)
         else NONE
     | _ => NONE



fun first_match v ps =
  SOME (first_answer (fn p => match (v, p)) ps)
  handle NoAnswer => NONE
