(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

fun all_except_option (s, sl) =
  case sl of
       [] => NONE
     | x::xs' => if same_string (x, s)
                  then SOME xs'
                  else case all_except_option (s, xs') of
                            NONE => NONE
                          | SOME l => SOME (x :: l)

fun get_substitutions1 (sll, s) =
  case sll of
       [] => []
     | sl::sll' => case all_except_option(s, sl) of
                        NONE => get_substitutions1(sll', s)
                      | SOME l => l @ get_substitutions1(sll', s)

fun get_substitutions2 (sll, s) =
  let
    fun get_substitutions2 (sll, acc) =
      case sll of
           [] => acc
         | sl::sll' => case all_except_option(s, sl) of
                            NONE => get_substitutions2(sll', acc)
                          | SOME l => get_substitutions2(sll', acc @ l)
  in
    get_substitutions2 (sll, [])
  end


fun similar_names (sll, {first, middle, last}) =
  let
    val subs = get_substitutions2 (sll, first)
    fun similar_names subs =
      case subs of
           [] => []
         | s::subs' => {first=s, middle=middle, last=last} :: similar_names subs'
  in
    {first=first, middle=middle, last=last} :: similar_names subs
  end



(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw

exception IllegalMove

(* put your solutions for problem 2 here *)

fun card_color (suit : suit, _ : rank) =
  case suit of
       Clubs => Black
     | Spades => Black
     | _ => Red


fun card_value (_ : suit, rank : rank) =
  case rank of
       Num i => i
     | Ace => 11
     | _ => 10


fun remove_card (cs : card list, c : card, e : exn) =
  case cs of
       [] => raise e
     | c1::cs' => if c1 = c
                  then cs'
                  else c1 :: remove_card (cs', c, e)


fun all_same_color (cs : card list) =
  case cs of
       [] => true
     | c :: [] => true
     | c1 :: c2 :: cs' => card_color c1 = card_color c2 andalso
                          all_same_color(c2 :: cs')


fun sum_cards (cs : card list) =
  let
    fun sum_cards (cs : card list, sum : int) =
      case cs of
           [] => sum
         | x::xs' => sum_cards(xs', card_value x + sum)
  in
    sum_cards (cs, 0)
  end


fun score (cs : card list, goal : int) =
  let
    val sum = sum_cards cs
    val score = if (sum > goal) then (sum - goal) * 3 else (goal - sum)
  in
    if all_same_color cs
    then score div 2
    else score
  end

fun officiate (cs : card list, moves : move list, goal : int) =
  let fun officiate (cs, moves, hc) =
      case moves of
           [] => score(hc, goal)
         | m::moves' => case m of
                          Discard c => let val hc = remove_card(hc, c, IllegalMove)
                                       in
                                         officiate(cs, moves', hc)
                                       end
                        | Draw => case cs of
                                       [] => score(hc, goal)
                                       | c::cs' => let val hc = c::hc
                                                   in
                                                     if sum_cards hc > goal
                                                     then score(hc, goal)
                                                     else officiate(cs', moves', hc)
                                                   end
  in
    officiate (cs, moves, [])
  end
