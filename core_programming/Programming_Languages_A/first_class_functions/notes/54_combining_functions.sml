(* Programming Languages, Dan Grossman *)
(* Section 3: Another Closure Idiom: Combining Functions *)

fun compose (f,g) = fn x => f (g x)

fun sqrt_of_abs i = Math.sqrt(Real.fromInt (abs i))

fun sqrt_of_abs i = (Math.sqrt o Real.fromInt o abs) i

val sqrt_of_abs = Math.sqrt o Real.fromInt o abs

(* tells the parser !> is a function that appears between its two arguments *)
infix !>

(* operator more commonly written |>, but that confuses the current version
   of SML Mode for Emacs, leading to bad editing and formatting *)

(* definition of the pipeline operator *)
fun x !> f = f x

fun sqrt_of_abs i = i !> abs !> Real.fromInt !> Math.sqrt

fun backup1 (f,g) = fn x => case f x of NONE => g x | SOME y => y

fun backup2 (f,g) = fn x => f x handle _ => g x

(* this is awesome !!! *)

infix |>

fun x |> f = f x

infix $

fun a $ b = a + b

val sum = 1 $ 2 $ 2

fun sqrt_of_abs i =
  i |> abs |> Real.fromInt |> Math.sqrt


val sqrt1 = sqrt_of_abs 100
