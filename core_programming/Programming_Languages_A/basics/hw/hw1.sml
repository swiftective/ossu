(*
*
* Is there a type declaration like this one to check valid date?
* type date = (1 <= int) * (1 <= int <= 12) * (1 <= int <= 31)
*
*)

type date = int * int * int

fun date_to_days (x : date) =
  (#1 x * 365) + (#2 x * 30) + #3 x

fun is_older (x : date, y : date) =
  date_to_days x < date_to_days y

fun number_in_month (xs : date list, m : int) =
  let
    fun number_in_month (xs : date list, acc : int) =
      if null xs
      then acc
      else if #2 (hd xs) = m
      then number_in_month(tl xs, acc + 1)
      else number_in_month(tl xs, acc)
  in
    number_in_month(xs, 0)
  end



fun number_in_months (xs : date list, ys : int list) =
  let
    fun number_in_months (ys : int list, acc : int) =
      if null ys
      then acc
      else
        let val acc = acc + number_in_month(xs, hd ys)
        in
          number_in_months(tl ys, acc)
        end
  in
    number_in_months(ys, 0)
  end


fun dates_in_month (xs : date list, m : int) =
  let
    fun dates_in_month (xs : date list, acc) =
      if null xs
      then acc
      else if #2 (hd xs) = m
      then dates_in_month(tl xs, hd xs :: acc)
      else dates_in_month(tl xs, acc)
  in
    dates_in_month(xs, [])
  end


fun dates_in_months (xs : date list, ys : int list) =
  let
    fun dates_in_months (ys: int list) =
      if null ys
      then []
      else dates_in_month (xs,  hd ys) @ dates_in_months (tl ys)
  in
    dates_in_months ys
  end


fun get_nth (xs : string list, n : int) =
  let
    fun get_nth (xs: string list, index : int) =
      if index = n
      then hd xs
      else get_nth (tl xs, index + 1)
  in
    get_nth (xs, 1)
  end

fun get_month (x: int) =
  let val months = ["January",
                    "February",
                    "March",
                    "April",
                    "May",
                    "June",
                    "July",
                    "August",
                    "September",
                    "October",
                    "November",
                    "December"]
  in
    get_nth(months, x)
  end

fun date_to_string (x : date) =
  get_month(#2 x)
  ^ " "
  ^ Int.toString (#3 x)
  ^ ", "
  ^ Int.toString (#1 x)


fun number_before_reaching_sum (sum : int, xs : int list) =
  let
    fun number_before_reaching_sum (xs, acc, index) =
    let
      val acc = hd xs + acc
    in
      if acc >= sum
      then index - 1
      else number_before_reaching_sum (tl xs, acc, index + 1)
    end
  in
    number_before_reaching_sum (xs, 0, 1)
  end


fun what_month (x : int) =
  let
    val months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  in
    number_before_reaching_sum(x, months) + 1
  end

fun month_range (x : int, y : int) =
  let
    fun month_range (y : int, acc : int list) =
      if x > y
      then acc
      else month_range (y - 1, what_month y :: acc)
  in
    month_range (y, [])
  end


fun oldest (xs : date list) =
  if null xs
  then NONE
  else
    let fun oldest (xs : date list) =
      if null (tl xs)
      then hd xs
      else
        let val old_date = oldest (tl xs)
        in
          if is_older(hd xs, old_date)
          then hd xs
          else old_date
        end
    in
      SOME (oldest xs)
    end
