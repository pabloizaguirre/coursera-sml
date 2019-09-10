fun is_older(d1 : int * int * int, d2 : int * int * int) =
    if #1 d1 < #1 d2
    then true
    else if #1 d1 > #1 d2
    then false
    else
        if #2 d1 < #2 d2
        then true
        else if #2 d1 > #2 d2
        then false
        else
            if #3 d1 < #3 d2
            then true
            else false

fun number_in_month(ld : (int * int * int) list, m : int) =
    if null ld
    then 0
    else if #2 (hd ld) = m
    then 1 + number_in_month(tl ld, m)
    else number_in_month(tl ld, m)

fun number_in_months(ld : (int * int * int) list, lm : int list) =
    if null lm
    then 0
    else number_in_month(ld, hd lm) + number_in_months(ld, tl lm)

fun dates_in_month(ld : (int * int * int) list, m : int) =
    if null ld
    then []
    else if #2 (hd ld) = m
    then hd ld :: dates_in_month(tl ld, m)
    else dates_in_month(tl ld, m)

fun dates_in_months(ld : (int * int * int) list, lm : int list) =
    if null lm
    then []
    else dates_in_month(ld, hd lm) @ dates_in_months(ld, tl lm)

fun get_nth(sl : string list, n : int) =
    if n = 1
    then hd sl
    else get_nth(tl sl, n - 1)

fun date_to_string(d : int * int * int) =
    let
        val months = ["January", "February", "March", "April",
"May", "June", "July", "August", "September", "October", "November", "December"]
    in 
        get_nth(months, #2 d) ^ " " ^ Int.toString(#3 d) ^ ", " ^ Int.toString(#1 d)
    end

fun number_before_reaching_sum(sum : int, il : int list) =
    if sum <= hd il
    then 0
    else 1 + number_before_reaching_sum(sum - hd il, tl il)

fun what_month(d : int) =
    let 
        val months_length = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
        number_before_reaching_sum(d, months_length) + 1
    end

(* Done without repeating months in the list *)
(* fun month_range(day1 : int, day2 : int) =
    if day1 > day2
    then []
    else 
        let
            val tl_ans = month_range(day1 + 1, day2)
            val mday1 = what_month(day1)
        in
            if null tl_ans
            then mday1 :: tl_ans
            else if (hd tl_ans) = mday1
            then tl_ans
            else mday1 :: tl_ans
        end *)

fun month_range(day1 : int, day2 : int) =
    if day1 > day2
    then []
    else what_month(day1) :: month_range(day1 + 1, day2)

fun oldest(dl : (int * int * int) list) = 
    if null dl
    then NONE
    else
        let
          val old = oldest(tl dl)
        in
          if isSome(old) andalso is_older(valOf(old), (hd dl))
          then old
          else SOME (hd dl)
        end
