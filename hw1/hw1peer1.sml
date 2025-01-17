(* 1 *)
fun is_older (date1 : int*int*int, date2 : int*int*int) =
    let
	val y1 = (#1 date1)
	val y2 = (#1 date2)
	val m1 = (#2 date1)
	val m2 = (#2 date2)
	val d1 = (#3 date1)
	val d2 = (#3 date2)
    in
	if y1 < y2
	then true
	else if y1 = y2
	then if m1 < m2
	     then true
	     else if m1 = m2
	     then d1 < d2
	     else false
	else false
    end;


		   
(* 2 *)
fun number_in_month (dates : (int*int*int) list, month : int) =
    if null dates
    then 0
    else if (#2 (hd dates)) = month
    then 1 + (number_in_month(tl dates, month))
    else number_in_month(tl dates, month);
			


(* 3 *)
fun number_in_months (dates : (int*int*int) list, months : int list) =
    if null months
    then 0
    else number_in_month(dates, hd months) + number_in_months(dates, tl months);



(* 4 *)
fun dates_in_month (dates : (int*int*int) list, month : int) =
    if null dates
    then []
    else if (#2 (hd dates)) = month
    then (hd dates) :: dates_in_month(tl dates, month)
    else dates_in_month(tl dates, month);



(* 5 *)
fun dates_in_months (dates : (int*int*int) list, months : int list) =
    if null months
    then []
    else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months);



(* 6 *)
fun get_nth (sl : string list, index : int) =
    if index = 1
    then hd sl
    else get_nth(tl sl, index - 1);



(* 7 *)
fun date_to_string (date : (int*int*int)) =
    let
	val calendar_month = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    in
	get_nth(calendar_month, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
    end;



(* 8 *)
fun number_before_reaching_sum (sum : int, xs: int list) =
    if sum - (hd xs) > 0
    then 1 + number_before_reaching_sum(sum - (hd xs), tl xs)
    else 0;



(* 9 *)
fun what_month (day : int) =
    let
	val number_of_days_in_months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
	number_before_reaching_sum(day, number_of_days_in_months) + 1
    end;



(* 10 *)
fun month_range (day1 : int, day2 : int) =
    if day1 > day2
    then []
    else what_month(day1) :: month_range(day1 + 1, day2);



(* 11 *)
fun oldest (dates : (int*int*int) list) =
    if null dates
    then NONE
    else let
	fun oldest_nonempty (dates : (int*int*int) list) =
	    if null (tl dates)
	    then hd dates
	    else let val tl_ans = oldest_nonempty (tl dates)
		 in
		     if is_older (hd dates, tl_ans)
		     then hd dates
		     else oldest_nonempty(tl dates)
		 end
    in
	SOME (oldest_nonempty dates)
    end;

(* 12 *)
fun remove_duplicate (xs : int list) =
    let
	fun contain (y : int, ys : int list) =
	    if null ys
	    then false
	    else if y = (hd ys)
	    then true
	    else contain(y, (tl ys))
    in
	if null (tl xs)
	then [hd xs]
	else if contain (hd xs, (tl xs))
	then remove_duplicate (tl xs)
	else (hd xs) :: remove_duplicate (tl xs)
    end;


fun number_in_months_challenge (dates : (int*int*int) list, months : int list) =
    let
	val remove_duplicate_months = remove_duplicate(months)
    in
	number_in_months(dates, remove_duplicate_months)
    end;

fun dates_in_months_challenge (dates : (int*int*int) list, months : int list) =
    let
	val remove_duplicate_months = remove_duplicate(months)
    in
	dates_in_months(dates, remove_duplicate_months)
    end;


(* 13 *)
fun reasonable_date (date : (int*int*int)) =
    let
	val y = (#1 date)
	val m = (#2 date)
	val d = (#3 date)
    in
	if y <= 0
	then false
	else if m = 2
	then if (y mod 400 = 0) orelse ((y mod 4 = 0) andalso (y mod 100 <> 0))
	     then d >= 1 andalso d <= 29
	     else d >= 1 andalso d <= 28
	else if m = 4 orelse m = 6 orelse m = 9 orelse m = 11
	then d >= 1 andalso d <= 30
	else d >= 1 andalso d <= 31
    end;
	





























































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































		
							      
		       
