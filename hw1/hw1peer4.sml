fun is_older (d1 : int * int * int, d2 : int * int * int) =
    let
		fun is_older_year () =
			(#1 d1) < (#1 d2)
		fun is_same_year () =
			(#1 d1) = (#1 d2)
		fun is_older_month () =
			(#2 d1) < (#2 d2)
		fun is_same_month () =
			(#2 d1) = (#2 d2)
		fun is_older_day () =
			(#3 d1) < (#3 d2)
    in
		if is_older_year()
		then true
		else if is_same_year() andalso is_older_month()
		then true
		else if is_same_year() andalso is_same_month() andalso is_older_day()
		then true
		else false
    end
								   
fun number_in_month (dates : (int * int * int) list, month : int) =
    if null dates
    then 0
    else
		let		    
			val d = hd dates
		in
			if (#2 d) = month
			then 1 + number_in_month (tl dates, month)
			else 0 + number_in_month (tl dates, month)
		end
	    
fun number_in_months (dates : (int * int * int) list, months : int list) =
    if null months
    then 0
    else number_in_month (dates, hd months) + number_in_months (dates, tl months)
	    
fun dates_in_month (dates : (int * int * int) list, month : int) =
    if null dates
    then []
    else
		let val d = hd dates
		in
			if (#2 d) = month
			then d::dates_in_month (tl dates, month)
			else dates_in_month (tl dates, month)
		end
	    
fun dates_in_months (dates : (int * int * int) list, months : int list) =
    if null months
    then []
    else dates_in_month (dates, hd months)@dates_in_months (dates, tl months)
	
fun get_nth (xs : string list, n : int) =
    let
		fun get_one (xs: string list, times : int) =
			if times = n
			then hd xs
			else get_one (tl xs, times + 1)
    in
		get_one (xs, 1)
    end

fun date_to_string (date : int * int * int) =
	let val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
	in
    	get_nth(months, (#2 date)) ^ " " ^ Int.toString (#3 date) ^ ", " ^ Int.toString (#1 date)
	end
	
fun number_before_reaching_sum (sum : int, xs : int list) =
	let
		fun find_index (i : int, acc : int, xs : int list) =
			let val new_sum = acc + hd xs
			in
				if new_sum < sum
				then find_index (i + 1, new_sum, tl xs)
				else i
			end
	in
		find_index (0, 0, xs)
	end

fun what_month (day : int) =
	let val month_list = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
	in
		number_before_reaching_sum (day, month_list) + 1
	end

fun month_range (d1 : int, d2 : int) =
	if d1 > d2
	then []
	else what_month d1::month_range(d1 + 1, d2)

fun oldest (dates : (int * int * int) list) =
	if null dates
	then NONE
	else
		let val tl_oldest = oldest(tl dates)
		in 
			if isSome tl_oldest andalso is_older(valOf (tl_oldest), hd dates)
			then tl_oldest
			else SOME (hd dates)
		end