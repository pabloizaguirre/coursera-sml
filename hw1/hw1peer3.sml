fun is_older ((y, m, d), (yy, mm, dd)) =
    if y < yy then true else if y > yy then false
    else if m < mm then true else if m > mm then false
    else d < dd

fun number_in_month (dates: (int * int * int) list, month: int) =
    let fun in_month mon (_, m, _) = m = mon
    in length (List.filter (in_month month) dates)
    end

fun number_in_months (dates: (int * int * int) list, months: int list) =
    let fun num_in_month m = number_in_month(dates, m)
    in foldl op+ 0 (map num_in_month months)
    end

fun dates_in_month (dates: (int * int * int) list, month: int) =
    let fun in_month mon (_, m, _) = m = mon
    in List.filter (in_month month) dates
    end

fun dates_in_months (dates: (int * int * int) list, months: int list) =
    let fun date_in_mon m = dates_in_month(dates, m)
    in foldr op@ [] (map date_in_mon months)
    end

fun get_nth (l: 'a list, i: int) = if i = 1 then hd l else get_nth (tl l, i-1)

fun date_to_string (y: int, m: int, d: int) =
    let val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    in get_nth (months, m) ^ " " ^ Int.toString d ^ ", " ^ Int.toString y
    end

fun number_before_reaching_sum (s, x::xs) =
    if s <= x then 0
    else 1 + number_before_reaching_sum (s-x, xs)

fun what_month x = 1 + number_before_reaching_sum (x, [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31])

fun month_range (x, y) = if x > y then [] else what_month x :: month_range (x+1, y)

fun oldest [] = NONE
  | oldest [x] = SOME x
  | oldest (x::y::xs) = if is_older (x, y) then oldest (x::xs) else oldest (y::xs)

fun number_in_months_challenge (dates: (int * int * int) list, months: int list) =
    let val uniq_months = foldl (fn (x, xs) => if List.exists (fn elem => elem = x) xs then xs else (x::xs)) [] months
    in number_in_months (dates, uniq_months)
    end

fun dates_in_months_challenge (dates: (int * int * int) list, months: int list)=
    let val uniq_months = foldl (fn (x, xs) => if List.exists (fn elem => elem = x) xs then xs else (x::xs)) [] months
    in dates_in_months (dates, uniq_months)
    end


fun reasonable_date (0, _, _) = false
  | reasonable_date (y, 2, d) = if (y mod 4 = 0) andalso (y mod 100 <> 0 orelse y mod 400 = 0) then d > 0 andalso d < 30 else d > 0 andalso d < 29
  | reasonable_date (_, m, d) =
      let fun exists _ [] = false
            | exists e (x::xs) = if e = x then true else exists e xs
      in if exists m [1, 3, 5, 7, 8, 10, 12] then d > 0 andalso d < 32
         else if exists m [4, 6, 9, 11] then d > 0 andalso d < 31
         else false
      end
