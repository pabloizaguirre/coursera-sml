(* Homework1 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

val test1   = is_older((1,2,3),(2,3,4)) = true
val test1_1 = is_older((1,2,3),(1,2,3)) = false
val test1_2 = is_older((1,2,3),(1,2,4)) = true
val test1_3 = is_older((1,2,3),(1,3,3)) = true
val test1_4 = is_older((1,2,3),(2,2,3)) = true
val test1_5 = is_older((1,2,3),(1,2,2)) = false
val test1_6 = is_older((1,2,3),(1,1,3)) = false
val test1_7 = is_older((2,2,3),(1,2,3)) = false
val test1_8 = is_older((1,3,2),(1,2,3)) = false

val test2 = number_in_month([(2012,2,28),(2013,12,1)],2) = 1
val test2_1 = number_in_month([],1) = 0
val test2_2 = number_in_month([(2000,3,1),(2001,2,3),(2,1,2),(2,1,2)],2) = 1
val test2_3 = number_in_month([(2000,3,1),(2001,2,3),(2,2,2),(2,1,2)],2) = 2

val test3 = number_in_months([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = 3
val test3_1 = number_in_months([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[]) = 0
val test3_2 = number_in_months([],[2,3,4]) = 0
val test3_3 = number_in_months([(2012,2,28),(2013,2,1),(2011,3,31),(2011,5,28)],[2,3,7]) = 3

val test4 = dates_in_month([(2012,2,28),(2013,12,1)],2) = [(2012,2,28)]

val test5 = dates_in_months([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)]

val test6 = get_nth(["hi", "there", "how", "are", "you"], 2) = "there"

val test7 = date_to_string((2013, 6, 1)) = "June 1, 2013"

val test8 = number_before_reaching_sum(10, [1,2,3,4,5]) = 3
(*val test8_1 = number_before_reaching_sum(100, [1,2,3,4,5]) = 0*)
val test8_2 = number_before_reaching_sum(11, [1,2,3,4,5]) = 4

val test9 = what_month(70) = 3
val test9_1 = what_month(1) = 1
val test9_2 = what_month(31) = 1
val test9_3 = what_month(32) = 2
val test9_4 = what_month(59) = 2
val test9_5 = what_month(60) = 3
val test9_6 = what_month(365) = 12
(*val test9_7 = what_month(366) = 12*)

val test10 = month_range(31, 34) = [1,2,2,2]

val test11 = oldest([(2012,2,28),(2011,3,31),(2011,4,28)]) = SOME (2011,3,31)
val test11_1 = oldest([]) = NONE
val test11_2 = oldest([(2010,2,28),(2011,3,31),(2011,4,28)]) = SOME (2010,2,28)
val test11_3 = oldest([(2012,2,28),(2011,3,31),(2010,4,28)]) = SOME (2010,4,28)


(* val test13 = number_in_months_challenge([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4,3]) = 3
val test14 = dates_in_months_challenge([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4,3]) = [(2012,2,28),(2011,4,28),(2011,3,31)]

val test15_1 = reasonable_date(1972,5,14)
val test15_2 = not (reasonable_date(0,5,14))
val test15_3 = not (reasonable_date(~1,5,14))

val test15_4 = not (reasonable_date(1972,0,14))
val test15_5 = not (reasonable_date(1972,~1,14))
val test15_6 = reasonable_date(1972,1,14)
val test15_7 = reasonable_date(1972,12,14)
(*val test15_8 = not (reasonable_date(1972,13,14))
*)
val test15_9 = not (reasonable_date(1972,5,0))
val test15_10 = not (reasonable_date(1972,5,~1))
val test15_11 = reasonable_date(1972,5,31)
val test15_12 = not (reasonable_date(1972,5,32))
val test15_13 = reasonable_date(1972,6,30)
val test15_14 = not (reasonable_date(1972,6,31))

val test15_15 = not(reasonable_date(1999,2,29))
val test15_16 = reasonable_date(2000,2,29)
val test15_17 = not(reasonable_date(1900,2,29))
val test15_18 = reasonable_date(2004,2,29)
 *)
