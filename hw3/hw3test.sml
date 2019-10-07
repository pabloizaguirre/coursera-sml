(* Homework3 Simple Test*)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

val test1 = only_capitals ["A","B","C"] = ["A","B","C"]
val test1b = only_capitals ["astr", "Basics", "com", "def", "Eleg"] = ["Basics", "Eleg"]
val test1c = only_capitals ["astr", "basics", "com", "def", "eleg"] = []

val test2 = longest_string1 ["A","bc","C"] = "bc"
val test2b = longest_string1 ["hola", "bye", "ciao"] = "hola"
val test2c = longest_string1 [] = ""

val test3 = longest_string2 ["A","bc","C"] = "bc" 
val test3b = longest_string2 ["hola", "bye", "adeu", "ciao"] = "ciao"

val test4a = longest_string3 ["A","bc","C"] = "bc"
val test4b = longest_string3 ["hola", "bye", "ciao"] = "hola"


val test4c = longest_string4 ["A","B","C"] = "C"
val test4d = longest_string4 ["a", "bcd", "ad"] = "bcd"

val test5 = longest_capitalized ["A","bc","C"] = "A"

val test6 = rev_string "abc" = "cba"
val test6b = rev_string "Pablo" = "olbaP"
val test6c = rev_string "" = ""

val test7 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4

val test8 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE
val test8b = all_answers (fn x => if x = 1 then SOME [x] else NONE) [] = SOME []
val test8c = all_answers (fn x => if x > 1 then SOME [x] else NONE) [2, 3, 4, 5, 6] = SOME [2, 3, 4, 5, 6]

val test9a = count_wildcards Wildcard = 1
val test9a1 = count_wildcards (TupleP [Wildcard, Wildcard, ConstP 3, Wildcard]) = 3

val test9b = count_wild_and_variable_lengths (Variable("a")) = 1
val test9b1 = count_wild_and_variable_lengths (TupleP [Wildcard, Variable("asdasdasd"), Variable("asdasd"), Wildcard, UnitP, ConstP 10]) = 17
val test9b2 = count_wild_and_variable_lengths (TupleP [Wildcard, TupleP [Wildcard, Variable("asdasd")], UnitP, Wildcard]) = 9

val test9c = count_some_var ("x", Variable("x")) = 1
val test9c1 = count_some_var ("a", TupleP [Variable "c", Wildcard, TupleP [Variable "a", Variable "w", Wildcard, Variable "a"], Wildcard, Variable "a"]) = 3

val test10 = check_pat (TupleP [Variable "x", Wildcard, Variable "a", TupleP [Wildcard, Variable "d", Variable "p"], Wildcard])
val test10b = check_pat (Variable("x"))
val test10c = not(check_pat (TupleP [Variable "x", Wildcard, TupleP [Variable "a", Wildcard, Variable "x"]]))

val test11 = match (Const(1), UnitP) = NONE


val test12 = first_match Unit [UnitP] = SOME []

