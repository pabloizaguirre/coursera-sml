(* Homework3 Simple Test*)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

val test1 = only_capitals ["A","B","C"] = ["A","B","C"]
val test1_1 = only_capitals ["a","b","c"] = []
val test1_2 = only_capitals [] = []
val test1_3 = only_capitals ["Aa","aB","cc"] = ["Aa"]

val test2 = longest_string1 [] = ""
val test2_1 = longest_string1 ["A","bc","C"] = "bc"
val test2_2 = longest_string1 ["A","bc","cd"] = "bc"

val test3 = longest_string2 ["A","bc","C"] = "bc"
val test3_0 = longest_string2 [] = ""
val test3_1 = longest_string2 ["A","bc","C"] = "bc"
val test3_2 = longest_string2 ["A","bc","cd"] = "cd"

val test4a= longest_string3 ["A","bc","C"] = "bc"
val test4b= longest_string4 ["A","B","C"] = "C"

val test4x2 = longest_string3 [] = ""
val test4x2_1 = longest_string3 ["A","bc","C"] = "bc"
val test4x2_2 = longest_string3 ["A","bc","cd"] = "bc"

val test4x3 = longest_string4 ["A","bc","C"] = "bc"
val test4x3_0 = longest_string4 [] = ""
val test4x3_1 = longest_string4 ["A","bc","C"] = "bc"
val test4x3_2 = longest_string4 ["A","bc","cd"] = "cd"

val test5 = longest_capitalized ["A","bc","C"] = "A";
val test5_1 = longest_capitalized [] = "";
val test5_2 = longest_capitalized ["A","bc","C"] = "A";
val test5_3 = longest_capitalized ["asd", "AC","b","CD"] = "AC";
val test5_4 = longest_capitalized ["a","bc","c"] = "";

val test6 = rev_string "abc" = "cba";
val test6_1 = rev_string "" = "";
val test7 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4
val test7_1 = ((first_answer (fn x => if x > 10 then SOME x else NONE) [1,2,3,4,5]); false) handle NoAnswer => true
val test7_2 = ((first_answer (fn x => if x > 10 then SOME x else NONE) []); false) handle NoAnswer => true

val test8 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE
val test8_1 = all_answers (fn x => if x > 1 then SOME [2*x] else NONE) [2,3,4,5,6,7] = SOME [4,6,8,10,12,14]
val test8_2 = all_answers (fn x => if x = 4 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE
val test8_3 = all_answers (fn x => if x > 5 then SOME [x, x+1] else SOME [x]) [2,3,4,5,6,7] = SOME [2,3,4,5,6,7,7,8]  

val w = Wildcard
val c1 = ConstP 4
val c2 = ConstP 5
val v1 = Variable "aaa"
val v2 = Variable "b"
val u = UnitP
val r1 = ConstructorP ("cons", c1)
val r2 = ConstructorP ("cons2", v1)
val t1 = TupleP [Wildcard, ConstP 8] 
val t2 = TupleP [Wildcard, Variable "123456789"]
val t3 = TupleP [v1, w, v1]
val t4 = TupleP [w, v1, u, v2]
val t5 = TupleP [r1, r1]

val test9a = count_wildcards Wildcard = 1
val test9a_1 = count_wildcards c1 = 0
val test9a_2 = count_wildcards v1 = 0
val test9a_3 = count_wildcards u = 0
val test9a_4 = count_wildcards r1 = 0
val test9a_5 = count_wildcards t1 = 1
val test9a_6 = count_wildcards t2 = 1

val test9b = count_wild_and_variable_lengths (Variable("a")) = 1
val test9b_1 = count_wild_and_variable_lengths c1 = 0
val test9b_2 = count_wild_and_variable_lengths v1 = 3
val test9b_3 = count_wild_and_variable_lengths u = 0
val test9b_4 = count_wild_and_variable_lengths r1 = 0
val test9b_5 = count_wild_and_variable_lengths r2 = 3
val test9b_6 = count_wild_and_variable_lengths t1 = 1
val test9b_7 = count_wild_and_variable_lengths t2 = 10

val test9c = count_some_var ("x", Variable("x")) = 1
val test9c_1 = count_some_var ("aaa", w) = 0
val test9c_2 = count_some_var ("aaa", v1) = 1
val test9c_3 = count_some_var ("aab", v1) = 0
val test9c_4 = count_some_var ("aaa", c1) = 0
val test9c_5 = count_some_var ("aaa", u) = 0
val test9c_6 = count_some_var ("aaa", r1) = 0
val test9c_7 = count_some_var ("cons", r1) = 0
val test9c_8 = count_some_var ("aaa", r2) = 1
val test9c_9 = count_some_var ("aaa", t1) = 0
val test9c_10 = count_some_var ("aaa", t2) = 0
val test9c_11 = count_some_var ("123456789", t2) = 1

val test10 = check_pat (Variable("x")) = true
val test10_1 = check_pat v1 = true
val test10_2 = check_pat t2 = true 
val test10_3 = check_pat t3 = false
val test10_4 = check_pat t4 = true
val test10_5 = check_pat w = true
val test10_6 = check_pat c1 = true
val test10_7 = check_pat t5 = true
val test10_8 = check_pat u = true
val test10_9 = check_pat (ConstructorP ("hi",TupleP[Variable "x",Variable "x"])) = false
val test10_10 = check_pat (ConstructorP ("hi",TupleP[Variable "x",ConstructorP ("yo",TupleP[Variable "x",UnitP])])) = false
val test10_11 = check_pat (Variable "x") = true

val vc1 = Const 4
val vc2 = Const 5
val vu = Unit
val vr1 = Constructor ("cons", vc1)
val vr2 = Constructor ("cons2", vc2)

val vt1 = Tuple [vc1, vc2]
val vt1b = Tuple [vc1, vc1]
val pt1 = TupleP [c1, c2]
val pt1b = TupleP [c1, v2]

val vt2 = Tuple [vr1]
val pt2 = TupleP [v1]

val vt3 = Tuple [vt1, vt2]
val pt3 = TupleP [pt1b, pt2]


val test11 = match (Const(1), UnitP) = NONE
val test11_1 = match (vt2, w) = SOME []
val test11_2 = match (vr1, v2) = SOME [("b", vr1)]
val test11_3 = match (vu, u) = SOME []
val test11_4 = match (vc1, c1) = SOME []
val test11_5 = match (vc2, c1) = NONE
val test11_6 = match (vt1, pt1) = SOME []
val test11_7 = match (vt1, pt1b) = SOME [("b", vc2)]
val test11_8 = match (vt2, pt2) = SOME [("aaa", vr1)]
val test11_9 = match (vt1, pt2) = NONE
val test11_10 = match (vt3, pt3) = SOME [("b", vc2), ("aaa", vr1)]
val test11_11 = match (vr1, r1) = SOME []
val test11_12 = match (vr2, r2) = SOME [("aaa", vc2)]
val test11_13 = match (vr1, r2) = NONE

val test11_14 = match (Unit, ConstP 17) = NONE
val test11_15 = match (Unit, TupleP[]) = NONE

val test12 = first_match Unit [UnitP] = SOME []
val test12_1 = first_match vc1 [] = NONE
val test12_2 = first_match vc1 [t1, v1, v2] = SOME [("aaa", vc1)] 

(*
val test13_1 = pattern_to_type [] w = SOME Anything 
val test13_2 = pattern_to_type [] c1 = SOME IntT
val test13_3 = pattern_to_type [] v1 = SOME Anything
val test13_4 = pattern_to_type [] u = SOME UnitT
val test13_5 = pattern_to_type [("cons", "atype", IntT)] r1 = SOME (Datatype "atype")
val test13_5b = pattern_to_type [("cons", "atype", UnitT)] r1 = NONE
val test13_6 = pattern_to_type [] t1 = SOME (TupleT [Anything, IntT])
val test13_7 = pattern_to_type [] t2 = SOME (TupleT [Anything, Anything])
val test13_8 = pattern_to_type [] t3 = SOME (TupleT [Anything, Anything, Anything])
val test13_9 = pattern_to_type [] t4 = SOME (TupleT [Anything, Anything, UnitT, Anything])
val test13_10 = pattern_to_type [] pt3 = SOME (TupleT [TupleT [IntT, Anything], TupleT [Anything]])
val test13_11 = pattern_to_type [("Empty","list",UnitT),("List","list",TupleT[Anything, Datatype "list"])] 
                          ( ConstructorP("List",TupleP[ConstP 10, ConstructorP("Empty",UnitP)]))
              = SOME (Datatype "list")
val test13_12 = pattern_to_type [("Empty","list",UnitT),("List","list",TupleT[Anything, Datatype "list"])] 
                          ( TupleP[ConstP 10, ConstructorP("Empty",UnitP)])
              = SOME (TupleT [IntT,Datatype "list"])

val test14_1 = complies (IntT, IntT)
val test14_2 = complies (IntT, Anything)
val test14_3 = complies (Anything, IntT)
val test14_4 = not (complies (Datatype "aaa", Datatype "bbb"))
val test14_5 = complies (TupleT [IntT, UnitT, IntT, Datatype "ttt"], TupleT [IntT, UnitT, Anything, Datatype "ttt"])
val test14_6 = not (complies (TupleT [IntT], TupleT [IntT, IntT]))

val tmap1 = [("cons1a", "dt1", IntT)
             ,("cons1b", "dt1", TupleT[IntT, IntT])
             ,("cons2a", "dt2", IntT)
            ]
           

val test15_0 = typecheck_patterns ([], [ConstP 1, Variable("x")]) = SOME IntT
val test15_1 = typecheck_patterns ([], [TupleP[Variable("x"),Variable("y")] , TupleP[Wildcard,Wildcard]])
             = SOME (TupleT[Anything,Anything])
val test15_2 = typecheck_patterns ([], [TupleP[Wildcard,Wildcard], TupleP[Wildcard,TupleP[Wildcard,Wildcard]]])
             = SOME (TupleT[Anything,TupleT[Anything,Anything]])
val test15_3 = typecheck_patterns (tmap1, [ConstructorP ("cons1a", ConstP 1)]) = SOME (Datatype "dt1")
val test15_4 = typecheck_patterns (tmap1, [ConstructorP ("cons1b", ConstP 1)]) = NONE
val test15_5 = typecheck_patterns (tmap1, [ConstructorP ("cons1c", ConstP 1)]) = NONE
val test15_6 = typecheck_patterns (tmap1, [ConstructorP ("cons1a", Wildcard)]) = SOME (Datatype "dt1")
val test15_7 = typecheck_patterns (tmap1, [ConstructorP ("cons1a", ConstP 1), ConstructorP ("cons1a", Wildcard)]) = SOME (Datatype "dt1")
val test15_8 = typecheck_patterns (tmap1, [ConstructorP ("cons1a", ConstP 1), ConstructorP ("cons1b", TupleP[ConstP 2, ConstP 3])]) = SOME (Datatype "dt1")
val test15_9 = typecheck_patterns (tmap1, [ConstructorP ("cons1a", ConstP 1), ConstructorP ("cons12a", ConstP 2)]) = NONE

val test15_10 = typecheck_patterns ([], [UnitP, ConstP 5]) = NONE
val test15_11 = typecheck_patterns ([], []) = NONE

val test15_12 = typecheck_patterns ([], [TupleP[c1], TupleP[c1, c2]]) = NONE
val test15_13 = typecheck_patterns ([], [TupleP[c1], TupleP[]]) = NONE
val test15_14 = typecheck_patterns ([], [TupleP[], TupleP[]]) = SOME (TupleT[])
 
val test15_15 = typecheck_patterns ([], [ConstP 10, Variable "a"]) = SOME IntT
val test15_16 = typecheck_patterns ([], [TupleP[Variable "a", ConstP 10, Wildcard], TupleP[Variable "b", Wildcard, ConstP 11], Wildcard])
              = SOME (TupleT [Anything,IntT,IntT])
val test15_17 = typecheck_patterns (
                                    [("Sedan","auto", Datatype "color"),("Truck","auto",TupleT[IntT, Datatype "color"]),("SUV","auto",UnitT)],
                                    [ConstructorP("Sedan", Variable "a"), ConstructorP("Truck", TupleP[Variable "b", Wildcard]), Wildcard]
                                   )
              = SOME (Datatype "auto")

val test15_18 = typecheck_patterns (
                                    [("Empty","list",UnitT),("List","list",TupleT[Anything, Datatype "list"])],
                                    [ConstructorP("Empty",UnitP),
                                     ConstructorP("List",TupleP[ConstP 10, ConstructorP("Empty",UnitP)]),
                                     Wildcard]
                                   )
              = SOME (Datatype "list")

*)