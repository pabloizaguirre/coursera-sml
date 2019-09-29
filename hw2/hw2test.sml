(* Homework2 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

val test1 = all_except_option ("string", ["string"]) = SOME []

val test14 = all_except_option ("a", ["a", "b", "c", "d"]) = SOME ["b", "c", "d"]

val test15 = all_except_option ("c", ["a", "b", "c", "d"]) = SOME ["a", "b", "d"]

val test16 = all_except_option ("a", ["b", "c", "d"]) = NONE

val test17 = all_except_option ("a", []) = NONE



val test2 = get_substitutions1 ([["foo"],["there"]], "foo") = []

val test18 = get_substitutions1 ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "Fred") = ["Fredrick","Freddie","F"]

val test19 = get_substitutions1([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]],
"Jeff") = ["Jeffrey","Geoff","Jeffrey"]

val test20 = get_substitutions1([], "jeff") = []

val test21 = get_substitutions1([["jeff"]], "jeff") = []

val test22 = get_substitutions1([["a", "b", "c"], ["a", "b", "d"]], "e") = []

val test23 = get_substitutions1([["a", "b", "c"], ["a", "b", "d"]], "a") = ["b", "c", "b", "d"]

val test3 = get_substitutions2 ([["foo"],["there"]], "foo") = []

val test4 = similar_names ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"}) =
	    [{first="Fred", last="Smith", middle="W"}, {first="Fredrick", last="Smith", middle="W"},
	     {first="Freddie", last="Smith", middle="W"}, {first="F", last="Smith", middle="W"}]

val test24 = similar_names ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Mike", middle="W", last="Smith"}) =
	    [{first="Mike", last="Smith", middle="W"}]

val test25 = similar_names([], {first="Mike", middle="W", last="Smith"}) = [{first="Mike", middle="W", last="Smith"}]

val test5 = card_color (Clubs, Num 2) = Black

val test26 = card_color (Hearts, Jack) = Red 

val test27 = card_color (Diamonds, Num 10) = Red

val test6 = card_value (Clubs, Num 2) = 2

val test28 = card_value (Diamonds, Jack) = 10

val test29 = card_value (Hearts, King) = 10

val test30 = card_value (Diamonds, Ace) = 11

val test7 = remove_card ([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = []

val test31 = (remove_card ([(Hearts, Ace), (Diamonds, Jack), (Diamonds, Num 1)], (Clubs, Ace), IllegalMove) handle IllegalMove => [(Spades, Num 12)]) = [(Spades, Num 12)] 

val test32 = (remove_card ([(Hearts, Ace), (Diamonds, Jack), (Diamonds, Num 1)], (Diamonds, Jack), IllegalMove) handle IllegalMove => [(Spades, Num 12)]) = [(Hearts, Ace), (Diamonds, Num 1)]

val test33 = (remove_card ([(Hearts, Ace), (Diamonds, Jack), (Diamonds, Jack)], (Diamonds, Jack), IllegalMove) handle IllegalMove => [(Spades, Num 12)]) = [(Hearts, Ace), (Diamonds, Jack)]

val test8 = all_same_color [(Hearts, Ace), (Hearts, Ace)] = true

val test34 = all_same_color [(Hearts, Ace), (Hearts, Ace), (Diamonds, Ace), (Diamonds, Jack)] = true

val test35 = all_same_color [(Hearts, Ace), (Hearts, Ace), (Diamonds, Ace), (Spades, Jack)] = false

val test36 = all_same_color [] = true

val test37 = all_same_color [(Spades, Ace)] = true

val test9 = sum_cards [(Clubs, Num 2),(Clubs, Num 2)] = 4

val test38 = sum_cards[(Hearts, Ace), (Hearts, Num 2), (Diamonds, Jack), (Diamonds, Num 4)] = 11+2+10+4

val test39 = sum_cards[] = 0

val test40 = sum_cards[(Hearts, Ace)] = 11

val test10 = score ([(Hearts, Num 2),(Clubs, Num 4)],10) = 4

val test41 = score([(Hearts, Num 2), (Diamonds, Ace)], 14) = 0

val test42 = score([(Hearts, Num 2), (Clubs, Ace)], 13) = 0

val test43 = score([(Hearts, Num 2), (Clubs, Ace)], 10) = 3*3

val test44 = score([(Hearts, Num 2), (Diamonds, Ace)], 10) = 3*3 div 2


val test11 = officiate ([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6

val test12 = officiate ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                        [Draw,Draw,Draw,Draw,Draw],
                        42)
             = 3

val test13 = ((officiate([(Clubs,Jack),(Spades,Num(8))],
                         [Draw,Discard(Hearts,Jack)],
                         42);
               false) 
              handle IllegalMove => true)
             
              
