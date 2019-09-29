(* Homework2 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

val test1 = all_except_option("string", ["string"]) = SOME []
val test1_1 = all_except_option("a", ["b", "c"]) = NONE
val test1_2 = all_except_option("a", ["a", "b", "c"]) = SOME ["b", "c"]
val test1_3 = all_except_option("b", ["a", "b", "c"]) = SOME ["a", "c"]
val test1_4 = all_except_option("c", ["a", "b", "c"]) = SOME ["a", "b"]
val test1_5 = all_except_option("a", []) = NONE
					       
val test2 = get_substitutions1([["foo"],["there"]], "foo") = []
val test2_1 = get_substitutions1([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],"Fred")
	      = ["Fredrick","Freddie","F"] 
		    
val test3 = get_substitutions2([["foo"],["there"]], "foo") = []
val test3_1 = get_substitutions2([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],"Fred")
	      = ["Fredrick","Freddie","F"]
		    
val test4 = similar_names([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"}) =
	    [{first="Fred", last="Smith", middle="W"}, {first="Fredrick", last="Smith", middle="W"},
	     {first="Freddie", last="Smith", middle="W"}, {first="F", last="Smith", middle="W"}]


val test5 = card_color((Clubs, Num 2)) = Black
val test5_1 = card_color((Spades, Num 2)) = Black
val test5_2 = card_color((Diamonds, Num 2)) = Red
val test5_3 = card_color((Hearts, Num 2)) = Red
						
val test6 = card_value((Clubs, Num 2)) = 2
val test6_1 = card_value((Clubs, Ace)) = 11
val test6_2 = card_value((Clubs, King)) = 10
					      
					      
val test7 = remove_card  ([(Hearts, Ace)                ], (Hearts, Ace), IllegalMove) = []
val test7_1 = remove_card([(Hearts, Ace), (Clubs, Num 5)], (Hearts, Ace), IllegalMove) = [(Clubs, Num 5)]
val test7_2 = (remove_card([(Hearts, King)], (Hearts, Ace), IllegalMove) = []) handle IllegalMove => true
													 
val test8 = all_same_color([(Hearts, Ace), (Hearts, Ace)]) = true
val test8_1 = all_same_color([(Hearts, Ace), (Spades, Ace)]) = false
val test8_2 = all_same_color([(Hearts, Ace), (Diamonds, Ace)]) = true
								     
val test9 = sum_cards([(Clubs, Num 2),(Clubs, Num 2)]) = 4
val test10 = score([(Hearts, Num 2),(Clubs, Num 4)],10) = 4
val test10_1 = score([(Hearts, Num 8),(Clubs, Num 4)],10) = 6
val test10_2 = score([(Spades, Num 8),(Clubs, Num 4)],10) = 3
								
	       
val test11 = officiate([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6
									  
val test11_1 = officiate([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
			 [Draw,Draw,Draw,Draw,Draw],
			 42)
               = 3
		     
val test11_2 = ((officiate([(Clubs,Jack),(Spades,Num(8))],
                           [Draw,Discard(Hearts,Jack)],
                           42);
		 false) 
		handle IllegalMove => true)
val test11_3 =  officiate([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Diamonds,King)],
			  [Draw,Draw,Draw,Draw,Draw],
			  20)
		= 3

(*)
val test12 = score_challenge([(Hearts, Num 2),(Clubs, Num 4)], 10) = 4
val test12_1 = score_challenge([(Hearts, Num 8),(Clubs, Num 4)], 10) = 6
val test12_2 = score_challenge([(Spades, Num 8),(Clubs, Num 4)], 10) = 3
val test12_3 = score_challenge([(Hearts, Ace)], 1) = 0
val test12_4 = score_challenge([(Hearts, Ace)], 11) = 0
							  
							  
val test13 = officiate_challenge([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6
										    
val test13_1 = officiate_challenge([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
				   [Draw,Draw,Draw,Draw,Draw],
				   42)
               = 3
val test13_2 = ((officiate_challenge([(Clubs,Jack),(Spades,Num(8))],
				     [Draw,Discard(Hearts,Jack)],
				     42);
		 false)
		handle IllegalMove => true)
val test13_3 = officiate_challenge([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Diamonds,King)],
				   [Draw,Draw,Draw,Draw,Draw],
				   20)
               = 7
val test13_4 = officiate_challenge([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
				   [Draw,Draw,Draw,Draw,Draw],
				   44)
               = 0
val test13_5 = officiate_challenge([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
				   [Draw,Draw,Draw,Draw,Draw],
				   34)
               = 0
val test13_6 = officiate_challenge([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
				   [Draw,Draw,Draw,Draw,Draw],
				   24)
               = 0
val test13_7 = officiate_challenge([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
				   [Draw,Draw,Draw,Draw,Draw],
				   14)
               = 0
val test13_8 = officiate_challenge([(Clubs,Ace),(Clubs,Ace),(Clubs,Ace),(Clubs,Ace),(Clubs,Ace),(Clubs,Ace),(Clubs,Ace)],
				   [Draw,Draw,Draw,Draw,Draw,Draw,Draw,Draw,Draw,Draw,Draw,Draw],
				   5)
               = 1
		     
val test14 = careful_player([], 1) = []
val test14_1 = careful_player([], 20) = [Draw] 
val test14_2 = careful_player([(Clubs, Num 9), (Clubs, Num 8)], 12) = [Draw]
val test14_3 = careful_player([(Clubs, Num 9), (Clubs, Num 8)], 19) = [Draw]
val test14_4 = careful_player([(Clubs, Num 9), (Clubs, Num 8)], 20) = [Draw, Draw]
val test14_5 = careful_player([(Clubs, Num 9), (Clubs, Num 3)], 12) = [Draw]
val test14_6 = careful_player([(Clubs, Num 3), (Clubs, Num 6), (Clubs, Num 9), (Clubs, Num 5)], 20)
               = [Draw, Draw, Draw, Discard (Clubs, Num 3), Draw] 
val test14_7 = careful_player([(Clubs, Num 9), (Clubs, Ace), (Clubs, Num 2)], 20) = [Draw, Draw]
											
											
*)