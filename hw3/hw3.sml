(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

fun only_capitals xs =
	List.filter (fn x => Char.isUpper(String.sub(x, 0))) xs

fun longest_string1 xs =
	let
	  	fun is_longer (x, y) = String.size x > String.size y
	in
		List.foldl (fn (x, y) => if is_longer(x, y) then x else y) "" xs
	end
	
fun longest_string2 xs =
	let
	  	fun is_longer_or_equal (x, y) = String.size x >= String.size y
	in
		List.foldl (fn (x, y) => if is_longer_or_equal(x, y) then x else y) "" xs
	end

fun longest_string_helper f xs =
	List.foldl (fn (x, y) => if f(String.size x, String.size y) then x else y) "" xs

val longest_string3 = longest_string_helper (fn (x, y) => x > y)

val longest_string4 = longest_string_helper (fn (x, y) => x >= y)

val longest_capitalized = longest_string1 o only_capitals 

val rev_string = implode o List.rev o explode

fun first_answer f xs =
	case xs of
		[] => raise NoAnswer
	 |	y::ys => case f y of
	 			     NONE => first_answer f ys
				  |	 SOME v => v

(* fun all_answers f xs = 
	let
		fun aux acc xs = 
			case xs of
				[] => SOME acc
			 |	x::xs => case f x of
			 			 	 NONE => NONE
						  |	 SOME v => aux (v @ acc) xs
	in
	  	aux [] xs
	end *)

fun all_answers f xs = 
	case xs of
		[] => SOME []
	 |	x::xs' => case f x of
	 			 	 NONE => NONE
				  |	 SOME v => case all_answers f xs' of
								   NONE => NONE
								 | SOME y => SOME (v @ y) 


val count_wildcards = g (fn x => 1) (fn x => 0) 

val count_wild_and_variable_lengths = g (fn x => 1) (String.size)

fun count_some_var (s, p) = g (fn x => 0) (fn x => if x = s then 1 else 0) p

fun check_pat p = 
	let
		fun strings_in_p (acc, p) = 
			case p of
				Variable x => x::acc
			  |	TupleP x => List.foldl (fn (y, z) => strings_in_p(acc, y) @ z) [] x
			  | ConstructorP (_, x) => strings_in_p(acc, x)
			  |	_ => acc

		fun has_repeats xs = 
			case xs of
			  	[] => false
			  | x::[] => false
			  |	x::xs => (List.exists (fn y => y = x) xs) orelse has_repeats xs
	in
		not (has_repeats (strings_in_p ([], p)))
	end

fun match (v, p) = 
	case p of 
		Wildcard => SOME []
	  | Variable x => SOME [(x, v)]
	  |	UnitP => if v = Unit then SOME [] else NONE
	  | ConstP x => (case v of
	  					Const s => if s = x then SOME [] else NONE
					  |	_ => NONE)
	  | TupleP ps => (case v of 
	  					 Tuple vs => if (List.length ps) <> (List.length vs) then NONE else all_answers (match) (ListPair.zip(vs, ps))
					   | _ => NONE)
	  | ConstructorP (s1, p') => (case v of 
	  							    Constructor (s2, v') => if s1 <> s2 then NONE else (match (v', p'))
								  |	_ => NONE)
	
fun first_match v ps =
	SOME (first_answer (fn x => (match (v, x))) ps)
	handle NoAnswer => NONE 
	

(* longest_string_helper(),  *)
(* fun mystery f = fn xs =>
    let
        fun g xs =
           case xs of
               [] => NONE
             | x::xs' => if f x then SOME x else g xs'
    in
        case xs of
            [] => NONE
          | x::xs' => if f x then g xs' else mystery f xs'
    end *)

fun null xs=if null xs then true else false