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
(* 
1. Write a function only_capitals that takes a string list and returns a string list that has only
the strings in the argument that start with an uppercase letter. Assume all strings have at least 1
character. Use List.filter, Char.isUpper, and String.sub to make a 1-2 line solution. *)
fun only_capitals (ss: string list) =
    List.filter (fn s => Char.isUpper(String.sub(s, 0))) ss

(* 
2. Write a function longest_string1 that takes a string list and returns the longest string in the
list. If the list is empty, return "". In the case of a tie, return the string closest to the beginning of the
list. Use foldl, String.size, and no recursion (other than the implementation of foldl is recursive). *)
fun longest_string1 ([]: string list) = ""
  | longest_string1 (x::xs) =
    foldl (fn (s1, s2) => if (String.size(s1) > String.size(s2)) then s1 else s2) x xs
    
(* 
3. Write a function longest_string2 that is exactly like longest_string1 except in the case of ties
it returns the string closest to the end of the list. Your solution should be almost an exact copy of
longest_string1. Still use foldl and String.size. *)
fun longest_string2 ([]) = ""
  | longest_string2 (x::xs) =
    foldl (fn (s1, s2) => if (String.size(s1) >= String.size(s2)) then s1 else s2) x xs

(*     
4. Write functions longest_string_helper, longest_string3, and longest_string4 such that:
• longest_string3 has the same behavior as longest_string1 and longest_string4 has the
same behavior as longest_string2.
• longest_string_helper has type (int * int -> bool) -> string list -> string
(notice the currying). This function will look a lot like longest_string1 and longest_string2
but is more general because it takes a function as an argument.
• If longest_string_helper is passed a function that behaves like > (so it returns true exactly
when its first argument is stricly greater than its second), then the function returned has the same
behavior as longest_string1.
• longest_string3 and longest_string4 are defined with val-bindings and partial applications
of longest_string_helper. *)
fun longest_string_helper _ [] = ""
  | longest_string_helper cmp (s::rest) =
    foldl (fn (s, acc) => if cmp(String.size(s), String.size(acc)) then s else acc) s rest

val longest_string3 = longest_string_helper op> 
val longest_string4 = longest_string_helper op>=

(* 
5. Write a function longest_capitalized that takes a string list and returns the longest string in
the list that begins with an uppercase letter, or "" if there are no such strings. Assume all strings
have at least 1 character. Use a val-binding and the ML library’s o operator for composing functions.
Resolve ties like in problem 2. *)
val longest_capitalized = longest_string1 o only_capitals 

(* 
6. Write a function rev_string that takes a string and returns the string that is the same characters in
reverse order. Use ML’s o operator, the library function rev for reversing lists, and two library functions
in the String module. (Browse the module documentation to find the most useful functions.) *)
val rev_string = implode o rev o explode

(* 7. Write a function first_answer of type (’a -> ’b option) -> ’a list -> ’b (notice the 2 arguments are curried). 
The first argument should be applied to elements of the second argument in order
until the first time it returns SOME v for some v and then v is the result of the call to first_answer.
If the first argument returns NONE for all list elements, then first_answer should raise the exception
NoAnswer. Hints: Sample solution is 5 lines and does nothing fancy. *)
fun first_answer f xs =
	let 
		fun g (x, (SOME a)) = (SOME a)
		  | g (x, NONE) = f x
	in
		case (List.foldl g NONE xs) of
			NONE => raise NoAnswer
		|	(SOME a) => a
	end 

(* 
8. Write a function all_answers of type (’a -> ’b list option) -> ’a list -> ’b list option
(notice the 2 arguments are curried). The first argument should be applied to elements of the second
argument. If it returns NONE for any element, then the result for all_answers is NONE. Else the
calls to the first argument will have produced SOME lst1, SOME lst2, ... SOME lstn and the result of
all_answers is SOME lst where lst is lst1, lst2, ..., lstn appended together (order doesn’t matter).
Hints: The sample solution is 8 lines. It uses a helper function with an accumulator and uses @. Note
all_answers f [] should evaluate to SOME [] *)
fun all_answers f xs = 
	let
		fun g (x, NONE) = NONE
		  | g (x, (SOME a)) = 
		  	case f x of
		  		SOME z => SOME (z @ a)
			|	NONE => NONE
	in
		List.foldr g (SOME []) xs
	end

(* 
9. (This problem uses the pattern datatype but is not really about pattern-matching.) A function g has
been provided to you.
(a) Use g to define a function count_wildcards that takes a pattern and returns how many Wildcard
patterns it contains.
(b) Use g to define a function count_wild_and_variable_lengths that takes a pattern and returns
the number of Wildcard patterns it contains plus the sum of the string lengths of all the variables
in the variable patterns it contains. (Use String.size. We care only about variable names; the
constructor names are not relevant.)
(c) Use g to define a function count_some_var that takes a string and a pattern (as a pair) and
returns the number of times the string appears as a variable in the pattern. We care only about
variable names; the constructor names are not relevant. *)
val count_wildcards = g (fn _ => 1) (fn _ => 0)
val count_wild_and_variable_lengths = g (fn _ => 1) String.size
fun count_some_var (s: string, p: pattern) =
	let 
		fun f s1 = if s = s1 then 1 else 0
	in 
		g (fn _ => 0) f p
	end

(* 
10. Write a function check_pat that takes a pattern and returns true if and only if all the variables
appearing in the pattern are distinct from each other (i.e., use different strings). The constructor
names are not relevant. Hints: The sample solution uses two helper functions. The first takes a
pattern and returns a list of all the strings it uses for variables. Using foldl with a function that uses
@ is useful in one case. The second takes a list of strings and decides if it has repeats. List.exists may
be useful. Sample solution is 15 lines. These are hints: We are not requiring foldl and List.exists
here, but they make it easier. *)
fun has_repeats [] = false
  | has_repeats (x::xs) = (List.exists (fn y => x = y) xs) orelse (has_repeats xs)

fun all_var_names (Wildcard, acc) = acc
  | all_var_names ((Variable x), acc) = x::acc
  | all_var_names (ConstructorP(_,p), acc) = all_var_names (p, acc)
  | all_var_names (TupleP ps, acc) = let val t = List.foldl (fn (p, a) => all_var_names (p, a)) [] ps in t @ acc end	  

fun check_pat p =
	not (has_repeats (all_var_names (p, [])))

(* 11. Write a function match that takes a valu * pattern and returns a (string * valu) list option,
namely NONE if the pattern does not match and SOME lst where lst is the list of bindings if it does.
Note that if the value matches but the pattern has no patterns of the form Variable s, then the result
is SOME []. Hints: Sample solution has one case expression with 7 branches. The branch for tuples
uses all_answers and ListPair.zip. Sample solution is 13 lines. Remember to look above for the
rules for what patterns match what values, and what bindings they produce. These are hints: We are
not requiring all_answers and ListPair.zip here, but they make it easier. *)
(* datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu *)
(* datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern *)
fun listlen xs = List.foldr (fn (_, acc) => acc + 1) 0 xs

fun match (_, Wildcard) = SOME []
  | match (v, Variable s) = SOME [(s,v)]
  | match (Unit, UnitP) = SOME []
  | match (Const i, ConstP j) = 
  		if i = j 
		then SOME [] 
		else NONE
  | match (Tuple vs, TupleP ps) = 
  		if (listlen vs) <> (listlen ps) 
		then NONE 
		else (all_answers match (ListPair.zip (vs, ps)))
  | match (Constructor (s1, v), ConstructorP (s2, p)) = 
  		if (s1 <> s2) 
		then NONE 
		else match (v, p)
  | match (_, _) = NONE

(* 
12. Write a function first_match that takes a value and a list of patterns and returns a
(string * valu) list option, namely NONE if no pattern in the list matches or SOME lst where
lst is the list of bindings for the first pattern in the list that matches. Use first_answer and a
handle-expression. Hints: Sample solution is 3 lines. *)
fun first_match v ps = 
	let fun f (p: pattern) = match (v, p)
	in
		SOME (first_answer f ps) handle NoAnswer => NONE
	end
(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)