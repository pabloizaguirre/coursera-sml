(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option(s1 : string, slist : string list) =
    case slist of
        [] => NONE
      | x::xs => if same_string(s1, x)
                 then SOME xs
                 else case all_except_option(s1, xs) of
                          NONE => NONE
                        | SOME i => SOME (x :: i)

fun get_substitutions1 (subs : string list list, s : string) = 
    case subs of
        [] => []
      | x::xs => case all_except_option(s, x) of
                    NONE => get_substitutions1(xs, s)
                  | SOME i => i @ get_substitutions1(xs, s)

fun get_substitutions2 (subs : string list list, s : string) =
    let 
        fun aux(subs : string list list, s : string, ans : string list) =
            case subs of
                [] => ans
              | x::xs => aux(xs, s, case all_except_option(s, x) of
                                        NONE => ans
                                      | SOME i => ans @ i)
    in
        aux(subs, s, [])
    end

fun similar_names(subs : string list list, fullname : {first : string, middle : string, last : string}) = 
    let
        fun build_names_list(slist : string list, fullname : {first : string, middle : string, last : string}) = 
            case slist of
                [] => []
              | x::xs => case fullname of
                            {first = _, middle = m, last = l} => {first = x, middle = m, last = l}::build_names_list(xs, fullname)
    in
        case fullname of
            {first = i, middle = _, last = _} => fullname::build_names_list(get_substitutions1(subs, i), fullname)
    end

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

fun card_color(card1 : card) =
    case card1 of 
        (Clubs, _) => Black
      | (Spades, _) => Black
      | (Diamonds, _) => Red
      | (Hearts, _) => Red

fun card_value (card1 : card) = 
    case card1 of  
        (_, Num i) => i
      | (_, Ace) => 11
      | (_, _) => 10

fun remove_card(cs : card list, c : card, e : exn) = 
    case cs of
        [] => raise e
      | x::xs => if x = c 
                 then xs
                 else x::remove_card(xs, c, e)

fun all_same_color(cs : card list) =
    case cs of
        [] => true
      | x::[] => true
      | x::xs => case xs of 
                     y::_ => if card_color(x) = card_color(y)
                              then true andalso all_same_color(xs)
                              else false

fun sum_cards(cs : card list) =
    let
        fun aux(cs : card list, sum : int) =
            case cs of
                [] => sum
              | x::xs => aux(xs, sum + card_value(x))
    in
        aux(cs, 0)
    end

fun score(cs : card list, goal : int) =
    let 
        val sum = sum_cards(cs)
        val prel_score = if sum > goal
        then 3 * (sum - goal)
        else (goal - sum)
    in 
        if all_same_color(cs)
        then prel_score div 2
        else prel_score
    end

fun officiate(cs : card list, ms : move list, goal : int) =
    let
        fun aux(cs : card list, ms : move list, goal : int, held_cards : card list) =
            case ms of 
                [] => score(held_cards, goal)
              | (Discard i)::xs => aux(cs, xs, goal, remove_card(held_cards, i, IllegalMove))
              | (Draw)::xs => case cs of
                                  [] => score(held_cards, goal)
                                | y::ys => if sum_cards(y::held_cards) > goal
                                           then score(y::held_cards, goal)
                                           else aux(ys, xs, goal, y::held_cards)
    in
        aux(cs, ms, goal, [])
    end


(* challenge problems *)
fun score_challenge(cs : card list, goal : int) =
    let 
        fun sum_cards_ace(cs' : card list) =
            let
                fun aux(cs : card list, sum : int) =
                    case cs of
                        [] => sum
                    | x::xs => aux(xs, sum + card_value(x))
            in
                aux(cs, 0)
            end


        val sum = sum_cards(cs)
        val prel_score = if sum > goal
        then 3 * (sum - goal)
        else (goal - sum)
    in 
        if all_same_color(cs)
        then prel_score div 2
        else prel_score
    end
