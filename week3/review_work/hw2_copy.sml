(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(* Problem 1(a) *)
(* Takes a string s and a string list lst, returns NONE if s is not in lst, else returns SOME lst without the string s. *)
(* ASSUME s is in lst at most once *)
fun all_except_option (s, lst) =
    case lst of
        [] => NONE
      | x::xs' => if same_string(s, x) 
                  then SOME xs'
                  else case all_except_option(s, xs') of
                           NONE => NONE
                         | SOME y => SOME (x::y)

(* Problem 1(b) *)  
(* Takes a string list list substitutions, a single string s and returns a string list, which are the strings from the lists in substitutions that contain s, but don't include s itself*)
(*ASSUME: no repeated strings in each list of substitutions. But strings can repeat across different lists in substitutions.*)
fun get_substitutions1 (substitutions, s) = 
    case substitutions of
        [] => []
      | x::xs' => case all_except_option(s, x) of
                      NONE => get_substitutions1(xs', s)
                    | SOME y => y@get_substitutions1(xs', s)

(* Problem 1(c) *)
(* Same as Problem 1(b) but tail recursive *)
fun get_substitutions2 (substitutions, s) =
    let fun aux(substitutions, acc) = 
            case substitutions of
                [] => acc
              | x::xs' => case all_except_option(s, x) of
                              NONE => aux(xs', acc)
                            | SOME y => aux(xs', acc@y)
    in 
        aux(substitutions, [])
    end



(* Problem 1(d) *)
(* Takes a string list list substitutions and a full name, returns a full name list*)
fun similar_names (substitutions, fullname) =
    let val {first=x, middle=y, last=z} = fullname
        val names = get_substitutions2(substitutions, x)
        fun aux(names, acc) =
            case names of
                [] => fullname::acc
              | x::xs' => aux(xs', {first=x, middle=y, last=z}::acc)
    in
        aux(names, [])
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

(* Problem 2(a) *)
(* Takes a card and returns the color of the card *)
fun card_color (cd) =
    case cd of
        (suit, _) => if suit = Clubs orelse suit = Spades
                     then Black
                     else Red

(* Problem 2(b) *)
(* Takes a card and returns the value *)
fun card_value (cd) =
    case cd of
        (_, Num value) => value
      | (_, rank) => if rank = Ace then 11
                     else 10


(* Problem 2(c) *)
(* Removes the first card c from the given card list cs, raise an exception if not found*)
fun remove_card (cs, c, e) =
    case cs of 
        [] => raise e
      | x::xs' => if c = x
                  then xs'
                  else remove_card(xs', c, e)

(* Problem 2(d) *)
(* Returns true if all cards in the card list cs are of the same color. Also returns true if there are no cards or only one card in the given list.*)
fun all_same_color (cs) = 
    case cs of
        c1::c2::cs' => card_color c1 = card_color c2 andalso all_same_color(c2::cs')
      | _ => true 

(* Problem 2(e) *)
(* Returns sum of values of cards in card list cs with tail recursion and constant stack space use per function call. *)
fun sum_cards (cs) =
    let fun aux (cs, acc) = 
            case cs of
                [] => acc
              | x::xs' => aux(xs', acc+ card_value(x))
    in
        aux(cs, 0)
    end
        

(* Problem 2(f) *)
(* Takes a card list cs and an int goal and computes a score as described by the rules in the question.*)
fun score (cs, goal) =
    let val cs_sum = sum_cards(cs)
        val prelim = if cs_sum > goal then 3 * (cs_sum - goal) else goal - cs_sum
    in
        if all_same_color(cs) then prelim div 2 else prelim
    end

(* Problem 2(g) *)
(* Runs a game. Takes a card list cs, a move list ms, an int goal and returns a score at the end of the game. *)
fun officiate (cs, ms, goal) =
    let fun aux (cs, ms, acc) =
            case (cs, ms) of
                (_, []) => score(acc, goal)
               |([], move::moves) => (case move of
                                          Draw => score(acc, goal)
                                        | Discard c => aux([], moves, remove_card(acc, c, IllegalMove)))
               |(card::cards, move::moves) => case move of
                                                  Discard c => aux(card::cards, moves, remove_card(acc, c, IllegalMove))
                                                | Draw => if sum_cards(card::acc) > goal
                                                          then score(card::acc, goal)
                                                          else aux(cards, moves, card::acc)

    in
        aux(cs, ms, [])
    end
