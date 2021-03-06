(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

fun all_except_option (s : string, sl : string list) =
    case sl of
        [] => NONE
      | x::xs' => if same_string(x,s)
                  then SOME(xs')
                  else case all_except_option(s,xs') of
                           NONE => NONE
                         | SOME y => SOME(x :: y)

fun get_substitutions1 (sll : string list list, s:string ) =
    case sll of
        [] => []
      | x::xs' => case all_except_option(s,x) of
                      NONE => get_substitutions1(xs', s)
                    | SOME y  => y @ get_substitutions1(xs', s)

fun get_substitutions2 (sll : string list list, s : string) =
    let
        fun helper (s : string, tp : string list list, r : string list) =
            case tp of
                [] => r
              | x::xs => case all_except_option(s,x) of
                             NONE => helper(s,xs,r)
                                  | SOME y => helper(s,xs,r @ y)
    in
        helper(s,sll,[])
    end

fun similar_names (sll : string list list, {first = f, middle = y, last = z}) =
    let
        val substitutions = get_substitutions2(sll, f)
        fun helper (sl : string list) =
            case sl of
                [] => []
              | x::xs => {first = x, middle = y, last = z} :: helper(xs)
    in
        {first = f, middle = y, last = z} :: helper(substitutions)
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

fun card_color((s,r) : card) =
    case s of
        Clubs => Black
      | Spades => Black
      | _ => Red

fun card_value((s,r) : card) =
    case r of
        Num(i) => i
      | Ace => 11
      | _ => 10

fun remove_card(cs : card list, c : card, e) =
    case cs of
        [] => raise e
      | x::xs => if x = c
                 then xs
                 else x :: remove_card(xs,c,e)

fun all_same_color (cs : card list) =
    case cs of
        [] => true
      | head::[] => true
      | head::(neck::body) => (card_color(head) = card_color(neck))
                              andalso all_same_color((neck::body))

fun sum_cards (cs : card list) =
    let
        fun helper (cs : card list, sum : int) =
        case cs of
            [] => sum
               | x::xs => helper(xs, sum + card_value(x))
    in
        helper(cs,0)
    end

fun score (held_cards : card list, goal : int) =
    let
        val sum = sum_cards(held_cards)
        val preliminary_score = if sum > goal
                                then 3 * (sum - goal)
                                else (goal - sum)
    in
        if all_same_color(held_cards) then preliminary_score div 2 else preliminary_score
    end
fun officiate (deck : card list, moves : move list, goal : int) =
    let
        fun helper (deck : card list, moves : move list, hand : card list ) =
            case moves of
                [] => score(hand, goal)
              | Discard (y) :: ys => helper ( deck, ys, remove_card(hand, y, IllegalMove))
              | draw :: ys  => case deck of
                             [] => score(hand,goal)
                                | x::xs => let val new_hand = x::hand
                                           in
                                               if sum_cards(new_hand) < goal
                                               then helper(xs, ys, new_hand)
                                               else score(new_hand,goal)
                                           end
    in
        helper(deck, moves,[])
    end
