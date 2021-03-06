(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option(our_string, given_strings) = 
    let
        fun aux (given_strings, acc, found) = 
            case given_strings of 
                [] => (found, acc)
            |   hd_str :: tl_strs => 	if same_string(hd_str, our_string) 
					then aux(tl_strs, acc, true) 
					else aux(tl_strs, hd_str :: acc, found)
    in
      case aux (given_strings, [], false) of
        (false, _) => NONE
    |   (true, res_list) => SOME res_list
    end
    

fun get_substitutions1(substitutions, name) = 
    case substitutions of 
        [] => []
    |   hd_sub :: tl_subs => 
                let val remaining_names = all_except_option(name, hd_sub)
                in
                    case remaining_names of 
                        NONE => get_substitutions1(tl_subs, name)
                        | SOME e => e @ get_substitutions1(tl_subs, name) 
                end

fun get_substitutions2(substitutions, name) = 
    let fun aux(subs, acc) = 
            case subs of 
                [] => acc
            |   hd_sub :: tl_subs => 
                    let val remaining_names = all_except_option(name, hd_sub)
                    in
                        case remaining_names of 
                            NONE => aux(tl_subs, acc)
                        |   SOME e => aux(tl_subs, e @ acc)
                    end
    in
        aux(substitutions, [])
      
    end


fun similar_names(substitutions, {first = f_name, middle = m_name, last = l_name}) = 
    let fun aux(f_names, acc) = 
            case f_names of 
                [] => acc
            |   f_n :: f_ns =>  aux(f_ns, {first = f_n, last = l_name, middle = m_name} :: acc)
    in
        {first = f_name, last = l_name, middle = m_name} :: aux(get_substitutions2(substitutions, f_name), [])
    end



datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
fun card_color(sample_card) = 
    case sample_card of 
    (Clubs, _) => Black 
    | (Spades, _) => Black 
    | (Diamonds, _ ) => Red 
    | (Hearts, _) => Red 


fun card_value (sample_card) = 
    case sample_card of
    (_, Num e) => e
    | (_, Ace) => 11
    | _ => 10

(* Make this function work properly --> Handle exceptions the right way *)
fun remove_card(cards, c, exp) = 
    case cards of 
	[] => raise exp
	| crd :: crds => if c = crd then crds else c :: remove_card(crds, c, exp)        

fun all_same_color(cards) = 
    case cards of 
        [] => true
    |  [c] => true
    |  hd_card :: neck_card :: rem_cards => (card_color hd_card = card_color neck_card) andalso all_same_color(neck_card :: rem_cards)
    


fun sum_cards(cards) = 
    let fun aux(cards, acc) = 
        case cards of 
            [] => acc
        |   c :: cs => aux(cs, acc + card_value(c))
    in
        aux(cards, 0)
    end

fun score(card_list, goal) = 
    let
        val sum_list = sum_cards(card_list) (*0 for Empty list*)
        val are_cards_same = all_same_color(card_list) (* True for empty list *)
        val prelim_sum = if sum_list > goal 
                        then (3 * (sum_list - goal)) 
                        else goal - sum_list
    in
        if are_cards_same 
        then prelim_sum div 2
        else prelim_sum
    end


fun officiate(card_list, move_list, goal) =
    let fun helper(move_list, card_list, held_cards) = 
            case move_list of 
                [] => score(held_cards, goal)
            |   Discard c :: rem_moves => helper(rem_moves, card_list, remove_card(held_cards, c, IllegalMove))
            |   Draw :: rem_moves => 
                        case card_list of 
                            [] => score(held_cards, goal) 
                        |   c :: cs =>  if sum_cards (c::held_cards) > goal 
                                        then score(c::held_cards, goal) 
                                        else helper(rem_moves, cs, c::held_cards)

    in helper(move_list, card_list, [])
    end

(*SOME TESTS FAILED TO RUN --> LETS SEE WHAT ARE THOSE TESTS*)
(*At least 85 marks we will get... If Not then fuck it*)
