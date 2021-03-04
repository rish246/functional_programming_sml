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
            |   hd_str :: tl_strs => if same_string(hd_str, our_string) then aux(tl_strs, acc, true) else aux(tl_strs, hd_str :: acc, found)
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
                        | SOME e => get_substitutions1(tl_subs, name) @ e
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