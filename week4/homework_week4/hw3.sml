(* Coursera Programming Languages, Homework 3, Provided Code *)
exception NoAnswer



(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

fun only_capitals strings = 
    let val filterCapitals = List.filter (fn s => Char.isUpper (String.sub(s, 0)))
    in 
        filterCapitals strings
    end

fun longest_string1 strings = 
    let val getLongestStr = List.foldl (fn (s, acc) => if String.size acc < String.size s then s else acc) ""
    in 
        getLongestStr strings
    end

fun longest_string2 strings = 
    let val getLongestStr = List.foldl (fn (s, acc) => if String.size acc <= String.size s then s else acc) ""
    in 
        getLongestStr strings
    end

fun longest_string_helper func strings = 
    let 
        fun longer (s1, s2) = if func(String.size s1, String.size s2) then s1 else s2
        val resFunc = List.foldl longer ""
    in 
        resFunc strings
    end 

val longest_string3 = longest_string_helper (fn (s1_sz, s2_sz) => s1_sz > s2_sz) 
val longest_string4 = longest_string_helper (fn (s1_sz, s2_sz) => s1_sz >= s2_sz) 

fun longest_capitalized strings = 
    (longest_string3 o only_capitals) strings

fun rev_string input = 
    (String.implode o List.rev o String.explode) input

fun first_answer func list = 
    case list of 
        [] => raise NoAnswer 
    |   v :: vs => 
                case func(v) of 
                    NONE => first_answer func vs 
                |   SOME v' => v'

fun all_answers func list = 
    let 
        fun get_valid_elements(valid_els, vs, found_none) =
            case vs of 
                [] => (valid_els, found_none) 
            |   v :: vs' => 
                        case func(v) of 
                            NONE => ([], true)
                        |   SOME v' => get_valid_elements(valid_els @ v', vs', found_none)

        val (right_elements, found_none) = get_valid_elements([], list, false)
    in 
        if found_none then NONE else SOME right_elements
    end







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


(*Problem number : 9*)
val count_wildcards = g (fn () => 1) (fn x => 0)

(*Poor style to call fn x => otherFunc x --> just call f*)

val count_wild_and_variable_lengths = g (fn () => 1) String.size

fun count_some_var (s, p) = g (fn () => 0) (fn x => if x = s then 1 else 0) p

fun check_pat pat = 
    let 
        fun is_valid (input_pattern) = 
            case input_pattern of   
                Variable x => (count_some_var(x, pat) = 1)
            |   TupleP ps => List.foldl (fn (p,i) => (is_valid p) andalso i) true ps 
            |   _ => true
    in 
        is_valid pat 
    end



fun match (value, pat) = 
    case (value, pat) of 
        (_, Wildcard) => SOME []
    |   (x, Variable y) => SOME [(y, x)]
    |   (Unit, UnitP) => SOME []
    |   (Const x, ConstP y) => if x = y then SOME [] else NONE
    |   (Constructor (s1, val_nested), ConstructorP(s2, pat_nested)) => if s1 = s2 
                                                                        then match(val_nested, pat_nested)
                                                                        else NONE
    |   (Tuple vs, TupleP ps) => all_answers match (ListPair.zip(vs, ps))
    |   _ => NONE


fun first_match value patterns = 
    let val matches = fn p => match(value, p)
    in 
        (SOME (first_answer matches patterns)) handle NoAnswer => NONE
    end
