exception NoAnswer

val only_capitals = List.filter (fn str => Char.isUpper (String.sub(str, 0)))

val longest_string1 = foldl (fn (str, acc) => if String.size(str) > String.size(acc)
					   then str else acc) ""

val longest_string2 = foldl (fn (str, acc) => if String.size(str) >= String.size(acc)
					   then str else acc) ""

fun longest_string_helper bool_fun =
    foldl (fn (str, acc) => if bool_fun(String.size(str), String.size(acc))
			    then str else acc) ""

val longest_string3 = longest_string_helper (fn (str_size, acc_size) => str_size > acc_size)

val longest_string4 = longest_string_helper (fn (str_size, acc_size) => str_size >= acc_size)

(*5*)
val longest_capitalized = longest_string1 o only_capitals

fun rev_string str = String.implode (rev (String.explode str))

fun first_answer conv_fn lo_args =
    case lo_args of
	[] => raise NoAnswer
      | arg::rest => case conv_fn arg of
			 SOME ans => ans
		       | NONE => first_answer conv_fn rest 

fun all_answers conv_fn lo_args =
    let fun loc_all_answers (lo_args, accum) =
	    case lo_args of
		[] => accum
	      | arg::rest => case conv_fn arg of
				 SOME ans => loc_all_answers (rest, SOME(valOf(accum) @ ans))
			       | NONE => NONE 
    in loc_all_answers (lo_args, (SOME [])) end
					   

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

val count_wildcards = g (fn _ => 1) (fn _ => 0) 

val count_wild_and_variable_lengths = g (fn _ => 1) (fn str => String.size(str))

fun count_some_var (s, p) = g (fn _ => 0) (fn x => if x = s then 1 else 0) p

fun check_pat p =
    let fun pat_to_str p =
	    case p of
	        Variable x        => [x]
	      | TupleP ps         => List.foldl (fn (p,i) => i @ (pat_to_str p)) [] ps
	      | ConstructorP(_,p) => pat_to_str p 
	      | _                 => []
	fun not_unique_str los =
	    case los of
		[] => false
	      | s::rest => (List.exists (fn el => (s = el)) rest)
			   orelse not_unique_str rest
    in not (not_unique_str (pat_to_str p)) end

fun match (v, p) =
    case p of 
	Wildcard          => SOME []
      | Variable s        => SOME [(s, v)]
      | UnitP             => (case v of Unit => SOME [] 
				      | _ => NONE)
      | ConstP x          => (case v of Const y => if x = y then SOME [] else NONE
				      | _ => NONE)
      | TupleP ps         => (case v of Tuple vs => if List.length(ps) = List.length(vs)
						    then all_answers match (ListPair.zipEq (vs, ps))
						    else NONE
				      | _ => NONE) 
      | ConstructorP(s1,x)=> (case v of Constructor(s2,y) => if s1 = s2 then match (y, x) else NONE
				      | _ => NONE)

fun first_match v lop =
    SOME (first_answer (fn p => match(v, p)) lop)
	 handle NoAnswer => NONE

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

fun typecheck_patterns (lo_typ, lo_patt) =
   1 
	
