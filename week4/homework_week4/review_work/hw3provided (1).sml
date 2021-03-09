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
fun only_capitals (strlist : string list) = List.filter (fn x => Char.isUpper (String.sub (x, 0))) strlist

fun longest_string2 (strlist : string list) = List.foldl (fn (x, y )  => if String.size x >= String.size y then x else y)  "" strlist

fun longest_string1 (strlist : string list) = List.foldl (fn (x, y )  => if String.size x > String.size y then x else y)  "" strlist

fun longest_string_helper f  strlist = List.foldl (fn (x, y)=> if f(String.size x, String.size y) then x else y) ""  strlist

val longest_string3 = longest_string_helper (fn (x, y) => x > y)

val longest_string4 = longest_string_helper (fn (x, y) => x >= y)

val longest_capitalized = longest_string1 o only_capitals

val rev_string = implode o rev o explode

fun first_answer f l =
    case l of  (x::xs) => (case (f x) of
		      SOME x => x
		    |  NONE  => first_answer f xs)
	         | []  => raise NoAnswer

fun all_answers f l =
    let fun aux (input, output) =
	    case input of
	     [] => SOME output
	     | x::xs  => (case (f x) of
			   NONE => NONE
			  | SOME x   => aux(xs ,x@output))
    in
         aux (l, [])
    end

fun count_wildcards (p : pattern) =
    case p of
     Wildcard => 1
     |  TupleP (x::xs)  => (count_wildcards x) + (count_wildcards( TupleP xs))
     |  ConstructorP(_, patt)  => count_wildcards patt
     |  _  => 0

fun count_wild_and_variable_lengths(p : pattern) =
    let val wildcount = count_wildcards p in
	let fun aux p =
	  case p of
	    Variable(str) => String.size str
	   |  TupleP (x::xs) => (aux x) + aux(TupleP xs)
	   |  ConstructorP(_, patt) => aux patt
	   | _  => 0
	in
	    wildcount + (aux p)
	end
    end

fun count_some_var (s1 : string,  p : pattern) =
    case p of
	Variable(s1) => 1
     |  TupleP (x::xs)  => count_some_var(s1, x) + count_some_var(s1 , TupleP xs)
     |  ConstructorP(_, patt)  => count_some_var(s1, patt)
     |  _  => 0

fun all_distinct l =
    case l of
	[] => true
     | (x::xs) => if List.exists (fn y => x = y) xs
		  then false
	          else all_distinct xs

fun all_variable_strings (p : pattern) =
    case p of
	Variable(s) => [s]
	|  TupleP (x::xs)  => all_variable_strings(x) @ all_variable_strings(TupleP xs)
     |  ConstructorP(_, patt)  => all_variable_strings(patt)
     |  _  => []
			      
fun check_pat (p : pattern)  = all_distinct (all_variable_strings p)
	
fun match ((v, p) : (valu * pattern)) =
    case p of Wildcard => SOME []
	   |  Variable (s) => SOME [(s, v)]
	   |  UnitP  => if v = Unit
			then SOME []
		        else NONE
	   |  ConstP (i) => if v = Const (i) then SOME []
		            else NONE
	   | ConstructorP (s1, pat)  => (case v of Constructor (s2, vl) => if s1 = s2
									  then match (vl, pat)
									   else NONE
						| _  => NONE)
				
	   |  TupleP ps  => (case v of Tuple vs => let val lp = ListPair.zip(vs, ps)
					          in
                                                     all_answers match lp
						  end
				     | _  => NONE)

fun first_match value  patterns =
    SOME (first_answer(fn x => match(value, x)) patterns)
    handle NoAnswer => NONE
	  	    
