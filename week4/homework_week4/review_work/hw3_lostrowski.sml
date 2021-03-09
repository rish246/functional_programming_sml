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

(* Problem 1 *)

val only_capitals = List.filter(fn x => (Char.isUpper o String.sub) (x, 0))

(* Problem 2 *)

val longest_string1 = foldl (fn (x,y) => if String.size x > String.size y then x else y) ""

(* Problem 3 *)

val longest_string2 = foldl (fn (x,y) => if String.size x >= String.size y then x else y) ""

(* Problem 4 *)

fun longest_string_helper f xs = foldl (fn (x,y) =>
    if f(String.size(x), String.size(y)) then x else y) "" xs 

val longest_string3 = longest_string_helper (fn (x,y) => x > y)

val longest_string4 = longest_string_helper (fn (x,y) => x >= y)

(* Problem 5 *)

val longest_capitalized = longest_string1 o only_capitals

(* Problem 6 *)

val rev_string = String.implode o rev o String.explode

(* Problem 7 *)

fun first_answer f xs =
    case xs of
        [] => raise NoAnswer
        | x::xs' => case f x of 
            SOME v => v
            | NONE => first_answer f xs'

(* Problem 8 *)

fun all_answers f xs =
    let fun aux (xs, acc) =
        case xs of
            [] => acc
            | x::xs' => case (f x, acc) of
                (SOME lst1, SOME lst2) => aux(xs', SOME(lst1 @ lst2))
                | _ => NONE

    in
        aux(xs, SOME [])
    end

(* Problem 9a *)

val count_wildcards = g (fn () => 1) (fn _ => 0)

(* Problem 9b *)

val count_wild_and_variable_lengths = g (fn () => 1) String.size

(* Problem 9c *)

fun count_some_var (str, pat) = g (fn () => 0) (fn var => if str = var then 1 else 0) pat

(* Problem 10 *)

fun check_pat pat =
    let fun aux(pat, acc) =
        case pat of
            Variable(str) => str::acc
            | TupleP(xs) => foldl (fn (p,y) => aux(p, acc) @ y) acc xs
            | ConstructorP(_,pat) => aux(pat, acc)
            | _ => []
    val list = aux(pat, [])
    fun f (xs,acc) =
        case xs of 
	        [] => acc
            | x::xs' => not(List.exists (fn v => v = x) xs') andalso f(xs', acc)
    in
        f(list, true)
    end

(* Problem 11 *)

fun match (valu, pat) = case (pat, valu) of
    (Wildcard, _) => SOME []
    | (Variable s, Unit) => SOME [(s, Unit)]
    | (Variable s, Const int) => SOME [(s, Const(int))]
    | (Variable s, Tuple lst ) => SOME [(s, Tuple(lst))]
    | (Variable s, Constructor(str,valu)) => SOME [(s, Constructor(str,valu))]
    | (UnitP, Unit) => SOME []
    | (ConstP x, Const y) => if x = y then SOME [] else NONE 
    | (TupleP ps, Tuple vs) => all_answers (fn(v,p) => match(v,p)) (ListPair.zipEq(vs, ps)
        (* passing something that won't match - handling unequal lists length *)
        handle UnequalLengths => [(Const(17), UnitP)])
    | (ConstructorP(s1, p), Constructor(s2, v)) => if s1 = s2 andalso isSome(match(v,p))
        then match(v,p) else NONE
    | _ => NONE

(* Problem 12 *)

fun first_match valu ps = SOME (first_answer (fn p => match(valu, p)) ps)
    handle NoAnswer => NONE
