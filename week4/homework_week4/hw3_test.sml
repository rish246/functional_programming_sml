val test1 = only_capitals ["A","B","C"] = ["A","B","C"]
val test2 = only_capitals ["A","bc","C"] = ["A", "C"]
val test3 = only_capitals [] = []
val test4 = longest_string1 ["A","bc","C", "Baddy", "Armag"] = "Baddy"

val test5 = longest_string2 ["A","bc","C", "Db"] = "Db"

val test6 = longest_string3 ["A","bc","C"] = "bc"
val test7 = longest_string3 ["A","bc","C", "Baddy", "Armag"] = "Baddy"


val test8 = longest_string4 ["A","B","C"] = "C"
val test9 = longest_string4 ["A","bc","C", "Db"] = "Db"
val test10 = longest_string4 [] = ""
val test11 = longest_string3 [] = ""


val test12 = longest_capitalized ["A","bc","C"] = "A"
val test13 = longest_capitalized ["Ab", "Panda", "Pants", "Ram"] = "Panda"
val test14 = longest_capitalized [] = ""

val test15 = rev_string "abc" = "cba"
val test16 = rev_string "" = ""
val test17 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4
val test18 = first_answer (fn x => if x > 3 then SOME x else NONE) [1, 2, 0, 1] handle NoAnswer => ~1

val test19 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE
val test20 = all_answers (fn x => if x > 3 then SOME [x] else NONE) [4,5,6,7] = SOME [4, 5, 6, 7]
val test21 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [] = SOME []


val test22 = count_wildcards (TupleP [Wildcard, Variable "Var", ConstP 3]) = 1
val test23 = count_wildcards (TupleP [Wildcard, Variable "NewVar", Wildcard, ConstP 5, ConstructorP ("Cons", Wildcard)]) = 3
val test24 = count_wildcards (UnitP) = 0

val test25 = count_wild_and_variable_lengths (Variable("a")) = 1
val test26 = count_wild_and_variable_lengths (TupleP [Wildcard, Variable "NewVar", Wildcard, ConstP 5, ConstructorP ("Cons", Wildcard)]) = 9
val test27 = count_wild_and_variable_lengths (UnitP) = 0

val test28 = count_some_var ("x", Variable("x")) = 1
val test29 = count_some_var ("NewVar", (TupleP [Wildcard, Variable "NewVar", Wildcard, ConstP 5, ConstructorP ("Cons", Wildcard)])) = 1
val test30 = count_some_var ("NotNewVar", (TupleP [Wildcard, Variable "NewVar", Wildcard, ConstP 5, ConstructorP ("Cons", Wildcard)])) = 0


val test31 = check_pat (Variable("x")) = true
val test32 = check_pat (TupleP [Wildcard, Variable "NewVar", Wildcard, ConstP 5, ConstructorP ("Cons", Wildcard)]) = true
val test33 = check_pat (TupleP [Wildcard, Variable "NewVar", Wildcard, ConstP 5, Variable "NewVar", ConstructorP ("Cons", Wildcard)]) = false
val test34 = check_pat (UnitP) = true



(*Tests for function match*)
(* val test35 = match (Unit, UnitP) = SOME []
val test36 = match (Const 13, ConstP 13) = SOME []
val test37 = match (Const 15, ConstP 13) = NONE
val test38 = match ((Tuple [Const 13]), Variable "myName") = SOME [(Tuple [Const 13], "myName")] *)


(* val test39 = match (Constructor ("C1", Const 13), ConstructorP("C1", ConstP 13)) = SOME []
val test40 = match (Constructor ("C1", Const 13), ConstructorP("C2", ConstP 13)) = NONE
val test41 = match (Constructor ("C1", Const 13), ConstructorP("C1", ConstP 17)) = NONE 
val test42 = match (Constructor ("C1", Unit), ConstructorP("C1", UnitP)) = SOME []
val test43 = match (Constructor ("C2", Tuple [Const 13]), ConstructorP("C2", Variable "myName")) = SOME [(Tuple [Const 13], "myName")]
val test44 = match (Const(1), UnitP) = NONE
val test45 = match (Tuple [Constructor ("C2", Tuple [Const 13])], TupleP [ConstructorP("C2", Variable "myName")]) = SOME [(Tuple [Const 13], "myName")]
val test46 = match (Tuple [Constructor ("C2", Tuple [Const 13])], ConstructorP("C2", Variable "myName")) = NONE *)


val test47 = first_match Unit [UnitP] = SOME []

(*Tests for function last*)






(*
val test5 = longest_capitalized ["A","bc","C"] = "A"

val test6 = rev_string "abc" = "cba"

val test7 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4

val test8 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE

val test9a = count_wildcards Wildcard = 1

val test9b = count_wild_and_variable_lengths (Variable("a")) = 1

val test9c = count_some_var ("x", Variable("x")) = 1

val test10 = check_pat (Variable("x")) = true

val test11 = match (Const(1), UnitP) = NONE

val test12 = first_match Unit [UnitP] = SOME [] *)
