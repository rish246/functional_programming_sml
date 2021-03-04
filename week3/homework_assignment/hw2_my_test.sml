val q1_test_list1 = ["First", "Second", "Third"]
val q1_test_list2 = []

val search_item = "Second"

val test1 = all_except_option(search_item, q1_test_list1) = SOME ["Third", "First"]
val test2 = all_except_option(search_item, q1_test_list2) = NONE
val test3 = all_except_option ("string", ["string"]) = SOME []


val test3 = get_substitutions1 ([["foo"],["there"]], "foo") = []
val test4 = get_substitutions1 ([["foo"],["there"]], "foo") = []
val test5 = get_substitutions1([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],
"Fred") = ["Fredrick","Freddie", "F"]
(* val test6 = get_substitutions1([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]],
"Jeff") = ["Jeffrey","Geoff","Jeffrey"] *)

val test6 = get_substitutions1([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]],"Jeff") = ["Jeffrey","Geoff","Jeffrey"]



val test7 = get_substitutions1([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]],"Jeff") = get_substitutions2([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]],"Jeff")
val test8 = get_substitutions1([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "Fred") = get_substitutions2([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "Fred")




val test9 = similar_names ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"}) =
	    [{first="Fred", last="Smith", middle="W"}, {first="Fredrick", last="Smith", middle="W"},
	     {first="Freddie", last="Smith", middle="W"}, {first="F", last="Smith", middle="W"}]




val test10  = card_color (Clubs, Num 2) = Black

val test11 = card_value (Clubs, Num 2) = 2
val test12 = card_value (Spades, Jack) = 10
val test13 = card_value (Clubs, Ace) = 11



val test14 = remove_card ([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = []
val test15 = remove_card ([(Hearts, Ace), (Clubs, Num 13), (Hearts, Ace)], (Hearts, Ace), IllegalMove) = [(Hearts, Ace), (Clubs, Num 13)]


val test16 = all_same_color [(Hearts, Ace), (Hearts, Ace), (Diamonds, Num 5)] = true
val test17 = all_same_color [(Hearts, Ace), (Hearts, Ace), (Spades, Num 10)] = false
val test18 = all_same_color [(Spades, Ace), (Clubs, Ace), (Spades, Num 13)] = true


val test19 = sum_cards [(Clubs, Num 2),(Clubs, Num 2)] = 4
val test20 = sum_cards [(Clubs, Ace),(Spades, Queen)] = 21
val test21 = sum_cards [(Clubs, Num 3),(Clubs, Ace)] = 14