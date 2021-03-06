val test16 = all_same_color [(Hearts, Ace), (Hearts, Ace), (Diamonds, Num 5)] = true
val test17 = all_same_color [(Hearts, Ace), (Hearts, Ace), (Spades, Num 10)] = false
val test18 = all_same_color [(Spades, Ace), (Clubs, Ace), (Spades, Num 13)] = true
val test19 = all_same_color [] = true
val test20 = all_same_color [(Spades, Ace)] = true