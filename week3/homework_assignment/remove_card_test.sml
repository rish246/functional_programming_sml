val test14 = remove_card ([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = []
val test15 = remove_card ([(Hearts, Ace), (Clubs, Num 13), (Hearts, Ace)], (Hearts, Ace), IllegalMove) = [(Clubs, Num 13), (Hearts, Ace)] 

(* Test Exception cases*)
val test16 = remove_card ([], (Hearts, Ace), IllegalMove) handle IllegalMove => []
(* val test17 = remove_card ([], (), IllegalMove) handle IllegalMove => true *)