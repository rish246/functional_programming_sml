val test25 = officiate ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                        [Draw,Draw,Draw,Draw,Draw],
                        42) = 3

val test13 = ((officiate([(Clubs,Jack),(Spades,Num(8))],
                         [Draw,Discard(Hearts,Jack)],
                         42);
               false) 
              handle IllegalMove => true) 

val test14 = officiate ([],
                        [Draw],
                        42) = 21

val test15 = officiate( [(Clubs, Num(8)), (Spades, Jack)],
                        [], 
                        15) = 7

(*What do i need to return in the end then... Is it not the score *)
(*Test cases for officiate*)