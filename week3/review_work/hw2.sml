fun same_string(s1 : string, s2 : string) =
    s1 = s2;

fun all_except_option(str, lst) =
    case lst of
	[] => NONE
    | x::xs' => if same_string(str, x)
		  then SOME xs'
		  else
		      case all_except_option(str, xs') of
			   NONE => NONE
		         | SOME list  => SOME (x::list);


fun get_substitutions1(strll, str)=
    case strll of
	[] => []
      | x::xs' =>
	case all_except_option(str, x) of
	     NONE => get_substitutions1(xs', str)
	   | SOME list => list @ (get_substitutions1(xs', str));
		      
    
fun get_substitutions2(strll, str)=
    let fun helper (str, str12, stridk) =
	case all_except_option(str, str12) of
	     NONE => get_substitutions1(stridk, str)
	   | SOME list => list @ (get_substitutions1(stridk, str))
   in 
	    case strll of
	[] => []
      | x::xs' => helper(str, x, xs')
   end

fun similar_names( lst, fullname : {first:string, middle:string, last:string})=
    let	fun pulp (lst1,fullname3 : {first:string, middle:string, last:string}) =
	    case fullname3 of
		{first=a,middle=b,last=c} => get_substitutions1(lst1, a)
	fun gulp (lst2, fullname2)=
	    case lst2 of
		[] => [fullname2] 
	      | x::xs' => case fullname2 of
  {first=a,middle=b,last=c} => ( (gulp(xs',fullname2)) @ [{first=x,middle=b,last=c}] )	    
    in
	gulp(pulp(lst,fullname),fullname)
    end;
				   
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove;

fun card_color (bebe : card)=
    case bebe of
	(Clubs, rank) => Black : color 
      | (Spades, rank) => Black : color
      | (Diamonds, rank) => Red : color
      | (Hearts, rank) => Red : color;

fun card_value (bebe : card) =
    case bebe of
	(suit, Jack) => 10
      | (suit, Queen) => 10
      | (suit, King ) => 10
      | (suit, Ace)  => 11
      |(suit,  Num i)  => i;

fun remove_card (loc : card list, c : card, excp)=
    case loc of
	[] => raise excp
      | x::xs' => if x = c
		  then xs'
		  else x::(remove_card(xs', c, excp));

fun all_same_color (loc)=
    case loc of
	[] => true
      |  x::[] => true
      | x::y::xs' => if (card_color(x)) <> (card_color(y))
		     then false
		     else all_same_color(y::xs');


fun sum_cards (loc)=
    let val acc = 0
	fun help(loc, acc)=
	    case loc of
		[] => acc
	      | x::xs' => help(xs', ((card_value(x) + acc)));
    in
   help(loc, acc)
    end;

fun score(loc, goal)=
    let val sum = sum_cards(loc)
	fun prelim(loc, goal)=
            if sum_cards(loc) > goal
	    then 3 * (sum - goal)
	    else (goal - sum)
	fun col(loc, goal)=
	    if all_same_color(loc)
	    then ((prelim(loc, goal)) div 2)
	    else (prelim(loc, goal))
    in
	col(loc, goal)
    end;

fun officiate(loc, lom, goal) =
    let exception IllegalMove
	fun game(loc, lom, goal, hand)=
	    case lom of
		[] => (score(hand,goal))
	      | x::xs' =>
		case x of
		   Discard c  => game(loc, xs', goal, (remove_card(hand, c, IllegalMove)))
		 | Draw =>
		    case loc of
			 [] => game([], [], goal, hand)
		       | a::ab' =>
			 if sum_cards((a::hand)) > goal
			 then (score((a::hand),goal))
			 else game(ab', xs', goal,( a::hand))
    in
	game(loc, lom, goal, [])
    end;


	    
					   
							     
			          
			 
			      

						    
						    
	      
		      

			     
	   
