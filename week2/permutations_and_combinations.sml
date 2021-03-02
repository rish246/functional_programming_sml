fun factorial ( n : int) =

    if ( n = 0 orelse n = 1)
    then 1
    else n * factorial(n-1)

fun permutation(n : int, x : int) =

    factorial(n) div factorial(x)
(* make a function vector(start, end) which creates a list from start to end  *)
fun vector(from : int, to : int) =

    if from = to
    then
	to::[]
    else
	from :: vector(from + 1, to)


fun search(values : int list, key : int) =
    if null values
    then
	false
    else
	if hd values = key
	then
	    true
	else
	    search(tl values, key)

		  
    
(* Enough practice... now we will solve hw problems *)
