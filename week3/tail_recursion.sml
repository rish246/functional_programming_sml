exception InvalidListException

fun reverse list = 
    case list of 
        [] => []
        | x :: xs' => reverse(xs') @ [x]


fun reverse_tl_recursion list = 
    let
       fun aux(our_list, acc) =
            case our_list of
                [] => acc
                | x :: xs' => aux(xs', x :: acc)
    in
      aux(list, [])
    end

val test_list = [1, 3, 2, 5]

val rev_list = (reverse test_list) 

val test_1 = rev_list = [5, 2, 3, 1]

val test_2 = reverse_tl_recursion test_list = rev_list