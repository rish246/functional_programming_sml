(*Do until loop*)
fun do_until f g x = 
    if g x
    then x (*These two are ints*) 
    else do_until f g (f x)





val test1 = do_until (fn x => x div 2) (fn x => x mod 2 <> 1) 6 = 6
val test2 = do_until (fn x => x div 2) (fn x => x mod 2 <> 1) 3 = 0


(*Write factorial in terms of do_until*)
fun fact n = 
    #1 (do_until 
        (fn (acc, n) => (acc * n, n - 1))
        (fn (_, x) => x <= 1)
        (1, n))


fun fixed_point f x = 
    do_until f 
             (fn x => x = (f x)) 
             x

fun map2 f (v1, v2) = 
    (f v1, f v2)

fun foldr f acc xs =
    case xs of 
        [] => acc 
    |   x :: xs' => f(x, (foldr f acc xs'))

fun map f xs = 
    List.foldr (fn (x, acc) => (f x) :: acc) [] xs


fun filter f xs = 
    List.foldr 
        (fn (x, acc) => if (f x) then x :: acc else acc)
        []
        xs 


datatype bin_tree = Node of int * bin_tree * bin_tree 
                |   Leaf 

val our_tree = Node(3, Node(5, Leaf, Leaf), Leaf)

fun map_tree f our_tree = 
    case our_tree of 
        Leaf => []
    |   Node(v, lefttree, righttree) => [f(v)] @ (map_tree f lefttree) @ (map_tree f righttree)


val test3 = fact 3 = 6
val test4 = fact 5 = 120
val test5 = fact 1 = 1

val test6 = map2 (fn a => a + 1) (3, 5) = (4, 6)
val test7 = map (fn x => x + 1) [1, 3, 2, 5] = [2, 4, 3, 6]
val test8 = map (fn x => x * 2) [] = []

val test9 = foldr (fn (x, acc) => x * acc) 1 [1, 2, 3, 4] = 24
val test10 = filter (fn x => x > 2) [1, 2, 3, 5] = [3, 5] 
val test11 = map_tree (fn x => x + 1) our_tree = [4, 6]