(* Binary Tree Implementation in SML *)
(* Make a TreeNode datatype *)
datatype 'a node = Leaf of 'a
                |  Node of 'a * ('a node) * ('a node)
        
(* Lets contruct a node *)
val our_tree = Node(3, Leaf 3, Leaf 3)

fun sum_tree tree = 
    case tree of 
        Leaf v => v
       | Node(v, left, right) => v + sum_tree left + sum_tree right

val sum_our_tree = sum_tree our_tree = 9