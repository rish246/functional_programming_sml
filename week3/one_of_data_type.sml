(* Simple expression evaluator using sml *)

(* Define grammar for an expression ---> Can be shown using one-of type *)
datatype exp = Number of int
            |  Multiply of exp * exp
            |  Addition of exp * exp 
            |  Negation of exp
            (* |  Negation of exp *)
            
            


val test_expression : exp = Multiply(Addition(Number 2, Number 3), Number 2)

fun eval (e : exp) = 
    case e of
       Number i => i
    |  Multiply(e1, e2) => eval(e1) * eval(e2)
    | Addition(e1, e2) => eval(e1) + eval(e2)
    | Negation(e3)  =>  ~ (eval e3)




fun max_constant (e : exp) =  
    case e of 
        Number e1 => e1
        | Negation e1 => max_constant e1
        | Addition(e1, e2) => Int.max(max_constant e1, max_constant e2)
        | Multiply(e1, e2) => Int.max(max_constant e1, max_constant e2)



(* make List as an inbuilt data type *)
datatype intList = Empty 
                |  Cons of int * intList

val my_new_list : intList = Cons(3, Empty)

(* Get the head of our list *)
fun get_hd our_list = 
    case our_list of 
        Empty => 0
       | Cons(hd_val, our_list') => hd_val


fun get_tl our_list = 
    case our_list of 
        Empty => Empty
        | Cons(hd_val, our_list') => our_list'



