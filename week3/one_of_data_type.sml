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



