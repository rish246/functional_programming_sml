fun append_list (xs, ys) = 
    (* We dont need to define the type as long as we are doing something obvious *)
    case xs of 
        [] => ys
        | x::xs' => x :: append_list (xs', ys)

(* Python's zip function *)
(* [1, 2], [3, 4] => [(1, 3), (2, 4)] *)
fun zip_lists (xs, ys) = 
    case (xs, ys) of 
            ([], []) => []
        |   (hd_xs::tl_xs, hd_ys::tl_ys) => (hd_xs, hd_ys) :: zip_lists(tl_xs, tl_ys)


fun is_non_decreasing list = 
    case list of 
            [] => true
        |   [x] => true
        |   hd_l :: (neck_l :: rest_l) => is_non_decreasing(neck_l :: rest_l) andalso hd_l > neck_l


datatype sign = Positive | Negative | Zero

fun mult_sign(x1, x2) = 
    case (x1, x2) of
            (_, Zero)  => Zero
        |   (Zero, _) => Zero
        |   (sign1, sign2) => if sign1 = sign2 then Positive else Negative



val test_mult_sign1 = mult_sign(Positive, Positive) = Positive
val test_mult_sign2 = mult_sign(Negative, Negative) = Positive
val test_mult_sign3 = mult_sign(Negative, Positive)  = Negative
val test_mult_sign4 = mult_sign(Zero, Positive) = Zero






val test_list_one = [2, 3, 2]
val test_list_two = [3, 2, 1]

val test_list_three = [3, 2, 1, 4, 5]
val test_list_four = [0, 1, 0]

val test_one = append_list(test_list_one, test_list_two) = [2, 3, 2, 3, 2, 1]
val test_two = append_list(test_list_three, test_list_four) = [3, 2, 1, 4, 5, 0, 1, 0]


val test_three = is_non_decreasing test_list_two = true
val test_four = is_non_decreasing test_list_four = false