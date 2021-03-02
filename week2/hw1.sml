fun is_older (date_first : int * int * int, date_second : int * int * int) = 
    
    if #1 date_first < #1 date_second
    then 
        true
    else
        if #1 date_first > #1 date_second
        then
            false
        else
            (* Years are same ... Check for months *)
            if #2 date_first < #2 date_second
            then 
                true
            else
                if #2 date_first > #2 date_second
                then
                    false
                else
                    if #3 date_first < #3 date_second
                    then 
                        true
                    else
                        false
                        



fun number_in_month(dates : (int * int * int) list, month : int) = 
    if null dates
    then 
        0
    else
        let 
            val current_head = hd dates
            val n_months_in_tail = number_in_month(tl dates, month)
        in
            if #2 current_head = month
            then
                n_months_in_tail + 1
            else
                n_months_in_tail

        end


val test_dates = [(2000, 3, 2), (2000, 4, 3), (3432, 3, 4), (3421, 2, 1)];
val test_months = [3, 4, 2, 1];

fun number_in_months(dates : (int * int * int) list, months : int list) = 
    if (null months)
    then 
        []
    else
        let
            val number_in_cur_month = number_in_month(dates, hd months)
        in
            number_in_cur_month :: number_in_months(dates, tl months)
        end


fun dates_in_month(dates : (int * int * int) list, month : int) = 
    if null dates
    then 
        []
    else
        (* Check if head in month... concat head to the list of remaining dates *)
        let 
            val found_dates_in_month = dates_in_month(tl dates, month)
        in
            if #2 (hd dates) = month
            then
                hd(dates) :: found_dates_in_month
            else
                found_dates_in_month
        end


fun dates_in_months(dates : (int * int * int ) list, months : int list) =
    if null months
    then
        []
    else
        if null dates
        then 
            []
        else 
            let 
                val dates_in_cur_month = dates_in_month(dates, hd months)
            in
                dates_in_cur_month @ dates_in_months(dates, tl months)
            end



fun get_nth(strings : string list, index : int) = 
    if index = 1
    then
        hd strings
    else
        get_nth(tl strings, index - 1)


fun date_to_string(date : int * int * int) = 
    let
        val year_string = Int.toString(#1 date)
        val day_string = Int.toString(#3 date)

        val months_in_an_year = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]

        val month_string = get_nth(months_in_an_year, #2 date)
    in
        month_string ^ " " ^ day_string ^ ", " ^ year_string
    end


fun number_before_reaching_sum(numbers : int list, req_sum : int) = 
    let 
        fun accumulative_sum(nums : int list, acc_sum : int, n : int) = 
            if (acc_sum + hd nums) >= req_sum
            then
                n
            else
                accumulative_sum(tl nums, acc_sum + hd nums, n + 1)

    in
        accumulative_sum(numbers, 0, 0)

    end
    


fun what_month(day_of_year : int) = 
    let 
        val list_days_in_months = [31, 28, 31, 30, 31, 30, 31, 30, 31, 30, 31, 30]
    in
        1 + number_before_reaching_sum(list_days_in_months, day_of_year)
    end



fun month_range(from : int, to : int) = 
    if from > to
    then 
        []
    else
        what_month(from) :: month_range(from + 1, to)
        