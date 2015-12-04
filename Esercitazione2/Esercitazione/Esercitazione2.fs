module Esercitazione2

let NomeCognome : string = "Federico Longhin"

let return_leap_days (year : int) : int =
    if year % 400 = 0 || (year % 4 = 0 && year % 100 <> 0) then
        29
    else
        28
    ;;

let is_date (day : int) (month : int) (year : int) : bool =
    if year < 1 || month < 1 || month > 12 || day < 1 || day > 31 || (month = 2 && day > 29) then
        false
    else
        true
    ;;

let rec month_length (month : int) (year : int) : int =
    if month = 0 then
        0
    else
        let leap_days : int = return_leap_days year

        match month with
        | 2 -> leap_days + (month_length (month - 1) year)
        | 4 | 6 | 9 | 11 -> 30 + (month_length (month - 1) year)
        | _ -> 31 + (month_length (month - 1) year)
    ;;

let data_to_day (day : int) (month : int) (year : int) : int =
    if month = 1 then
        day
    else
        day + (month_length (month - 1) year)
    ;;
        
let rec data_difference (day1 : int) (month1 : int) (year1 : int) (day2 : int) (month2 : int) (year2 : int) : int =
    if is_date day1 month1 year1 = false || is_date day2 month2 year2 = false then
        failwith "Input is not a well formatted date!"
    elif year1 = year2 then
            (data_to_day day1 month1 year1) - (data_to_day day2 month2 year2)
        elif year2 < year1 then
            (data_to_day day1 month1 year1) + (data_difference 31 12 (year1 - 1) day2 month2 year2)
        else
            -1 * (data_difference day2 month2 year2 day1 month1 year1)
    ;;

let rec num_length (n : int) =
    if n < 10 then
        1
    else
        (num_length (n / 10)) + 1
    ;;

let rec raise (num_base : int) (exponent : int) : int =
    if exponent = 1 then
        num_base
    else
        num_base * (raise num_base (exponent - 1))
    ;;

let rec reverse_number (n : int) : int =
    if n < 10 then
        n
    else
        let base_of_number : int = raise 10 ((num_length n) - 1)
        let digit : int = (n % 10) * base_of_number
        digit + reverse_number (n / 10)
    ;;

let next_blue_moon (day : int) : int =
    (15 - (day % 29 + 29) % 29 + 29) % 29
    ;;

let month_real_length (month : int) (year : int) : int =
    if month = 0 then
        0
    else
        let leap_days : int = return_leap_days year

        match month with
        | 2 -> leap_days
        | 4 | 6 | 9 | 11 -> 30
        |_ -> 31
    ;;

let blue_moon (year : int) : int =
    let mutable blue_moon_month : int = 1

    let rec blue_moon_nested (year : int) =
        if blue_moon_month > 12 then
            failwith "NON TROVATO"
        else
            let difference : int = data_difference 1 blue_moon_month year 1 1 2000
            let next : int = next_blue_moon difference
            let day : int = (2 * next) + 29
        
            if day <= month_real_length blue_moon_month year then
                let auxiliar : int = blue_moon_month
                blue_moon_month <- 1
                auxiliar
            else
                blue_moon_month <- blue_moon_month + 1
                blue_moon_nested year

    blue_moon_nested year
    ;;