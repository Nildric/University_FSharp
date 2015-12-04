module Esercitazione1

let NomeCognome : string = "Federico Longhin"

let is_date (day : int) (month : int) (year : int) : bool =
    if year < 1 || month < 1 || month > 12 || day < 1 || day > 31 || (month = 2 && day > 28) then
        false
    else
        true
    ;;

let rec month_length (month : int) : int =
    if month = 0 then
        0
    else
        match month with
        | 2 -> 28 + (month_length (month - 1))
        | 4 | 6 | 9 | 11 -> 30 + (month_length (month - 1))
        | _ -> 31 + (month_length (month - 1))
    ;;

let data_to_day (day : int) (month : int) (year : int) : int =
    if is_date day month year = false then
        failwith "!Input is not a date!"
    elif month = 1 then
        day
    else
        day + (month_length (month - 1))
    ;;
        
let data_difference (day1 : int) (month1 : int) (year1 : int) (day2 : int) (month2 : int) (year2 : int) : int =
    if is_date day1 month1 year1 = false || is_date day2 month2 year2 = false then
        failwith "!Input is not a date!"
    else
        let difference : int = year1 - year2
    
        (data_to_day day1 month1 year1) - (data_to_day day2 month2 year2) + (difference * 365)
    ;;

let lunar_cycle (day : int) (month : int) (year : int) : string =
    if is_date day month year = false then
        failwith "!Input is not a date!"
    else
        let difference : int = abs(data_difference day month year 1 1 2000) % 29

        if difference >= 0 && difference <= 3 then
            "LN"
        elif difference >= 4 && difference <= 6 then
            "LR"
        elif difference >= 7 && difference <= 10 then
            "PQ"
        elif difference >= 11 && difference <= 14 then
            "GR"
        elif difference >= 15 && difference <= 18 then
            "LP"
        elif difference >= 19 && difference <= 21 then
            "GA"
        elif difference >= 22 && difference <= 25 then
            "UQ"
        elif difference >= 26 && difference <= 28 then
            "LA"
        else
            failwith "!Error!"
    ;;