module Esercitazione5

let NomeCognome : string = "Federico Longhin"

let rec listSize list : int =
    match list with
        | [] -> 0
        | z :: zs -> 1 + (listSize zs)

let rec itemAt list index =
   match list with
       | head::_ when index = 0 -> head
       | _::tail -> itemAt tail (index - 1)
       | [] -> failwith "Index Error"
    ;;

let rec remove i l =
    match i, l with
        | (0, z :: zs) -> zs
        | (i, z :: zs) -> z :: remove (i - 1) zs
        | (i, []) -> failwith "Index Error"
    ;;

let rec eval_poly (coeff : float list) (x : float) : float = 
    if (listSize coeff) = 0 then
        0.0
    else
        let n = ((listSize coeff) - 1)
        let head = (itemAt coeff 0)
        let newList = (remove 0 coeff)
        (head * (x ** float(n))) + (eval_poly newList x)
    ;;

let rec find_min_nested (inputList : int list) (min : int) (index : int) : int = 
    if index = listSize inputList then
            min
    elif itemAt inputList index < min then
        find_min_nested inputList (itemAt inputList index) (index + 1)
    else
        find_min_nested inputList min (index + 1)
    ;;

let rec find_min (l : int list) : int =    
    if (listSize l) = 0 then
        failwith "La Lista e' Vuota"

    find_min_nested l (itemAt l 0) 0
  ;;

let rec levenshtein_distance (w1 : char list) (w2 : char list) : int = 
    if (listSize w1) = 0 then
        listSize w2
    elif (listSize w2) = 0 then
        listSize w1
    else
        if (itemAt w1 ((listSize w1) - 1)) = (itemAt w2 ((listSize w2) - 1)) then
            find_min [
                (levenshtein_distance (remove (listSize w1 - 1) w1) w2) + 1; 
                (levenshtein_distance w1 (remove (listSize w2 - 1) w2)) + 1; 
                (levenshtein_distance (remove (listSize w1 - 1) w1) (remove (listSize w2 - 1) w2)) + 0
            ]
        else
            find_min [
                (levenshtein_distance (remove (listSize w1 - 1) w1) w2) + 1; 
                (levenshtein_distance w1 (remove (listSize w2 - 1) w2)) + 1; 
                (levenshtein_distance (remove (listSize w1 - 1) w1) (remove (listSize w2 - 1) w2)) + 1
            ]
    ;;

let sign (num : float) : int = 
    if num > 0.0 then
       1
    elif num < 0.0 then
        -1
    else 0
;;

let rec zeros (coeff : float list) (threshold : float) (min : float, max : float) : float = 

    let pivot : float = ((min + max) / 2.0)
    if (abs (eval_poly coeff pivot)) <= threshold then
        pivot
    elif (sign (eval_poly coeff pivot)) = (sign (eval_poly coeff min)) then
        zeros coeff threshold (pivot, max)
    else
        zeros coeff threshold (min, pivot)
;;