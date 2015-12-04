module Esercitazione3

let NomeCognome : string = "Federico Longhin"

// First Function
let rec count l : int =
    match l with
        | [] -> 0
        | z :: zs -> 1 + (count zs)

let rec sum (l : int list) : float =
    match l with
        | [] -> 0.
        | z :: zs -> float(z) + (sum zs)  
    ;;

let arithmetic_mean (l : int list) : float =
    float(sum l) / float(count l)
    ;;

let rec times (l : int list) : float =
    match l with
        | [] -> 1.
        | z :: zs -> float(z) * (times zs)
    ;;

let geometric_mean (l : int list) : float =
    float(times l) ** (1. / float(count l))
    ;;

let rec inverse_sum (l : int list) : float =
    match l with
        | [] -> 0.
        | z :: zs -> (1. / float(z)) + (inverse_sum zs)
    ;;

let harmonic_mean (l : int list) : float =
    float(count l) / (inverse_sum l)
    ;;

let means (l : int list) : (float * float * float) =
    ((arithmetic_mean l), (geometric_mean l), (harmonic_mean l))
    ;;

// Second Function
let find_min (l : int list) : int =
    let mutable min : int = l.Head
    
    let rec find_min_nested (l : int list) : int =
        match l with
            | [] -> min
            | z :: zs -> if z < min then 
                             min <- z 
                             find_min_nested zs
                         else
                             find_min_nested zs
    
    find_min_nested l
    ;;

let find_max (l : int list) : int =
    let mutable max : int = l.Head
    
    let rec find_max_nested (l : int list) : int =
        match l with
            | [] -> max
            | z :: zs -> if z > max then 
                             max <- z 
                             find_max_nested zs
                         else
                             find_max_nested zs
    
    find_max_nested l
    ;;

let min_max (l : int list) : (int * int) =
    ((find_min l), (find_max l))
    ;;

// Third Function
let rec itemAt l index =
   match l with
       | head :: _ when index = 0 -> head
       | _ :: tail -> itemAt tail (index - 1)
       | [] -> failwith "Index out of bounds"
    ;;

let rec find_index_list (l : (int * int) list) (key : int)=
    let mutable index = 0

    let rec find_index_list_nested (l : (int * int) list) =
        if index = (count l) then
            -1
        elif (fst (itemAt l index)) = key then
                index
        else
            index <- index + 1
            find_index_list_nested l
            
    find_index_list_nested l
    ;;

let rec find_max_tuples (l : (int * int) list) =
    let mutable index = 0
    let mutable max : int = (snd (itemAt l 0))
    let mutable key : int = (fst (itemAt l 0))

    let rec find_max_tuples_nested (l : (int * int) list) : int =
        if index = (count l) then
            key
        elif (snd (itemAt l index)) > max then
                max <- (snd (itemAt l index))
                key <- (fst (itemAt l index))
                index <- index + 1
                find_max_tuples_nested l
        else
            index <- index + 1
            find_max_tuples_nested l
            
    find_max_tuples_nested l
    ;;

let rec insert v i l =
    match i, l with
    | 0, xs -> v :: xs
    | i, x :: xs -> x :: insert v (i - 1) xs
    | i, [] -> failwith "Index out of range"

let rec remove i l =
    match i, l with
    | 0, x :: xs -> xs
    | i, x :: xs -> x :: remove (i - 1) xs
    | i, [] -> failwith "Index out of range"

let rec mode (l : int list) : int = 
    let mutable occurrences : (int * int) list = []
    let mutable index = 0
    let mutable first = 0
    let mutable second = 0

    let rec count_occurrences (l : int list) =
        if index <> (count l) then
            let found_index = (find_index_list occurrences (itemAt l index))
            if found_index <> -1 then
                 first <- (fst (itemAt occurrences found_index))
                 second <- ((snd (itemAt occurrences found_index)) + 1)
                 occurrences <- remove found_index occurrences
                 occurrences <- insert (first, second) found_index occurrences
                 index <- index + 1
                 count_occurrences l
            else
                first <- (itemAt l index)
                second <- 1
                occurrences <- (occurrences @ [(first, second)])
                index <- index + 1
                count_occurrences l

    count_occurrences l
    find_max_tuples occurrences
  ;;

(*
    Pseudo-Code Function

    let rec mode (l : int list) : int =
        let mutable element_list = [(l.Head, 1)]
        let mutable new_element : bool = true

        let rec check_element auxiliar_list (element : int) =
            match auxiliar_list with
                | [] -> if new_element then
                            List.append element_list [(element, 1)]
                        else
                            "In posizione dell'elemento (x, y), presente in element_list, assegna (x, (y + 1))"
                | (x, y) :: zs -> if x = element then
                                      new_element <- false
                                      check_element [] element
                                  else
                                      check_element zs element
        
        match l.Tail with
            | [] -> "Trova il PRIMO massimo tra le occorrenze di element_list e ritorna il l'elemento corrispondente"
            | z :: zs -> check_element element_list z
        ;;

    l <- [2; 1; 2; 1; 1; 2]
    element_list <- [(2, 3); (1, 3)]
*)