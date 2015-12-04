module Esercitazione4

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

let rec intervals (min : float, max : float) (nsteps : int) : (float * float) list = 
    match nsteps with
        | 0 -> failwith "Numero Minimo di Intervalli = 1"
        | 1 -> [min,max]
        | _ -> let interval : float = (max - min) / float(nsteps)
               [min, min + interval] @ (intervals (min + interval, max) (nsteps - 1))
    ;;

let rec gauss_integral_nested (split : (float * float) list) (result : float) (index : int) : float = 
    if index = (listSize split) then
            result
        else
            let subList = (itemAt split index)
            let first : float = (fst subList)
            let second : float = (snd subList)

            gauss_integral_nested split (result + (((exp (-1.0 * (first ** 2.0))) + (exp (-1.0 * (second ** 2.0)))) * (abs (first - second)) / 2.0)) (index + 1)
    ;;


let rec gauss_integral (interval : (float * float)) (nsteps : int) : float = 
    gauss_integral_nested (intervals (fst interval, snd interval) nsteps) 0.0 0
    ;;

let rec split_eq_nested (inputList : int list) (result : int list list) (tempList : int list) (index : int) =  
    if index < (listSize inputList) then
            if (listSize tempList) = 0 || (itemAt tempList ((listSize tempList) - 1)) <> (itemAt inputList index) then
                split_eq_nested inputList result (tempList @ [(itemAt inputList index)]) (index + 1)
            else
                split_eq_nested inputList (result @ [tempList]) [(itemAt inputList index)] (index + 1)
        else
            (result @ [tempList])

let rec split_eq (l : int list) : int list list = 
    if (listSize l) = 0 then
        []
    else
       split_eq_nested l [] [] 0
  ;;