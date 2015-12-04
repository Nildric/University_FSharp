let rec somma (n : int) : int =
    let mutable abs_n = abs(n)

    match abs_n with
    | 0 -> 0
    |_ -> n + (somma (n - 1))
    ;;

let rec subtract (n : int) : int =
    if n = 0 then
        0
    else
        (subtract (n-1)) - n
    ;;

let rec somma_intervallo (min : int) (max : int) :int =
    if min > max then
        0
    else
        (somma_intervallo min (max - 1)) + max
    ;;

let rec conta_cifre (n : int) =
    // (if n % 10 then 0) Wrong if n = 10^x && x > 0
    if n < 10 then
        1
    else
        (conta_cifre (n / 10)) + 1
    ;;

(*
    Verifica se esiste un numero compreso tra d e n-1 che divide n.
    Se esiste restituisce false, altrimenti restituisce true.
*)
let rec divide (d : int) (n : int) : bool =
    if n = d then
        false
    elif n % d = 0 then
        true
    else divide (d + 1) n
    ;;

(*
    Dato n >= 2
    Il numero 2 è primo
    Un numero n > 2 è primo se non è divisibile per alcun numero (Divero da 1 e da n) || (Compreso tra 2 e n-1)
*)
let rec is_primo (n : int) : bool =
    if n < 2 then
        false
    elif n = 2 then
        true
    elif divide 2 n then
        false
    else
        true
    ;;

(*
    Algoritmi non funzionanti ne all'interno di F# ne all'interno di CamL ma bisogna stdiarli perchè SWAG!
    Algoritmi corretti nel terzo capitolo della dispensa a pagina 65.

    let rec is_primo n = let divide d n = (n = d) || ((n % d <> 0) && (divide (d+1))) divide 2;;

    let is_primo n = let rec divide d n = 
        if (d = 1) then
            true
        else ((n % d <> 0) && (divide d - 1)) (divide 2 n);;
*)

(*
    Torri di HANOI

    Caso Base -> n = 0
    Passo induttivo -> n > 0
    
    1) Sposto n-1 dischi da A a B usando C come appoggio;
    2) Sposto un disco da A a B;
    3) Sposto n-1 dischi B a C usando A come appoggio.

    #hanoi(3, "A", "B", "C")
    - Sposta un disco da A a C
    - Sposta un disco da A a B
    - Sposta un disco da C a B
    - Sposta un disco da A a C
*)
let move (x : string) (y : string) : string = "Sposta un disco da " + x + " a " + y + "\n";;

let rec hanoi (n : int) (inizio : string) (fine : string) (appoggio : string) : string = 
    if n = 0 then
        ""
    else
        hanoi (n - 1) inizio appoggio fine
        + move inizio fine
        + hanoi (n-1) appoggio fine inizio
    ;;

(*
    Controllo di una lista vuota o piena
*)
let is_null xs : bool =
    match xs with
        | [] -> true
        | _ -> false
    ;;

(*
    Controllo che una lista abia un unico elemento
*)
let single_element xs : bool =
    match xs with
        | [x] -> true
        | _ -> false
    ;;

// Operatore di concatenazione di liste -> @
let example = [1; 2] @ [7; 8];;
let example_with_empty = [1; 2] @ [] @ [3];;
// x :: xs = [x] @ xs -> Nel pattern matching si usa semprel'operatore :: per scomporre una lista

(*
    Append Operator

    append ([1; 2; 3], [4; 5; 6]);;
    val it : int list = [1; 2; 3; 4; 5; 6]
*)
let rec append (xs, ys) =
    match xs with 
        | [] -> ys
        | z :: zs -> z :: (append (zs, ys))
    ;;

(*
    Conta i valori all'interno di una lista
*)
let rec conta list : int =
    match list with
        | [] -> 0
        | z :: zs -> 1 + (conta zs)
    ;;

(*
    Somma i valori allinterno di una lista
*)
let rec somma_lista list : int =
    match list with
        | [] -> 0
        | z :: zs -> z + (somma_lista zs)
    ;;

(*
    Somma i valori dispari allinterno di una lista
*)
let rec somma_lista_dispari list : int =
    match list with
        | [] -> 0
        | z :: zs -> if z % 2 <> 0 then 
                        z + (somma_lista_dispari zs) 
                     else 
                        somma_lista_dispari zs
        (*
            | z :: zs when (z % 2 <> 0) -> z + (somma_lista_dispari zs)
            | z :: zs when (z % 2 = 0) -> somma_lista_dispari zs
        *)
    ;;

(*
    Head && Tail

    hd -> head -> Data una lista non vuota mi ritorna il primo elemento
        hd [1; 2; 3] -> 1
    tl -> tail -> Data una lista non vuota mi ritorna la coda della lista
        tl [1; 2; 3] -> [2; 3]
*)

(*
    combine ([1; 2; 3], ['a'; 'b'; 'c']) = [(1, 'a'); (2, 'b'); (3, 'c')]
*)
let rec combine (first_list, second_list) =
    match (first_list, second_list) with
        | ([], []) -> []
        | (x :: xs, y :: ys) -> (x, y) :: combine (xs, ys)
        | _ -> failwith "ERROR"
    ;;

(*
    split [(1, 'a'); (2, 'b'); (3, 'c')] = ([1; 2; 3], ['a'; 'b'; 'c'])
*)
let rec split list =
    match list with
        | [] -> ([], [])
        | (x, y) :: ls -> let (xs, ys) = (split ls) in (x :: xs, y :: ys)
    ;;

(*
    copia (5, 'R') = ['R'; 'R'; 'R'; 'R'; 'R']
*)
let rec copia (n, x) =
    match n with
        | 0 -> []
        | _ -> x :: copia (n - 1, x)
    ;;

(*
    duplica [1; 2; 3] = [1; 1; 2; 2; 3; 3]
*)
let rec duplica list =
    match list with
        [] -> []
        | x :: xs -> x :: x :: duplica xs
    ;;

// Costruzione di un tipo Enumerato
type federico = Long | Int;;

type segno = Positivo | Negativo;;
let segno_intero (n : int) : segno =
    if n > 0 then
        Positivo
    else
        Negativo
    ;;

type giorno =
    | Lun
    | Mar
    | Mer
    | Gio
    | Ven
    | Sab
    | Dom
    ;;

let giorno_lavorativo (x : giorno) =
    match x with
        | Sab | Dom -> false
        | _ -> true
    ;;

// Tipo Unione
type identifier =
    | Name of string
    | Code of int;; // Name e Code sono Costruttori del tipo identifier
let identifier = Code 1234;;

type tempertature =
    | Celsius of float
    | Farhenheit of float
    | Kelvin of float
    ;;
let converter_celsius x =
    match x with
        | Celsius n -> Celsius n
        | Farhenheit n -> Celsius ((5. / 9.) * (n - 32.))
        | Kelvin n -> Celsius (n + 273.)
    ;;

// ===== Esercizi di Ripasso - Start =====

// Primo Esercizio - Conta il numero di cifre di una determinata cifra che soddisfano delle determinate condizioni (test)
let rec rev_conta_cifre (n:int) test : int =
    if n < 0 then
        rev_conta_cifre (-n) test
    else
        if n < 10 then
            if (test n) then
                1
            else
                0
        else
            if (test (n % 10)) then
                 1 + (rev_conta_cifre (n / 10) test)
            else
                rev_conta_cifre (n/10) test
    ;;

let rev_conta_cifre_dispari (n : int) =
    rev_conta_cifre n (fun x -> x % 2 = 1)
    ;;

let rev_conta_cifre_pari (n : int) =
    rev_conta_cifre n (fun x -> x % 2 = 0)
    ;;

let rev_conta_cifre_sei (n : int) =
    rev_conta_cifre n (fun x -> x = 6)
    ;;

// Secondo Esercizio - Data una lista applica una determinata funzione a tutti gi elementi della lista di partenza
let rec rev_map f list =
    match list with
        | [] -> []
        | x :: xs -> (f x) :: (rev_map f xs)
    ;;

let rev_double x =
    2 * x
    ;;
// rev_map [4;5;6] rev_double => [8;10;12]
// rev_map (rev_map rev_double) [[4;5];[6]] => [[8; 10]; [12]]

// Terzo Esercizio - Data una listra elimina gli elementi all'interno che soddisfano una determinata condizione (test)
let rec rev_filter test list =
    match list with
        | [] -> []
        | x :: xs -> if (test x) then
                         x :: (rev_filter test xs)
                     else
                         rev_filter test xs
    ;;

let rev_odd (n:int) =
    if n % 2 <> 0 then
        true
    else
        false
    ;;

// Quarto Esercizio - Trova il massimo all'interno di una lista
let rec rev_max_in_list list =
    match list with
        | [] -> failwith "Casistica non considerata"
        | [x] -> x
        | x :: y :: xs -> if x > y then
                              rev_max_in_list (x :: xs)
                          else
                              rev_max_in_list (y :: xs)
    ;;

// Quinto esercizo -> Data una lista di liste calcolare il minimo tra i massimi di ciascuna lista
let rec rev_min_in_list list =
    match list with
        | [] -> failwith "Casistica non considerata"
        | [x] -> x
        | x :: y :: xs -> if x < y then
                              rev_min_in_list (x :: xs)
                          else
                              rev_min_in_list (y :: xs)
    ;;

let rec rev_min_max list =
    match list with
        | [] -> failwith "Casistica non considerata"
        | [x] -> rev_max_in_list x
        | x :: y :: xs -> rev_min_in_list [(rev_max_in_list x); (rev_min_max (y :: xs))]
    ;;

// Sesto Esercizio - Data una lista restituisce una lista di coppie (posizione_elemento_nella_lista, elemento)
let rev_enumerate list =
    let rec rev_auxiliar n list =
        match list with
            | [] -> []
            | x :: xs -> (n, x) :: (rev_auxiliar (n + 1) xs)
    
    rev_auxiliar 0 list
    ;;

// rev_enumerate [1;2;3;4;5;6;7;8;9;10];; => [(0, 1); (1, 2); (2, 3); (3, 4); (4, 5); (5, 6); (6, 7); (7, 8); (8, 9); (9, 10)];;

// ===== Esercizi di Ripasso - End =====

// ===== Random Testing - Start =====

let fst (x, y) = 
    x
    ;;
let snd (x, y) =
    y
    ;;

let first_element = fst (1, 2);;
let second_element = snd (1, 2);;

let rec eliminateItemAt k lst =
    match lst with
        | [] -> []
        | x :: xs -> if k = 0 then
                         xs
                     else
                         x :: (eliminateItemAt (k - 1) xs)
    ;;

let is_prime n =
    let rec try_next i =
        if abs(n) = i then
            true
        else 
            if abs(n) % i = 0 then
                false
            else
                try_next (i + 1)

    if n > -1 && n < 2 then
        false
    else
        try_next 2
    ;;

let goldbach n =
    let rec try_values fst =
        if is_prime fst && is_prime (n - fst) then
            (fst, (n - fst))
        else
            try_values (fst + 1)
    
    try_values 2
    ;;

let primes n =
    let rec aux n test =
        if n = 1 then
            []
        else
            if n % test = 0 then
                test :: (aux (n / test) test)
            else
                aux n (test + 1)
    
    aux n 2
    ;;

let rec zip lst =
    match lst with
        | [] -> []
        | [x] -> [x]
        | x :: y :: xs when x <> y -> x :: zip (y :: xs)
        | x :: y :: xs when x = y -> zip (y :: xs)
        | _ -> failwith "Error"
    ;;

let rec insert_item_at element index list =
    match list with
        | [] -> []
        | z :: zs -> if index = 0 then
                         element :: z :: zs
                     else
                         z :: (insert_item_at element (index - 1) zs)
    ;;

let rec sponchiado_remove i l =
    match i, l with
        | (0, z :: zs) -> zs
        | (i, z :: zs) -> z :: sponchiado_remove (i - 1) zs
        | (i, []) -> failwith "Index Error"
    ;;

let rec sponchiado_insert v i l =
    match i, l with
        | (0, zs) -> v :: zs
        | (i, z :: zs) -> z :: sponchiado_insert v (i - 1) zs
        | (i, []) -> failwith "Index Error"
    ;;

let rec get_item_at i l =
    match i, l with
        | (0, z :: zs) -> z
        | (i, z :: zs) -> get_item_at (i - 1) zs
        | (i, []) -> failwith "Index Error"
    ;;

let rec list_size l =
    match l with
        | [] -> 0
        | z :: zs -> 1 + (list_size zs)
    ;;

let rec combina_copie_di combina valore quantita =
    if quantita = 1 then
        valore
    else
        (combina valore (combina_copie_di combina valore (quantita - 1)))
    ;;
// ===== Random Testing - End =====