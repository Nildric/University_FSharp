ESERCITAZIONE 4

SI RICORDA CHE SI POSSONO USARE SOLO I CONCETTI VISTI A LEZIONE E NON SI POSSONO USARE LE LIBRERIE

 

Alcune note utili:

Nelle esercitazioni potreste aver bisogno delle nozioni del calcolo dell'integrale tramite la regola del trapezio (la stessa dell'esercitazione 0 e spiegata a lezione, solo che in questo caso è un approssimazione dell'integrale e la regola del trapezio può dover essere applicata a più sottosezioni dell'intervallo da considerare).

https://it.wikipedia.org/wiki/Regola_del_trapezio

Esercizio 1:

Dati un intervallo e un numero positivo di passi (diverso da 0) in cui suddividerlo, si richiede di scrivere una funzione che:
presi in input una coppia contenente gli estremi dell'intervallo (rispettivamente minore e maggiore) e il numero di volte in cui suddividere l'intervallo, restituisca come output una lista di coppie contenente l'estremo inferiore e l'estremo superiore degli intervalli in cui è stato suddiviso l'intervallo in input.

intervals (1.0,10.0) 1 ==> [(1.0,10.0)]

intervals (1.0,10.0) 9 ==> [(1.0,2.0);(2.0,3.0);(3.0,4.0);(4.0,5.0);(5.0,6.0);(6.0,7.0);(7.0,8.0);(8.0,9.0);(9.0,10.0)]

intervals (1.0,1.0) 5 ==> [(1.0,1.0);(1.0,1.0);(1.0,1.0);(1.0,1.0);(1.0,1.0)]

intervals (-2.0,7.0) 6 ==> [(-2.0,-0.5);(-0.5,1.0);(1.0,2.5);(2.5,4.0);(4.0,5.5);(5.5,7.0)]

Firma:

intervals (float * float) -> int -> (float * float) list




Esercizio 2:

Data la funzione exp (- (x ** 2.)) e dati un intervallo della funzione e un numero di passi in cui suddividerlo, si richiede di scrivere una funzione che:
presi in input una coppia contenente gli estremi dell'intervallo della funzione da considerare (rispettivamente minore e maggiore) e il numero di volte in cui suddividerlo, restituisca come output l'area sottesa dalla funzione calcolata tramite la regola del trapezio applicata su tutti i sotto intervalli su cui è stato diviso l'intervallo di funzione.

 

Esempio:


gauss_integral (-1.0,1.0) 1 ==> 0.7357588823

gauss_integral (-1.0,1.0) 1000 ==> 1.493647775

gauss_integral (0.0,0.0) 1000 ==> 0.0



Firma:

gauss_integral (float * float) -> int -> float



Esercizio 3:

Data una lista di interi, si richiede di scrivere una funzione che:
presa in input la lista di numeri, restituisca come output una lista di liste, ciascuna delle quali contenente il numero massimo di elementi consecutivi della lista in input senza contenere due elementi consecutivi uguali.

 

Esempio:

split_eq [1;2;5;2;2;3;4;5;5;4:5;5] ==> [[1;2;5;2];[2;3;4;5];[5;4:5];[5]]

split_eq [2;1;2;1;1;2] ==> [[2;1;2;1];[1;2]]

split_eq [-1;2;1;2;5;-1;5;2] ==> [[-1;2;1;2;5;-1;5;2]]

split_eq [2] ==> [[2]]

split_eq [] ==> []

split_eq [7;7;7] ==> [[7];[7];[7]]

Firma:

split_eq int list -> int list list

 

DOWNLOAD TEMPLATE ESERCITAZIONE:

Esercitazione4.zip

SOLUZIONI

Esercizio 1

let rec intervals (min : float, max : float) (nsteps : int) : (float * float) list = 
    if nsteps = 0 then failwith "error"
    elif nsteps = 1 then
        [(min, max)]
    else
        let step = (max - min) / float nsteps
        (min, min + step)::intervals (min + step, max) (nsteps - 1)


Esercizio 2

let rec gauss_integral (interval : (float * float)) (nsteps : int) : float = 
    let gauss x = exp (- (x ** 2.))
    let trapezoid (min, max) = (gauss min + gauss max) * (abs (min - max)) / 2.
    let rec apply_get l =
        match l with
        | [] -> failwith "error"
        | [interval] -> trapezoid interval
        | x::xs -> trapezoid x + apply_get xs
    apply_get (intervals interval nsteps)


Esercizio 3

let rec split_eq (l : int list) : int list list = 
    match l with
    | [] -> []
    | x::y::xs when x = y -> 
        [x]::split_eq (y::xs)
    | x::xs ->
        match split_eq (xs) with
        | [] -> [[x]]
        | zs::zss -> (x::zs)::zss