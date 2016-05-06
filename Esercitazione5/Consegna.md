ESERCITAZIONE 5

SI RICORDA CHE SI POSSONO USARE SOLO I CONCETTI VISTI A LEZIONE E NON SI POSSONO USARE LE LIBRERIE

 

Alcune note utili:

Nelle esercitazioni potreste aver bisogno della nozione della distanza di Levenshtein.

La distanza di Levenshtein è una misura per la differenza fra due stringhe. Serve a determinare quanto due stringhe siano simili.

La distanza di Levenshtein tra due stringhe A e B è il numero minimo di modifiche elementari che consentono di trasformare la A nella B. Per modifica elementare si intende:

la cancellazione di un carattere,
la sostituzione di un carattere con un altro, o
l'inserimento di un carattere.
Per esempio, per trasformare "bar" in "biro" occorrono due modifiche:

"bar" -> "bir" (sostituzione di 'a' con 'i')
"bir" -> "biro" (inserimento di 'o')
Matematicamente, la distanza di Levenshtein tra due stringhe a, b (di lunghezza |a| and |b| rispettivamente) è data da: \operatorname{lev}_{a,b}(|a|,|b|) dove:

Distanza di Levenshtein

dove 1_{(a_i \neq b_j)} è ugiale a 0 quando a_i = b_j e uguale a 1 altrimenti. \operatorname{lev}_{a,b}(i,j) è la distanza tra le prime i-lettere di a e le prime j-lettere di b.

(Si noti che gli elementi nel minimo corrispondo a:

cancellazione (da a a b),
inserimento,
corrispondenza o meno delle lettere considerate, cioè se sono lo stesso simbolo o meno.)
Per una spegazione più dettagliata:

https://en.wikipedia.org/wiki/Levenshtein_distance

Inoltre potreste aver bisogno della nozione del metodo fi bisezione:

https://it.wikipedia.org/wiki/Metodo_della_bisezione

Esercizio 1:

Data una funzione espressa da un polinomio nella seguente forma:

y = a_n x^n + a_{n-1}x^{n-1} + \dotsb + a_2 x^2 + a_1 x + a_0,
dove a_0, \ldots, a_n sono numeri float e x è una variabile, si richiede di scrivere una funzione che:

presi in input una lista contenente i coefficenti delle variabili in ordine decrescente (cioè an,...,a0) e un numero (che rappresenta la variabile x), restituisca come output il valore della funzione in quel punto.

 

Esempio:

eval_poly [1.0;1.0;1.0] 2.0 = 7.0

eval_poly [1.0] 1.0 = 1.0

eval_poly [1.0;2.0;3.0;4.0] 1.0 = 10.0

eval_poly [1.0;2.0;3.0;4.0] 1.5 = 16.375

Firma:

eval_poly float list -> float -> float

Esercizio 2:

Date due parole, si richiede di scrivere una funzione che:
prese in input due liste di char contenenti le lettere che compongono rispettivamente le sue parole, restituisca come output la distanza di Levenshtein tra le due parole.

levenshtein_distance è['k';'i';'t';'t';'e';'n'] ['s';'i';'t';'t';'i';'n';'g'] ==> 3

levenshtein_distance è['k';'i';'l';'l'] ['b';'i';'l';'l'] ==> 1

levenshtein_distance è['k';'i';'d'] ['k';'i';'l';'l'] ==> 2

levenshtein_distance è['z';'e';'r';'o'] ['z';'e';'r';'o'] ==> 0

levenshtein_distance è['t';'r';'e'] [] ==> 3

Firma:

levenshtein_distance char list -> char list -> int




Esercizio 3:

Data una funzione espressa da un polinomio come nell'esercizio 1, si richiede di scrivere una funzione che:
presi in input una lista contenente i coefficenti delle variabili in ordine decrescente (cioè an,...,a0), un numero e che rappresenta la soglia di tolleranza e una coppia contenente gli estremi dell'intervallo della funzione da considerare (rispettivamente minore e maggiore), restituisca come output il valore di x che corrisponde allo zero della funzione (y = 0 ± e) calcolato con il metodo delle bisezione.

 

Esempio:


zeros [1.0;2.0;3.0;0.0] 0.005 (-1.0,33.0) ==> 0.000244140

zeros [2.0;0.0;-4.0] 0.005 (0.0,3.0) ==> 1,415039
Firma:

zeros float list -> float -> (float * float) -> float

 

DOWNLOAD TEMPLATE ESERCITAZIONE:

Esercitazione5.zip

SOLUZIONI

Esercizio 1


//Calcola la lunghezza di una lista

let length (xs : 'a list) : int = 
    let rec length_t acc xs = 
        match xs with
        | [] -> acc
        | _::xs -> length_t (acc + 1) xs
    length_t 0 xs

let rec eval_poly (coeff : float list) (x : float) : float = 
    match coeff with
    | [] -> failwith "Bad poly"
    | [y] -> y
    | y::ys -> y * (x ** float (length ys)) + eval_poly ys x

Esercizio 2


let rec levenshtein_distance (w1 : char list) (w2 : char list) : int =
    let min3 x y z = 
        if   x <= y && x <= z then x 
        elif y <= x && y <= z then y
        else z
    let rec get_i i xs = 
        match xs with
        | [] -> failwith "index out of bound exception"
        | x::xs when i = 1 -> x
        | x::xs -> get_i (i - 1) xs
    let len xs = 
        let rec len_q a =
            function
            | [] -> a
            | _::xs -> len_q (a + 1) xs
        len_q 0 xs
    let i_f w1 w2 i j = 
        if get_i i w1 = get_i j w2 then 0
        else 1
    let rec lev (w1 : char list) (w2 : char list) i j : int = 
        if min i j = 0 then
            max i j
        else
            let fst = lev w1 w2 (i - 1) j + 1
            let snd = lev w1 w2 i (j - 1) + 1
            let thrd = lev w1 w2 (i - 1) (j - 1) + i_f w1 w2 i j
            min3 fst snd thrd
    lev w1 w2 (len w1) (len w2)

Esercizio 3


let rec zeros (coeff : float list) (threshold : float) (min : float, max : float) : float = 
    let f = eval_poly coeff
    let mid = (min + max) / 2. 
    if abs (f mid) <= threshold then
        mid
    else
        if sign (f mid) = sign(f min) then zeros coeff threshold (mid, max) 
        else zeros coeff threshold (min, mid)