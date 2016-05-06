ESERCITAZIONE 2

SI RICORDA CHE SI POSSONO USARE SOLO I CONCETTI VISTI A LEZIONE

 

Alcune note utili:

Il terzo esercizio è collegato alla scorsa esercitazione, si consiglia di leggere la scorsa esercitazione (se già non l'avete fatto)

Convenzioni

Le seguenti convenzioni sono da ritenersi vincolanti affinché la vostra soluzione venga considerata corretta!

La durata di un ciclo lunare è pari ad ESATTAMENTE 29 giorni.

Ipotizziate che il 1 gennaio del 2000 fosse l'inizio di un ciclo lunare (ci fosse quindi la Luna Nuova e il giorno corrispondente del ciclo lunare fosse 0).

Le fasi lunari sono definite come segue:

Fase	Intervallo Giorni     
Luna Nuova	 0 - 3
Luna Crescente	 4 - 6
Primo Quarto	 7 - 10
Gibbosa Crescente   	 11 - 14
Luna Piena	 15 - 18
Gibbosa Calante	 19 - 21
Ultimo Quarto	 22 - 25
Luna Calante	 26 - 28
Una luna piena è considerata parte di un mese se il suo 15° giorno di ciclo fa parte del mese in questione

La durata dei mesi è quella del calendario Gregoriano, COMPRESI GLI ANNI BISESTILI .

Un anno si definisce bisestile se il suo numero è divisibile per 4, con l'eccezione degli anni secolari (quelli divisibili per 100) che non sono divisibili per 400. 

L'anno 2000 è bisestile perché è un anno secolare ma è divisibile per 400.

L'anno 1900 non è bisestile perché è secolare ma non è divisibile per 400.

L'anno 1988 è bisestile perché è divisibile per 4.

L'anno 1989 non è bisestile perchè non è divisibile per 4.

Una data gg/mm/aaaa è definita come segue:

Il numero	corrisponde a      	con le limitazioni
gg	giorno del mese	da 1 a 31
mm	mese dell'anno
da 1 a 12
aaaa	cifre dell'anno 
da 0 in avanti
 


Esercizio 1:

Date due date gg/mm/aaaa si richiede di scrivere una funzione che:
presi in input i numeri che compongono le due date, restituisca come output la differenza in giorni tra le due date, considerando anche gli anni bisestili.

data_difference 10 1 1989 15 12 1988 ==> 26

data_difference 30 9 2015 8 10 2015 ==> -8

data_difference 8 10 2015 8 10 2015 ==> 0

data_difference 6 1 2001 1 1 2000 ==> 371

data_difference 6 1 2005 1 1 2000 ==> 1832

data_difference 28 2 2012 1 3 2012 ==> -2



Firma:

data_difference: int -> int -> int -> int -> int -> int -> int




Esercizio 2:

Dato un numero, si richiede di scrivere una funzione che:
preso in input un numero, restituisca come output il numero con le cifre invertite.

NON USATE la conversione in Stringhe

 

Esempio:


reverse_number 12345 ==> 54321

reverse_number 7  ==> 7

reverse_number 100 ==> 1

reverse_number 101 ==> 101

reverse_number 1203 ==> 3021



Firma:

reverse_number: int -> int



Esercizio 3:

Dato il termine Luna Blu, che indica la seconda luna piena di un mese, si richiede di scrivere una funzione che:

preso in input un anno, restituisca come output il numero del primo mese dell'anno che contenga una luna blu, in caso nessun mese ne abbia si deve lanciare un'eccezione failwith "NON TROVATO", considerando anche gli anni bisestili.

 

Esempio:

blue_moon 1988 ==> NON TROVATO

blue_moon 2020 ==> NON TROVATO

blue_moon 2016 ==> 1

blue_moon 1994 ==> 3

blue_moon 2004 ==> 4

blue_moon 2014 ==> 5

blue_moon 2019 ==> 6

blue_moon 1997 ==> 7

blue_moon 2012 ==> 8

blue_moon 2017 ==> 9

blue_moon 1990 ==> 10

blue_moon 2000 ==> 11

blue_moon 2010 ==> 12

Firma:

blue_moon: int -> int

 

DOWNLOAD TEMPLATE ESERCITAZIONE:

Esercitazione2.zip

SOLUZIONI



Esercizio 1

//Funzione booleana che ritorna TRUE se "year"  è un anno bisestile, altrimenti FALSE

let leap_year (year : int) : bool =
    (year % 400 = 0) || ((year % 4 = 0) && (year % 100 <> 0))

//Calcola il numero di giorni del mese "month"
let month_real_length (month : int) (year : int) : int =
    match month with
    | 0 -> failwith "Error"
    | _ when month > 12 -> failwith "Error"
    | 1 -> 31
    | 2 -> if leap_year year then 29 else 28
    | 4 | 6 | 9 | 11 -> 30 
    | n -> 31

//Calcola il numero di giorni trascorsi dall'inizio dell'anno fino al mese "month"
let rec month_length (month : int) (year : int) : int =
    if month < 2 then month_real_length month year
    else month_real_length month year + month_length (n - 1) year

//Calcola il numero di giorni trascorsi dall'inizio dell'anno fino al mese "month" e giorno "day"
let rec data_to_day (day : int) (month : int) (year : int) : int =
    if month = 1 then
        day
    else
        day + month_length (month - 1) year


let rec data_difference (day1 : int) (month1 : int) (year1 : int) (day2 : int) (month2 : int) (year2 : int) : int =
    if year1 = year2 then
        data_to_day day1 month1 year1 - data_to_day day2 month2 year2 
    else
        if year2 < year1 then
            data_to_day day1 month1 year1 + (data_difference 31 12 (year1 - 1) day2 month2 year2)
        else
            -(data_difference day2 month2 year2 day1 month1 year1)

Esercizio 2


let rec reverse_number (n : int) : int = 
    let rec nlength n = 
        if n < 10 then
            1.
        else 
            1. + nlength (n / 10)
    let rec reverse n = 
        if n < 10 then
            n
        else
            let int = (n % 10) * (int (10. ** (nlength n - 1.) ))
            int + reverse (n / 10)
    reverse n

Esercizio 3


let rec blue_moon (year : int) : int = 
    let rec blue_moon month = 
        if month > 12 then
            failwith "NON TROVATO"
        else
            let f_day = data_difference 1 month year 1 1 2000
            let fst_moon = (15 - (((f_day % 29) + 29) % 29) + 29) % 29 
            let snd_moon = fst_moon + 30
            if snd_moon <= month_real_length month year then 
                month
            else 
                blue_moon (month + 1)
    blue_moon 1