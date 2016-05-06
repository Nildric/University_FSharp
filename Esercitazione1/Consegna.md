ESERCITAZIONE 1

Alcune note utili:

Nelle esercitazioni potreste aver bisogno dell'operatore modulo.
In F# il simbolo per tale operatore è %, e restituisce il RESTO della divisione tra due numeri interi m e n. Per definizione di divisione sappiamo che m / n = (q,r) dove q * n + r = m, quindi r ? [0,n[. Di conseguenza m % n restituisce sempre un numero compreso tra 0 e n - 1.

Convenzioni

Le seguenti convenzioni sono da ritenersi vincolanti affinché la vostra soluzione venga considerata corretta!

La durata di un ciclo lunare è pari a ESATTAMENTE 29 giorni.

Ipotizziate che il 1 gennaio del 2000 fosse l'inizio di un ciclo lunare (ci fosse quindi la Luna Nuova e il giorno corrispondente del ciclo lunare fosse 0).

La durata dei mesi è quella del calendario Gregoriano NON ci sono però gli anni bisestili.

 

Esercizio 1:

Data una data gg/mm/aaaa, definita come segue:

Il numero	corrisponde a      	con le limitazioni
gg	giorno del mese	da 1 a 31
mm	mese dell'anno
da 1 a 12
aaaa	cifre dell'anno 
da 0 in avanti
si richiede di scrivere una funzione che:
presi in input i numeri che compongono la data, restituisca come output il numero del giorno rappresentato dalla data all'interno dell'anno (quindi un numero compreso tra 1 e 365).

Esempio:


data_to_day 10 1 1989 ==> 10
data_to_day 15 12 1988 ==> 349

Firma:

data_to_day: int -> int -> int -> int



Esercizio 2:
Date due date gg/mm/aaaa, definite come il primo esercizio si richiede di scrivere una funzione che:
presi in input i numeri che compongono le due date, restituisca come output la differenza in giorni tra le due date.

Esempio:


data_difference 3 1 2012 1 1 2012 ==> 2
data_difference 10 1 1989 15 12 1988 ==> 26

data_difference 30 9 2015 8 10 2015 ==> -8

data_difference 8 10 2015 8 10 2015 ==> 0

data_difference 6 1 2001 1 1 2000 ==> 370

Firma:

data_difference: int -> int -> int -> int -> int -> int -> int


Esercizio 3:

Data una data gg/mm/aaaa, definita come il primo esercizio e le fasi lunari definite come segue:

Fase	Intervallo Giorni     	Codice
Luna Nuova	 0 - 3	 LN
Luna Crescente	 4 - 6
 LR
Primo Quarto	 7 - 10	 PQ
Gibbosa Crescente   	 11 - 14	 GR
Luna Piena	 15 - 18
 LP
Gibbosa Calante	 19 - 21	 GA
Ultimo Quarto	 22 - 25	 UQ
Luna Calante	 26 - 28	 LA
si richiede di scrivere una funzione che:
presi in input i numeri che compongono la data, restituisca come output il codice della fase lunare corrispondente.

NB il codice della fase lunare è richiesto essere scritto in MAIUSCOLO (è key sensitive)

Esempio:

lunar_cycle 10 1 2000 ==> 'PQ'

Firma:

lunar_cycle: int -> int -> int -> string

DOWNLOAD TEMPLATE ESERCITAZIONE:

Esercitazione1.zip

 

 

SOLUZIONI



Esercizio 1

//Calcola il numero di giorni trascorsi dall'inizio dell'anno fino al mese "month"

let rec month_length (month : int) (year : int) : int =
    match month with
    | 0 -> failwith "Wrong month"
    | _ when month > 12 -> failwith "Wrong month'"
    | 1 -> 31
    | 2 -> month_length (month - 1) year + 28
    | 4 | 6 | 9 | 11 -> 30 + month_length (month - 1) year 
    | n -> 31 + month_length (n - 1) year 

let data_to_day (day : int) (month : int) (year : int) : int =
    if month = 1 then
        day
    else
        day + month_length (month - 1) year

Esercizio 2

let data_difference (day1 : int) (month1 : int) (year1 : int) (day2 : int) (month2 : int) (year2 : int) : int =
    let dy = year1 - year2
    data_to_day day1 month1 year1 - data_to_day day2 month2 year2 + (dy * 365)

Esercizio 3

let rec lunar_cycle (day : int) (month : int) (year : int) : string = 
    let ph = (((data_difference day month year 1 1 2000) % 29) + 29) % 29
    if 0 <= ph && ph <= 3 then "LN"
    elif 4 <= ph && ph <= 6 then "LR"
    elif 7 <= ph && ph <= 10 then "PQ"
    elif 11 <= ph && ph <= 14 then "GR"
    elif 15 <= ph && ph <= 18 then "LP"
    elif 19 <= ph && ph <= 21 then "GA"
    elif 22 <= ph && ph <= 25 then "UQ"
    elif 26 <= ph && ph <= 28 then "LA"
    else failwith "Wrong lunar phase"