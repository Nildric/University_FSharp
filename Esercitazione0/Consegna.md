ESERCITAZIONE 0

Alcune note utili:

Nelle esercitazioni potreste aver bisogno del Teorema di Euclide.
https://it.wikipedia.org/wiki/Primo_teorema_di_Euclide
Inoltre potreste aver bisogno della funzione che calcola il valore assoluto di un numero.

Es. 
abs (-7)



Esercizio 1:

Dato un triangolo rettangolo, si richiede di scrivere una funzione che:
presi in input la misura di un cateto e della sua proiezione sull'ipotenusa, restituisca come output la dimensione dell'ipotenusa.

Esempio:

euclides_hypotenuse 3. 2. ==> 4.5

Firma:

euclides_hypotenuse: float -> float -> float

Esercizio 2:

Dato un piano cartesiano, si richiede di scrivere una funzione che:
presi in input le coordinate bidimensionali di due punti, restituisca come output l'area del quadrilateo formato dal segmento congiungente i due punti e la proiezione dello stesso sull'asse delle ascisse.



Esempio:

segment_integral 3. 3. 2. 2. ==> 2.5



Firma:

segment_integral: float -> float -> float -> float -> float

Esercizio 3:

Dato un triangolo rettangolo, si richiede di scrivere una funzione che:
presi in input la misura di un cateto e della sua proiezione sull'ipotenusa, restituisca come output l'area del triangolo.



Esempio:

triangle_area 15. 9. ==> 150.0



Firma:

triangle_area: float -> float -> float



DOWNLOAD TEMPLATE ESERCITAZIONE:

Esercitazione0.zip



SOLUZIONI



Esercizio 1

let euclides_ipotenusa (cb : float) (ch : float) : float = 
  (cb * cb) /  ch



Esercizio 2

let segment_integral (x1 : float) (y1 : float) (x2 : float) (y2 : float)  : float = 
  (y1 + y2) * (abs(x2 - x1)) / 2.



Esercizio 3

let triangle_area (cb : float) (ch : float) : float = 
  let ipotenusa = euclides_ipotenusa cb ch
  let h = sqrt(cb * cb - ch * ch)
  ipotenusa * h / 2.