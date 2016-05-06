PROGETTO F#

Regolamento:

Il progetto si compone di due funzioni da implementare. Si ricorda che se il progetto non sarà valutato sufficiente si dovrà sostenere l'orale obbligatorio. Inoltre il progetto avrà una valutazione di 6 punti che si aggiungeranno a quelli delle altre esercitazioni svolte.
La data di consegna limite del progetto è Giovedì 26 Novembre alle 23:55. La consegna viene effettuata come per le normali esercitazioni di laboratorio.
Il progetto può essere svolto in gruppo. I gruppi possono essere formati al massimo da 3 persone, non sono ammesse deroghe a ciò. Tutti i membri del gruppo devono consegnare l'esercitazione, pena una valutazione negativa per le persone sprovviste di consegna.
I gruppi vanno comunicati ai tutors entro Giovedì 19 Novembre tramite l'apposita discussione aperta nel forum. Dopo tale data verrà compilata una lista per la prova orale in cui ad ogni gruppo verrà assegnato un tutor per la discussione del progetto.
Utilizzate il template di soluzione che trovate nel link alla fine del regolamento.
La valutazione è individuale e non di gruppo, ciò vuol dire che tutti devono dimostrare di aver lavorato sul progetto e devono conoscere il progetto per intero. Se qualcuno non dovesse conoscere il progetto sviluppato, sarà valutato molto negativamente (anche insufficiente).
La data precisa per la discussione del progetto verrà comunicata prossimamente. Indicativamente sarà ai primi di Dicembre. In caso di adeguate motivazioni (ad esempio per studenti lavoratori), è possibile concordare una data alternativa (anche anticipata) per la discussione del progetto.
Non presentarsi alla discussione comporta una valutazione nulla.
Tenete presente che verrà valutata anche la performance del vostro progetto perchè con input molto grandi (es. nei due dizionari messi a disposizione) potreste avere dei problemi di gestione e di computazione.
Dovete modificare SOLO il BODY del file Program.fs e consegnare l'intero archivio.
Ricordate di consegnare il progetto seguendo le istruzioni CORRETTAMENTE, se avete dubbi chiedete.
Esercizio 1 (Correct):
Data una lista di stringhe rappresentante un dizionario e una frase inserita in input si richiede di fare una funzione che:
presa in input la frase, controlli se le parole sono presenti nel dizionario o in alternativa cerchi di effettuare una correzione per ritornare in output
una lista di frasi contenenti tutte le possibili correzioni della frase (o la frase stessa se tutte le parole che la compongono sono nel dizionario).

La distanza tra parole è definita come segue:
Date due parole della stessa lunghezza, la differenza è rappresentata dal numero di lettere da sostituire nella prima per ottenere la seconda.
Due parole di diversa lunghezza non possono essere confrontate e quindi non possono essere usate come correzione l'una per l'altra.

esempio:
distanza: ciao - cia  => N/D
distanza: ciao - ciab => 1
distanza: iao - cia   => 3
distanza: cooo - ciao => 2

Una correzione è definita come segue:
Data una parola non presente nel dizionario, la sua correzione è una lista di parole contentente la parola stessa e le parole che hanno distanza minima dalla parola di origine.

esempio:

Dizionario = [ciao; cia; ciab; ciaa; cian; caaa]

input = ciad
output = [ciad; ciao; ciab; ciaa; cian]

input = ciao
output = [ciao]


Dizionario = [ciao; cia; ciab; cian; caaa; a; b; c; te; tr]

input = ciad a tu
output = [ciad a tu; ciad a te; ciad a tr;
          ciao a tu; ciao a te; ciao a tr;
          ciab a tu; ciab a te; ciab a tr;
          cian a tu; cian a te; cian a tr]

input = ciaoo a tu
output = [ciaoo a tu; ciaoo a te; ciaoo a tr]

input = ciao a te
output = [ciao a te]

input = ciaoo aaaaa teeee
output = [ciaoo aaaaa teeee]

Esercizio 2 (Find):
Data una lista di stringhe rappresentante un dizionario e una parola:
presa in input la parola, controlli se è presente nel dizionario o in alternativa, se questa contiene i carattersi speciali " . " e/o " * ", cerchi tutte le parole che possano soddisfare i caratteri speciali per ritornare in output questa lista di parole.

L'output deve avere questa forma:

(string * string * int) list

Che rappresenta in ordine le seguenti cosa:

La parola cercata
La parola trovata
La posizione della parola trovata all'interno del dizionario
Il carattere . rappresenta una lettera generica e può comparire in qualunque posizione (anche in prima o ultima posizione):

esempio:

Dizionario = [ciao; cia; ciab; ciaa; cian; caao]

input = c.ao
output = [(c.ao; ciao; 0); (c.ao; caao; 5)]

input = cia.
output = [(cia.; ciao; 0); (cia.; ciab; 2); (cia.; ciaa; 3); (cia.; cian; 4)]

Il carattere * rappresenta 0 o più lettere generiche e può comparire solo alla fine della parola:

esempio:

Dizionario = [ciao; cia; ciab; ciaa; cian; caao]

input = cia*
output = [(cia*; ciao; 0); (cia*; cia; 1); (cia*; ciab; 2); (cia*; ciaa; 3); (cia*; cian; 4)]

input = ci*
output = [(ci*; ciao; 0); (ci*; cia; 1); (ci*; ciab; 2); (ci*; ciaa; 3); (ci*; cian; 4)]

input = c*
output = [(c*; ciao; 0); (c*; cia; 1); (c*; ciab; 2); (c*; ciaa; 3); (c*; cian; 4); (c*; caao; 5)]

input = *
output = [(*; ciao; 0); (*; cia; 1); (*; ciab; 2); (*; ciaa; 3); (*; cian; 4); (*; caao; 5)]

Download template del progetto:
Template esercitazione

Come testare il vostro programma:

Nel template del progetto non è presente un correttore automatico come per le normali esercitazioni, perché il progetto verrà provato e discusso durante la prova orale. Potete testare il vostro programma nel modo seguente:

Nel menu a tendina vicino all'icona del tasto "avvia debug" (quello con il simbolo di play verde), cambiate la voce da Debug a Release.
Lanciate il programma premendo il tasto "avvia debug" (quello con il simbolo di play verde) o F5. Così facendo il programma verrà automaticamente salvato, compilato, e lanciato (salvo errori di compilazione, che vengono visualizzati come al solito).
Se volete compilare ma non lanciare il programma potete usare il menu Compila -> Compila soluzione, oppure usare la shortcut da tastiera CTRL + SHIFT + B.
Il programma vi chiedera di scegliere tra tre tipi di dizionari:

Il primo contenente 1.000 parole
Il secondo contenente 60.000 parole
Il terzo inserito in input da voi (inserendo cioè tutte le parole del vostro dizionario separate dal carattere spazio, ricordate di premere INVIO solo quando avete inserito tutte le parole che volete)
Una volta scelto il dizionario, il programma vi chiederà di scegliere quale funzione usare:

Correzioni della frase (Esercizio 1)
Ricerca di una parola (Esercizio 2)
Suggerimenti:

Sfruttate il fatto di poter lavorare in gruppo.
Cercate di lavorare quanto più possibile assieme alla soluzione del progetto. Ragionare in due/tre sullo stesso problema è più efficiente che ragionare in due/tre su altrettanti problemi diversi.
Prendetevi per tempo. Avete 14 giorni di tempo per consegnare, il progetto è tarato per essere svolto in un arco temporale ampio, quindi non prendetevi all'ultimo momento altrimenti vi trovate a dover programmare 10 ore al giorno (e non è facile).
Ragionate per sottoproblemi: non tentate di scrivere l'intero programma di getto, procedete per gradi e pensate ai passi intermedi da risolvere per costruire la soluzione finale. Ad esempio, iniziando con le funzioni di supporto da usare.