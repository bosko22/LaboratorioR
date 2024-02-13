library(here)
here()

# elenco oggetti presenti nel workspace
ls()

# rimuovere oggetto
a <- c(1,2,3)
rm(a)
# per rimuovere tutti gli oggetti: Session -> Clear Workspace

# per sapere in quale directory di lavoro siamo
getwd()
# per cambiare directory di lavoro
setwd("R")

# nel momento in cui il pacchetto here() viene caricato, la directory di lavoro è quella del tuo file .Rproj
here()

# Elenco dei file contenuti nelle cartelle
list.files()
list.files(here())


# R è un linguaggio di programmazione orientata agli oggetti. Gli oggetti appartengono a una o più classi.
head(cars)
class(cars$speed)
class(cars)
# I metodi (funzioni e comandi) si applicano diversamente a seconda della classe degli oggetti
summary(cars$speed)
summary(cars)

x <- 3 + 5 #ASSEGNAMENTO
y <- "stringa"

x + 4
y + 4

# VETTORI----
# insiemi indicizzati di oggetti

# i più comuni sono:
# logical:	valori logici (TRUE / FALSE)
# integer:	valori interi
# double:	valori reali
# character:	caratteri (testi)
# factor: fattori (variabili categoriali)

# Le variabili contenute in una matrice dei dati sono vettori di R

# Gli elementi di testo devono sempre essere digitati fra virgolette oppure apici
a <- "questa stringa è un insieme di parole"
a
b <- 'questa stringa è un insieme di parole'
b
c <- "questa stringa contiene le \"virgolette\""
cat(c) # concatena e stampa 
d <- "questa stringa\nva a capo"
cat(d)

# concatenare più elementi
e <- c("uno", "due", "tre", "quattro", "cinque")
class(e)

# vettore numerico
f <- c(1, 2, 3, 4, 5)
class(f)

# se inseriamo un elemento diverso, il vettore verrà interpretato come character
g <-c(1, 2, 3, 4, "cinque")
g
class(g)

# è possibile trasformare i vettori in vettore carattere (non è sempre possibile trasformare i vettori in numerici)
h <- as.character(f)
h
i <- as.numeric(h)
i
j <- as.numeric(e)
j

# nota: quando importiamo i dati, solitamente le date sono riconosciute come variabili di testo

# INFORMAZIONI SUI VETTORI
length(f)
class(f)
# accedere ad un elemento; l'elemento con valore 3 occupa la terza posizione, ed ha quindi indice 3
f[3]

cars
cars$speed
cars$speed[3]
cars[3, 1]
