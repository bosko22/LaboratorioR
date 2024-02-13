# carico i pacchetti----
library(tidyverse)
library(readxl)

# INTRO: operatore PIPE----
# L'operatore %>% passa il risultato di un comando all'interno del comando successivo
# viene utilizzato per esprimere una sequenza di più operazioni
# il risultato di un comando diventa l'argomento del comando successivo
# shortcut: ctrl+shift+M

# Il codice seguente crea un vettore di 15 valori numerici generati casualmente
# utilizziamo set.seed(1) per far sì che vengano prodotti gli stessi valori casuali ogni volta che eseguiamo il codice
set.seed(123)
number_data <- runif(n = 15, min = 0, max = 100)
number_data

# Questo vettore viene quindi arrotondato a due cifre significative, ordinato in ordine decrescente e quindi con il comando head() visualizziamo i primi valori più grandi.
head(sort(round(number_data, digit = 0), decreasing = TRUE))
# il codice R base sopra, si legge dalle parentesi più interne a quelle più esterne. 
# Questo annidamento di funzioni può rendere impegnativa la lettura del codice R di base.

# Un altro approccio di R base che evita le funzioni di annidamento consiste nel salvare i risultati intermedi. I risultati intermedi vengono quindi utilizzati nella funzione successiva come comando separato.

number_round <- round(number_data, digit = 0)
number_sort <- sort(number_round, decreasing = TRUE)
head(number_sort)

# L'operatore pipe semplifica la scrittura del codice e consente di unire insieme le funzioni in un'unica pipeline, che può essere eseguita tutta in una volta.


# Il pipe potrebbe non sembrare molto utile all'inizio, ma semplifica notevolmente il monitoraggio e la modifica


# carico i dati----

titanic <- read_excel("titanic_train.xlsx")


# struttura dei dati----
class(titanic)
head(titanic)
View(titanic)
glimpse(titanic)
summary(titanic)


# 12 variabili: 
# ID del passeggero (PassengerId), 
# variabile binaria di sopravvivenza del passeggero (Survived): 0 = No, 1 = Yes
# classe del passeggero (Pclass)
# nome del passeggero (Name)
# sesso (Sex)
# età (Age)
# numero di fratelli e coniugi a bordo (Sibsp)
# numero di figli/genitori a bordo (Parch)
# numero del biglietto (Ticket)
# tariffa pagata (Fare)
# numero di cabina (Cabin)
# luogo di imbarco del passeggero (Embarked): C = Cherbourg, Q = Queenstown, S = Southampton

# rename()----
# RINOMINARE COLONNE
names(titanic)

janitor::clean_names(titanic)

# titanic %>% 
#   rename_with(tolower, everything())

titanic %>% 
  rename(Id = PassengerId,
         nome = Name,
         sesso = Sex)

titanic <- janitor::clean_names(titanic)

# arrange()----
# ORDINARE IL DATASET
titanic %>% 
  arrange(age)

titanic %>% 
  arrange(desc(age)) 

# Ordiniamo i passeggeri in ordine crescente di età, quindi ordiniamo alfabeticamente per nome i passeggeri della stessa età.
titanic %>% 
  arrange(desc(age), name)

## slice()----
# utile insieme ad arrange, estrae n righe
titanic %>% 
  arrange(desc(age)) %>% 
  slice(1, 2, 3)

titanic %>% 
  arrange(desc(age)) %>% 
  slice(1:3)

titanic %>% 
  arrange(desc(age)) %>% 
  head(3)

titanic %>% 
  arrange(age) %>% 
  head(3)

# nota: arrange mette in coda (tail) al dataset i valori mancanti
titanic %>% 
  arrange(age) %>% 
  tail(3)



# select()----
# SELEZIONARE SOTTOINSIEME DI COLONNE
titanic %>% 
  names()

titanic %>%
  select(name, sex, age)

titanic %>% 
  select(4, 5, 6)

titanic %>% 
  select(-passenger_id, -survived) %>% 
  dim()

 titanic %>% 
  select(-c(passenger_id, survived)) -> X

titanic %>%
  select(contains("_"))

titanic %>%
  select(starts_with("s"))

titanic %>%
  select(ends_with("d"))

titanic %>% 
  select(where(is.numeric))

titanic %>% 
  select(where(is.character))

titanic %>% 
  select(where(is.factor))


# mutate()----
# CREARE NUOVE COLONNE O TRASFORMARE COLONNE ESISTENTI

titanic <- titanic %>%
  mutate(sex = as.factor(sex))

titanic %>% 
  mutate(parenti = sib_sp + parch)
  # relocate(parenti, .before = name)

titanic %>% 
  mutate(parenti = sib_sp + parch, .after = parch) %>% View()

titanic %>% 
  mutate(parenti = sib_sp + parch, .before = ticket)

titanic %>% 
  mutate(child =   
            ifelse(age < 18, "minorenni", "maggiorenni")
         )

## case_when()----
# titanic %>%
#   distinct(sex) #mantenere solo righe univoche

titanic %>% 
  mutate(
  sex = case_when(
    sex == "male" ~ "M",
    sex == "female" ~ "F"
    ),
  child = case_when(
    age < 18 ~ "minorenni",
    TRUE ~ "maggiorenni"
  )) %>% View()

titanic %>% 
  mutate(sex = case_when(
    sex == "male" ~ "M",
    TRUE ~ "F"
  ))

titanic %>% 
  mutate(sex = case_when(
    sex == "male" ~ "M"
  )) %>% View()
  

titanic %>% 
  summary()

titanic %>% 
  mutate(survived = case_when(
    survived == 0 ~ "No",
    survived == 1 ~ "Si"
  ))

titanic %>% 
  mutate(sopravvissuto = case_when(
    survived == 0 ~ "No",
    survived == 1 ~ "Si"), .after = survived 
  ) %>% View()

titanic <- titanic %>% 
  mutate(age_class = case_when(
    age < 10 ~ "<10",
    age >= 10 & age < 30 ~ "10-30",
    age >= 30 & age < 50 ~ "30-50",
    age >= 50 & age < 70 ~ "50-70",
    age >= 70 ~ ">80",
    TRUE ~ "unknown"), .after = age)  

classi<- c("10-30", "30-50")

titanic %>% 
  mutate(age_class = ifelse(age<10, "<10", 
                            ifelse(age >=10 & age < 30, "10-30", 
                                   ifelse(age >=30 & age < 50, "30-50", "> 50"))))



# trasformare più colonne contemporaneamente
titanic %>% 
  mutate(across(c(survived, sex, embarked), as.factor))

## ifelse()----


#separate() e unite()----

# filter()----
# SELEZIONARE SOTTOINSIEME DI RIGHE

titanic %>% 
  filter(survived = 1)

# quanti sono i sopravvissuti?
titanic %>% 
  filter(survived == 1) %>% View()
  nrow()

titanic %>% 
  filter(survived == 1) %>% 
  tally()
# nrow() restituisce un numero, tally() restituisce un tibble


titanic %>% 
  filter(survived == 1 & sex == "male") %>% 
  nrow()

titanic %>% 
  filter(age_class %in% c("10-30", "30-50")) %>% View()
  nrow()

titanic %>% 
  filter(pclass %in% c(1, 2)) %>% 
  nrow()

titanic %>% 
  filter(!pclass %in% c(1, 2))  

# rimuovi righe con valori mancanti NA in una specifica colonna
titanic %>% 
  filter(!is.na(age)) %>%  View()

# rimuovi righe con valori mancanti NA in qualsiasi colonna
titanic %>% 
  na.omit()


# percentuale di passeggeri sopravvissuti

Titanic_survived <- titanic %>% 
  filter(survived == 1)


nrow(Titanic_survived)/nrow(titanic)

# percentuale di passeggeri donne sopravvissute
Titanic_survived_w <- titanic %>% filter(survived == 1, sex == "female")


nrow(Titanic_survived_w)/nrow(titanic)


# summarise()----
# ottenere statistiche di riepilogo delle colonne 

titanic %>%
  summarise(meanAge = mean(age))

titanic %>% 
  filter(!is.na(age)) %>%
  summarise(meanAge = mean(age))

titanic %>% 
  summarise(meanAge = mean(age, na.rm = T))

round(mean(titanic$age, na.rm = TRUE), 2)

library(openxlsx)

tabage <- titanic %>%
  filter(!is.na(age)) %>%
  summarise(meanAge = mean(age), 
            sdAge = sd(age),
            minAge = min(age),
            maxAge = max(age), 
            count = n()) 
  write.xlsx(tabage, file = here("report",  "tabella.xlsx"))
  

## group_by()----
# Raggruppare le osservazioni in base a una variabile categoriale
# combinare con summarise per ottenere riepiloghi di gruppo
titanic %>%
  filter(!is.na(age)) %>%
  group_by(pclass) %>% 
  summarise(meanAge = mean(age), 
            sdAge = sd(age),
            minAge = min(age),
            maxAge = max(age), 
            count = n()) 



titanic %>% 
  group_by(survived) %>% 
  summarise(n = n())  
  # count()


titanic %>%
  filter(!is.na(age)) %>%
  group_by(sex) %>% 
  summarise(meanAge = mean(age), 
            sdAge = sd(age),
            minAge = min(age),
            maxAge = max(age), 
            count = n()) 


# percentuale di passeggeri sopravvissuti
titanic %>% 
  group_by(survived) %>%
  summarise(count = n()) %>% 
  mutate(#total = sum(count),
         percent = count/sum(count)) 



# percentuale di passeggeri sopravvissuti per sesso
titanic %>% 
  group_by(survived, sex) %>%
  summarise(count = n()) %>%
  mutate(total = sum(count),
         percent = count/total,
         perc = paste(round(count/total*100, 1), "%")) %>%  View()
  

# percentuale di passeggeri sopravvissuti per classe
titanic %>%
  group_by(survived, pclass) %>%
  summarise(count = n()) %>%
  mutate(total = sum(count),
         percent = count/total*100)   


# titanic %>%
#   group_by(sex) %>%
#   slice_max(age, n = 3, with_ties = FALSE)

#  esempio covid----
dt <- read_csv(here("dati","covid-2024-01-29.csv"))

dt %>% 
  distinct(Finalità)

dt %>% 
  count(Finalità)

dt %>% 
  count(Materiale) %>% 
  arrange(desc(n))

dt %>% 
  count(Prova) %>% 
  arrange(desc(n))

dt %>% 
  count(Reparto) %>% 
  arrange(desc(n))

# R BASE
# table(dt$Finalità)
# table(dt$Materiale)
# table(dt$Prova)
# table(dt$Reparto)

dt %>% 
  filter(Finalità == "Emergenza COVID-19") %>% 
  distinct(Prova)

# PROVA
dt %>%
  mutate(Prova = case_when(
    str_detect(Prova, "Sequenziamento") ~ "SARS-CoV-2: sequenziamento",
    str_detect(Prova, "identificazione") ~ "SARS-CoV-2: identificazione varianti",
    #str_detect(Prova, "Agente eziologico") ~ "SARS-CoV-2: agente eziologico",
    TRUE ~ Prova
  )) %>%
  count(Prova) %>%
  arrange(desc(n))

# MATERIALE
dt %>% 
  mutate(Materiale = case_when(
    str_detect(Materiale, "TAMPO") ~ "TAMPONE",
    str_detect(Materiale, "SALIVA") ~ "SALIVARE",
    Materiale == "RNA" ~ "RNA SARS-CoV-2",
    TRUE ~ Materiale)) %>%
  count(Materiale) %>%
  arrange(desc(n))


dt <- dt %>%
  #PROVA
  mutate(Prova = case_when(
    str_detect(Prova, "Sequenziamento") ~ "SARS-CoV-2: sequenziamento",
    str_detect(Prova, "identificazione") ~ "SARS-CoV-2: identificazione varianti",
    #str_detect(Prova, "Agente eziologico") ~ "SARS-CoV-2: agente eziologico",
    TRUE ~ Prova
  )) %>%
  filter(!Prova == "Agente eziologico") %>% 
  #MATERIALE
  mutate(Materiale = case_when(
    str_detect(Materiale, "TAMPO") ~ "TAMPONE",
    str_detect(Materiale, "SALIVA") ~ "SALIVARE",
    Materiale == "RNA" ~ "RNA SARS-CoV-2",
    TRUE ~ Materiale)) %>%
  filter(Materiale %in% c("TAMPONE", "SALIVARE", "RNA SARS-CoV-2")) %>% 
  #REPARTO
  filter(Reparto %in% c("Reparto Tecnologie Biologiche Applicate", "Sede Territoriale di Pavia", "Sede Territoriale di Modena"))

dt %>% 
  summarise(min = min(dtconf),
            max = max(dtconf))

# Reshape dati----
##pivot_wider  ----

salm <- read_excel(here("dati","salmonelle.xlsx"))
# salm %>%
#   view()
# 
# str(salm)
# 
# num <- salm %>% 
#   rename(esito = `Salmonella spp`) %>% 
#   mutate(anno = year(dtreg)) %>% 
#   group_by(specie, anno)  %>%
#   summarise(count = n()) 
# 
# salm %>% 
#   rename(esito = `Salmonella spp`) %>% 
#   mutate(anno = year(dtreg)) %>% 
#   group_by(specie, anno, esito)  %>%
#   summarise(count = n())  %>%  
#   mutate(total = sum(count),
#          percent = count/total*100) %>% 
#  
#   
#    ungroup() %>% 
#   pivot_wider(id_cols = c(specie, anno), names_from = esito, values_from = percent, values_fill = 0) %>% 
#   
#   
#   
#   left_join(num,
#             by = c("specie", "anno")) %>% View()
#   relocate(count, .before = neg) %>% 
#   mutate(pos = round(pos,0),
#          neg = round(neg,0)) %>% 
#   rename("% neg" = neg, "% pos" = pos) %>% 
#   gt::gt(groupname_col = "specie")

  

  
  salm %>% # fai l'esempio prima senza l'anno e poi con l'anno...
    rename(esito = `Salmonella spp`) %>% 
    mutate(anno = year(dtreg)) %>% 
    select( specie, esito, anno ) %>%  
    group_by(anno, specie, esito ) %>%
    count() %>% 
    pivot_wider(names_from = esito, values_from = n, values_fill = 0) %>%  
    mutate(tot = pos+neg, 
           prev = 100*pos/tot) %>% 
    select(anno, specie,  prev) %>%  
    pivot_wider(names_from = anno, values_from = prev) %>% 
    write.xlsx(file = "tab1.xlsx")
  
  ##pivot_longer----
  
  dt <- read_excel("SPERIMENTAZIONE prova132.xlsx", col_names = FALSE)
  
  
  #puliza del dataset
  
 dt <-  dt[-c(1:3, 24:33), ]
  #dt[1,1]<- "id"
  
  dt %>% 
    row_to_names(1) %>% View()
    clean_names() %>%
    rename("id" = na) %>% 
    filter (!id %in% c("GRUPPO 1", "GRUPPO 2", 
                       "GRUPPO 3", "GRUPPO TOPI DI CONTROLLO")) %>% 
    separate(id, into = c("id", "gruppo")) -> dtsperim
  dtsperim %>% 
    mutate(x1 = as.numeric(x1)) %>% 
    pivot_longer(cols =  3:19, names_to = "day", values_to = "peso")
  
 
  
  
 
  
  
  # JOIN----
  # ## esempio conferimenti---
  # conf <- readRDS(here("conf2024.rds"))
  # # 
  # # conf %>% 
  # #   mutate(Nconf = paste0(year(dtreg), nconf), .after = nconf) %>%
  # #   View()
  # # 
  # # # vedi Nconf = 20246007
  # 
  # conf %>% 
  #   mutate(Nconf = paste0(year(dtreg), nconf), .after = nconf) %>%  
  #   distinct(Nconf, finalita) %>%  
  #   pivot_wider(names_from = "finalita", values_from = "finalita") %>%  
  #   unite("finalita", 2:ncol(.), na.rm = TRUE, remove = FALSE) %>% 
  #   select(Nconf, finalita) %>% View()
  #   View()
  # 
  # conf %>% 
  #   mutate(Nconf = paste0(year(dtreg), nconf), .after = nconf) %>%
  #   distinct(Nconf, .keep_all = TRUE) %>% 
  #   select(-finalita) %>% 
  #   left_join(
  #     conf %>% 
  #       mutate(Nconf = paste0(year(dtreg), nconf), .after = nconf) %>%
  #       distinct(Nconf, finalita) %>% 
  #       pivot_wider(names_from = "finalita", values_from = "finalita") %>%  
  #       unite("finalita", 2:ncol(.), na.rm = TRUE, remove = FALSE) %>% 
  #       select(Nconf, finalita), by = "Nconf") %>% 
  #   relocate(finalita, .after = settore) %>% 
  #   View()
  
  
  
  
  
  ## esempio dati aziendali ----
  
tabella_anagrafe <- read.xlsx("tabella_anagrafe.xlsx")
tabella_coordinate <- read.xlsx("tabella_coordinate.xlsx")
tabella_prove <- readRDS("prove.RDS")
tabella_conferimenti <- readRDS("conferimenti.RDS")
  
      
  tabella_anagrafe %>% 
    distinct(codice, .keep_all = TRUE) %>%  
    left_join(
      tabella_coordinate, by = c("codice" = "CODICE")
    )-> t1 # guarda le dimensioni delle tabelle 


  tabella_coordinate %>% 
    left_join(
      tabella_anagrafe %>% 
        distinct(codice, .keep_all = TRUE), by = c("CODICE" = "codice")
    ) -> t2 # guarda le dimensioni delle tabelle
  

   
  # nella tabella anagrafe ci sono codici duplicati per via dei sottocodici...sistemare la questione
  
  
  tabella_conferimenti %>% 
    select(nconf, dtreg, codaz, finalita, motivo_prel, dtprel, specie, materiale, comune, NrCampioni) %>%  
    mutate(codaz = substr(codaz, 1, 8), 
           codaz = casefold(codaz, upper = TRUE)) %>%  
  left_join(
      t1, by = c("codaz" = "codice")
    ) %>%  
    
    left_join(
      tabella_prove, by = "nconf"
      
    )  %>%  View()
  
  # esempio dati pupe----
  dt <- read_excel(here("datipupe.xlsx"))
  dt <- clean_names(dt)

  new_dati <- dt %>%
    select(specie, larval_diet, replica, day) %>%
    unique() %>%
    group_by(specie, larval_diet, replica) %>%
    mutate(timing = rank(day)) %>% View()
    right_join(dt, by = c("specie", "larval_diet", "replica", "day"))
  # 
  # 
  # 
  
  
  
  # anagrafe <- read_excel("coordbv.xlsx")
  # 
  # conferimenti <- readRDS(here("conferimenti.RDS"))
  
  
  # # proveIN <- readRDS(here("proveIN.RDS"))
  # # proveOUT <- readRDS(here( "proveOUT.RDS"))
  # 
  # #Abr <- readRDS(here("Modena", "data", "processed", "Abr.RDS"))
  # 
  # proveIN %>%  
  #   mutate(proveINOUT = rep("in_sede", nrow(.))) %>% 
  #   bind_rows(
  #     proveOUT %>% 
  #       mutate(proveINOUT = rep("fuori_sede", nrow(.)))   
  #     ) %>%  
  #  mutate(Nconf = paste0(anno, nconf), 
  #          # tecnica = sapply(tecnica, iconv, from = "latin1", to = "UTF-8", sub = ""), 
  #          # testodett = sapply(testodett, iconv, from = "latin1", to = "UTF-8", sub = ""), 
  #          # modexpr2 = sapply(modexpr2, iconv, from = "latin1", to = "UTF-8", sub = ""), 
  #          # prova = sapply(prova, iconv, from = "latin1", to = "UTF-8", sub = ""), 
  #          # esiti = sapply(esiti, iconv, from = "latin1", to = "UTF-8", sub = ""),
  #          # str_analisi = sapply(str_analisi, iconv, from = "latin1", to = "UTF-8", sub = ""), 
  #          # lab_analisi = sapply(lab_analisi, iconv, from = "latin1", to = "UTF-8", sub = "")
  #   ) -> prove
  #  #saveRDS(file = here("Modena", "data", "processed", "prove.RDS"))
  # 
  # # conferimenti <- conferimenti %>% 
  # #   mutate(
  # #     comune = sapply(comune, iconv, from = "latin1", to = "UTF-8", sub = ""),
  # #     finalita = sapply(finalita, iconv, from = "latin1", to = "UTF-8", sub = ""),
  # #     motivo_prel = sapply(motivo_prel, iconv, from = "latin1", to = "UTF-8", sub = ""),
  # #     materiale = sapply(materiale, iconv, from = "latin1", to = "UTF-8", sub = "")
  # #     )
  # 
  # conferimenti %>%
  #  #distinct() %>% View()
  #   mutate(anno = year(dtconf),
  #          annoprel = year(dtprel),
  #          annoreg = year(dtreg),
  #          codaz = casefold(codaz, upper = TRUE),
  #          Nconf = paste0(anno, nconf)) %>% 
  #   distinct(Nconf, .keep_all = TRUE) %>% 
  #   dplyr::select(-finalita) %>% 
  #   left_join(
  #     conferimenti %>%
  #       distinct() %>% 
  #       mutate(anno = year(dtconf),
  #              Nconf = paste0(anno,nconf)) %>% 
  #       dplyr::select(Nconf, finalita) %>% 
  #       distinct() %>% 
  #       pivot_wider(names_from = "finalita", values_from = "finalita") %>%  
  #       unite("finalita", 2:85, na.rm = TRUE, remove = FALSE) %>% 
  #       dplyr::select(1,2), by="Nconf") %>% View()
  #   
  #     
  #     saveRDS(file = here("Modena", "data", "processed", "conf.RDS"))
  # 
  #   
  # 
  # 
  # 
  # 
  # 
  #   
  #   conferimenti %>%
  #     select(nconf,codaz, dtprel, dtconf, dtreg, settore, finalita,str_acc,str_propr, specie, materiale, matrice, 
  #            NrCampioni) %>%  
  #     mutate(anno = year(dtconf),
  #            # annoprel = year(dtprel),
  #            # annoreg = year(dtreg),
  #            codaz = casefold(codaz, upper = TRUE),
  #            Nconf = paste0(anno, nconf)) %>% View()
  #     distinct(Nconf, .keep_all = TRUE) %>% 
  #   right_join(
  #     
  #     prove %>% 
  #       select(Nconf, str_analisi,prova, tecnica, proveINOUT ), 
  #     by = "Nconf") %>%  View()
  #   
  #   
  # ## esempio coordinate---
  # 
  #....
  
  # df <- read.csv(here('anaglomb.csv'), sep=',', header=T) #tabella_anagrafe
  # df$duplmarch[is.na(df$duplmarch)] <- 0
  # df$tipo= ifelse(df$tipologia==1|df$tipologia==2|df$tipologia==3|df$tipologia==4|df$tipologia==5, 'ripro','prod')
  # 
  # df %>% 
  #   write.xlsx(file = "tabella_anagrafe.xlsx")
  # 
  # library(foreign)
  # coord <- read.dbf(here('coordinate.dbf')) #tabella_coordinate
  # write.xlsx(coord, file =  "tabella_coordinate.xlsx")
  # 
  # prove_izsler <- readRDS(file = "prove BG.RDS")
  # write.xlsx(prove_izsler, file = "tabella_prove.xlsx")
    # conf_izsler <-  readRDS(file = "conferimenti BG.RDS")
    # write.xlsx(conf_izsler, file = "tabella_conferimenti.xlsx")
   