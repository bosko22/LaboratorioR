
covid <- read_csv("C:/Users/stefano.baselli/Desktop/R_progetti/Git_Project/R_Project/dati/covid.csv")

glimpse(covid)


covid = clean_names(covid)

# 

# 

# 

# 

# 


covid = covid %>% 
  mutate(materiale = if_else(materiale %in% c("TAMPOE", "TAMPONI"), "TAMPONE", materiale))

unique(covid$materiale)

         
View(glimpse(arrange(covid, desc(anno))))

View(filter(covid, anno == 2023))

dfgrouped = group_by(covid, anno)
#raggruppo sulla variabile anno

#View(dfgrouped)



# Compiti_a_casa ----

# -numero di esami eseguiti per anno
covid_2 = covid %>%
  select(tot, anno) %>%
  group_by(anno) %>%
  summarise(tot = sum(tot, na.rm = TRUE), n = n())


print(unique(covid$reparto))

# -numero di esami per reparto e anno
covid_3 = covid %>%
  select(tot, anno, reparto) %>%
  group_by(anno,reparto) %>%
  summarise(tot = sum(tot, na.rm = TRUE), n = n())
  
# -numero di esami per Provincia
covid_4 = covid %>%
  select(tot, provincia) %>%
  group_by(provincia) %>%
  summarise(tot = sum(tot, na.rm = TRUE), n = n())


# -numero di esami eseguiti con la prova SARS-CoV-2: agente eziologico per reparto e anno

print(unique(covid$prova))

covid_5 = covid %>%
  select(tot, reparto, anno, prova) %>%
  group_by(anno) %>%
  filter(prova == "SARS-CoV-2: agente eziologico")
  summarise(tot = sum(tot, na.rm = TRUE), n = n())

# -numero di esami per prova e per anno
covid_6 = covid %>%
  select(tot, anno, prova) %>%
  group_by(anno, prova) %>%
summarise(tot = sum(tot, na.rm = TRUE), n = n())

# -numero di esami per Materiale
covid_7 = covid %>%
  select(tot, materiale) %>%
  group_by(materiale) %>%
  summarise(tot = sum(tot, na.rm = TRUE), n = n())

# -numero dei differenti tipi di Materiale

covid_8 = covid %>%
  select(materiale) %>%
  group_by(materiale) %>%
  summarise(n = n())


# -numero di Comuni
## Non credo di aver capito questa consegna (faccio cio' che credo di aver capito)

covid_9 = covid %>%
  select(comune) %>%
  group_by(comune) %>%
  summarise(n = n())
View(covid_9)


# -numero di Conferenti
glimpse(covid)
covid_10 = covid %>%
  select(conferente) %>%
  group_by(conferente) %>%
  summarise(n = n())
View(covid_10)

covid_10.1 = unique(covid$conferente)
covid_10.1 #se mi interessa il numero, basta così no?! E' una "scorciatoia"?


# -numero di esami eseguiti per anno e per conferente
glimpse(covid)
covid_11 = covid %>%
  select(tot_eseguiti, anno, conferente) %>%
  group_by(anno, conferente) %>%
summarise(tot = sum(tot_eseguiti, na.rm = TRUE), n = n())

View(covid_11)
