library(tidyverse)
library(janitor)
library(here)
library(readxl)
# installed.packages("gt")
library(gt)

# GT TABLE
# https://gt.rstudio.com/articles/gt.html
# https://gt.albert-rapp.de/getting_started
# https://themockup.blog/static/resources/gt-cookbook.html

# TABLES IN R
# https://rfortherestofus.com/2019/11/how-to-make-beautiful-tables-in-r

# COVID----
covid <- readRDS(here("dati", "covid.rds"))

View(covid)
names(covid)


covid %>% 
  count(Finalità)
covid %>% 
  count(Materiale) %>% arrange(Materiale)
covid %>% 
  count(Prova) %>% arrange(Prova)
covid %>% 
  count(Reparto)

table(covid$Finalità)
table(covid$Materiale)
table(covid$Prova)
table(covid$Reparto)
table(covid$Prova,covid$Finalità)

covid %>% 
  summarise(min(dtconf),max(dtconf))

covid %>% 
  distinct(Prova) %>% arrange(Prova)

# escludiamo prova = agente eziologico
# accorpiamo prova = sequenziamento E prova = identificazione varianti

covid %>%
  #PROVA
  filter(!Prova == "Agente eziologico") %>% 
  mutate(Prova2 = case_when(
    str_detect(Prova, "Sequenziamento") ~ "SARS-CoV-2: sequenziamento",
    str_detect(Prova, "identificazione") ~ "SARS-CoV-2: identificazione varianti",
    TRUE ~ Prova
  ), .after = Prova) %>% 
  distinct(Prova, Prova2) %>%
  arrange(Prova) %>% 
  View()


covid %>% 
  distinct(Materiale) %>% arrange(Materiale)

# accorpiamo materiale = tampone E materiale = salivare E materiale = RNA SARS-COV-2
# escludiamo tutti gli altri materiali


covid %>%
  #PROVA
  filter(!Prova == "Agente eziologico") %>% 
  mutate(Prova = case_when(
    str_detect(Prova, "Sequenziamento") ~ "SARS-CoV-2: sequenziamento",
    str_detect(Prova, "identificazione") ~ "SARS-CoV-2: identificazione varianti",
    TRUE ~ Prova
  )) %>% 
  #MATERIALE
  mutate(Materiale2 = case_when(
    str_detect(Materiale, "TAMPO") ~ "TAMPONE",
    str_detect(Materiale, "SALIVA") ~ "SALIVARE",
    str_detect(Materiale, "RNA") ~ "RNA SARS-CoV-2",
    TRUE ~ "ALTRI MATERIALI"), .after = Materiale) %>%
  # filter(!Materiale == "ALTRI MATERIALI") %>%
  distinct(Materiale, Materiale2) %>%
  arrange(Materiale) %>% 
  View()
  
# escludiamo tutti gli altri materiali e salviamo

dt <- covid %>%
  #PROVA
  filter(!Prova == "Agente eziologico") %>% 
  mutate(Prova = case_when(
    str_detect(Prova, "Sequenziamento") ~ "SARS-CoV-2: sequenziamento",
    str_detect(Prova, "identificazione") ~ "SARS-CoV-2: identificazione varianti",
    TRUE ~ Prova
  )) %>% 
  #MATERIALE
  mutate(Materiale = case_when(
    str_detect(Materiale, "TAMPO") ~ "TAMPONE",
    str_detect(Materiale, "SALIVA") ~ "SALIVARE",
    str_detect(Materiale, "RNA") ~ "RNA SARS-CoV-2",
    TRUE ~ "ALTRI MATERIALI"), .after = Materiale) %>%
  filter(!Materiale == "ALTRI MATERIALI")

## tab campioni per conferente----

dt %>% 
  group_by(Conferente) %>% 
  summarise(esami = sum(Tot_Eseguiti, na.rm = TRUE)) %>%
  arrange(desc(esami)) %>% 
  ungroup()

# colleghiamo informazioni conferenti da file conf
conf <- read_excel(here("dati", "conferenti.xlsx"))
View(conf)

dt %>% 
  group_by(Conferente) %>% 
  summarise(esami = sum(Tot_Eseguiti, na.rm = TRUE)) %>%
  arrange(desc(esami)) %>% 
  ungroup() %>% 
  left_join(conf, by = c("Conferente"= "conferente")) %>% View()
  group_by(classificazione) %>% 
  summarise(esami = sum(esami, na.rm = TRUE)) %>% View()


dt %>% 
  group_by(Conferente) %>% 
  summarise(esami = sum(Tot_Eseguiti, na.rm = TRUE)) %>%
  arrange(desc(esami)) %>% 
  ungroup() %>% 
  mutate(Conferente = str_trim(Conferente)) %>% 
  left_join(conf, by = c("Conferente"= "conferente")) %>% # View()
  group_by(classificazione) %>% 
  summarise(esami = sum(esami, na.rm = TRUE)) %>% View()
  

dt1 <- dt %>% 
    group_by(Conferente) %>% 
    summarise(esami = sum(Tot_Eseguiti, na.rm = TRUE)) %>%
    arrange(desc(esami)) %>% 
    ungroup() %>% 
    mutate(Conferente = str_trim(Conferente)) %>% 
    left_join(conf, by = c("Conferente"= "conferente")) %>%
    group_by(classificazione) %>% 
    summarise(esami = sum(esami, na.rm = TRUE)) %>% 
  # togli classificazione ="da eliminare"
    filter(!classificazione == "da eliminare") %>% 
    arrange(desc(esami)) %>% 
    rename(Conferente = classificazione,
           Campioni = esami)



dt1 %>%
  gt() %>%
  tab_header(
    title = "IZSLER - Campioni processati per tipo di conferente",
    subtitle = "marzo 2020 - settembre 2022"
  ) %>% 
  tab_style(
    style = list(
      cell_text(align = "left")),
    locations = cells_title()) %>% 
  tab_style(style = list(
    cell_text(weight = "bold")),
    locations = cells_column_labels()) # %>% gtsave("tab1.png")


## tab campioni per materiale----

dt %>%
  filter(Materiale %in% c("TAMPONE", "SALIVARE")) %>% 
  group_by(anno, Materiale) %>% 
  summarise(esami = sum(Tot_Eseguiti, na.rm = TRUE)) %>%
  arrange(desc(esami)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = anno, values_from = esami) %>% 
  gt() %>%
  tab_header(
    title = "IZSLER - Campioni processati per tipo di matrice",
    subtitle = "marzo 2020 - settembre 2022"
  ) %>% 
  tab_style(style = list(
    cell_text(align = "left")),
    locations = cells_title()) %>% 
  tab_style(style = list(
    cell_text(weight = "bold")),
    locations = cells_column_labels())  %>% 
  sub_missing(
    columns = "2020", #oppure everything()
    missing_text = "-"
  ) #%>% gtsave("tab2.png")

# SALMONELLE----
salmonelle <- read_excel(here("dati","salmonelle.xlsx"))

View(salmonelle)
names(salmonelle)

salmonelle %>% 
  summarise(min(dtreg),max(dtreg))

salm <- salmonelle %>% 
  filter(dtreg < "2022-01-01") %>% 
  rename(esito = `Salmonella spp`) %>% 
  mutate(anno = year(dtreg)) %>% 
  group_by(anno, specie, esito) %>%
  count() %>% 
  ungroup() %>% 
  pivot_wider(names_from = esito, values_from = n, values_fill = 0) %>%  
  mutate(tot = pos+neg, 
         prev = pos/tot) %>% 
  select(anno, specie, prev) %>%  
  pivot_wider(names_from = anno, values_from = prev) 

salm %>% 
  gt::gt() %>%
    # cols_label(specie = "") %>% 
  sub_missing(
    columns = everything(),
    missing_text = "-"
  ) %>% 
  fmt_percent(
    decimals = 1,
    drop_trailing_zeros = TRUE
  ) %>% 
  # gt table insert title
    tab_header(
      title = "Prevalenza salmonella per specie",
      subtitle = "gennaio 2016 - dicembre 2021"
    ) %>% 
  # R align column headers gt table
  opt_align_table_header(align = "left") %>% 
  
  # OPPURE
  # https://gt.rstudio.com/reference/tab_style.html
  # https://gt.rstudio.com/reference/cells_body.html#overview-of-location-helper-functions
  
    # tab_style(style = list(
    #   cell_text(align = "left")),
    #   locations = cells_title()) %>% 
  
  
    tab_style(style = list(
      cell_text(weight = "bold")),
      locations = cells_column_labels()) %>% 
  
  # https://gt.rstudio.com/reference/tab_style_body.html
    tab_style_body(
      columns = where(is.numeric),
      style = cell_text(color = "red"),
      fn = function(x) x > 0.1)  
  
  
  # tab_style(
  #   style = list(
  #     cell_fill(color = "#F9E3D6"),
  #     cell_text(style = "italic")
  #   ),
  #   locations = cells_body(
  #     columns = everything(),
  #     rows = c(1,2)
  #   ))
