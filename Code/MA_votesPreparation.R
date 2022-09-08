############################ Prepare swissvotes Data ############################

# Libraries
library(tidyverse)
library(lubridate)
library(stringr)

setwd("/Users/alinelaurametzler/Documents/Universität/Master/Master Thesis/MA-Code/")
swissvotes <- read.csv("./Data/swissvotes/swissvotes_raw.csv", sep = ';')
labels <- read.csv("./Data/swissvotes/vote_labels.csv", sep = ';') %>% 
  mutate(Code = as.character(Code))

# Prepare Data for Analysis
swissvotes <- swissvotes %>% 
  mutate(across(starts_with('dat'), ~ as.Date(., '%d.%m.%Y'))) %>% 
  mutate(across(starts_with('d1e'), ~ as.character(.))) %>% 
  left_join(labels, by = c("d1e1" = "Code")) %>% 
  dplyr::rename(d1e1_label = Label) %>% 
  left_join(labels, by = c("d1e2" = "Code")) %>% 
  dplyr::rename(d1e2_label = Label) %>% 
  left_join(labels, by = c("d1e3" = "Code")) %>% 
  dplyr::rename(d1e3_label = Label) %>% 
  rename_with(~str_replace(., '-', '.')) %>% 
  select(datum, anr,
         sg.berecht, sg.stimmen, sg.bet, sg.japroz,
         titel_kurz_d, titel_off_d, swissvoteslink, anzahl, rechtsform, kurzbetitel, 
         anneepolitique, bkchrono.de, d1e1, d1e1_label, d1e2, d1e2_label, d1e3, d1e3_label,
         br.pos, bv.pos, nr.pos, nrja, nrnein, sr.pos, srja, srnein,
         p.fdp, p.svp, p.sps, p.mitte, p.gps, p.glp, p.evp, 
         pdev.fdp_SG, pdev.svp_SG, pdev.sps_SG, pdev.mitte_SG, pdev.gps_SG, pdev.glp_SG, pdev.evp_SG,
         w.fdp, w.svp, w.sp, w.mitte, w.gps, w.glp, w.evp,
         ja.lager,
         nein.lager,
         keinepar.summe, leer.summe, freigabe.summe, neutral.summe, unbekannt.summe,
         inserate.total, inserate.je.ausgabe, inserate.ja, inserate.nein, inserate.neutral,
         inserate.jaanteil,
         mediares.tot, mediares.d,
         mediaton.tot, mediaton.d,
         volk, stand,
         bet, volkja.proz
         )

# only keep relevant years
swissvotes <- swissvotes %>% 
  filter(datum >= "2010-01-01" & datum <= "2021-01-01") 
  
rm(labels)

# Write csv for analysis
write_csv(swissvotes, "./Data/swissvotes/swissvotes.csv")


######################## Analyse der Nachbefragungsdaten #########################
library(haven)

## Voxit: 2010-2016
# Voxit: Standardisierter Datensatz für alle Vorlagen zwischen 1977 und 2016
voxit_20102016 <- haven::read_dta("./Data/VOX_Voto/VOX/689_VoxIt_Data_CumulatedFile_Vorlagen_15_121_D.dta")

# Empfundene Relevanz & Komplexität
# zwischen 2010 und 2020 können dazu die aggregierten, repräsentativen Umfragewerte für deutschsprechende Stimmberechtigte abgerufen werde
voxit_20102016 <- voxit_20102016 %>% 
  mutate(datum = ymd(paste(annee, mois, jour, sep = "-"))) %>% 
  select(datum, jour, mois, annee, projetx, id, canton, regiling, regeco, a84x,  a89x) %>% 
  filter(annee >= 2010) %>% 
  mutate(source = "voxit",
         mois = as.character(mois),
         canton = as.character(canton),
         regiling = as.character(regiling)) %>% 
  rename(vote_nr = projetx,
         langreg = regiling,
         reg = regeco,
         difficulty = a84x,
         importance = a89x) %>% 
  # Only respondents from German speaking part of Switzerland
  filter(langreg == "1") %>% 
  # TODO: Voxit doesn't have any weights? How aggregate data then?
  group_by(datum, jour, mois, annee, vote_nr) %>% 
  mutate(schwer = case_when(
    difficulty == 1 ~ 1,
    TRUE ~ 0
  )) %>% 
  # Change values of difficulty so it matches structure of voto
  mutate(alle = case_when(
    difficulty == 1 ~ 1,
    difficulty == 2 ~ 1,
    TRUE ~ 0
  )) %>% 
  summarize(sum_schwer = sum(schwer, na.rm=TRUE),
            sum_alle = sum(alle, na.rm=TRUE),
            difficulty = (sum_schwer/sum_alle)*100, # Share of people who found it difficult
            importance = mean(importance, na.rm=TRUE)) %>% # Mean of importance level (0-10)
  select(-sum_schwer, -sum_alle) %>% 
  ungroup() %>% 
  # Z-Standardization for later Join with voto
  mutate(difficulty = (difficulty - mean(difficulty))/sd(difficulty),
         importance = (importance - mean(importance))/sd(importance))



## Voto: 2016-2020
# Get a List of DF in the directory
files <- list.files("./Data/VOX_Voto/Voto/", pattern=".+.dta$", full.names=TRUE, recursive = TRUE) 

# split to save names; name for data frame will be sixth element
names <- strsplit(files, "/")

# get the files
for (i in 1:length(files)) { # for each file in the list
  fileName <- files[[i]] 
  dataName <- names[[i]][[6]] 
  tempData <- read_dta(fileName) 
  assign (dataName, tempData, envir=.GlobalEnv)
}

rm(tempData, names)

## TODO: Are these variables similar enough? I contacted Prof. Dr. Stadelmann-Steffen
## of paper "Who decides? Characteristics of a Vote and its Influence on the Electorate"
## and she did it almost the same way

## Importance
# voto_826$importance: Personal importance of voting proposal
# Reden wir jetzt davon, wie wichtig die Vorlagen vom
# [DATE] für Sie persönlich gewesen sind.
# Sagen Sie mir bitte jeweils eine Zahl zwischen 0 und 10.
# 0 bedeutet überhaupt nicht wichtig, 10 bedeutet sehr wichtig.

# voxit$a89x: Bedeutung der Vorlage für mich persönlich
# «Sprechen wir jetzt von der Bedeutung, welche diese Abstimmung 
# für Sie persönlich hatte. Sagen Sie mir bitte anhand derselben 
# Karte, welche Bedeutung für Sie persönlich die Abstimmung über 
# die TITLE VOTE hatte? Nennen Sie mir eine Zahl zwischen 0 und 10. 
# 0 bedeutet überhaupt keine Bedeutung, 10 sehr grosse Bedeutung.»

## Difficulty
# voto_826$difficult: Difficulty with understanding
# Ist es Ihnen bei der/beim TITLE VOTING PROPOSAL
# eher leicht oder eher schwer gefallen zu verstehen,
# um was es gegangen ist?

# voxit$a84x: Schwierigkeit sich eine Meinung zu bilden (zur Vorlage)


# Select and prepare variables
prep_voto_data <- function(name, day, month, year){
  name %>% 
    mutate(jour = day,
           mois = month,
           annee = year,
           datum = ymd(paste(annee, mois, jour, sep = "-"))) %>% 
    mutate(langreg = ifelse("communelanguage" %in% names(name) == TRUE, communelanguage, NA)) %>% 
    mutate(weight = ifelse("w_dtccpv" %in% names(name) == TRUE, w_dtccpv, 1)) %>% # only round 826 has weights?! -->TODO: what about weighting here?
    select(datum, jour, mois, annee, personid_4, weight, reportingcanton, bigregion, langreg, starts_with("importance"), starts_with("difficul")) %>% 
    mutate(reportingcanton = as.character(reportingcanton)) %>% 
    mutate(across(starts_with("importance"), as.character)) %>% 
    mutate(across(starts_with("difficul"), as.character)) %>% 
    gather(vote, value, -datum, -jour, -mois, -annee, -personid_4, -weight, -reportingcanton, -bigregion, -langreg) %>% 
    mutate(vote_nr = case_when(
      vote == "importance1" ~ 1,
      vote == "importance2" ~ 2,
      vote == "importance3" ~ 3,
      vote == "importance4" ~ 4,
      vote == "importance5" ~ 5,
      vote == "importance_1" ~ 1,
      vote == "importance_2" ~ 2,
      vote == "importance_3" ~ 3,
      vote == "importance_4" ~ 4,
      vote == "importance_5" ~ 5,
      vote == "difficul1" ~ 1,
      vote == "difficul2" ~ 2,
      vote == "difficul3" ~ 3,
      vote == "difficul4" ~ 4,
      vote == "difficul5" ~ 5
    )) %>%
    mutate(var = case_when(
      grepl("importance", vote) ~ "importance",
      grepl("difficul", vote) ~ "difficulty"
    )) %>% 
    rename(id = personid_4,
           canton = reportingcanton,
           reg = bigregion) %>% 
    select(datum, jour, mois, annee, vote_nr, id, weight, canton, langreg, reg, var, value) %>% 
    spread(var, value) %>% 
    mutate(source = "voto",
           langreg = as.character(langreg),
           difficulty = as.double(difficulty),
           importance = as.double(importance)) %>%
    # Code NAs
    mutate(difficulty = case_when(
      difficulty == 8 ~ NA_real_,
      difficulty == 9 ~ NA_real_,
      TRUE ~ difficulty
    )) %>% 
    mutate(importance = case_when(
      importance == 98 ~ NA_real_,
      importance == 99 ~ NA_real_,
      TRUE ~ importance
    )) %>% 
    # Aggregate difficulty and importance per vote
    group_by(datum, jour, mois, annee, vote_nr) %>% 
    mutate(schwer = case_when(
      difficulty == 2 ~ 1,
      TRUE ~ 0
    )) %>% 
    mutate(alle = case_when(
      difficulty == 1 ~ 1,
      difficulty == 2 ~ 1,
      TRUE ~ 0
    )) %>% 
    summarize(sum_schwer = sum(schwer, na.rm=TRUE),
              sum_alle = sum(alle, na.rm=TRUE),
              difficulty = (sum_schwer/sum_alle)*100, # Share of people who found it difficult
              importance = mean(importance, na.rm=TRUE)) %>% # Mean of importance level (0-10)
    select(-sum_schwer, -sum_alle) %>% 
    ungroup()
}

       
         
voto_826 <- prep_voto_data(swissubase_826_5_0, 25, "9", 2016)
voto_839 <- prep_voto_data(swissubase_839_3_0, 27, "11", 2016)
voto_851 <- prep_voto_data(swissubase_851_3_0, 12, "2", 2017)
voto_855 <- prep_voto_data(swissubase_855_3_0, 21, "5", 2017)
voto_872 <- prep_voto_data(swissubase_872_2_0, 24, "9", 2017)
voto_921 <- prep_voto_data(swissubase_921_1_0, 4, "3", 2018)
voto_938 <- prep_voto_data(swissubase_938_1_0, 10, "6", 2018)
voto_948 <- prep_voto_data(swissubase_948_1_0, 23, "9", 2018)
voto_957 <- prep_voto_data(swissubase_957_1_0, 25, "11", 2018)
voto_972 <- prep_voto_data(swissubase_972_1_0, 10, "2", 2019)
voto_1072 <- prep_voto_data(swissubase_1072_1_0, 19, "5", 2019)
voto_1151 <- prep_voto_data(swissubase_1151_1_0, 9, "2", 2020)
voto_1225 <- prep_voto_data(swissubase_1225_2_0, 27, "9", 2020)

rm(swissubase_826_5_0, swissubase_839_3_0, swissubase_851_3_0, swissubase_855_3_0,
   swissubase_872_2_0, swissubase_921_1_0, swissubase_938_1_0, swissubase_948_1_0,
   swissubase_957_1_0, swissubase_972_1_0, swissubase_1072_1_0, swissubase_1151_1_0,
   swissubase_1225_2_0)

voto_20162020 <- voto_826 %>% 
  bind_rows(voto_839, voto_851, voto_855, voto_872, voto_921, voto_938,
            voto_948, voto_957, voto_972, voto_1072, voto_1151, voto_1225) %>% 
  # Z-Standardization for later Join with voto
  mutate(difficulty = (difficulty - mean(difficulty))/sd(difficulty),
         importance = (importance - mean(importance))/sd(importance))

rm(voto_826, voto_839, voto_851, voto_855, voto_872, voto_921, voto_938, 
   voto_948, voto_957, voto_972, voto_1072, voto_1151, voto_1225)


## Join Voxit & Voto dfs
vote_survey_data <- voxit_20102016 %>% 
  bind_rows(voto_20162020)

# Join with Code key of swissvotes and Voxit/voto
codes_key <- read.csv("./Data/VOX_Voto/codes_swissvotes_vox_voto.csv", sep = ";") %>% 
  mutate(datum = ymd(datum)) %>% 
  select(-titel_kurz_d)

vote_survey_data <- vote_survey_data %>% 
  left_join(codes_key, by = c("vote_nr", "datum"))
  
rm(codes_key)
############################ Join Voxit & Swissvotes ###########################

# Join Voxit/Voto Data with swissvotes
votes_data <- swissvotes %>% 
  left_join(vote_survey_data, by = c("anr", "datum"))


# keep the vote per day with the highest turnout (if multiple votes have the 
# same turnout then filter later by mean perceived relevance)
votes_data_short <- votes_data %>% 
  group_by(datum) %>% 
  filter(sg.bet == max(sg.bet)) %>% 
  filter(importance == max(importance))

# Write csv for analysis
write_csv(votes_data, "./Data/PreparedData/votes_data.csv")
write_csv(votes_data_short, "./Data/PreparedData/votes_data_short.csv")
