#############################Prepare swissvotes Data#############################

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

# keep the vote per day with the highest turnout (if multiple votes have the same turnout then filter later by mean perceived relevance)
swissvotes_short <- swissvotes %>% 
  group_by(datum) %>% 
  filter(sg.bet == max(sg.bet)) %>% 
  filter(datum >= "2010-01-01" & datum <= "2021-01-01") 
  

# Write csv for analysis
write_csv(swissvotes, "./Data/swissvotes/swissvotes.csv")


######################## Analyse der Nachbefragungsdaten #########################

library(haven)

## Voxit: 2010-2016
# Voxit: Standardisierter Datensatz für alle Vorlagen zwischen 1977 und 2016
voxit_20102016 <- haven::read_dta("./Data/VOX_Voto/VOX/689_VoxIt_Data_CumulatedFile_Vorlagen_15_121_D.dta")
# voxit_termine <- haven::read_dta("./Data/VOX_Voto/VOX/689_VoxIt_Data_CumulatedFile_Abstimmungstermine_15_121_D.dta")
voxit_codes <- read.csv("./Data/VOX_Voto/VOX/VorlagenCodes.csv", sep = ';') %>% 
  mutate(projetx = as.double(Code)) %>% 
  na.omit

# Empfundene Relevanz & Komplexität
# zwischen 2010 und 2020 können dazu die aggregierten,repräsentativen Umfragewerte für deutschsprechende Stimmberechtigte abgerufen werde
voxit_20102016 <- voxit_20102016 %>% 
  mutate(datum = ymd(paste(annee, mois, jour, sep = "-"))) %>% 
  select(datum, jour, mois, annee, themex, projetx, typex, scrutin, id, canton, regiling, agglomer, impactx, a81, a82x, a83, a84x,  a88x, a89x) %>% 
  filter(annee >= 2010) 


# join mit codes
voxit_20102016 <- voxit_20102016 %>% 
  left_join(voxit_codes, by = "projetx")

rm(voxit_codes)


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

# Select and prepare variables
prep_voto_data <- function(name, day, month, year){
  name %>% 
    mutate(jour = day,
           mois = month,
           annee = year,
           datum = ymd(paste(annee, mois, jour, sep = "-"))) %>% 
    select(datum, jour, mois, annee, personid_4, reportingcanton, bigregion, starts_with("importance"))
}

voto_826 <- prep_voto_data(swissubase_826_5_0, "25", "September", "2016")
voto_826 <- prep_voto_data(swissubase_839_3_0, "27", "November", "2016")
voto_826 <- prep_voto_data(swissubase_851_3_0, "12", "Februar", "2017")
voto_826 <- prep_voto_data(swissubase_855_3_0, "21", "Mai", "2017")
voto_826 <- prep_voto_data(swissubase_872_2_0, "24", "September", "2017")
voto_826 <- prep_voto_data(swissubase_921_1_0, "4", "März", "2018")
voto_826 <- prep_voto_data(swissubase_938_1_0, "10", "Juni", "2018")
voto_826 <- prep_voto_data(swissubase_948_1_0, "23", "September", "2018")
voto_826 <- prep_voto_data(swissubase_957_1_0, "25", "November", "2018")
voto_826 <- prep_voto_data(swissubase_972_1_0, "10", "Februar", "2019")
voto_826 <- prep_voto_data(swissubase_1072_1_0, "19", "Mai", "2019")
voto_826 <- prep_voto_data(swissubase_1151_1_0, "9", "Februar", "2020")
voto_826 <- prep_voto_data(swissubase_1225_2_0, "27", "September", "2020")

rm(swissubase_826_5_0, swissubase_839_3_0, swissubase_851_3_0, swissubase_855_3_0,
   swissubase_872_2_0, swissubase_921_1_0, swissubase_938_1_0, swissubase_948_1_0,
   swissubase_957_1_0, swissubase_972_1_0, swissubase_1072_1_0, swissubase_1151_1_0,
   swissubase_1225_2_0)


# voto_826$importance: Personal importance of voting proposal
# voxit$a89x: Bedeutung der Vorlage für mich persönlich

############################ Join Voxit & Swissvotes ###########################