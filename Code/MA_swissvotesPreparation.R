#############################Prepare swissvotes Data#############################

# Libraries
library(tidyverse)
library(lubridate)
library(stringr)

swissvotes <- read.csv("./Data/swissvotes/swissvotes_raw.csv", sep = ';')
labels <- read.csv("./Data/swissvotes/vote_labels.csv", sep = ';') %>% 
  mutate(Code = as.character(Code))

# Prepare Data for Analysis
swissvotes <- swissvotes %>% 
  mutate(across(starts_with('dat'), ~ as.Date(as.character(.), format = '%d%m%Y'))) %>% 
  mutate(across(starts_with('d1e'), ~ as.character(.))) %>% 
  left_join(labels, by = c("d1e1" = "Code")) %>% 
  dplyr::rename(d1e1_label = Label) %>% 
  left_join(labels, by = c("d1e2" = "Code")) %>% 
  dplyr::rename(d1e2_label = Label) %>% 
  left_join(labels, by = c("d1e3" = "Code")) %>% 
  dplyr::rename(d1e3_label = Label) %>% 
  rename_with(~str_replace(., '-', '.')) %>% 
  select(datum, titel_kurz_d, titel_off_d, swissvoteslink, anzahl, rechtsform, kurzbetitel, 
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

# Write csv for analysis
write_csv(swissvotes, "./Data/swissvotes/swissvotes.csv")




#####################Option: Analyse der Nachbefragungsdaten#####################
