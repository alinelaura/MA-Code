#######################   Prepare administrative Data    ########################

# Libraries
library(tidyverse)
library(dplyr)
library(zoo)
library(lubridate)
library(tidyr)


setwd("/Users/alinelaurametzler/Documents/Universität/Master/Master Thesis/MA-Code/")
getwd()

# Read Data
# data_adminold <- read.csv("./Data/Archiv/IndividualdatenSTISTAT_2010-2020_5818.csv") 
data_admin <- read.csv("./Data/IndividualdatenSTISTAT_2010-2020/IndividualdatenSTISTAT_2010-2020_pa5818_20220818.csv") 
# data_admin_old <- read.csv("./Data/Archiv/IndividualdatenSTISTAT_2010-2020_5818.csv") 

# Rename variables and format dates
data_admin <- data_admin %>% 
  dplyr::rename(steuerb_einkommen=ziffer_268_veranlagt,
         reinvermoegen=ziffer_326_veranlagt,
         anz_ki_kinderabzug=ziffer_25,
         anz_ki_u15_hh=ziffer_18,
         steuer_tarif=ziffer_10
  ) %>%
  mutate(datum = as.Date(abstimmungsdatum, "%m/%d/%Y"),
         abstimmungsjahr = as.integer(abstimmungsjahr),
         abstimmungsmonat = as.integer(abstimmungsmonat)) %>% 
  mutate(steuer_tarif = as.factor(steuer_tarif)) %>% 
  filter(!is.na(id_ek)) %>% 
  select(-id_loganto, -historyMunicipalityID_swissTown_placeOfBirth, -cantonAbbreviation_swissTown_placeOfBirth,
         -geburtsstaatregion, -haushalttyp_sw_2, -haushalttyp_sw_3, -haushalttyp_sw_4,
         -haushalttyp_sw_5, -haushalttyp_sw_6, -quartiergruppen, -statquartiere, -Quartierverein,
         -artstimmabgabe, -stimmabgabedatum)


############## Account for people who moved and then moved back #################

# All unique individuals and the years they lived in the community
id_year <- data_admin %>% 
  select(id_ek, abstimmungsjahr, bfsnr) %>% 
  group_by(id_ek, abstimmungsjahr, bfsnr) %>% 
  unique() %>% 
  arrange(id_ek, abstimmungsjahr, bfsnr)


# fill in missing years where people moved away and then moved back
# mainly in city SG a problem where more years available
id_year <- id_year %>% 
  group_by(id_ek) %>% 
  mutate(abstimmungsjahr = as.numeric(as.character(abstimmungsjahr))) %>% 
  complete(abstimmungsjahr = first(abstimmungsjahr):max(abstimmungsjahr), 
           fill = list(bfsnr = 0)) %>% 
  mutate(abstimmungsjahr = as.integer(abstimmungsjahr)) 

# different ID for individuals before and after they move
# for every change in bfsnr increase by 1
id_year <- id_year %>% 
  ungroup() %>% 
  dplyr::mutate(pers_change = cumsum(c(1, diff(id_year$bfsnr) != 0))) %>% 
  group_by(id_ek, bfsnr, pers_change) %>% 
  dplyr::mutate(ID_move_change = cur_group_id()) %>% 
  ungroup() %>% 
  select(-pers_change) 

# # Control that it worked 
# weg_und_zuzuege <- id_year %>%
#   ungroup() %>%
#   filter(bfsnr == 0) %>%
#   select(id_ek) %>%
#   unique() %>%
#   left_join(id_year, by = "id_ek")


# Join new Identification to big DF
# delete the missing years again
data_prep <- id_year %>% 
  filter(bfsnr != 0) %>% 
  left_join(data_admin, by = c("id_ek", "abstimmungsjahr", "bfsnr"))

rm(id_year)
rm(data_admin)
########################  Missing tax data from moving ##########################

# Impute tax data of previous year if tax from previous year is available
# create dummy variable for imputation
data_imp <- data_prep %>% 
  group_by(id_ek, ID_move_change) %>% 
  arrange(id_ek, bfsnr, ID_move_change, abstimmungsjahr) %>% 
  # mutate(veranlagung_id_imputed = veranlagung_id) %>% 
  # fill(veranlagung_id_imputed, .direction = "down") %>%
  mutate(steuer_tarif = case_when(
    steuer_tarif == "alleinstehend/unverheiratet" ~ 1,
    steuer_tarif == "verheiratet" ~ 2,
    TRUE ~ NA_real_
  ))  %>% 
  mutate(steuer_tarif_imputed = steuer_tarif) %>% 
  fill(steuer_tarif_imputed, .direction = "down") %>%
  mutate(steuer_tarif = case_when(
    steuer_tarif == 1 ~ "alleinstehend/unverheiratet",
    steuer_tarif == 2 ~ "verheiratet",
    TRUE ~ NA_character_
  )) %>% 
  mutate(steuer_tarif_imputed = case_when(
    steuer_tarif_imputed == 1 ~ "alleinstehend/unverheiratet",
    steuer_tarif_imputed == 2 ~ "verheiratet",
    TRUE ~ NA_character_
  )) %>% 
  mutate(anz_ki_u15_hh_imputed = anz_ki_u15_hh) %>% 
  fill(anz_ki_u15_hh_imputed, .direction = "down") %>%
  mutate(anz_ki_kinderabzug_imputed = anz_ki_kinderabzug) %>% 
  fill(anz_ki_kinderabzug_imputed, .direction = "down") %>%
  mutate(steuerb_einkommen_imputed = steuerb_einkommen) %>% 
  fill(steuerb_einkommen_imputed, .direction = "down") %>% 
  mutate(reinvermoegen_imputed = reinvermoegen) %>% 
  fill(reinvermoegen_imputed, .direction = "down") %>%
  mutate(massgebendesEinkommen_imputed = massgebendesEinkommen) %>% 
  fill(massgebendesEinkommen_imputed, .direction = "down") %>%
  mutate(imputed = case_when(
    !is.na(steuerb_einkommen) ~ 0,
    is.na(steuerb_einkommen_imputed) ~ 0,
    (is.na(steuerb_einkommen) & !is.na(steuerb_einkommen_imputed)) ~ 1
  ))

rm(data_prep)

######################### Impute missing values in df ###########################
# TODO: how should I account for the imputed values? They are very likely to be correct
# but do I need to do some test/ include dummies for the imputation?

# So far only impute vars that I need for analysis
# TODO: impute age (but how, since we don't know birthday?)
# -->should I just take last known age and not care whether people got a year older?
data_imp2 <- data_imp %>% 
  ungroup() %>% 
  group_by(householdID_sw) %>%
  mutate(householdID_sw_imp = as.numeric(cur_group_id())) %>% 
  mutate(householdID_sw_imp = case_when(
    householdID_sw_imp != 1 ~ householdID_sw_imp,
    TRUE ~ NA_real_
  )) %>% 
  ungroup() %>% 
  group_by(id_ek, ID_move_change, bfsnr) %>% 
  arrange(id_ek, ID_move_change, abstimmungsjahr, abstimmungsmonat) %>% 
  mutate(sex_imp = case_when(
    sex == "Frau" ~ 2,
    sex == "Mann" ~ 1,
    TRUE ~ NA_real_
  )) %>%
  mutate(konfession_imp = case_when(
    konfession == "evangelisch-reformierte Kirche" ~ 1,
    konfession == "Römisch-katholische Kirche" ~ 2,
    konfession == "andere oder keine Konfessionszugehörigkeit" ~ 3,
    TRUE ~ NA_real_
  )) %>%
  mutate(residenz_imp = case_when(
    aufenthaltsdaueringemeinde == "höchtens 1 Jahr" ~ 0, 
    aufenthaltsdaueringemeinde == "2-5 Jahre" ~ 2, 
    aufenthaltsdaueringemeinde == "6-10 Jahre" ~ 3,
    aufenthaltsdaueringemeinde == "mehr als 10 Jahre" ~ 4,
    TRUE ~ NA_real_
  )) %>%
  mutate(haushalttyp_sw_1_imp = case_when(
    haushalttyp_sw_1 == "Privathaushalt" ~ 1,
    haushalttyp_sw_1 == "Kollektivhaushalt" ~ 2,
    haushalttyp_sw_1 == "Sammelhaushalt" ~ 3,
    TRUE ~ NA_real_
  )) %>% 
  fill(sex_imp, .direction = "updown") %>% 
  fill(konfession_imp, .direction = "updown") %>% 
  fill(residenz_imp, .direction = "updown") %>%
  fill(Geburtsstaat, .direction = "updown") %>% 
  fill(householdID_sw_imp, .direction = "updown") %>% 
  fill(haushalttyp_sw_1_imp, .direction = "updown") %>% 
  fill(privathaushaltgroesse_sw, .direction = "updown") %>% 
  fill(anzahljahreingemeinde, .direction = "updown")
  
# test <- data_imp2 %>% 
#   select(abstimmungsjahr, abstimmungsmonat, id_ek, householdID_sw, householdID_sw_imp)

##### Make categories for numerical variables & define levels of variables #####
# Age:
# 1st Quintile: 18-30 years
# 2nd Quintile: 31-45 years
# 3rd Quintile: 46-60 years
# 4th Quintile: 61-75 years
# 5th Quintile: >75 years
# 
# Gender:
# Male: 1
# Female: 2
# 
# (Zivilstand: )
# 
# Konfession:
# - Christliche Konfession
# - Andere/Keine Konfession
# 
# Aufenthalt in Gemeinde:
# - 0-10: Jahre
# - >10 Jahre
# 
# Zugezogen:
# - In CH Geboren
# - Zugezogen
# 
# Einkommen (massgebendes Einkommen vs. Äquivalnezeinkommen:
# 1st Quartile: 0-25'000.-
# 2nd Quartile: 25'000-55'000.-
# 3rd Quartile: 55'000-90'000.-
# 4th Quartile: >90'000.-
#              
# Vermögen:
# 1st Quartile: 0-8'000.-
# 2nd Quartile: 8'000-60'000.-
# 3rd Quartile: 60'000-185'000.-
# 4th Quartile: >185'000.-


data_cat <- data_imp2 %>% 
  ungroup() %>% 
  mutate(alter_c = as.factor(case_when(
    alter_v <= 30 ~ "18-30-Jährige",
    alter_v > 30 & alter_v <= 45 ~ "31-45-Jährige",
    alter_v > 45 & alter_v <= 60  ~ "46-60-Jährige",
    alter_v > 60 & alter_v <= 75 ~ "61-75-Jährige",
    alter_v > 75 ~ "Über 75-Jährige"
    ))) %>% 
  mutate(sex_c = as.factor(case_when(
    sex_imp == 1 ~ "Mann",
    sex_imp == 2 ~ "Frau"
  ))) %>%
  mutate(konfession_c= as.factor(case_when(
    konfession_imp %in% c(1, 2) ~ "Christliche Konfession",
    konfession_imp == 3 ~ "Andere/keine Konfession"
  ))) %>%
  mutate(residenz10 = as.factor(case_when(
    residenz_imp %in% c(0, 2, 3) ~ "0-10 Jahre",
    residenz_imp == 4 ~ "Mehr als 10 Jahre"
  ))) %>%
  mutate(zugezogen = as.factor(case_when(
    Geburtsstaat != 8100 ~ "CH zugezogen",
    Geburtsstaat == 8100 ~ "In CH geboren",
    is.na(Geburtsstaat) ~ NA_character_
  ))) %>% 
  mutate(mEinkommen_c = as.factor(case_when(
    massgebendesEinkommen_imputed <= 25000 ~ "0-25'000.-",
    massgebendesEinkommen_imputed > 25000 & massgebendesEinkommen_imputed <= 55000 ~ "25'000-55'000.-",
    massgebendesEinkommen_imputed > 55000 & massgebendesEinkommen_imputed <= 90000 ~ "55'000-90'000.-",
    massgebendesEinkommen_imputed > 90000 ~ "Über 90'000.-"
  ))) %>% 
  mutate(Vermoegen_c = as.factor(case_when(
    reinvermoegen_imputed <= 8000 ~ "0-8'000.-",
    reinvermoegen_imputed > 8000 & reinvermoegen_imputed <= 60000 ~ "8'000-60'000.-",
    reinvermoegen_imputed > 60000 & reinvermoegen_imputed <= 185000 ~ "60'000-185'000.-",
    reinvermoegen_imputed > 185000 ~ "Über 185'000.-"
  ))) %>% 
  mutate(alter_c = fct_relevel(alter_c, c("18-30-Jährige","31-45-Jährige","46-60-Jährige","61-75-Jährige","Über 75-Jährige")),
         sex = fct_relevel(sex, c("Mann", "Frau")),
         konfession_c = fct_relevel(konfession_c, c("Andere/keine Konfession", "Christliche Konfession")),
         residenz10 = fct_relevel(residenz10, c("0-10 Jahre", "Mehr als 10 Jahre")),
         zugezogen = fct_relevel(zugezogen, c("In CH geboren","CH zugezogen")),
         mEinkommen_c = fct_relevel(mEinkommen_c, c("0-25'000.-","25'000-55'000.-","55'000-90'000.-","Über 90'000.-")),
         Vermoegen_c = fct_relevel(Vermoegen_c, c("0-8'000.-","8'000-60'000.-","60'000-185'000.-","Über 185'000.-")))

summary(data_cat)
rm(data_imp, data_imp2)


################ calculate weighted household income and wealth #################
data_income <- data_cat %>% 
  group_by(abstimmungsjahr, id_ek) %>% 
  filter(abstimmungsmonat == max(abstimmungsmonat)) %>% 
  ungroup() %>% 
  select(abstimmungsjahr, id_ek, householdID_sw_imp, haushalttyp_sw_1_imp,privathaushaltgroesse_sw,
         hh_anzahlstimmberechtigte, steuer_tarif_imputed, anz_ki_u15_hh_imputed,
         anz_ki_kinderabzug_imputed, steuerb_einkommen_imputed, reinvermoegen_imputed,
         massgebendesEinkommen_imputed) %>% 
  group_by(abstimmungsjahr, householdID_sw_imp) %>% 
  mutate(anz_pers_ue15 = privathaushaltgroesse_sw - anz_ki_u15_hh_imputed,
         steuerb_einkommen_imputed = as.numeric(steuerb_einkommen_imputed),
         reinvermoegen_imputed = as.numeric(reinvermoegen_imputed)) %>% 
  mutate(steuerbEinkommen_gedeckelt = case_when(
    steuerb_einkommen_imputed == 163200 ~ 1,
    TRUE ~ 0
  )) %>% 
  mutate(reinvermoegen_gedeckelt = case_when(
    reinvermoegen_imputed == 423417 ~ 1,
    TRUE ~ 0
  )) %>% 
  # Preparation for household income: all incomes in a household are added
  mutate(steuerb_einkommen_imputed2 = case_when(
    steuer_tarif_imputed == "verheiratet" ~ (0.5 * steuerb_einkommen_imputed),
    steuer_tarif_imputed == "alleinstehend/unverheiratet" ~ steuerb_einkommen_imputed,
    TRUE ~ steuerb_einkommen_imputed
  )) %>% 
  mutate(reinvermoegen_imputed2 = case_when(
    steuer_tarif_imputed == "verheiratet" ~ 0.5 * reinvermoegen_imputed,
    steuer_tarif_imputed == "alleinstehend/unverheiratet" ~ reinvermoegen_imputed,
    TRUE ~ reinvermoegen_imputed
  )) %>% 
  # Married people with shared taxation will be divided by 1.5 because they still
  # have more money than a single person since many of the costs are shares
  mutate(steuerb_einkommen_imputed = case_when(
    steuer_tarif_imputed == "verheiratet" ~ (steuerb_einkommen_imputed/1.5),
    steuer_tarif_imputed == "alleinstehend/unverheiratet" ~ steuerb_einkommen_imputed,
    TRUE ~ steuerb_einkommen_imputed
  )) %>% 
  mutate(massgebendesEinkommen_imputed = case_when(
    steuer_tarif_imputed == "verheiratet" ~ (massgebendesEinkommen_imputed/1.5),
    steuer_tarif_imputed == "alleinstehend/unverheiratet" ~ massgebendesEinkommen_imputed,
    TRUE ~ massgebendesEinkommen_imputed
  )) %>% 
  mutate(reinvermoegen_imputed = case_when(
    steuer_tarif_imputed == "verheiratet" ~ (reinvermoegen_imputed/1.5),
    steuer_tarif_imputed == "alleinstehend/unverheiratet" ~ reinvermoegen_imputed,
    TRUE ~ reinvermoegen_imputed
  )) %>% 
  # Calculate household income
  mutate(hhaequEinkommen =  case_when(
    haushalttyp_sw_1_imp == 1 ~ (sum(steuerb_einkommen_imputed2)/(1 + (anz_pers_ue15-1)*0.5 + anz_ki_u15_hh_imputed*0.3)), 
    haushalttyp_sw_1_imp == 2 ~ steuerb_einkommen_imputed2, # there is no household income for "Kollektivhaushalte"
    haushalttyp_sw_1_imp == 3 ~ steuerb_einkommen_imputed2, # there is no household income for "Sammelhaushalte"
    TRUE ~ NA_real_
  )) %>% 
  mutate(hhaequVermoegen = case_when(
    haushalttyp_sw_1_imp == 1 ~ (sum(reinvermoegen_imputed2)/(1 + (anz_pers_ue15-1)*0.5 + anz_ki_u15_hh_imputed*0.3)),
    haushalttyp_sw_1_imp == 2 ~ reinvermoegen_imputed2, # there is no household income for "Kollektivhaushalte"
    haushalttyp_sw_1_imp == 3 ~ reinvermoegen_imputed2, # there is no household income for "Sammelhaushalte"
    TRUE ~ NA_real_
  )) %>% 
  select(-steuerb_einkommen_imputed2, -reinvermoegen_imputed2)

rm(data_catmm)

##################### Create variable with type of voters #######################

# Filter individuals that at least have 10 consecutive votes  
data_mlogit <- data %>% 
  group_by(id_ek, ID_move_change) %>% 
  dplyr::mutate(anz_folg_abst = max(abst_reihe)) %>% 
  filter(anz_folg_abst >= 10) %>% 
  ungroup() 

# if individual moved and lived in community for 10 votes more than once
# then take most recent stay (most recent move) in city
data_mlogit <- data_mlogit %>% 
  group_by(id_ek) %>%
  dplyr::mutate(ind_mostrecent = max(ID_move_change)) %>% 
  filter(ID_move_change %in% ind_mostrecent) 
# %>% 
#   select(-ID_move_change, -ind_mostrecent)


# for individuals that have more than 10 consecutive votes, take most recent 10
data_mlogit <- data_mlogit %>% 
  arrange(id_ek, desc(abst_reihe)) %>% 
  group_by(id_ek) %>%
  slice(1:10) %>% 
  arrange(id_ek, abst_reihe)


length(unique(data_mlogit$id_ek))  


# create type of voters
# count number of times participated, then create variable of voter type
data_mlogit <- data_mlogit %>% 
  group_by(id_ek) %>% 
  dplyr::mutate(beteiligt = case_when(
    beteiligt == "mit Stimmbeteiligung" ~ 1,
    TRUE ~ 0
  )) %>% 
  dplyr::mutate(anz_teilnahme = sum(beteiligt)) %>% 
  dplyr::mutate(vote_type = case_when(
    anz_teilnahme < 1 ~ "never voter",
    anz_teilnahme > 9 ~ "always voter",
    TRUE ~ "selective voter"
  )) %>% 
  dplyr::mutate(vote_type_det = case_when(
    anz_teilnahme <1 ~ "never voter",
    anz_teilnahme >= 1 & anz_teilnahme < 4 ~ "seldom voter",
    anz_teilnahme >= 4 & anz_teilnahme < 7 ~ "occasional voter",
    anz_teilnahme >= 7 & anz_teilnahme < 10 ~ "frequent voter",
    TRUE ~ "always voter"
  ))

# only have most recent vote for individuals for mlogit
data_mlogit <- data_mlogit %>% 
  group_by(id_ek) %>% 
  filter(abst_reihe == max(abst_reihe)) %>% 
  mutate(vote_type = as.factor(vote_type),
         vote_type_det = as.factor(vote_type_det)) %>%
  mutate(vote_type =  fct_relevel(vote_type, c("never voter", "selective voter", "always voter")),
         vote_type_det =  fct_relevel(vote_type_det, c("never voter","seldom voter","occasional voter","frequent voter","always voter")))


################# Control: look at last 15 votes for vote types ##################
# Filter individuals that at least have 10 consecutive votes  
data_mlogit15 <- data %>% 
  group_by(id_ek, ID_move_change) %>% 
  dplyr::mutate(anz_folg_abst = max(abst_reihe)) %>% 
  filter(anz_folg_abst >= 15) %>% 
  ungroup() 

# if individual moved and lived in community for 10 votes more than once
# then take most recent stay (most recent move) in city
data_mlogit15 <- data_mlogit15 %>% 
  group_by(id_ek) %>%
  dplyr::mutate(ind_mostrecent = max(ID_move_change)) %>% 
  filter(ID_move_change %in% ind_mostrecent) 
# %>% 
#   select(-ID_move_change, -ind_mostrecent)


# for individuals that have more than 10 consecutive votes, take most recent 10
data_mlogit15 <- data_mlogit15 %>% 
  arrange(id_ek, desc(abst_reihe)) %>% 
  group_by(id_ek) %>%
  slice(1:15) %>% 
  arrange(id_ek, abst_reihe)


length(unique(data_mlogit15$id_ek))  # 58'535 individuals for SG city


# create type of voters
# count number of times participated, then create variable of voter type
data_mlogit15 <- data_mlogit15 %>% 
  group_by(id_ek) %>% 
  dplyr::mutate(beteiligt = case_when(
    beteiligt == "mit Stimmbeteiligung" ~ 1,
    TRUE ~ 0
  )) %>% 
  dplyr::mutate(anz_teilnahme = sum(beteiligt)) %>% 
  dplyr::mutate(vote_type = case_when(
    anz_teilnahme < 2 ~ "never voter",
    anz_teilnahme > 13 ~ "always voter",
    TRUE ~ "selective voter"
  )) %>% 
  dplyr::mutate(vote_type_det = case_when(
    anz_teilnahme < 2 ~ "never voter",
    anz_teilnahme >= 2 & anz_teilnahme < 6 ~ "seldom voter",
    anz_teilnahme >= 6 & anz_teilnahme < 10 ~ "occasional voter",
    anz_teilnahme >= 10 & anz_teilnahme < 14 ~ "frequent voter",
    TRUE ~ "always voter"
  ))

# only have most recent vote for individuals for mlogit
data_mlogit15 <- data_mlogit15 %>% 
  group_by(id_ek) %>% 
  filter(abst_reihe == max(abst_reihe)) %>% 
  mutate(vote_type = as.factor(vote_type),
         vote_type_det = as.factor(vote_type_det)) %>%
  mutate(vote_type =  fct_relevel(vote_type, c("never voter", "selective voter", "always voter")),
         vote_type_det =  fct_relevel(vote_type_det, c("never voter","seldom voter","occasional voter","frequent voter","always voter")))


  summary(data_mlogit)
  
  
  #################### Write csv with df ready for analysis #####################
  write.csv(data,"./Data/PreparedData/data.csv", row.names = FALSE)
  write.csv(data_mlogit,"./Data/PreparedData/data_mlogit.csv", row.names = FALSE)
  write.csv(data_mlogit15,"./Data/PreparedData/data_mlogit15.csv", row.names = FALSE)
  