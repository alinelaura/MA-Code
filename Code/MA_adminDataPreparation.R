#######################   Prepare administrative Data    ########################

# Libraries
library(tidyverse)
library(dplyr)
library(zoo)
library(lubridate)
library(tidyr)

# Read Data
data_admin <- read.csv("/Users/alinelaurametzler/Documents/Universität/Master/Master Thesis/Data/IndividualdatenSTISTAT_2010-2020_5818_Lieferungsdatum-2022-04-12/IndividualdatenSTISTAT_2010-2020_5818.csv") 

# Rename variables and format dates
data_admin <- data_admin %>% 
  dplyr::rename(steuerb_einkommen=ziffer_268_veranlagt,
         reinvermoegen=ziffer_326_veranlagt,
         anz_ki_kinderabzug=ziffer_25,
         anz_ki_u15_hh=ziffer_18,
         steuer_tarif=ziffer_10
  ) %>%
  mutate(abstimmungsdatum = as.Date(abstimmungsdatum, "%d.%m.%Y"),
         abstimmungsjahr = as.integer(abstimmungsjahr),
         abstimmungsmonat = as.integer(abstimmungsmonat)) %>% 
  filter(!is.na(id_ek))


#####################  Municipalities with no tax data  #########################

# For now: only SG city has tax data available
data_admin <- data_admin %>% 
  filter(bfsnr == 3203)


############## Account for people who moved and then moved back #################

# All individuals and the years they lived in the community
id_year <- data_admin %>% 
  select(id_ek, abstimmungsjahr, bfsnr) %>% 
  group_by(id_ek, abstimmungsjahr) %>% 
  unique() %>% 
  arrange(id_ek, abstimmungsjahr)


# fill in missing years where people moved away and then moved back
id_year <- id_year %>% 
  group_by(id_ek) %>% 
  mutate(abstimmungsjahr = as.numeric(as.character(abstimmungsjahr))) %>% 
  complete(abstimmungsjahr = first(abstimmungsjahr):max(abstimmungsjahr), 
           fill = list(bfsnr = 0)) %>% 
  mutate(abstimmungsjahr = as.integer(abstimmungsjahr)) 

# different ID for individuals before and after they move
# for every change in bfsnr increase by 1
id_year <- id_year %>% 
  mutate(pers_change = cumsum(c(1, diff(id_year$bfsnr) != 0))) %>% 
  group_by(id_ek, pers_change) %>% 
  dplyr::mutate(ID_move_change = cur_group_id()) %>% 
  ungroup() %>% 
  select(-pers_change) 

# # Controll that it worked 
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
  arrange(id_ek, ID_move_change, abstimmungsjahr) %>% 
  mutate(steuerb_einkommen_imputed = steuerb_einkommen) %>% 
  fill(steuerb_einkommen_imputed, .direction = "down") %>% 
  mutate(imputed = case_when(
    !is.na(steuerb_einkommen) ~ 0,
    is.na(steuerb_einkommen_imputed) ~ 0,
    (is.na(steuerb_einkommen) & !is.na(steuerb_einkommen_imputed)) ~ 1
  ))

rm(data_prep)
##################  Count of consequtively following votes ######################

data <- data_imp %>% 
  arrange(id_ek, ID_move_change, abstimmungsjahr, abstimmungsmonat) %>% 
  group_by(id_ek, ID_move_change) %>% 
  dplyr::mutate(abst_reihe = row_number())

rm(data_imp)
##################### Create variable with type of voters #######################

# Filter individuals that at least have 10 consecutive votes  
data <- data %>% 
  group_by(id_ek, ID_move_change) %>% 
  dplyr::mutate(anz_folg_abst = max(abst_reihe)) %>% 
  filter(anz_folg_abst >= 10) %>% 
  ungroup() 

# if individual moved and lived in community for 10 votes more than once
# then take most recent stay (most recent move) in city
data <- data %>% 
  group_by(id_ek) %>%
  dplyr::mutate(ind_mostrecent = max(ID_move_change)) %>% 
  filter(ID_move_change %in% ind_mostrecent) 
# %>% 
#   select(-ID_move_change, -ind_mostrecent)


# for individuals that have more than 10 consecutive votes, take most recent 10
data <- data %>% 
  arrange(id_ek, desc(abst_reihe)) %>% 
  group_by(id_ek) %>%
  slice(1:10) %>% 
  arrange(id_ek, abst_reihe)


length(unique(data$id_ek))  # 58'535 individuals for SG city


# create type of voters
# count number of times participated, then create variable of voter type
data <- data %>% 
  group_by(id_ek) %>% 
  dplyr::mutate(anz_teilnahme = sum(beteiligt)) %>% 
  dplyr::mutate(vote_type = case_when(
    anz_teilnahme <1 ~ "never voter",
    anz_teilnahme >= 9 ~ "always voter",
    TRUE ~ "selective voter"
  ))


count(data$vote_type)  
  