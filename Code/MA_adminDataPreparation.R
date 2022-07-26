#######################   Prepare administrative Data    ########################

# Libraries
library(tidyverse)
library(dplyr)
library(zoo)
library(lubridate)
library(tidyr)

# Read Data
data_sg <- read.csv("/Users/alinelaurametzler/Documents/Universität/Master/Master Thesis/Data/IndividualdatenSTISTAT_2010-2020_5818_Lieferungsdatum-2022-04-12/IndividualdatenSTISTAT_2010-2020_5818.csv") 


data_prep <- data_sg %>% 
  dplyr::rename(steuerb_einkommen=ziffer_268_veranlagt,
         reinvermoegen=ziffer_326_veranlagt,
         anz_ki_kinderabzug=ziffer_25,
         anz_ki_u15_hh=ziffer_18,
         steuer_tarif=ziffer_10
  ) %>%
  mutate(abstimmungsdatum = as.Date(abstimmungsdatum, "%d.%m.%Y"),
         abstimmungsjahr = as.integer(abstimmungsjahr),
         abstimmungsmonat = as.integer(abstimmungsmonat))


#####################  Municipalities with no tax data  #########################

# Bis abgeklärt Steuern in anderen Gemeinden: Nur Stadt SG
data_prep <- data_prep %>% 
  filter(bfsnr == 3203)


############## Account for people who moved and then moved back #################

id_year <- data_prep %>% 
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

id_year <- id_year %>% 
# different ID for before and after move
  # for every change in bfsnr increase by 1
  mutate(pers_change = cumsum(c(1,diff(id_year$bfsnr)!=0))) %>% 
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
data <- id_year %>% 
  filter(bfsnr != 0) %>% 
  left_join(data_prep, by = c("id_ek", "abstimmungsjahr", "bfsnr"))


##################  Count of consequtively following votes ######################

data <- data %>% 
  arrange(id_ek, abstimmungsjahr, abstimmungsmonat) %>% 
  group_by(ID_move_change) %>% 
  dplyr::mutate(abst_reihe = row_number())


###########################  People that moved away  ############################

# Personen, welche kein NA bei Steuerdaten haben (community_live_longer1=2)

# Personen, die in mehr als einem Jahr in Gemeinde wohnen, aber keine Steuerdaten 
# aufweisen (community_live_longer1=1). Hier könnte Steuerwert von Vorjahr verwendet
# werden

# Personen, welche keine Steuerdaten aufweisen und nicht in zwei unterschiedlichen 
# Jahren in Gemeinde gewohnt haben (community_live_longer1=0). Hier kann kein Wert 
# imputed werden.


people_moved <- data %>% 
  group_by(id_ek) %>% 
  mutate(community_live_longer1 = case_when(
    !is.na(steuerb_einkommen) ~ 2,
    length(unique(abstimmungsjahr)) > 1 ~ 1,
    TRUE ~ 0
  )) %>% 
  select(id_ek, abstimmungsjahr, abstimmungsdatum, community_live_longer1, abst_reihe, steuerb_einkommen) 


# Impute tax data of previous year if community_live_longer1 = 1
# create dummy variable for imputation
test <- people_moved %>% 
  group_by(id_ek) %>% 
  arrange(id_ek, abstimmungsjahr, abst_reihe) %>% 
  mutate(steuerb_einkommen_imputed = steuerb_einkommen) %>% 
  fill(steuerb_einkommen_imputed, .direction = "downup") %>% 
  mutate(imputed = case_when(
    !is.na(steuerb_einkommen) ~ 0,
    is.na(steuerb_einkommen_imputed) ~ 0,
    (is.na(steuerb_einkommen) & !is.na(steuerb_einkommen_imputed)) ~ 1
  ))

# Only impute when the tax data is from previous year
test <- test %>% 
  group_by(id_ek, abstimmungsjahr) %>% 
  mutate(steuerb_einkommen_imputed = case_when(
    
  ))

people_short <- people_moved %>% 
  select(id_ek, abstimmungsjahr, abstimmungsdatum, community_live_longer1, abst_reihe, steuerb_einkommen) 
  
  
  
  
  
  
  
  