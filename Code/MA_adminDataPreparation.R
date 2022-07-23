###########################Prepare administrative Data###########################

# Libraries
library(tidyverse)
library(dplyr)

# Read Data
data_sg <- read.csv("/Users/alinelaurametzler/Documents/Universität/Master/Master Thesis/Data/IndividualdatenSTISTAT_2010-2020_5818_Lieferungsdatum-2022-04-12/IndividualdatenSTISTAT_2010-2020_5818.csv") 

data <- data_sg %>% 
  rename(steuerb_einkommen=ziffer_268_veranlagt,
         reinvermoegen=ziffer_326_veranlagt,
         anz_ki_kinderabzug=ziffer_25,
         anz_ki_u15_hh=ziffer_18,
         steuer_tarif=ziffer_10
  ) %>% 
  mutate(abstimmungsdatum = as.Date(abstimmungsdatum, "%d.%m.%y"),
         abstimmungsjahr = as.Date(as.character(abstimmungsjahr), format = "%Y"))