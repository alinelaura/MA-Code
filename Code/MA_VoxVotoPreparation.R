##############################Prepare Vox/Voto Data##############################

# Libraries
library(tidyverse)
library(lubridate)



swissvotes <- read.csv("./Data/swissvotes/swissvotes.csv", sep = ';')
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
  dplyr::rename(d1e3_label = Label)

