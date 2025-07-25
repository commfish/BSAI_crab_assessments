# R script for summarizing survey data for prelim LBA
# 7-11-2024 / 7-23-25


# load ----
library(tidyverse)

# folder
# BBRKC/data/2024/survey
Cyear <- 2025
m_size <- read.csv(paste0(here::here(), "/BBRKC/data/", Cyear, "/survey/BBRKC_Allmale_SizeComps_2025.csv"))
fm_size <- read.csv(paste0(here::here(), "/BBRKC/data/", Cyear, "/survey/BBRKC_Allfemale_SizeComps_2025.csv"))


## totals ------
m_size %>% 
  summarise(total = sum(TOTAL_CRAB)) 

m_size %>% 
  filter(SIZE_BIN >= 135) %>% 
  summarise(total_legal = sum(TOTAL_CRAB))

m_size %>% 
  filter(SIZE_BIN >= 120) %>% 
  summarise(total_mature = sum(TOTAL_CRAB))

fm_size %>% 
  summarise(total = sum(TOTAL_CRAB))
# these are used in excel file to extrapolate to area swept

m_size %>% 
  mutate(shell = ifelse(SHELL_TEXT == "New Hard", "New", "Old")) %>% 
  mutate(cl_bin = ifelse(SIZE_BIN >160, 160, floor(SIZE_BIN/ 5)*5)) %>% 
  mutate(total_male = sum (TOTAL_CRAB)) %>% 
  group_by(total_male, shell, cl_bin) %>% 
  summarise(tcrab = sum(TOTAL_CRAB)) %>% 
  mutate(prop = tcrab/total_male) -> m_size2
  
write.csv(m_size2, paste0(here::here(), "/BBRKC/data/", Cyear, "/survey/prelim_male_size_comp.csv"), row.names = FALSE)


# females 
fm_size %>% 
  #mutate(shell = ifelse(SHELL_TEXT == "New Hard", "New", "Old")) %>% 
  mutate(cl_bin = ifelse(SIZE_BIN >140, 140, floor(SIZE_BIN/ 5)*5)) %>% 
  mutate(total_female = sum (TOTAL_CRAB)) %>% 
  group_by(total_female, cl_bin) %>% 
  summarise(tcrab = sum(TOTAL_CRAB)) %>% 
  mutate(prop = tcrab/total_female) -> fm_size2

write.csv(fm_size2, paste0(here::here(), "/BBRKC/data/", Cyear, "/survey/prelim_female_size_comp.csv"), row.names = FALSE)
