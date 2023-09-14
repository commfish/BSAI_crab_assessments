# k.palof katie.palof@alaska.gov
# date updated: 8-12-19 / 4-14-22 / 8-25-22

# Data manipulation for bycatch in Groundfish fisheries for SMBKC

# data obtained from AKFIN: provide instructions
#
#  URL: https://akfinbi.psmfc.org/analytics 
#  See login info in e-mail, results in file similar to "Crab Bycatch Estimates.csv" below
# "Crab Data" tab, "Estimates of Crab Bycatch in Groundfish Fisheries", "Crab Bycatch Estimates: 2009- present, by crab year"
#   From drop down menus "2009 to 2020", Fishery name: "Saint Matthew blue king crab", 
#   Species Group Name: Blue King Crab
#   Click - "detail report" 
#    at bottom of page click "export", choose Data - .csv - save in data folder for current year - here smbkc_XX/data

#   
# load -----
source("./SMBKC/code/packages.R")
model_yr = "smbkc_23"
cur_yr = 2023

# data -----
#gf_bycatch <- read.csv("C:/Users/kjpalof/Documents/SMBKC/DATA_SMBKC/Crab Bycatch Estimates.csv") # old location
gf_bycatch <- read.csv(paste0(here::here(), '/SMBKC/', model_yr, '/data/Crab Bycatch Estimates.csv'))

# clean-up data ---------
head(gf_bycatch)
# group by fixed gear and trawl 
# Agency.Gear.Code: HAL [hook & line], PTR [pelagic trawl], POT [pot], NPT [non-pelagic trawl]
gf_bycatch %>% 
  group_by(Crab.Year, Agency.Gear.Code) %>% 
  summarise(sum = sum(Estimate.Num..Sum.)) %>% # weird export
  reshape2::dcast( ., Crab.Year ~ Agency.Gear.Code, sum) %>% 
  mutate(trawl = (NPT + PTR)/2, fixed = (HAL + POT)/2) -> gf_by_nums# combine trawl and fixed, divide by 2 - MALE only

# this is for weight only  
 #mutate(trawl_thou = trawl/1000, fixed_thou = fixed/1000) -> gf_by_nums# needs to be divided by 1,000 to be metric tons.
# original is 1,000 kg with is 1ton, divide by 1,000 to get metric tons

# ignore most recent year , this is just the beginning of this season, data up until current year -2 if May, current year - 1 if Sept.
write.csv(gf_by_nums, paste0(here::here(), '/SMBKC/', model_yr, '/data/gf_bycatch_numbers.csv'), 
          row.names = FALSE)

# weight -------
gf_bycatch %>% 
  group_by(Crab.Year, Agency.Gear.Code) %>% 
  summarise(sum = sum(Estimate.Wt..kg.crab.)) %>% 
  reshape2::dcast( ., Crab.Year ~ Agency.Gear.Code, sum) %>% 
  mutate(trawl = (NPT + PTR)/2, fixed = (HAL + POT)/2, 
         trawl_thou = trawl/1000, fixed_thou = fixed/1000, 
         trawl_1000t = round(trawl_thou/1000, 3), fixed_1000t = round(fixed_thou/1000, 3), 
         status = ifelse(Crab.Year == cur_yr, "not completed", "complete"), 
         total_1000t = trawl_1000t + fixed_1000t, 
         total_lbs = round(total_1000t/0.453592, 3)) -> gf_by_weight
# combine trawl and fixed, divide by 2 - MALE only
# from SAFE intro  -1000 t to million lb [/0.453592]
# ignore 2019, this is just the beginning of this season, data up until 2018

# use trawl_thou and fixed_thou for .dat file 
gf_by_weight %>% 
  select(Crab.Year, trawl_thou, fixed_thou) 
  

write.csv(gf_by_weight, paste0(here::here(), '/SMBKC/', model_yr, '/data/gf_bycatch_weight.csv'), 
          row.names = FALSE)

# ** FIX ** need to add in lines here to summarize catch for executive summary tables 
