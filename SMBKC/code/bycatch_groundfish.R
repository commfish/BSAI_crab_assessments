# k.palof katie.palof@alaska.gov
# date updated: 8-12-19

# Data manipulation for bycatch in Groundfish fisheries for SMBKC

# data obtained from AKFIN: provide instructions

# load -----
source("./SMBKC/code/packages.R")

# data -----
by_weight <- read.csv("C:/Users/kjpalof/Documents/SMBKC/DATA_SMBKC/EBSCrab_AB_Sizegroup.csv")
gf_bycatch <- read.csv("C:/Users/kjpalof/Documents/SMBKC/DATA_SMBKC/Crab Bycatch Estimates.csv")

# clean-up data ---------
#head(by_weight)
#by_weight %>% 
#  filter(DISTRICT_CODE == "STMATT") 

head(gf_bycatch)
# group by fixed gear and trawl 
# Agency.Gear.Code: HAL [hook & line], PTR [pelagic trawl], POT [pot], NPT [non-pelagic trawl]
gf_bycatch %>% 
  group_by(Crab.Year, Agency.Gear.Code) %>% 
  summarise(sum = sum(Estimate.Num)) %>% 
  dcast( ., Crab.Year ~ Agency.Gear.Code, sum) %>% 
  mutate(trawl = (NPT + PTR)/2, fixed = (HAL + POT)/2) -> gf_by_nums# combine trawl and fixed, divide by 2 - MALE only
# ignore 2019, this is just the beginning of this season, data up until 2018

# weight -
gf_bycatch %>% 
  group_by(Crab.Year, Agency.Gear.Code) %>% 
  summarise(sum = sum(Estimate.Wt..kg.crab.)) %>% 
  dcast( ., Crab.Year ~ Agency.Gear.Code, sum) %>% 
  mutate(trawl = (NPT + PTR)/2, fixed = (HAL + POT)/2, 
         trawl_thou = trawl/1000, fixed_thou = fixed/1000) -> gf_by_weight# combine trawl and fixed, divide by 2 - MALE only
# ignore 2019, this is just the beginning of this season, data up until 2018