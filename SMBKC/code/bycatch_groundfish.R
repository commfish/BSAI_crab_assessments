# k.palof katie.palof@alaska.gov
# date updated: 8-12-19

# Data manipulation for bycatch in Groundfish fisheries for SMBKC

# data obtained from AKFIN: provide instructions
#
#  URL: https://akfinbi.psmfc.org/analytics 
#  See login info in e-mail, results in file similar to "Crab Bycatch Estimates.csv" below
# "Crab Data" tab, "Estimates of Crab Bycatch in Groundfish Fisheries", "Crab Bycatch Estimates: 2009- present, by crab year"
#   From drop down menus "2009 to 2020", Fishery name: "Saint Matthew blue king crab", 
#   Species Group Name: Blue King Crab
#   Click - "detail report" 
#    at bottom of page click "export", choose Data - .csv - save in data folder for current year - here smbkc_20/data

#   
# load -----
source("./SMBKC/code/packages.R")

# data -----
#gf_bycatch <- read.csv("C:/Users/kjpalof/Documents/SMBKC/DATA_SMBKC/Crab Bycatch Estimates.csv") # old location
gf_bycatch <- read.csv(paste0(here::here(), '/SMBKC/smbkc_20/data/Crab Bycatch Estimates.csv'))

# clean-up data ---------
head(gf_bycatch)
# group by fixed gear and trawl 
# Agency.Gear.Code: HAL [hook & line], PTR [pelagic trawl], POT [pot], NPT [non-pelagic trawl]
gf_bycatch %>% 
  group_by(Crab.Year, Agency.Gear.Code) %>% 
  summarise(sum = sum(Estimate.Num)) %>% 
  dcast( ., Crab.Year ~ Agency.Gear.Code, sum) %>% 
  mutate(trawl = (NPT + PTR)/2, fixed = (HAL + POT)/2) -> gf_by_nums# combine trawl and fixed, divide by 2 - MALE only
# ignore 2019, this is just the beginning of this season, data up until 2018
write.csv(gf_by_nums, paste0(here::here(), '/SMBKC/smbkc_19/data/gf_bycatch_numbers.csv'), 
          row.names = FALSE)

# weight -
gf_bycatch %>% 
  group_by(Crab.Year, Agency.Gear.Code) %>% 
  summarise(sum = sum(Estimate.Wt..kg.crab.)) %>% 
  dcast( ., Crab.Year ~ Agency.Gear.Code, sum) %>% 
  mutate(trawl = (NPT + PTR)/2, fixed = (HAL + POT)/2, 
         trawl_thou = trawl/1000, fixed_thou = fixed/1000) -> gf_by_weight# combine trawl and fixed, divide by 2 - MALE only
# ignore 2019, this is just the beginning of this season, data up until 2018
write.csv(gf_by_weight, paste0(here::here(), '/SMBKC/smbkc_19/data/gf_bycatch_weight.csv'), 
          row.names = FALSE)
