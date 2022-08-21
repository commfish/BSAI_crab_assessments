# k.palof katie.palof@alaska.gov
# date updated: 8-14-22

# Data manipulation for bycatch in Groundfish fisheries for BBRKC

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
model_yr = "bbrkc_22f"
cur_yr = 2022 # need a note here if this should be 2021 or 2022
cal_yr = cur_yr

# data -----
#gf_bycatch <- read.csv("C:/Users/kjpalof/Documents/SMBKC/DATA_SMBKC/Crab Bycatch Estimates.csv") # old location
gf_bycatch <- read.csv(paste0(here::here(), '/BBRKC/data/', cal_yr, '/groundfish bycatch/Crab Bycatch Estimates.csv'))
gf_length <- read.csv(paste0(here::here(), '/BBRKC/data/', cal_yr, '/groundfish bycatch/norpac_length_report_22.csv'))

# clean-up data ---------
head(gf_bycatch)
# group by fixed gear and trawl 
# Agency.Gear.Code: HAL [hook & line], PTR [pelagic trawl], POT [pot], NPT [non-pelagic trawl]
gf_bycatch %>% 
  group_by(Crab.Year, Agency.Gear.Code) %>% 
  summarise(sum = sum(Estimate.Num..Sum.)) %>% # weird export
  reshape2::dcast( ., Crab.Year ~ Agency.Gear.Code, sum) %>% 
  mutate(trawl = (NPT + PTR), fixed = (HAL + POT)) -> gf_by_nums# combine trawl and fixed, male and female don't divide by 2 like smbkc


# ignore most recent year , this is just the beginning of this season, data up until current year -2 if May, current year - 1 if Sept.
write.csv(gf_by_nums, paste0(here::here(), '/BBRKC/data/', cal_yr, '/groundfish bycatch/gf_bycatch_numbers.csv'), 
          row.names = FALSE)

# this is for weight only  
 #mutate(trawl_thou = trawl/1000, fixed_thou = fixed/1000) -> gf_by_nums# needs to be divided by 1,000 to be metric tons.
# original is 1,000 kg with is 1ton, divide by 1,000 to get metric tons

# weight -
gf_bycatch %>% 
  group_by(Crab.Year, Agency.Gear.Code) %>% 
  summarise(sum = sum(Estimate.Wt..kg.crab.)) %>% 
  reshape2::dcast( ., Crab.Year ~ Agency.Gear.Code, sum) %>% 
  mutate(trawl = (NPT + PTR), fixed = (HAL + POT), # divide by 2 for for smbkc since it's male only
         trawl_thou = trawl/1000, fixed_thou = round(fixed/1000, 3), 
         trawl_1000t = round(trawl_thou/1000, 3), fixed_1000t = round(fixed_thou/1000, 3), 
         status = ifelse(Crab.Year == cur_yr, "not completed", "complete"), 
         total_1000t = trawl_1000t + fixed_1000t, 
         total_lbs = round(total_1000t/0.453592, 3)) -> gf_by_weight
# combine trawl and fixed, divide by 2 - MALE only
# from SAFE intro  -1000 t to million lb [/0.453592]
# ignore 2019, this is just the beginning of this season, data up until 2018

write.csv(gf_by_weight, paste0(here::here(), '/BBRKC/data/', cal_yr, '/groundfish bycatch/gf_bycatch_weight.csv'), 
          row.names = FALSE)

# ** FIX ** need to add in lines here to summarize catch for executive summary tables 

# for .dat file use columng "trawl_thou" and "fixed_thou"


# legnth comps -------
head(gf_length)
# seperate out BB data 
# south of 58.65 deg, east of -168 deg, north of 54.6
gf_length %>% 
  filter(LatDD.Start >= 54.6 | 
         LatDD.Start <= 58.65) %>% 
  filter(LonDD.Start > -168) -> gf_length_bb

## total sampled by gear type ------
gf_length_bb %>% 
  select(Year, Gear, Gear.Description, Species.Name, Sex, Length..cm.) %>% 
  mutate(Type = ifelse(Gear == 1 | Gear == 2, "TRW", 
                       ifelse(Gear == 6| Gear == 8, "FIX", "NA"))) %>% 
  group_by(Year, Type) %>% 
  summarise(Nsamp = n()) -> samp_by_gear

# effective sample size------
samp_by_gear %>% 
  mutate(eff_samp = 0.05*Nsamp) -> effective_sample

# male proportions ------
gf_length_bb %>% 
  select(Year, Gear, Gear.Description, Species.Name, Sex, Length..cm.) %>% 
  mutate(Type = ifelse(Gear == 1 | Gear == 2, "TRW", 
                       ifelse(Gear == 6| Gear == 8, "FIX", "NA"))) %>% 
  filter(Sex == "M", 
         Length..cm. >= 65) %>% 
  mutate(#total = rowSums(.[7:ncol(.)]), 
    size_bin = ifelse(Length..cm. > 160, 160, floor(Length..cm./5)* 5)) %>% 
  group_by(Year, Type, size_bin, .drop = F) %>% 
  summarise(Ncount = n()) %>% 
  left_join(samp_by_gear) %>% # total sample size by gear type - includes males and females
  mutate(Nprop = round(Ncount/Nsamp, 4)) %>% 
  select(Year, Type, size_bin, Nprop) %>% 
  as.data.frame() %>% 
  spread(size_bin, Nprop) -> output_male

output_male[is.na(output_male)] <- 0  

output_male %>% 
  filter(Year == cur_yr-1)

# female proportions ------
gf_length_bb %>% 
  select(Year, Gear, Gear.Description, Species.Name, Sex, Length..cm.) %>% 
  mutate(Type = ifelse(Gear == 1 | Gear == 2, "TRW", 
                       ifelse(Gear == 6| Gear == 8, "FIX", "NA"))) %>% 
  filter(Sex == "F", 
         Length..cm. >= 65) %>% 
  mutate(#total = rowSums(.[7:ncol(.)]), 
    size_bin = ifelse(Length..cm. > 140, 140, floor(Length..cm./5)* 5)) %>% 
  group_by(Year, Type, size_bin, .drop = F) %>% 
  summarise(Ncount = n()) %>% 
  left_join(samp_by_gear) %>% # total sample size by gear type - includes males and females
  mutate(Nprop = round(Ncount/Nsamp, 4)) %>% 
  select(Year, Type, size_bin, Nprop) %>% 
  as.data.frame() %>% 
  spread(size_bin, Nprop) -> output_female

output_female[is.na(output_female)] <- 0  

output_female %>% 
  filter(Year == cur_yr-1) # missing 65 need to add this
