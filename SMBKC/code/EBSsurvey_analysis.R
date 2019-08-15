# k.palof katie.palof@alaska.gov
# date updated: 8-15-19

# Data manipulation for EBS trawl survey results to put into SMBKC model

# data obtained from AKFIN: provide instructions
# by_weight - 
# "Crab Data" - tab
# "EBS Crab Survey" - "Summary Reports" - "Abundance/BIomass, Size Group Matrix" - 
# drop down menu - 1975 to current year (2019) - Blue King Crab 
# click "export" Data - csv
# load -----
source("./SMBKC/code/packages.R")
cur_yr = 2019

# data -----
by_weight <- read.csv("C:/Users/kjpalof/Documents/SMBKC/DATA_SMBKC/EBSCrab_AB_Sizegroup_2019.csv")

# clean-up data and output ---------
head(by_weight)
by_weight %>% 
  filter(DISTRICT_CODE == "STMATT") %>% 
  filter(SEX == "MALE") -> smbkc_area_swept

smbkc_area_swept %>% 
  filter(SIZE_GROUP == "MALE_GE90") %>% 
  select(SURVEY_YEAR, SPECIES_NAME, SIZE_GROUP, ABUNDANCE, ABUNDANCE_CV,  
         BIOMASS_LBS, BIOMASS_LBS_CV ,BIOMASS_MT, BIOMASS_MT_CV, BIOMASS_MT_CI) -> biomass_mt 
write.csv(biomass_mt, paste0(here::here(), '/SMBKC/smbkc_19/data/survey_biomass_mt.csv'), 
            row.names = FALSE)

# stats for current year data ---------
# 2019 value rank  - rank biomass_mt???

# 1978 - 2019 mean survey biomass
biomass_mt %>%  # all using biomass_mt metric tons
  filter(SURVEY_YEAR >= 1978) %>% 
  mutate(LT_MEAN = mean(BIOMASS_MT), pct.LT_MEAN = BIOMASS_MT/LT_MEAN)
         #avg3yr = ifelse(SURVEY_YEAR >= cur_yr -2, mean(BIOMASS_MT), 0))

# 3 year average and percent of LT mean 


# Trawl survey "recruitment" estimates  - line 91
  
  
  