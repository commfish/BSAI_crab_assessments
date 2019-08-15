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

# data -----
by_weight <- read.csv("C:/Users/kjpalof/Documents/SMBKC/DATA_SMBKC/EBSCrab_AB_Sizegroup_2019.csv")

# clean-up data ---------
head(by_weight)
by_weight %>% 
  filter(DISTRICT_CODE == "STMATT") %>% 
  filter(SEX == "MALE") -> smbkc_area_swept

smbkc_area_swept %>% 
  filter(SIZE_GROUP == "MALE_GE90") %>% 
  select(SURVEY_YEAR, SPECIES_NAME, SIZE_GROUP, ABUNDANCE, ABUNDANCE_CV, ABUNDANCE_CI, BIOMASS_MT, BIOMASS_MT_CV, BIOMASS_MT_CI) %>% 
  

  
  