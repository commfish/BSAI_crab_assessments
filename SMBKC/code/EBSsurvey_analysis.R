# k.palof katie.palof@alaska.gov
# date updated: 8-15-19

# Data manipulation for EBS trawl survey results to put into SMBKC model

# data obtained from AKFIN: provide instructions
# by_weight - 
# "Crab Data" - tab
# "EBS Crab Survey" - "Summary Reports" - "Abundance/BIomass, Size Group Matrix" - 
# drop down menu - 1975 to current year (2019) - Blue King Crab 
# click "export" Data - csv 
# for "by_weight"

# size_group 
# "EBS Crab Survey" - "Large Data Download" - "Abundance/BIomass, Large Data Download" - 
# drop down menu - SIZE_GROUP - Blue King Crab - STMATT
# click "export" Data - csv 

# haul data 
# "EBS Crab Survey" - "Large Data Download" - "Haul Data, Large Data Download" - 
# drop down menu - Blue King Crab

# load -----
source("./SMBKC/code/packages.R")
cur_yr = 2019

# data -----
by_weight <- read.csv("C:/Users/kjpalof/Documents/SMBKC/DATA_SMBKC/EBSCrab_AB_Sizegroup_2019.csv")
haul_bkc <- read.csv("C:/Users/kjpalof/Documents/SMBKC/DATA_SMBKC/EBSCrab_Haul_bkc_7519.csv")
size_group <- read.csv("C:/Users/kjpalof/Documents/SMBKC/DATA_SMBKC/EBSCrab_Abundance_Biomass_2019.csv")


# survey biomass cleanup/results ---------
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


# 6 year average recruitment % of LT mean 


## Length comps - survey -------------

head(size_group)
unique(size_group$SIZE_GROUP)

size_group %>% 
  filter(SEX == "MALE") %>% 
  group_by(SURVEY_YEAR, SIZE_GROUP) %>% 
  summarise(numbers = sum(ABUNDANCE), biomass_lbs = sum(BIOMASS_LBS), biomass_mt= sum(BIOMASS_MT)) %>% 
  filter(SIZE_GROUP == "MALE_90TO104" | SIZE_GROUP == "MALE_105TO119" | SIZE_GROUP == "MALE_GE120") %>% 
  as.data.frame() %>% 
  select(SURVEY_YEAR, SIZE_GROUP, numbers) %>% 
  spread(SIZE_GROUP, numbers) %>% 
  group_by(SURVEY_YEAR) %>% 
  mutate(total = sum(MALE_105TO119, MALE_90TO104, MALE_GE120), pct.total90 = MALE_90TO104/total, 
         pct.total105 = MALE_105TO119/total, pct.total120 = MALE_GE120/total) %>% 
  as.data.frame() -> proportion_by_group

## sample size for length comps??? ----------------
head(haul_bkc) # how to determine which ones are st.matt's???

# 2019 sampled
haul_bkc %>% 
  filter(AKFIN_SURVEY_YEAR == 2019 & MID_LATITUDE > 58.5) %>% 
  select(AKFIN_SURVEY_YEAR, GIS_STATION, AREA_SWEPT, SPECIES_NAME, SEX, LENGTH, SAMPLING_FACTOR) %>% 
  filter(SEX == 1 & LENGTH >= 90 & LENGTH <=135) %>% 
  group_by(GIS_STATION) %>% 
  summarise(numbers = sum(SAMPLING_FACTOR))%>% 
  mutate(total = sum(numbers)) # looking at data file max appears to be 50....keep with this and ask Jie.

# 2018 sampled
haul_bkc %>% 
  filter(AKFIN_SURVEY_YEAR == 2018 & MID_LATITUDE > 58.5) %>% 
  select(AKFIN_SURVEY_YEAR, GIS_STATION, AREA_SWEPT, SPECIES_NAME, SEX, LENGTH, SAMPLING_FACTOR) %>% 
  filter(SEX == 1 & LENGTH >= 90 & LENGTH <=135) %>% 
  group_by(GIS_STATION) %>% 
  summarise(numbers = sum(SAMPLING_FACTOR)) %>% 
  mutate(total = sum(numbers)) # 2018 .dat file has 31....not sure how to get this, closest I got was 39
