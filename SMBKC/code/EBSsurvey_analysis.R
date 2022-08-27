# k.palof katie.palof@alaska.gov
# date updated: 8-15-19 / 4-12-22 / 8-25-22

# Data manipulation for EBS trawl survey results to put into SMBKC model

# data obtained from AKFIN: provide instructions
# by_weight - 
# "Crab Data" - tab
# "EBS Crab Survey" - "Summary Reports" - "Abundance/BIomass, Size Group Matrix" - 
# drop down menu - 1975 to current year (2019) - Blue King Crab  - District - "STMATT"
# click "export" Data - csv , add current year to file name
# for "by_weight"
# save to 'SMBKC/data/trawl_survey/'

# size_group 
# "EBS Crab Survey" - "Large Data Download" - "Abundance/BIomass, Large Data Download" - 
# drop down menu - SIZE_GROUP - Blue King Crab - STMATT
# click "export" Data - csv 

# haul data 
# "EBS Crab Survey" - "Large Data Download" - "Haul Data, Large Data Download" - 
# drop down menu - Blue King Crab

# load -----
source("./SMBKC/code/packages.R")
cur_yr = 2022 # Current survey data 
folder = "smbkc_22f"
# data -----
# data files from AKFIN are saved as a different type of .csv open files and resave as csv general
by_weight <- read.csv(paste0(here::here(), '/SMBKC/data/trawl_survey/EBSCrab_AB_Sizegroup_', cur_yr, '.csv'))
# need to ignore first 5 rows here 
haul_bkc <- read.csv(paste0(here::here(), '/SMBKC/data/trawl_survey/EBSCrab_Haul_', cur_yr, '.csv'), skip = 5)
#haul_bkc <- read.csv("C:/Users/kjpalof/Documents/SMBKC/DATA_SMBKC/EBSCrab_Haul_bkc_7519.csv")
# size group file comes out with first 7 rows as identifiers. remove these, manually for now, automate later
#size_group <- read.csv("C:/Users/kjpalof/Documents/SMBKC/DATA_SMBKC/EBSCrab_Abundance_Biomass_2019.csv")
size_group <- read.csv(paste0(here::here(), '/SMBKC/data/trawl_survey/EBSCrab_Abundance_Biomass_', cur_yr, '.csv'), skip = 7)

# survey biomass cleanup/results ---------
head(by_weight)
by_weight %>% 
  filter(DISTRICT_CODE == "STMATT") %>% 
  filter(SEX == "MALE") -> smbkc_area_swept

smbkc_area_swept %>% 
  filter(SIZE_GROUP == "MALE_GE90") %>% 
  dplyr::select(SURVEY_YEAR, SPECIES_NAME, SIZE_GROUP, ABUNDANCE, ABUNDANCE_CV,  
         BIOMASS_LBS, BIOMASS_LBS_CV ,BIOMASS_MT, BIOMASS_MT_CV, BIOMASS_MT_CI) -> biomass_mt 
write.csv(biomass_mt, paste0(here::here(), '/SMBKC/smbkc_22/data/survey_biomass_mt2.csv'), 
            row.names = FALSE)


## table 8 in SAFE - table of abundance by size group and total biomass ------
Model_size <- c("MALE_90TO104", "MALE_105TO119", "MALE_GE120")
smbkc_area_swept %>% 
  filter(SIZE_GROUP %in% Model_size) %>% 
  filter(SURVEY_YEAR >= 2015) %>% 
  group_by(SURVEY_YEAR, SIZE_GROUP) %>% 
  summarise(numbers = sum(ABUNDANCE)/1000000)

smbkc_area_swept %>% 
  filter(SIZE_GROUP == "MALE_GE90") %>% 
  filter(SURVEY_YEAR >= 2015) %>% 
  group_by(SURVEY_YEAR, SIZE_GROUP) %>% 
  summarise(numbers = round(sum(ABUNDANCE)/1000000, 3), 
            n_cv = round(ABUNDANCE_CV, 3), 
            lb = round(sum(BIOMASS_LBS)/1000000, 3),
            lb_cv = round(BIOMASS_LBS_CV, 3)) %>% 
  as.data.frame()

# stats for current year data for SAFE executive summary---------
# 2019 value rank  - rank biomass_mt???
biomass_mt %>% 
  filter(SURVEY_YEAR >= 1978) %>% 
  dplyr::select(SURVEY_YEAR, BIOMASS_MT) %>% 
  mutate(rank = rank(BIOMASS_MT))
  
# rank since 2000
biomass_mt %>% 
  filter(SURVEY_YEAR >= 2000) %>% 
  dplyr::select(SURVEY_YEAR, BIOMASS_MT) %>% 
  mutate(rank = rank(BIOMASS_MT), avg = mean(BIOMASS_MT))
  
# 1978 - 2022 mean survey biomass
biomass_mt %>%  # all using biomass_mt metric tons
  filter(SURVEY_YEAR >= 1978) %>% 
  mutate(LT_MEAN = mean(BIOMASS_MT), pct.LT_MEAN = BIOMASS_MT/LT_MEAN) -> biomass_mt_mean
         #avg3yr = ifelse(SURVEY_YEAR >= cur_yr -2, mean(BIOMASS_MT), 0))


# 3 year average and percent of LT mean 
biomass_mt %>% 
  filter(SURVEY_YEAR >= cur_yr-3) %>% # !! change to 3 here to include actual last 3 years due to missing 2020
  summarise(mean_3yr = mean(BIOMASS_MT), pct.lt = mean_3yr/biomass_mt_mean$LT_MEAN[1])

# last years percent change 
biomass_mt %>% 
  filter(SURVEY_YEAR >= cur_yr-2) %>% # needs to be 2 here since no 2020 survey
  mutate(pct.change = (BIOMASS_MT[2]-BIOMASS_MT[1])/BIOMASS_MT[1],
         pct.change2 = (BIOMASS_LBS[2]-BIOMASS_LBS[1])/BIOMASS_LBS[1],
         pct.change3 = (ABUNDANCE[2]-ABUNDANCE[1])/ABUNDANCE[1])

# Trawl survey "recruitment" estimates  - line 142----------
head(smbkc_area_swept) # line 37
smbkc_area_swept %>% 
  filter(SURVEY_YEAR >= 1978) %>% 
  filter(SIZE_GROUP == "MALE_90TO104") %>% 
  dplyr::select(SURVEY_YEAR, SPECIES_NAME, SIZE_GROUP, ABUNDANCE, ABUNDANCE_CV,  
       BIOMASS_LBS, BIOMASS_LBS_CV ,BIOMASS_MT, BIOMASS_MT_CV, BIOMASS_MT_CI) -> recruit90to104 

recruit90to104 %>% 
  dplyr::select(SURVEY_YEAR, SIZE_GROUP, ABUNDANCE) %>% 
  mutate(rank = rank(ABUNDANCE), lt_mean = mean(ABUNDANCE), 
         pct.lt = ABUNDANCE/lt_mean)
# 6 year average recruitment
recruit90to104 %>% 
  filter(SURVEY_YEAR >= cur_yr-6) %>% ## !!! changed to 6 to include last 6 years due to dropping 2020
  summarise(mean_6yr = mean(ABUNDANCE)) %>% 
  mutate(mean_6yr/1012456) # need to change this value to the lt_mean ? I think (was 1026493)

write.csv(recruit90to104, paste0(here::here(), '/SMBKC/', folder, '/data/recruit90to104_biomass.csv'), 
          row.names = FALSE)

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
  dplyr::select(SURVEY_YEAR, SIZE_GROUP, numbers) %>% 
  spread(SIZE_GROUP, numbers) %>% 
  group_by(SURVEY_YEAR) %>% 
  mutate(total = sum(MALE_105TO119, MALE_90TO104, MALE_GE120), pct.total90 = MALE_90TO104/total, 
         pct.total105 = MALE_105TO119/total, pct.total120 = MALE_GE120/total) %>% 
  as.data.frame() -> proportion_by_group

write.csv(proportion_by_group, paste0(here::here(), '/SMBKC/', folder, '/data/proportion_size_class.csv'), 
          row.names = FALSE)

## NOT USED here corner station removal -------
corner_station <- read.csv(paste0(here::here(), '/SMBKC/', folder, '/model_1_corner/data/bk_stmatt_abundance_sizegroup.csv'))
corner_station %>% 
  filter(SURVEY_YEAR >= 1978) %>% 
  select(SURVEY_YEAR, NUM_MALE_90TO104, NUM_MALE_105TO119, NUM_MALE_GE120, NUM_MALE_GE90) %>% 
  #mutate(all_males = NUM_MALE_90TO104 + NUM_MALE_105TO119 + NUM_MALE_GE120)
  mutate(prop_PR = NUM_MALE_90TO104/NUM_MALE_GE90, 
         prop_R = NUM_MALE_105TO119/NUM_MALE_GE90, 
         prop_PoR = NUM_MALE_GE120/NUM_MALE_GE90) -> prop_length_wo_corner_station
write.csv(prop_length_wo_corner_station, paste0(here::here(), '/SMBKC/smbkc_22/model_1_corner/data/proportion_size_class_wo_C.csv'), 
          row.names = FALSE)

## sample size for length comps??? --------------

## sample size for length comps??? ----------------
head(haul_bkc) # how to determine which ones are st.matt's???

# 2022 sampled
haul_bkc %>% 
  filter(AKFIN_SURVEY_YEAR == 2022 & MID_LATITUDE > 58.5) %>% 
  dplyr::select(AKFIN_SURVEY_YEAR, GIS_STATION, AREA_SWEPT, SPECIES_NAME, SEX, LENGTH, SAMPLING_FACTOR) %>% 
  filter(SEX == 1 & LENGTH >= 90) %>% 
  group_by(GIS_STATION) %>% 
  summarise(numbers = sum(SAMPLING_FACTOR))%>% 
  mutate(total = sum(numbers))

# 2021 sampled
haul_bkc %>% 
  filter(AKFIN_SURVEY_YEAR == 2021 & MID_LATITUDE > 58.5) %>% 
  dplyr::select(AKFIN_SURVEY_YEAR, GIS_STATION, AREA_SWEPT, SPECIES_NAME, SEX, LENGTH, SAMPLING_FACTOR) %>% 
  filter(SEX == 1 & LENGTH >= 90) %>% 
  group_by(GIS_STATION) %>% 
  summarise(numbers = sum(SAMPLING_FACTOR))%>% 
  mutate(total = sum(numbers))

# 2019 sampled
haul_bkc %>% 
  filter(AKFIN_SURVEY_YEAR == 2019 & MID_LATITUDE > 58.5) %>% 
  dplyr::select(AKFIN_SURVEY_YEAR, GIS_STATION, AREA_SWEPT, SPECIES_NAME, SEX, LENGTH, SAMPLING_FACTOR) %>% 
  filter(SEX == 1 & LENGTH >= 90) %>% 
  group_by(GIS_STATION) %>% 
  summarise(numbers = sum(SAMPLING_FACTOR))%>% 
  mutate(total = sum(numbers)) # looking at data file max appears to be 50....keep with this and ask Jie.
# update Table 11 in SAFE with this value also
# should update .dat file with actual sample size commented out - see Jie's .dat file

# 2018 sampled
haul_bkc %>% 
  filter(AKFIN_SURVEY_YEAR == 2018 & MID_LATITUDE > 58.5) %>% 
  dplyr::select(AKFIN_SURVEY_YEAR, GIS_STATION, AREA_SWEPT, SPECIES_NAME, SEX, LENGTH, SAMPLING_FACTOR) %>% 
  filter(SEX == 1 & LENGTH >= 90) %>% 
  group_by(GIS_STATION) %>% 
  summarise(numbers = sum(SAMPLING_FACTOR)) %>% 
  mutate(total = sum(numbers)) # 2018 .dat file has 31 as input, 62 as actual, I got 55 actual?????

#Effective sample size = min(0.25*n , N) for trawl surveys where N max = 50
0.50*59

