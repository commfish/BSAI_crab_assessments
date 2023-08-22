# k.palof katie.palof@alaska.gov
# date updated: 8-16-2022 / 8-15-23

# Data manipulation for EBS trawl survey results to put into BBRKC model

# data obtained from AKFIN: provide instructions
# by_weight - 
# "Crab Data" - tab
# "EBS Trawl Survey" - "Summary Reports" - "Abundance/BIomass, Size Group Matrix" - 
# drop down menu - 1975 to current year (2023) - red King Crab  - District - "BB"
# click "export" Data - csv 
# for "by_weight"
# save to 'BBRKC/data/"cur_yr"/survey/'

# size_group 
# "EBS Trawl Survey" - "Large Data Download" - "Abundance/BIomass, Large Data Download" - 
# drop down menu - size_1mm - red King Crab - BB
# click "export" Data - csv 

# haul data 
# "EBS Trawl Survey" - "Large Data Download" - "Haul Data, Large Data Download" - 
# drop down menu - Red King Crab

## effective sample size data
# "Crab Data" 

# load -----
source("./SMBKC/code/packages.R")
library(ggplot2)
library(dplyr)
library(reshape)
library(ggridges)

cur_yr = 2023 # Current survey data 

# data -----
# data files from AKFIN are saved as a different type of .csv open files and resave as csv general
by_weight <- read.csv(paste0(here::here(), '/BBRKC/data/', cur_yr, '/survey/EBSCrab_AB_Sizegroup.csv'))
# need to ignore first 5 rows here 
haul_rkc <- read.csv(paste0(here::here(), '/BBRKC/data/', cur_yr, '/survey/EBSCrab_Haul.csv'), 
                     skip = 5)
#haul_bkc <- read.csv("C:/Users/kjpalof/Documents/SMBKC/DATA_SMBKC/EBSCrab_Haul_bkc_7519.csv")
# size group file comes out with first 7 rows as identifiers. remove these, manually for now, automate later
#size_group <- read.csv("C:/Users/kjpalof/Documents/SMBKC/DATA_SMBKC/EBSCrab_Abundance_Biomass_2019.csv")
size_group <- read.csv(paste0(here::here(), '/BBRKC/data/', cur_yr, '/survey/EBSCrab_Abundance_Biomass.csv'), 
                       skip = 7)

# survey biomass cleanup/results ---------
head(by_weight)
by_weight %>% 
  filter(DISTRICT_CODE == "BB") -> bbrkc_area_swept
  #filter(SEX == "MALE") -> smbkc_area_swept


bbrkc_area_swept %>% 
  filter(SIZE_GROUP == "MALE_GE65"| SIZE_GROUP == "FEMALE_GE65" | SIZE_GROUP == "MALE_FEMALE_GE65") %>% 
  dplyr::select(SURVEY_YEAR, SPECIES_NAME, SIZE_GROUP, ABUNDANCE, ABUNDANCE_CV,  
         BIOMASS_LBS, BIOMASS_LBS_CV ,BIOMASS_MT, BIOMASS_MT_CV, BIOMASS_MT_CI) -> biomass_mt 
biomass_mt %>% 
  filter(SURVEY_YEAR >= cur_yr-4)
write.csv(biomass_mt, paste0(here::here(), '/BBRKC/data/', cur_yr, '/survey/survey_biomass_mt2.csv'), 
            row.names = FALSE)

biomass_mt %>% 
  filter(SURVEY_YEAR >= cur_yr-1) %>% 
  filter(SIZE_GROUP == "FEMALE_GE65" | SIZE_GROUP == "MALE_GE65")
  
# use combined male and female CV - per Jie's instructions
biomass_mt %>% 
  filter(SURVEY_YEAR >= cur_yr-1) %>% 
  filter(SIZE_GROUP == "MALE_FEMALE_GE65")
## Length comps - survey see Tyler's code-------------
head(size_group)

head(haul_rkc)
#unique(size_group$SIZE_GROUP)
unique(haul_rkc$LENGTH_1MM)


# see Tyler's code here ---- update
# size comps in "female_race_size_comp.txt" - from Tyler - where is he calculating this???

## CV's for survey data in LBA - >=90 (just females currently)-----
bbrkc_area_swept %>% 
  filter(SIZE_CLASS_MM == "GE90") %>% 
  dplyr::select(SURVEY_YEAR, SPECIES_NAME, SIZE_GROUP, ABUNDANCE, ABUNDANCE_CV, ABUNDANCE_CI, 
                BIOMASS_LBS, BIOMASS_LBS_CV ,BIOMASS_MT, BIOMASS_MT_CV, BIOMASS_MT_CI) -> biomass2_mt 
head(biomass2_mt)
biomass2_mt %>% 
  select(SURVEY_YEAR, SIZE_GROUP, ABUNDANCE, ABUNDANCE_CV, ABUNDANCE_CI) %>% 
  mutate(ABUNDANCE = ABUNDANCE/1000000, 
         ABUNDANCE_CI = ABUNDANCE_CI/1000000) -> LBA_survey_CV
write.csv(LBA_survey_CV, paste0(here::here(), '/BBRKC/data/', cur_yr, '/survey/LBA_FE_survey_CV.csv'), 
          row.names = FALSE)

## sample size for length comps??? ----------------
head(haul_rkc) # how to determine which ones are bb???
## See Jie's notes
720*.25 # from prelim numbers from Jon

# 2021 sampled
haul_rkc %>% 
  filter(AKFIN_SURVEY_YEAR == 2022 & MID_LATITUDE > 54.6) %>% 
  filter(MID_LATITUDE < 58.65 & MID_LONGITUDE < -168) %>% 
  dplyr::select(AKFIN_SURVEY_YEAR, GIS_STATION, AREA_SWEPT, SPECIES_NAME, SEX, LENGTH, SAMPLING_FACTOR) %>% 
  filter(LENGTH >= 65) %>% 
  group_by(GIS_STATION) %>% 
  summarise(numbers = sum(SAMPLING_FACTOR))%>% 
  mutate(total = sum(numbers))

## LBA length comps -------
# females 90 to 140 
head(size_group)
unique(size_group$SIZE_CLASS_MM)

size_group %>% 
  filter(SURVEY_YEAR >= cur_yr-1) %>% 
  filter(SEX == "FEMALE") %>% 
  filter(SIZE_CLASS_MM > 89) %>% 
  mutate(size_bin = ifelse(SIZE_CLASS_MM >140, 140, floor(SIZE_CLASS_MM/5)*5)) %>% 
  group_by(SURVEY_YEAR, size_bin) %>% 
  summarise(bin_abun = sum(ABUNDANCE)/1000) %>% 
  as.data.frame()
## this is input for the LBA model for 'surveyf.dat' !! LBA !!

size_group %>% 
  filter(SURVEY_YEAR >= cur_yr-1) %>% 
  filter(SEX == "FEMALE") %>% 
  filter(SIZE_CLASS_MM > 89) %>% 
  group_by(SURVEY_YEAR) %>% 
  summarise(abun = sum(ABUNDANCE)/1000)

# males 95 to 160 
size_group %>% 
  filter(SURVEY_YEAR >= cur_yr-1) %>% 
  filter(SEX == "MALE") %>% 
  filter(SIZE_CLASS_MM > 94) %>% 
  group_by(SURVEY_YEAR) %>% 
  summarise(abun = sum(ABUNDANCE)/1000) -> abund_males
# use this in bbrkc_sizecomps.R
# - see bbrkc_sizecomp.R file need haul data for just BB
# mature and legal 
#size_group %>% 
#  filter(SURVEY_YEAR >= cur_yr-1) %>% 
#  filter(SEX == "MALE") %>% 
#  filter(SIZE_CLASS_MM > 119) %>% 
#  group_by(SURVEY_YEAR) %>% 
#  summarise(abun = sum(ABUNDANCE)/1000) 

# legal size crab -------
bbrkc_area_swept%>% 
  filter(SURVEY_YEAR >= cur_yr-1)

### length frequency info surey ----------
# from Cody --
#=======================================
# this file is from AKFIN with the Abundance/Biomass Large Data Download tab
# need to select by size_1mm
#=======================================
head(size_group)
#kod_dat<-read.csv("data/survey/EBSCrab_Abundance_Biomass.csv",header=T,skip=7)
#kod_dat_1<-filter(size_group,SEX=='FEMALE')

## females 5mm size bins ---------------------------
size_group %>% 
  filter(SEX == 'FEMALE') %>% 
  group_by(SURVEY_YEAR,SIZE_CLASS_MM) %>%
  summarize(abund=sum(ABUNDANCE)) -> kod_dat_f

### ggridges 5mm bins 
kod_dat_f %>% 
  mutate(size_bin = ifelse(SIZE_CLASS_MM > 190, 190, floor(SIZE_CLASS_MM/5)* 5)) %>% 
  group_by(SURVEY_YEAR, size_bin) %>% 
  summarize(abund = sum(abund)) -> kod_dat_f_5mm

p <- ggplot(dat=kod_dat_f_5mm) 
#p <- 
p <- p + geom_density_ridges(aes(x=size_bin, y=SURVEY_YEAR, height = abund,
                                 group = SURVEY_YEAR, 
                                 fill=stat(y),alpha=.9999), stat = "identity",scale=15) +
  scale_fill_viridis_c()+
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90)) +
  labs(x="Carapace width (mm)", y = "Female abundance in survey year")+
  xlim(25,190)
png(paste0(here::here(), "/BBRKC/bbrkc_23f/doc/figures/size_bins_comp_Kodiak_f_5mm.png"),height=9,width=6,res=400,units='in')
print(p)
dev.off()

# last 5 years of data 5mm females -----
kod_dat_f_5mm %>% 
  filter(SURVEY_YEAR >= (cur_yr-6)) -> kod_dat_f_5mm_2
p <- ggplot(dat=kod_dat_f_5mm_2) 
#p <- 
p <- p + geom_density_ridges(aes(x=size_bin, y=SURVEY_YEAR, height = abund,
                                 group = SURVEY_YEAR, 
                                 fill=stat(y),alpha=.9999), stat = "identity",scale=2) +
  scale_fill_viridis_c()+
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90)) +
  labs(x="Carapace width (mm)", y = "Female abundance in survey year") +
  xlim(25,190)
png(paste0(here::here(), "/BBRKC/bbrkc_23f/doc/figures/size_bins_comp_Kodiak_f_5mm_LAST5.png"),height=4,width=6,res=400,units='in')
print(p)
dev.off()

## males 5mm size bins -----------------
size_group %>% 
  filter(SEX == 'MALE') %>% 
  group_by(SURVEY_YEAR,SIZE_CLASS_MM) %>%
  summarize(abund=sum(ABUNDANCE)) -> kod_dat_m

### ggridges 5mm bins ----------
kod_dat_m %>% 
  mutate(size_bin = ifelse(SIZE_CLASS_MM > 190, 190, floor(SIZE_CLASS_MM/5)* 5)) %>% 
  group_by(SURVEY_YEAR, size_bin) %>% 
  summarize(abund = sum(abund)) -> kod_dat_m_5mm

p <- ggplot(dat=kod_dat_m_5mm) 
#p <- 
p <- p + geom_density_ridges(aes(x=size_bin, y=SURVEY_YEAR, height = abund,
                            group = SURVEY_YEAR, 
                            fill=stat(y),alpha=.9999), stat = "identity",scale=15) +
  scale_fill_viridis_c()+
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90)) +
  labs(x="Carapace width (mm)", y = "Male abundance in survey year") +
  xlim(25,190)
png(paste0(here::here(), "/BBRKC/bbrkc_23f/doc/figures/size_bins_comp_Kodiak_m_5mm.png"),height=9,width=6,res=400,units='in')
print(p)
dev.off()

### last 5 years males -------

kod_dat_m_5mm %>% 
  filter(SURVEY_YEAR >= (cur_yr-6)) -> kod_dat_m_5mm_2
p <- ggplot(dat=kod_dat_m_5mm_2) 
#p <- 
p <- p + geom_density_ridges(aes(x=size_bin, y=SURVEY_YEAR, height = abund,
                                 group = SURVEY_YEAR, 
                                 fill=stat(y),alpha=.9999), stat = "identity",scale=2) +
  scale_fill_viridis_c()+
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90)) +
  labs(x="Carapace width (mm)", y = "Male abundance in survey year") +
  xlim(25,190)
png(paste0(here::here(), "/BBRKC/bbrkc_23f/doc/figures/size_bins_comp_Kodiak_m_5mm_LAST5.png"),height=4,width=6,res=400,units='in')
print(p)
dev.off()

### ggridges raw 1mm data 
p <- ggplot(dat=kod_dat_m) 
#p <- 
  p + geom_density_ridges(aes(x=SIZE_CLASS_MM, y=SURVEY_YEAR, height = abund,
                                 group = SURVEY_YEAR, 
                                 fill=stat(y),alpha=.9999), stat = "identity",scale=15) +
  scale_fill_viridis_c()+
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90)) +
  labs(x="Carapace width (mm)") +
  xlim(25,200)
#png("plots/size_bins_comp_Kodiak_f.png",height=9,width=6,res=400,units='in')
#print(p)
#dev.off()


kod_dat_m %>% 
  ggplot(aes(x = SIZE_CLASS_MM, y = SURVEY_YEAR, group = SURVEY_YEAR), fill = SURVEY_YEAR, height = abund) +
    geom_density_ridges(aes(height = abund), alpha= 0.25, 
                            scale = 5)


############### nothing below this line is being used --------------------
# male size comps
haul_rkc %>% 
  filter(AKFIN_SURVEY_YEAR >= cur_yr-2) %>% 
  select(AKFIN_SURVEY_YEAR, SPECIES_NAME, SEX, LENGTH_1MM, SAMPLING_FACTOR) %>% 
  filter(SEX == 1, 
         LENGTH_1MM >= 65) %>% 
  mutate(#total = rowSums(.[7:ncol(.)]), 
         size_bin = ifelse(LENGTH_1MM > 160, 160, floor(LENGTH_1MM/5)* 5)) %>% 
  group_by(AKFIN_SURVEY_YEAR, SEX, size_bin, .drop = F) %>% 
  summarise(Ncount = n()) %>% 
  mutate(total_samp = sum(Ncount), # this needs to be for both males and females
         Nprop = round(Ncount/total_samp, 4)) %>% 
  select(AKFIN_SURVEY_YEAR, total_samp, size_bin, Nprop) %>% 
  as.data.frame() %>% 
  spread(size_bin, Nprop) 
 


# sample size by year
haul_rkc %>% 
  filter(AKFIN_SURVEY_YEAR >= cur_yr-2) %>% 
  select(AKFIN_SURVEY_YEAR, SPECIES_NAME, SEX, LENGTH_1MM, SAMPLING_FACTOR) %>% 
  filter(#SEX == 1, 
    LENGTH_1MM >= 65) %>% 
  group_by(AKFIN_SURVEY_YEAR) %>% 
  summarise(total_samp = n()) -> samp_by_year
  

## sample size for length comps??? ----------------





#######OLD _-------------




# old smbkc --- not sure if I need this.
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
  
# 1978 - 2021 mean survey biomass
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

write.csv(recruit90to104, paste0(here::here(), '/SMBKC/smbkc_22/data/recruit90to104_biomass.csv'), 
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

write.csv(proportion_by_group, paste0(here::here(), '/SMBKC/smbkc_22/data/proportion_size_class.csv'), 
          row.names = FALSE)

## corner station removal -------
corner_station <- read.csv(paste0(here::here(), '/SMBKC/smbkc_22/model_1_corner/data/bk_stmatt_abundance_sizegroup.csv'))
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
