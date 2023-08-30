# red king crab size comps 
# 2023 stock structure template
# k.palof

# load ----

library(tidyverse)
library(patchwork)
library(FNGr); theme_set(theme_sleek())

cur_yr <- 2023 # update annually
folder <- "stock_structure" # update annually
.FIGS     = c(paste0("./BBRKC/", folder, "/"))
# y axis minor ticks
yr_axis = tickr(tibble(yr = 1975:2024), yr, 5)

# data ----

# specimen data (haul data dump)
read.csv("./BBRKC/data/2023/survey/EBSCrab_Haul.csv", skip = 5) %>%
  rename_all(tolower) -> specimen

# strata file (strata dump) - do NOT presort by BB when pull it from AKFIN
# AKFIN/Crab Data/"Lookups/Translations"/"Stata/Station Characteristics" - 1975 to 2023, RED KING CRAB
read.csv("./BBRKC/data/2023/survey/EBSCRAB - Strata Report_all.csv") %>%
  rename_all(tolower) %>%
  rename_at(1, ~"station_id") -> strata

# used in 'EBSSurvey_analysis.R' for size comp figures 
size_group <- read.csv(paste0(here::here(), '/BBRKC/data/', cur_yr, '/survey/EBSCrab_Abundance_Biomass_bb_north/EBSCrab_Abundance_Biomass.csv'), 
                       skip = 7)

# size comps -----------
head(size_group)

## females 5mm size bins ---------------------------
size_group %>% 
  filter(SEX == 'FEMALE') %>% 
  group_by(DISTRICT_CODE, SURVEY_YEAR,SIZE_CLASS_MM) %>%
  summarize(abund=sum(ABUNDANCE)) -> fem_dat

### ggridges 5mm bins 
kod_dat_f %>% 
  mutate(size_bin = ifelse(SIZE_CLASS_MM > 190, 190, floor(SIZE_CLASS_MM/5)* 5)) %>% 
  group_by(DISTRICT_CODE, SURVEY_YEAR, size_bin) %>% 
  summarize(abund = sum(abund)) -> fem_dat_f_5mm

p <- ggplot(dat=fem_dat_f_5mm) 
#p <- 
p <- p + geom_density_ridges(aes(x=size_bin, y=SURVEY_YEAR, height = abund,
                                 group = SURVEY_YEAR, 
                                 fill=stat(y),alpha=.9999), stat = "identity",scale=15) +
  facet_wrap(~DISTRICT_CODE) +
  scale_fill_viridis_c()+
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90)) +
  labs(x="Carapace width (mm)", y = "Female abundance in survey year")+
  xlim(25,190)

png(paste0(here::here(), "/BBRKC/", folder,"/N_BB_size_bins_comp_fem_5mm.png"),height=9,width=6,res=400,units='in')
print(p)
dev.off()

# last 5 years of data 5mm females -----
fem_dat_f_5mm %>% 
  filter(SURVEY_YEAR >= (cur_yr-6)) -> kod_dat_f_5mm_2
p <- ggplot(dat=kod_dat_f_5mm_2) 
#p <- 
p <- p + geom_density_ridges(aes(x=size_bin, y=SURVEY_YEAR, height = abund,
                                 group = SURVEY_YEAR, 
                                 fill=stat(y),alpha=.9999), stat = "identity",scale=2, panel_scaling = FALSE) +
  facet_wrap(~DISTRICT_CODE) +
  scale_fill_viridis_c()+
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90)) +
  labs(x="Carapace width (mm)", y = "Female abundance (not scaled)") +
  xlim(25,190)
png(paste0(here::here(), "/BBRKC/", folder,"/N_BBsize_bins_comp_fem_5mm_LAST5.png"),height=4,width=6,res=400,units='in')
print(p)
dev.off()

## males 5mm size bins -----------------
size_group %>% 
  filter(SEX == 'MALE') %>% 
  group_by(DISTRICT_CODE, SURVEY_YEAR,SIZE_CLASS_MM) %>%
  summarize(abund=sum(ABUNDANCE)) -> male_dat_m

### ggridges 5mm bins ----------
male_dat_m %>% 
  mutate(size_bin = ifelse(SIZE_CLASS_MM > 190, 190, floor(SIZE_CLASS_MM/5)* 5)) %>% 
  group_by(DISTRICT_CODE,SURVEY_YEAR, size_bin) %>% 
  summarize(abund = sum(abund)) -> male_dat_m_5mm

p <- ggplot(dat=male_dat_m_5mm) 
#p <- 
p <- p + geom_density_ridges(aes(x=size_bin, y=SURVEY_YEAR, height = abund,
                                 group = SURVEY_YEAR, 
                                 fill=stat(y),alpha=.9999), stat = "identity",scale=15) +
  facet_wrap(~DISTRICT_CODE) +
  scale_fill_viridis_c()+
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90)) +
  labs(x="Carapace width (mm)", y = "Male abundance in survey year") +
  xlim(25,190)
png(paste0(here::here(), "/BBRKC/", folder,"/N_BB_size_bins_comp_male_5mm.png"),height=9,width=6,res=400,units='in')
print(p)
dev.off()

### last 5 years males -------

male_dat_m_5mm %>% 
  filter(SURVEY_YEAR >= (cur_yr-6)) -> male_dat_m_5mm_2
p <- ggplot(dat=male_dat_m_5mm_2) 
#p <- 
p <- p + geom_density_ridges(aes(x=size_bin, y=SURVEY_YEAR, height = abund,
                                 group = SURVEY_YEAR, 
                                 fill=stat(y),alpha=.9999), stat = "identity",scale=2, panel_scaling = FALSE) +
  facet_wrap(~DISTRICT_CODE) +
  scale_fill_viridis_c()+
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90)) +
  labs(x="Carapace width (mm)", y = "Male abundance (not scaled)") +
  xlim(25,190)
png(paste0(here::here(), "/BBRKC/", folder,"/N_BBsize_bins_comp_male_5mm_LAST5.png"),height=4,width=6,res=400,units='in')
print(p)
dev.off()

### ggridges raw 1mm data 
p <- ggplot(dat=male_dat_m) 
#p <- 
p + geom_density_ridges(aes(x=SIZE_CLASS_MM, y=SURVEY_YEAR, height = abund,
                            group = SURVEY_YEAR, 
                            fill=stat(y),alpha=.9999), stat = "identity",scale=15) +
  facet_wrap(~DISTRICT_CODE) +
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



















# refine strata information ----

## list of bristol bay stations
strata %>%
  filter(district == "Bristol Bay") %>%
  distinct(station_id) %>% pull -> bb_stations

## survey area in bristol bay nmi2
bb_area <- length(bb_stations) * 401 #each full station is 401 nmi2

## list of northern unstratified stations
strata %>%
  filter(survey_year == 2021) %>%
  #count(district)
  filter(district == "Northern Unstratified") %>%
  distinct(station_id) %>% pull -> nu_stations

## survey area in northern unstratified nmi2
nu_area <- length(nu_stations) * 401 #each full station is 401 nmi2

# bristol bay male estimates ----

# every station towed in each year
strata %>%
  filter(district %in% c("Bristol Bay", "Northern Unstratified")) %>%
  distinct(survey_year, district, station_id, total_area_sq_nm) %>%
  dplyr::rename(akfin_survey_year = survey_year, gis_station = station_id) %>%
  expand_grid(sex = 1, length_1mm = 0:250) -> hauls
