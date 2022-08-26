# katie.palof@alaska.gov  9-5-19 / 4-22-22/ 8-25-22
# map creation for SMBKC trawl survey samples from recent years

# WIP   
# https://geocompr.robinlovelace.net/adv-map.html

# load -------
library(PBSmapping)
library(tidyverse)
library(mapproj)
data("nepacLLhigh")
cur_yr = "2022"
folder = "smbkc_22f"
  
### data ---
# haul data from st.matt area NOAA trawl survey
# AKFIN data - crab data - EBS trawl survey - CPUE by Haul Downloads - "bkc st.matt's"
#C:\Users\kjpalof\Documents\BSAI_crab_assessments\SMBKC\data\trawl_survey\ebs-crab-cpue-69323-stmatt\2021
smbkc_haul_cpue <-data.frame(read.csv(paste0(here::here(), "/SMBKC/data/trawl_survey/ebs-crab-cpue-69323-stmatt/", 
                                             cur_yr, "/ebs-crab-cpue-69323-stmatt-6.csv"),
                                      header=T, as.is = TRUE))
smbkc_haul_cpue %>% 
  filter(SURVEY_YEAR >= 2013, SIZE_GROUP == "MALE_GE90") -> m.df  

glimpse(m.df)

m.df %>% 
  dplyr::select(lat = MID_LATITUDE, long = MID_LONGITUDE, year = SURVEY_YEAR, catch = CRAB_NUM) -> df.out



nepacLLhigh %>% 
  dplyr::select(group=PID, POS=POS,long=X,lat=Y) -> ak 

ggplot() + 
  geom_polygon(data = ak, aes(long, lat, group = group), fill=8, color='black') +
  theme(panel.background = element_rect(fill = 'white')) + 
  xlab(expression(paste(Longitude^o,~'W'))) +
  ylab(expression(paste(Latitude^o,~'W'))) +
  coord_map(xlim = c(-176, -170.5), ylim = c(58.5, 61.5)) +
  geom_point(data = df.out, aes(long, lat, size = catch), color = "darkorange2") +
  facet_wrap(~year, dir = "v") +
  scale_size_area() +
  #FNGr::theme_sleek()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1.0))
ggsave(paste0(here::here("SMBKC/", folder, "/doc/safe_figure/CrabN_Station.png")),dpi=300, 
       width=8, height=8,units="in")
