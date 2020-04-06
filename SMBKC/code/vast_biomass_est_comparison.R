# VAST input for model

# katie palof, katie.palof@alaska.gov
# VAST estimates produced by Jon Richar at NOAA Kodiak for SMBKC trawl survey only

# VAST data --------
data_vast_19 <- read.csv(here::here("SMBKC/smbkc_19a/data/SMBKC_GE90_BIOMASS.csv"))


# current data in model -------------
data_trawl <- read.csv(here::here("SMBKC/smbkc_19/data/survey_biomass_mt.csv"))


### data clean up --------
data_vast_19 %>% 
  filter(Year >= 1978) %>% 
  mutate(CV_mt = SD_mt/Estimate_metric_tons) -> data_vast_19
data_vast_19 %>% select(Year, Estimate_metric_tons, CV_mt) # for entry into the .dat file

data_trawl %>% 
  filter(SURVEY_YEAR >= 1978) -> data_trawl

# visualization -------
data_vast_19 %>% 
  filter(Year >= 1978) %>% 
  ggplot(aes(Year, Estimate_metric_tons)) +
  geom_line()


data_trawl %>% 
  filter(SURVEY_YEAR >= 1978) %>% 
  ggplot(aes(SURVEY_YEAR, BIOMASS_MT)) +
  geom_line()


data_trawl %>% 
  select(Year = SURVEY_YEAR, BIOMASS_MT, BIOMASS_MT_CV) %>% 
  bind_cols(data_vast_19) %>% 
  select(Year, BIOMASS_MT, BIOMASS_MT_CV, Estimate_metric_tons, SD_mt) %>% 
  ggplot(aes(Year, BIOMASS_MT)) +
  geom_line() +
  geom_line(aes(Year, Estimate_metric_tons), col = "red")
