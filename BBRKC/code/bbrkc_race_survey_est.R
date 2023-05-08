# notes ----
## female biomass and size composition in nmfs survey with and without retow
## tyler jackson
## 5/1/2023

# load ----

library(tidyverse)

# data ----

# specimen data (haul data dump)
read.csv("./NMFS_survey/data/rkc_specimen.csv", skip = 5) %>%
  rename_all(tolower) -> specimen

# strata file (strata dump)
read.csv("./NMFS_survey/data/rkc_strata.csv") %>%
  rename_all(tolower) %>%
  rename_at(1, ~"station_id") -> strata

# refine strata information ----

## list of bristol bay stations
strata %>%
  filter(district == "Bristol Bay") %>%
  distinct(station_id) %>% pull -> bb_stations

## survey area in bristol bay nmi2
bb_area <- length(bb_stations) * 401 #each full station is 401 nmi2

# female abundance & biomass, w/ retow ----

# every station towed in each year
strata %>%
  filter(district == "Bristol Bay") %>%
  distinct(survey_year, station_id, total_area_sq_nm) %>%
  rename(akfin_survey_year = survey_year, gis_station = station_id) -> hauls

# estimates
specimen %>%
  # filter for component of the population of interest
  filter(gis_station %in% bb_stations,
         haul_type %in% c(3, 0, 4, 17),  # will likely want to remove haul types 0 and 4 after nmfs corrects their timeseries
         sex == 2,
         length_1mm >= 65) %>%
  # handle retows
  # rejoin to zero catch retows
  right_join(specimen %>%
               distinct(akfin_survey_year, gis_station, haul_type, area_swept)) %>%
  group_by(akfin_survey_year, gis_station) %>%
  nest %>% ungroup %>% 
  mutate(retow = purrr::map(data, function(data) {
    
    # no retow, no action
    if(!(17 %in% data$haul_type)) { out <- data}
    
    # retow, remove female data from original tows
    if(17 %in% data$haul_type) {
      
      data %>%
        filter(!(sex == 1 & haul_type == 17),
               !(is.na(sex) & haul_type == 3),
               !(sex == 2 & haul_type == 3)) -> out
      
    }
    
    return(out)
    
    
  })) %>%
  dplyr::select(-data) %>%
  unnest(retow) %>%
  # compute station cpue by clutch size
  group_by(akfin_survey_year, gis_station, haul_type) %>%
  summarise(n_crab = sum(sampling_factor),
            wt_crab = sum(sampling_factor * calculated_weight_1mm) / 1e6,
            cpue_n = n_crab / mean(area_swept),
            cpue_wt = wt_crab / mean(area_swept)) %>%
  # join to all hauls, fill in zero catches
  right_join(hauls) %>%
  replace_na(list(n_crab = 0, cpue_n = 0, cpue_wt = 0)) %>%
  # scale to abundance
  group_by(akfin_survey_year) %>%
  summarise(abundance = mean(cpue_n) * mean(total_area_sq_nm),
            abundance_cv = (sqrt((var(n_crab) / n()) * mean(total_area_sq_nm)^2)) / abundance,
            biomass_t = mean(cpue_wt) * mean(total_area_sq_nm),
            biomass_t_cv = (sqrt((var(cpue_wt) / n()) * mean(total_area_sq_nm)^2)) / biomass_t) -> est_w_retow
              
# female size comp w/ retow ----

# every station towed in each year
strata %>%
  filter(district == "Bristol Bay") %>%
  distinct(survey_year, station_id, total_area_sq_nm) %>%
  rename(akfin_survey_year = survey_year, gis_station = station_id) %>%
  expand_grid(length_1mm = 65:200, sex = 0:2) -> hauls

specimen %>%
  # filter for component of the population of interest
  filter(gis_station %in% bb_stations,
         haul_type %in% c(3, 0, 4, 17),  # will likely want to remove haul types 0 and 4 after nmfs corrects their timeseries
         length_1mm >= 65,
         sex %in% 0:2) %>%
         
  # handle retows
  # rejoin to zero catch retows
  right_join(specimen %>%
               distinct(akfin_survey_year, gis_station, haul_type, area_swept)) %>%
  group_by(akfin_survey_year, gis_station) %>%
  nest %>% ungroup %>% 
  mutate(retow = purrr::map(data, function(data) {
    
    # no retow, no action
    if(!(17 %in% data$haul_type)) { out <- data}
    
    # retow, remove female data from original tows
    if(17 %in% data$haul_type) {
      
      data %>%
        filter(!(sex == 1 & haul_type == 17),
               !(is.na(sex) & haul_type == 3),
               !(sex == 2 & haul_type == 3)) -> out
      
    }
    
    return(out)
    
    
  })) %>%
  dplyr::select(-data) %>%
  unnest(retow) %>%
  
  # compute station cpue by clutch size
  group_by(akfin_survey_year, gis_station, sex, length_1mm) %>%
  summarise(n_crab = n(),
            cpue_n = sum(sampling_factor) / mean(area_swept)) %>%
  # join to all hauls, fill in zero catches
  right_join(hauls) %>%
  replace_na(list(n_crab = 0, cpue_n = 0)) %>%
  # scale to abundance
  group_by(akfin_survey_year, sex, length_1mm) %>%
  summarise(n_hat = mean(cpue_n) * mean(total_area_sq_nm),
            nmeas = sum(n_crab)) %>%
  # add bin and summarise
  mutate(bin = ifelse(length_1mm > 160, 160, floor(length_1mm / 5) * 5),
         bin = ifelse(sex == 2 & bin >= 140, 140, bin)) %>%
  group_by(akfin_survey_year, sex, bin) %>%
  summarise(n_hat = sum(n_hat),
            nmeas = sum(nmeas)) %>%
  # get total abundance and total measured
  group_by(akfin_survey_year) %>%
  mutate(total_n_hat = sum(n_hat),
         nsamp = sum(nmeas)) %>% ungroup %>%
  # proportion by size
  mutate(prop = n_hat / total_n_hat) %>%
  group_by(akfin_survey_year, sex, bin, nsamp) %>%
  summarise(prop = sprintf("%.4f", sum(prop))) %>% ungroup -> size_comp_w_retow


# female format for gmacs
size_comp_w_retow %>%
  filter(sex == 2) %>%
  transmute(akfin_survey_year, nsamp, bin, prop) %>%
  pivot_wider(names_from = bin, values_from = prop) %>%
  replace(is.na(.), "0.0000")  %>%
  rename_at(3:18, ~paste0("l", seq(65, 140, 5))) %>%
  transmute(`#year` = akfin_survey_year,
            season = 1,
            fleet = 5, 
            sex = 2,
            type = 0,
            shell = 0,
            maturity = 0,
            nsamp = 0,
            l65, l70, l75, l80, l85, l90, l95, l100, l105, l110, l115, l120, l125, l130, l135, l140) %>%
  write_delim("./nmfs_female_size_comp_w_retow.txt", delim = "\t")


# female abundance & biomass, w/o retow ----

# every station towed in each year
strata %>%
  filter(district == "Bristol Bay") %>%
  distinct(survey_year, station_id, total_area_sq_nm) %>%
  rename(akfin_survey_year = survey_year, gis_station = station_id) -> hauls

# estimates
specimen %>%
  # filter for component of the population of interest
  filter(gis_station %in% bb_stations,
         haul_type %in% c(3, 0, 4),  # will likely want to remove haul types 0 and 4 after nmfs corrects their timeseries
         sex == 2,
         length_1mm >= 65) %>%

  # compute station cpue by clutch size
  group_by(akfin_survey_year, gis_station, haul_type) %>%
  summarise(n_crab = sum(sampling_factor),
            wt_crab = sum(sampling_factor * calculated_weight_1mm) / 1e6,
            cpue_n = n_crab / mean(area_swept),
            cpue_wt = wt_crab / mean(area_swept)) %>%
  # join to all hauls, fill in zero catches
  right_join(hauls) %>%
  replace_na(list(n_crab = 0, cpue_n = 0, cpue_wt = 0)) %>%
  # scale to abundance
  group_by(akfin_survey_year) %>%
  summarise(abundance = mean(cpue_n) * mean(total_area_sq_nm),
            abundance_cv = (sqrt((var(n_crab) / n()) * mean(total_area_sq_nm)^2)) / abundance,
            biomass_t = mean(cpue_wt) * mean(total_area_sq_nm),
            biomass_t_cv = (sqrt((var(cpue_wt) / n()) * mean(total_area_sq_nm)^2)) / biomass_t) -> est_wo_retow

# female size comp w/o retow ----

# every station towed in each year
strata %>%
  filter(district == "Bristol Bay") %>%
  distinct(survey_year, station_id, total_area_sq_nm) %>%
  rename(akfin_survey_year = survey_year, gis_station = station_id) %>%
  expand_grid(length_1mm = 65:200, sex = 0:2) -> hauls

specimen %>%
  # filter for component of the population of interest
  filter(gis_station %in% bb_stations,
         haul_type %in% c(3, 0, 4),  # will likely want to remove haul types 0 and 4 after nmfs corrects their timeseries
         length_1mm >= 65,
         sex %in% 0:2) %>%
  # compute station cpue by clutch size
  group_by(akfin_survey_year, gis_station, sex, length_1mm) %>%
  summarise(n_crab = n(),
            cpue_n = sum(sampling_factor) / mean(area_swept)) %>%
  # join to all hauls, fill in zero catches
  right_join(hauls) %>%
  replace_na(list(n_crab = 0, cpue_n = 0)) %>%
  # scale to abundance
  group_by(akfin_survey_year, sex, length_1mm) %>%
  summarise(n_hat = mean(cpue_n) * mean(total_area_sq_nm),
            nmeas = sum(n_crab)) %>%
  # add bin and summarise
  mutate(bin = ifelse(length_1mm > 160, 160, floor(length_1mm / 5) * 5),
         bin = ifelse(sex == 2 & bin >= 140, 140, bin)) %>%
  group_by(akfin_survey_year, sex, bin) %>%
  summarise(n_hat = sum(n_hat),
            nmeas = sum(nmeas)) %>%
  # get total abundance and total measured
  group_by(akfin_survey_year) %>%
  mutate(total_n_hat = sum(n_hat),
         nsamp = sum(nmeas)) %>% ungroup %>%
  # proportion by size
  mutate(prop = n_hat / total_n_hat) %>%
  group_by(akfin_survey_year, sex, bin, nsamp) %>%
  summarise(prop = sprintf("%.4f", sum(prop))) %>% ungroup -> size_comp_wo_retow


# female format for gmacs
size_comp_wo_retow %>%
  filter(sex == 2) %>%
  transmute(akfin_survey_year, nsamp, bin, prop) %>%
  pivot_wider(names_from = bin, values_from = prop) %>%
  replace(is.na(.), "0.0000")  %>%
  rename_at(3:18, ~paste0("l", seq(65, 140, 5))) %>%
  transmute(`#year` = akfin_survey_year,
            season = 1,
            fleet = 5, 
            sex = 2,
            type = 0,
            shell = 0,
            maturity = 0,
            nsamp = 0,
            l65, l70, l75, l80, l85, l90, l95, l100, l105, l110, l115, l120, l125, l130, l135, l140) %>%
  write_delim("./nmfs_female_size_comp_wo_retow.txt", delim = "\t")
