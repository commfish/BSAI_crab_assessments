# notes ----
## female estimate with and without retow
## method 1 - following same method as used for akfin reports and tech memo
## method 2 - method w/ corrected timeseries survey area and appropriate haul types in early timeseries
## tyler jackson 
## 9/2/2022 / 4-24-23 (edited by K.palof for bbrkc model runs spring 2023)

# load ----

library(tidyverse)

# data ----

# specimen data (haul data dump)
read.csv("./BBRKC/data/2022/survey/rkc_specimen.csv", skip = 5) %>%
  rename_all(tolower) -> specimen

# strata file (strata dump)
read.csv("./BBRKC/data/2022/survey/rkc_strata.csv") %>%
  rename_all(tolower) %>%
  rename_at(1, ~"station_id") -> strata

# refine strata information ----

## list of bristol bay stations
strata %>%
  filter(district == "Bristol Bay") %>%
  distinct(station_id) %>% pull -> bb_stations

## survey area in bristol bay nmi2
bb_area <- length(bb_stations) * 401 #each full station is 401 nmi2

# method 1, w/ retow ----

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
         length_1mm >= 65) %>% # need females GE 65mm
         #length_1mm >= 90) %>%
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
  group_by(akfin_survey_year, gis_station, haul_type, area_swept) %>%
  summarise(n_crab = sum(sampling_factor),
            cpue_n = n_crab / mean(area_swept), 
            weight_crab = sum(calculated_weight_1mm*sampling_factor), 
            cpue_wt = weight_crab / mean (area_swept)) %>%
  # join to all hauls, fill in zero catches
  right_join(hauls) %>%
  replace_na(list(n_crab = 0, cpue_n = 0, weight_crab = 0, cpue_wt = 0)) %>%
  
  group_by(akfin_survey_year) %>%
  summarise(n_tows = n(),
            total_catch_rt = sum(n_crab),
            mean_catch_tow_rt = mean(n_crab),
            se_catch_tow_rt = sqrt(var(n_crab) / n_tows),
            abundance_rt = mean(cpue_n) * mean(total_area_sq_nm),
            cv_rt = sqrt((var(cpue_n) / n_tows) * mean(total_area_sq_nm)^2) / abundance_rt, 
            weight_rt = mean(cpue_wt)* mean(total_area_sq_nm), 
            weight_cv_rt = sqrt((var(cpue_wt) / n_tows) * mean(total_area_sq_nm)^2) / weight_rt, 
            weight_rt_mt = weight_rt/1000000) -> method1_w_retow
    


# method 1, w/o retow ----

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
         length_1mm >= 65) %>% # GE 65 not 90 here 
  # compute station cpue
  group_by(akfin_survey_year, gis_station, area_swept) %>%
  summarise(n_crab = sum(sampling_factor),
            cpue_n = n_crab / mean(area_swept), 
            weight_crab = sum(calculated_weight_1mm*sampling_factor), 
            cpue_wt = weight_crab / mean (area_swept)) %>%
  # join to all hauls, fill in zero catches
  right_join(hauls) %>%
  replace_na(list(n_crab = 0, cpue_n = 0, weight_crab = 0, cpue_wt = 0)) %>%
  
  group_by(akfin_survey_year) %>%
  summarise(n_tows = n(),
            total_catch_no_rt = sum(n_crab),
            mean_catch_tow_no_rt = mean(n_crab),
            se_catch_tow_no_rt = sqrt(var(n_crab) / n_tows),
            abundance_no_rt = mean(cpue_n) * mean(total_area_sq_nm),
            cv_no_rt = sqrt((var(cpue_n) / n_tows) * mean(total_area_sq_nm)^2) / abundance_no_rt, 
            weight_no_rt = mean(cpue_wt)* mean(total_area_sq_nm), 
            weight_cv_no_rt = sqrt((var(cpue_wt) / n_tows) * mean(total_area_sq_nm)^2) / weight_no_rt, 
            weight_no_rt_mt = weight_no_rt/1000000) -> method1_wo_retow

# method 2, w/ retow ----

# every station towed in each year
strata %>%
  filter(district == "Bristol Bay") %>%
  distinct(survey_year, station_id, total_area_sq_nm) %>%
  rename(akfin_survey_year = survey_year, gis_station = station_id) -> hauls

# estimates
specimen %>%
  # filter for component of the population of interest
  filter(gis_station %in% bb_stations,
         haul_type %in% c(3, 17),
         sex == 2,
         length_1mm >= 90) %>%
  # handle retows
  right_join(specimen %>%
               distinct(akfin_survey_year, gis_station, haul_type, area_swept)) %>%
  group_by(akfin_survey_year, gis_station) %>%
  nest %>% ungroup %>%
  mutate(retow = purrr::map(data, function(data) {
    
    # no retow, no action
    if(!(17 %in% data$haul_type)) { out <- data}
    
    # retow, remove female data from original tows, male data from retows
    if(17 %in% data$haul_type) {
      
      data %>%
        filter(!(sex == 2 & haul_type == 3),
               !(sex == 1 & haul_type == 17)) -> out
      
    }
    
    return(out)
    
    
  })) %>%
  dplyr::select(-data) %>%
  unnest(retow) %>%
  # compute station cpue by clutch size
  group_by(akfin_survey_year, gis_station, area_swept) %>%
  summarise(n_crab = sum(sampling_factor),
            cpue_n = n_crab / mean(area_swept)) %>%
  # join to all hauls, fill in zero catches
  right_join(hauls) %>%
  replace_na(list(n_crab = 0, cpue_n = 0)) %>%
  
  
  group_by(akfin_survey_year) %>%
  summarise(n_tows = n(),
            total_catch_rt = sum(n_crab),
            mean_catch_tow_rt = mean(n_crab),
            se_catch_tow_rt = sqrt(var(n_crab) / n_tows),
            abundance_rt = mean(cpue_n) * bb_area,
            cv_rt = sqrt((var(cpue_n) / n_tows) * bb_area^2) / abundance_rt) -> method2_w_retow



# method 2, w/o retow ----

# every station towed in each year
strata %>%
  filter(district == "Bristol Bay") %>%
  distinct(survey_year, station_id, total_area_sq_nm) %>%
  rename(akfin_survey_year = survey_year, gis_station = station_id) -> hauls

# estimates
specimen %>%
  # filter for component of the population of interest
  filter(gis_station %in% bb_stations,
         haul_type == 3,  
         sex == 2,
         length_1mm >= 90) %>%
  # compute station cpue by clutch size
  group_by(akfin_survey_year, gis_station, area_swept) %>%
  summarise(n_crab = sum(sampling_factor),
            cpue_n = n_crab / mean(area_swept)) %>%
  # join to all hauls, fill in zero catches
  right_join(hauls) %>%
  replace_na(list(n_crab = 0, cpue_n = 0)) %>%
  
  group_by(akfin_survey_year) %>%
  summarise(n_tows = n(),
            total_catch_no_rt = sum(n_crab),
            mean_catch_tow_no_rt = mean(n_crab),
            se_catch_tow_no_rt = sqrt(var(n_crab) / n_tows),
            abundance_no_rt = mean(cpue_n) * bb_area,
            cv_no_rt = sqrt((var(cpue_n) / n_tows) * bb_area^2) / abundance_no_rt) -> method2_wo_retow


# splice output together ----

## method 1 
left_join(method1_w_retow, method1_wo_retow, by = c("akfin_survey_year", "n_tows")) %>%
  write_csv("BBRKC/data/NMFS_survey/output/race_mature_female_catch_w_and_wo_retow.csv")

## method 2
left_join(method2_w_retow, method2_wo_retow, by = c("akfin_survey_year", "n_tows")) %>%
  write_csv("BBRKC/data/NMFS_survey/output/race_mature_female_catch_w_and_wo_retow_method2.csv")


# plots of method 1 estimates ----

# estimates
specimen %>%
  # filter for component of the population of interest
  filter(gis_station %in% bb_stations,
         haul_type %in% c(3, 0, 4),  # will likely want to remove haul types 0 and 4 after nmfs corrects their timeseries
         sex == 2,
         length_1mm >= 90) %>%
  # compute station cpue by clutch size
  group_by(akfin_survey_year, gis_station, area_swept) %>%
  summarise(n_crab = sum(sampling_factor),
            cpue_n = n_crab / mean(area_swept)) %>%
  # join to all hauls, fill in zero catches
  right_join(hauls) %>%
  replace_na(list(n_crab = 0, cpue_n = 0)) -> cpue_haul_no_rt

specimen %>%
  # filter for component of the population of interest
  filter(gis_station %in% bb_stations,
         haul_type %in% c(3, 0, 4, 17),  # will likely want to remove haul types 0 and 4 after nmfs corrects their timeseries
         sex == 2,
         length_1mm >= 90) %>%
  # compute station cpue by clutch size
  group_by(akfin_survey_year, gis_station, area_swept) %>%
  summarise(n_crab = sum(sampling_factor),
            cpue_n = n_crab / mean(area_swept)) %>%
  # join to all hauls, fill in zero catches
  right_join(hauls) %>%
  replace_na(list(n_crab = 0, cpue_n = 0)) -> cpue_haul_rt


cpue_haul_rt %>% # where does this originate???
  ungroup %>%
  dplyr::select(akfin_survey_year, gis_station, total_area_sq_nm, cpue_n) %>%
  rename(cpue_rt = cpue_n) %>%
  left_join(cpue_haul_no_rt %>%
              ungroup %>%
              dplyr::select(akfin_survey_year, gis_station, total_area_sq_nm, cpue_n) %>%
              rename(cpue_no_rt = cpue_n)) %>%
  #compute covariance within year
  group_by(akfin_survey_year) %>%
  summarise(abundance_rt = mean(cpue_rt) * mean(total_area_sq_nm),
            abundance_rt_var = (var(cpue_rt) / n()) * mean(total_area_sq_nm)^2,
            abundance_no_rt = mean(cpue_no_rt) * mean(total_area_sq_nm),
            abundance_no_rt_var = (var(cpue_no_rt) / n()) * mean(total_area_sq_nm)^2,
            abundance_cov = cov(cpue_rt, cpue_no_rt) * mean(total_area_sq_nm)) %>%
  # compute difference
  mutate(diff = abundance_rt - abundance_no_rt,
         diff_var = abundance_rt_var + abundance_no_rt_var - (2 * abundance_cov),
         diff_l95 = diff - 1.96 * sqrt(diff_var),
         diff_u95 = diff + 1.96 * sqrt(diff_var)) %>%
  dplyr::select(akfin_survey_year, diff, diff_var, diff_l95, diff_u95) %>%
  
  ggplot()+
  geom_bar(aes(x = akfin_survey_year, y = diff), stat = "identity")+
  geom_errorbar(aes(x = akfin_survey_year, ymin = diff_l95, ymax = diff_u95), width = 0.1)+
  geom_hline(yintercept = 0, linetype = 2)+
  coord_cartesian(xlim = c(1983,2022), ylim = c(-2.5e7, 2.5e7))+
  labs(x = NULL, y = "Difference (Retow - Non-Retow Abundance")+
  FNGr::theme_sleek() -> x
ggsave("./NMFS_survey/figures/race_bbrkc_retow_diff.png", height = 3, width = 5, units = "in")
  


# method 1 - male ge 120, w/ retow, hypothetical ----

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
         sex == 1,
         length_1mm >= 120) %>%
  # handle retows
  # rejoin to zero catch retows
  right_join(specimen %>%
               distinct(akfin_survey_year, gis_station, haul_type, area_swept)) %>%
  group_by(akfin_survey_year, gis_station) %>%
  nest %>% ungroup %>% 
  mutate(retow = purrr::map(data, function(data) {
    
    # no retow, no action
    if(!(17 %in% data$haul_type)) { out <- data}
    
    # retow, remove male data from original tows
    if(17 %in% data$haul_type) {
      
      data %>%
        filter(!(sex == 1 & haul_type == 3),
               !(is.na(sex) & haul_type == 3),
               !(sex == 2 & haul_type == 3)) -> out
      
    }
    
    return(out)
    
    
  })) %>%
  dplyr::select(-data) %>%
  unnest(retow) %>%
  # compute station cpue by clutch size
  group_by(akfin_survey_year, gis_station, haul_type, area_swept) %>%
  summarise(n_crab = sum(sampling_factor),
            cpue_n = n_crab / mean(area_swept)) %>%
  # join to all hauls, fill in zero catches
  right_join(hauls) %>%
  replace_na(list(n_crab = 0, cpue_n = 0)) %>%
  
  group_by(akfin_survey_year) %>%
  summarise(n_tows = n(),
            total_catch_rt = sum(n_crab),
            mean_catch_tow_rt = mean(n_crab),
            se_catch_tow_rt = sqrt(var(n_crab) / n_tows),
            abundance_rt = mean(cpue_n) * mean(total_area_sq_nm),
            cv_rt = sqrt((var(cpue_n) / n_tows) * mean(total_area_sq_nm)^2) / abundance_rt) -> males_w_retow



# method 1 - male ge 120, w/o retow ----

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
         sex == 1,
         length_1mm >= 120) %>%
  # compute station cpue
  group_by(akfin_survey_year, gis_station, area_swept) %>%
  summarise(n_crab = sum(sampling_factor),
            cpue_n = n_crab / mean(area_swept)) %>%
  # join to all hauls, fill in zero catches
  right_join(hauls) %>%
  replace_na(list(n_crab = 0, cpue_n = 0)) %>%
  
  group_by(akfin_survey_year) %>%
  summarise(n_tows = n(),
            total_catch_no_rt = sum(n_crab),
            mean_catch_tow_no_rt = mean(n_crab),
            se_catch_tow_no_rt = sqrt(var(n_crab) / n_tows),
            abundance_no_rt = mean(cpue_n) * mean(total_area_sq_nm),
            cv_no_rt = sqrt((var(cpue_n) / n_tows) * mean(total_area_sq_nm)^2) / abundance_no_rt) -> males_wo_retow


left_join(males_w_retow, males_wo_retow, by = c("akfin_survey_year", "n_tows")) %>%
  write_csv("NMFS_survey/output/race_mature_males_catch_w_and_wo_retow_hypothetical.csv")
