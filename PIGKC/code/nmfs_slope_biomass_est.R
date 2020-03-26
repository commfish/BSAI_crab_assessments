# notes ----

## 'design based' abundance and biomass estimates of GKC from NMFS slope survey
## author: Tyler Jackson
## tyler.jackson@alaska.gov
## last updated: 2020/3/23

# load ----

library(tidyverse)

## source R scripts
source("./PIGKC/code/clean_nmfs_specimen_data.R")

## global option
YEAR <- 2020

# data ----

## haul data (2002, 2004, 2008, 2010, 2012, 2016)
haul <- read_csv("./PIGKC/data/nmfs_slope_haul_2002_2016.csv")

## stratum
strata <- read_csv("./PIGKC/data/nmfs_slope_strata.csv")

## specimen data (from clean_nmfs_specimen_data.R)
str(spec_0416)

## 2002 catch data
#catch_02 <- read_csv("./PIGKC/data/nmfs_slope_gkc_catch_2002.csv")

# data mgmt ----

## change names of haul data
names(haul) <- c("record_count", "load_date", "survey", "survey_year", "vessel", "cruise",
                 "haul", "haul_join", "stratum", "stratum_inpfc_area", "stratum_regulatory_name",
                 "stratum_description", "haul_type", "performance", "start_time", "duration", 
                 "distance_fished", "net_width", "net_measured", "net_height", "start_lat", 
                 "start_lon", "end_lat", "end_lon", "station_id", "gear_depth", "bottom_depth", 
                 "bottom_type", "surface_temp", "gear_temp", "wire_length", "gear", "asscessories", 
                 "subsample", "cruise_join", "audit_join", "statisfactory_performance", 
                 "performance_note")

## change names of strata data
names(strata) <- c("record_count", "load_date", "survey", "stratum", "stratum_area", "perimeter", 
                   "startum_inpfc_area", "min_depth", "max_depth", "description", "area_code",
                   "depth_code", "area_depth_code", "regulatory_area_name", "stratum_type")

## combine the haul, starta, and specimen data 2004 - 2016
haul %>%
  # remove a few pointless fields
  dplyr::select(-record_count, -load_date, -haul_join) %>%
  # join to stratum area
  left_join(strata %>%
              dplyr::select(survey, stratum, stratum_area),
            by = c("survey", "stratum")) %>%
  # extract subarea and area_swept
  mutate(subarea = as.numeric(substring(stratum, 1, 1)),
         area_swept = distance_fished * (net_width / 1000)) -> haul

## remove 2016, haul 5 in specimen data. Not include in haul data.
## 2016 tech memo reports 175 successful tows. There are 175 hauls in 2016 data excluding haul 5
spec_0416 %>%
  filter(!(survey_year == 2016 & haul == 5)) -> spec_0416


# eda ----

# quick map of stratum
# survey %>%
#   mutate(lon = (start_lon + end_lon) / 2,
#          lat = (start_lat + end_lat) /2) %>%
#   ggplot()+
#   geom_point(aes(x = lon, y = lat, color = factor(stratum)))+
#   facet_wrap(~survey_year)


# abundance and biomass estimates by sex/size group ----

## summarize 2004 - 2016 specimen data by year, haul, group
spec_0416 %>%
  # remove any sampling_factor = 0 rows (no catch)
  # remove any unknown or hermaphrodite sex crabs
  filter(sampling_factor != 0,
         sex != 3) %>%
  # create group field to denote demographic
  mutate(group = case_when(sex == 1 & length < 107 ~ "male_immature",
                           sex == 1 & length >= 107 & length < 124 ~ "male_mature_nonlegal",
                           sex == 1 & length >= 124 ~ "male_mature_legal",
                           sex == 2 ~ "female"),
         calc_wt_kg = case_when(group == "female" ~ 0.001424 * length^2.781 / 1000,
                             group == "male_immature" ~ 0.0002988 * length^3.135 / 1000,
                             group == "male_mature_nonlegal" ~ 0.0002988 * length^3.135 / 1000,
                             group == "male_mature_legal" ~ 0.0002988 * length^3.135 / 1000)) %>%
  # compute catch by haul
  group_by(survey_year, haul, group) %>%
  summarise(num_crab = ceiling(sum(sampling_factor)),
            wt_crab_kg = sum(sampling_factor * calc_wt_kg)) %>%
  # pivot wide so its easier to join to information
  unite(num_crab, wt_crab_kg, col = "catch", sep = "_") %>%
  pivot_wider(names_from = "group", values_from = catch) %>%
  replace_na(list(male_immature = "0_0", male_mature_nonlegal = "0_0", 
                  male_mature_legal = "0_0", female = "0_0")) %>%
  # combine nonlegal and legal mature males to get total mature males, drop nonlegal
  separate(male_mature_legal, sep = "_", into = c("mat_legal_count", "mat_legal_wt")) %>%
  separate(male_mature_nonlegal, sep = "_", into = c("mat_nonlegal_count", "mat_nonlegal_wt")) %>%
  mutate(male_mature_count = as.numeric(mat_legal_count) + as.numeric(mat_nonlegal_count),
         male_mature_wt = as.numeric(mat_legal_wt) + as.numeric(mat_nonlegal_wt)) %>%
  unite(male_mature_count, male_mature_wt, col = "male_mature", sep = "_") %>%
  unite(mat_legal_count, mat_legal_wt, col = "male_legal", sep = "_") %>%
  dplyr::select(-mat_nonlegal_count, -mat_nonlegal_wt) -> spec_summary

## combine the haul, strata, and specimen data 2004 - 2016
haul %>%
  # remove 2002 survey hauls
  filter(survey_year != 2002) %>%
  # select fields of interest
  dplyr::select(survey_year, haul, area_swept, stratum, stratum_area, subarea) %>%
  # join to specimen dat by haul
  full_join(spec_summary, by = c("survey_year", "haul")) %>%
  replace_na(list(male_immature = "0_0", male_mature = "0_0", 
                  male_legal = "0_0", female = "0_0")) %>%
  # return to long format and separate number of crab and weight
  pivot_longer(c(male_immature, male_legal, male_mature, female), names_to = "group") %>%
  separate(value, sep = "_", into = c("num_crab", "wt_crab_kg")) %>%
  mutate_at(c("num_crab", "wt_crab_kg"), as.numeric) -> catch_by_haul

## extrapolate catch by haul to abundance and biomass by stratum  
catch_by_haul %>%
  # get density in number and weight by stratum
  group_by(survey_year, stratum, subarea, stratum_area, group) %>%
  summarise(n_stations = n(),
            mean_density_num = mean(num_crab / area_swept),
            var_density_num = var(num_crab / area_swept),
            mean_density_wt = mean(wt_crab_kg / area_swept),
            var_density_wt = var(wt_crab_kg / area_swept)) %>%
  # get abundance and biomass within stratum
  mutate(abundance = stratum_area * mean_density_num,
         var_abundance = stratum_area^2 * var_density_num / n_stations,
         biomass = stratum_area * mean_density_wt,
         var_biomass = stratum_area^2 * var_density_wt / n_stations) %>%
  # compute weights for summing abundance and biomass for each subarea
  group_by(survey_year, subarea, group) %>%
  mutate(subarea_area = sum(stratum_area),
         w_subarea = stratum_area / subarea_area) %>%
  # compute a weighted sum of abundance and biomass within subareas
  summarise(abundance = sum(abundance * w_subarea),
            var_abundance = sum(var_abundance * w_subarea^2),
            cv_abund = sqrt(var_abundance) / abundance,
            biomass = sum(biomass * w_subarea),
            var_biomass = sum(var_biomass * w_subarea^2),
            cv_biomass = sqrt(var_biomass) / biomass) -> subarea_est
## save output
write_csv(subarea_est, "./PIGKC/output/survey_estimates_by_subarea.csv")

## sum estimators across subareas 2 - 4 to get totals for re-model
subarea_est %>%
  #filter(subarea %in% 2:4) %>%
  group_by(survey_year, group) %>%
  summarise(abundance = sum(abundance),
            se_abundance = sqrt(sum(var_abundance)),
            biomass = sum(biomass),
            se_biomass = sqrt(sum(var_biomass))) %>%
  # convert variances into a CV
  mutate(cv_abund = se_abundance / abundance,
         cv_biomass = se_biomass / biomass) -> pi_est

## export mature male estimates for subareas 2 - 4
pi_est %>%
  filter(group == "male_mature") %>%
  dplyr::select(-group) %>%
  write_csv("./PIGKC/output/nmfs_slope_subareas_2_3_4_mature_male_timeseries.csv")
  
## export random effects model input data file
### extract data
#### model years
pi_est %>%
  filter(group == "male_mature") %>%
  pull(survey_year) -> yrs
#### starting year
start <- min(yrs)
#### ending year
end <- 2020 
#### number of estimates
n <- nrow(filter(est, group == "male_mature"))
#### biomass estimates (in metric tons)
pi_est %>%
  filter(group == "male_mature") %>%
  pull(biomass) / 1000 -> biomass
#### cv of biomass estimates
pi_est %>%
  filter(group == "male_mature") %>%
  pull(cv_biomass) -> cv

### compile input file
rbind(c(start, "#Start year of model", rep("", n - 2)),
      c(end, "#End year of model", rep("", n - 2)),
      c(n, "#number of survey estimates", rep("", n - 2)),
      c("#Years of survey", rep("", n - 1)),
      c(yrs),
      c("#Biomass estimates mature males, all subareas", rep("", n - 1)),
      c(round(biomass, 2)),
      c("#Coefficients of variation for biomass estimates", rep("", n - 1)),
      c(round(cv, 2))) %>%
  write.table(., paste0("./PIGKC/model/", YEAR,"/mature_males_all_subareas/re.dat"), 
              quote = F, row.names = F, col.names = F)








  


  

  
  
  
  
  
