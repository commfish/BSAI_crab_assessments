# notes ----

## Analysis of GKC NMFS slope survey catch
## 'design based' abundance and biomass estimates for random effects model input
## author: Tyler Jackson
## tyler.jackson@alaska.gov
## last updated: 2020/3/23

# load ----

library(tidyverse)
library(magrittr)
library(FNGr); theme_set(theme_sleek())

## source R scripts
source("./PIGKC/code/clean_nmfs_specimen_data.R")
source("./PIGKC/code/adfg_map_functions.R")

## global options
### assessment year
YEAR <- 2020
### custom color/fill pallete
cb_palette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", 
                "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# data ----

## haul data (2002, 2004, 2008, 2010, 2012, 2016)
haul <- read_csv("./PIGKC/data/nmfs_slope_haul_2002_2016.csv")

## stratum
strata <- read_csv("./PIGKC/data/strata_area_prib_district.csv")

## specimen data (from clean_nmfs_specimen_data.R)
str(spec_0416)

## 2002 catch data
#catch_02 <- read_csv("./PIGKC/data/nmfs_slope_gkc_catch_2002.csv")

## map data
### land
ak <- maps::map("world", region=c('russia', 'usa:alaska', 'canada'), fill=T, plot=F) %>%
  broom::tidy()
### survey area
survey_area <- f_shp_prep("./PIGKC/data/maps/bssa1to6", "bssa1to6")
survey_area[[1]] %>%
  # add subarea factor and save data object
  mutate(subarea = case_when(B5_ %in% 76:105 ~ 1,
                             B5_ %in% 62:75 ~ 2,
                             B5_ %in% 44:61 ~ 3,
                             B5_ %in% 30:43 ~ 4,
                             B5_ %in% 16:29 ~ 5,
                             B5_ %in% 3:15 ~ 6),
         subarea = factor(subarea)) -> survey_poly
### Pribilof Island district boundary
f_shp_prep("./PIGKC/data/maps/pribilof_district", "Pribilof_District_Boundary") %>%
  # transform projection to match survey area
  f_transform_crs(to = survey_area[[2]]) %>%
  # save data object
  magrittr::extract2(1) -> pi_district


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
names(strata) <- c("stratum", "prop_in_prib_dist", "record_count", "load_date", "survey", "stratum_area", "perimeter", 
                   "startum_inpfc_area", "min_depth", "max_depth", "description", "area_code",
                   "depth_code", "area_depth_code", "regulatory_area_name", "stratum_type", "stratum_area_pi")

## combine the haul, starta, and specimen data 2004 - 2016
haul %>%
  # remove a few pointless fields
  dplyr::select(-record_count, -load_date, -haul_join) %>%
  # join to stratum area
  left_join(strata %>%
              dplyr::select(survey, stratum, stratum_area, stratum_area_pi),
            by = c("survey", "stratum")) %>%
  # extract subarea and area_swept
  mutate(subarea = as.numeric(substring(stratum, 1, 1)),
         area_swept = distance_fished * (net_width / 1000)) -> haul

## remove 2016, haul 5 in specimen data. Not include in haul data.
## 2016 tech memo reports 175 successful tows. There are 175 hauls in 2016 data excluding haul 5
## remove crabs with anomalous sizes (999 - missing??, 485 mm)
spec_0416 %>%
  filter(!(survey_year == 2016 & haul == 5),
         length <= 200) -> spec_0416

# size comps ----

## custom function to plot size comp by survey year
f_size_comp <- function(data, yr) {
  data %>%
    # remove any sampling_factor = 0 rows (no catch)
    # remove any unknown or hermaphrodite sex crabs
    filter(sampling_factor != 0,
           survey_year == yr,
           sex != 3) %>%
    left_join(haul %>%
                dplyr::select(survey_year, haul, subarea)) %>%
    ggplot()+
    geom_histogram(aes(x = length, weight = sampling_factor), 
                   binwidth = 5, color = "grey20", fill = cb_palette[2])+
    facet_grid(rows = vars(subarea), cols = vars(survey_year), scales = "free_y")+
    labs(x = "Carapace length (mm)", y = "Number of crab") -> x
  ggsave(paste0("./PIGKC/figures/size_comps_", yr, ".png"), 
         plot = x, height = 6, width = 4, units = "in")
  x
}
## size comps by year and subarea
c(2004, 2008, 2010, 2012, 2016) %>%
  purrr::map(~f_size_comp(data = spec_0416, yr = .))


# catch by haul by sex/size group (specimen data) ----

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
  # compute haul mid lat and lon
  mutate(lon = (start_lon + end_lon) / 2,
         lat = (start_lat + end_lat) / 2) %>%
  # select fields of interest
  dplyr::select(survey_year, haul, lon, lat, area_swept, stratum, stratum_area, stratum_area_pi, subarea) %>%
  # join to specimen dat by haul
  full_join(spec_summary, by = c("survey_year", "haul")) %>%
  replace_na(list(male_immature = "0_0", male_mature = "0_0", 
                  male_legal = "0_0", female = "0_0")) %>%
  # return to long format and separate number of crab and weight
  pivot_longer(c(male_immature, male_legal, male_mature, female), names_to = "group") %>%
  separate(value, sep = "_", into = c("num_crab", "wt_crab_kg")) %>%
  mutate_at(c("num_crab", "wt_crab_kg"), as.numeric) -> catch_by_haul

# ## map of cpue by year, haul
# catch_by_haul %>%
#   filter(group == "male_mature") %>%
#   mutate(wt_crab_kg = ifelse(wt_crab_kg == 0, NA, wt_crab_kg)) %>%
#   ggplot()+
#   geom_polygon(data = ak, 
#                aes(x = long, y = lat, group = group),
#                fill = "grey90", color = "grey90")+
#   geom_polygon(data = survey_poly, 
#                aes(x = long, y = lat, group = group, fill = factor(subarea)), 
#                alpha = 0.5, show.legend = F)+
#   scale_fill_manual(values = cb_palette[2:7])+
#   geom_polygon(data = pi_district, 
#                aes(x = long, y = lat, group = group), color = "black", fill = NA)+
#   geom_point(aes(x = lon, y = lat, size = wt_crab_kg / area_swept), 
#              alpha = 0.5)+
#   labs(x = "Longitude", y = "Latitude", size = "CPUE (kg / sq km)")+
#   facet_wrap(~survey_year)+
#   coord_quickmap(xlim = c(-180.5, -166), ylim = c(53, 62))+
#   theme(panel.background = element_rect(fill = "grey70"),
#         legend.position = "bottom") -> x
# ggsave("./PIGKC/figures/survey_cpue_wt_map.png", plot = x, height = 6, width = 8, units = "in")


# abundance and biomass estimates, scenario 2020a ----
## MMB 2008 - 2016, unweighted, subareas 2-4
  
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
         var_biomass = stratum_area^2 * var_density_wt / n_stations) -> stratum_est

## compute an unweighted sum of abundance and biomass within subareas
stratum_est %>%
  group_by(survey_year, subarea, group) %>%
  mutate(subarea_area = sum(stratum_area)) %>% 
  summarise(abundance = sum(abundance),
            var_abundance = sum(var_abundance),
            cv_abund = sqrt(var_abundance) / abundance,
            biomass = sum(biomass),
            var_biomass = sum(var_biomass),
            cv_biomass = sqrt(var_biomass) / biomass) -> subarea_est_unweighted

## save output
subarea_est_unweighted %>%
  filter(survey_year != 2004) %>%
write_csv("./PIGKC/output/survey_estimates_by_subarea_unweighted.csv")

## extract re-model inputs
subarea_est_unweighted %>%
  filter(subarea %in% c(2:4),
         survey_year != 2004) %>%
  group_by(survey_year, group) %>%
  summarise(abundance = sum(abundance),
            se_abundance = sqrt(sum(var_abundance)),
            biomass = sum(biomass),
            se_biomass = sqrt(sum(var_biomass))) %>%
  # convert variances into a CV
  mutate(cv_abund = se_abundance / abundance,
         cv_biomass = se_biomass / biomass) -> pi_est

## export random effects model input data file
### extract data
#### model years
pi_est %>%
  filter(group == "male_mature") %>%
  pull(survey_year) -> yrs
#### starting year
start <- min(yrs)
#### ending year
end <- 2022 
#### number of estimates
n <- nrow(filter(pi_est, group == "male_mature"))
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
      c("#Unweighted biomass estimates mature males, subarea 2-4", rep("", n - 1)),
      c(round(biomass, 2)),
      c("#Coefficients of variation for biomass estimates", rep("", n - 1)),
      c(round(cv, 2))) %>%
  write.table(., paste0("./PIGKC/model/", YEAR,"/2020a/re.dat"), 
              quote = F, row.names = F, col.names = F)

# abundance and biomass estimates, scenario 2020b ----
## MMB 2008 - 2016, unweighted, survey area inside PI district

## extrapolate catch by haul to abundance and biomass by stratum  
catch_by_haul %>%
  # remove hauls not in PI district
  rename(x = lon, y = lat) %>%
  mutate(in_pi = splancs::inout(pts = .,
                                poly = pi_district %>%
                                  dplyr::select(long, lat) %>%
                                  rename(x = long, y = lat))) %>%
  rename(lon = x, lat = y) %>%
  filter(in_pi == T) %>%
  # remove subarea from stratum
  mutate(stratum = substring(stratum, 2, 2)) %>%
  # get density in number and weight by stratum
  group_by(survey_year, stratum, group) %>%
  summarise(n_stations = n(),
            mean_density_num = mean(num_crab / area_swept),
            var_density_num = var(num_crab / area_swept),
            mean_density_wt = mean(wt_crab_kg / area_swept),
            var_density_wt = var(wt_crab_kg / area_swept)) %>%
  # join to stratm area
  left_join(strata %>%
              filter(stratum %in% 11:55) %>%
              mutate(stratum = substring(stratum, 2, 2)) %>%
              group_by(stratum) %>%
              summarise(stratum_area_pi = sum(stratum_area_pi)),
            by = "stratum") %>%
  # get abundance and biomass within stratum
  mutate(abundance = stratum_area_pi * mean_density_num,
         var_abundance = stratum_area_pi^2 * var_density_num / n_stations,
         biomass = stratum_area_pi * mean_density_wt,
         var_biomass = stratum_area_pi^2 * var_density_wt / n_stations) -> stratum_est

## compute an unweighted sum of abundance and biomass within survey area of PI district
stratum_est %>%
  group_by(survey_year, group) %>%
  summarise(n_stations = sum(n_stations),
            abundance = sum(abundance),
            var_abundance = sum(var_abundance),
            cv_abund = sqrt(var_abundance) / abundance,
            biomass = sum(biomass),
            var_biomass = sum(var_biomass),
            cv_biomass = sqrt(var_biomass) / biomass) -> subarea_est_unweighted_in_pi

## save output
subarea_est_unweighted_in_pi %>%
  filter(survey_year != 2004) %>%
  write_csv("./PIGKC/output/survey_estimates_by_subarea_in_pi_district_unweighted_.csv")

## extract re-model inputs
subarea_est_unweighted_in_pi %>%
  filter(survey_year != 2004)-> pi_est

## export random effects model input data file
### extract data
#### model years
pi_est %>%
  filter(group == "male_mature") %>%
  pull(survey_year) -> yrs
#### starting year
start <- min(yrs)
#### ending year
end <- 2022 
#### number of estimates
n <- nrow(filter(pi_est, group == "male_mature"))
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
      c("#Unweighted biomass estimates mature males, survey area inside PI District", rep("", n - 1)),
      c(round(biomass, 2)),
      c("#Coefficients of variation for biomass estimates", rep("", n - 1)),
      c(round(cv, 2))) %>%
  write.table(., paste0("./PIGKC/model/", YEAR,"/2020b/re.dat"), 
              quote = F, row.names = F, col.names = F)


# abundance and biomass estimates, scenario 2020c ----
## MMB 2008 - 2016, weighted, subareas 2-4

## extrapolate catch by haul to abundance and biomass by stratum  
catch_by_haul %>%
  # remain only subareas 2 - 4
  filter(subarea %in% 2:4) %>%
  # remove subarea from stratum and combine area
  mutate(stratum = substring(stratum, 2, 2)) %>%
  dplyr::select(-stratum_area, -stratum_area_pi) %>%
  left_join(strata %>%
              filter(stratum %in% 21:45) %>%
              mutate(stratum = substring(stratum, 2, 2)) %>%
              group_by(stratum) %>%
              summarise(stratum_area = sum(stratum_area)),
            by = "stratum") %>%
  # get density in number and weight by stratum
  group_by(survey_year, stratum, stratum_area, group) %>%
  summarise(n_stations = n(),
            mean_density_num = mean(num_crab / area_swept),
            var_density_num = var(num_crab / area_swept),
            mean_density_wt = mean(wt_crab_kg / area_swept),
            var_density_wt = var(wt_crab_kg / area_swept)) %>%
  # get denisty estimates for the survey area
  group_by(survey_year, group) %>%
  mutate(w = stratum_area / sum(stratum_area)) %>%
  summarise(survey_area = sum(stratum_area),
            mean_density_num = weighted.mean(mean_density_num, w = stratum_area),
            var_density_num = sum(var_density_num / n_stations * w^2),
            mean_density_wt = weighted.mean(mean_density_wt, w = stratum_area),
            var_density_wt = sum(var_density_wt / n_stations * w^2)) %>%
  # get survey abundance and biomass estimates
  mutate(abundance = survey_area * mean_density_num,
         var_abundance = survey_area^2 * var_density_num,
         cv_abund = sqrt(var_abundance) / abundance,
         biomass = survey_area * mean_density_wt,
         var_biomass = survey_area^2 * var_density_wt,
         cv_biomass = sqrt(var_biomass) / biomass) %>%
  dplyr::select(survey_year, group, abundance, cv_abund, biomass, cv_biomass) -> survey_est_weighted


## save output
survey_est_weighted %>%
  filter(survey_year != 2004) %>%
  write_csv("./PIGKC/output/survey_estimates_weighted_subareas_2_4.csv")

## extract re-model inputs
survey_est_weighted %>%
  filter(survey_year != 2004)-> pi_est

## export random effects model input data file
### extract data
#### model years
pi_est %>%
  filter(group == "male_mature") %>%
  pull(survey_year) -> yrs
#### starting year
start <- min(yrs)
#### ending year
end <- 2022
#### number of estimates
n <- nrow(filter(pi_est, group == "male_mature"))
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
      c("#Weighted biomass estimates mature males, subareas 2 - 4", rep("", n - 1)),
      c(round(biomass, 2)),
      c("#Coefficients of variation for biomass estimates", rep("", n - 1)),
      c(round(cv, 2))) %>%
  write.table(., paste0("./PIGKC/model/", YEAR,"/2020c/re.dat"), 
              quote = F, row.names = F, col.names = F)



# abundance and biomass estimates, scenario 2020d ----
## MMB 2008 - 2016, weighted, survey area inside PI district

catch_by_haul %>%
  # remove hauls not in PI district
  rename(x = lon, y = lat) %>%
  mutate(in_pi = splancs::inout(pts = .,
                                poly = pi_district %>%
                                  dplyr::select(long, lat) %>%
                                  rename(x = long, y = lat))) %>%
  rename(lon = x, lat = y) %>%
  filter(in_pi == T) %>%
  # remove subarea from stratum
  mutate(stratum = substring(stratum, 2, 2)) %>%
  # get density in number and weight by stratum
  group_by(survey_year, stratum, group) %>%
  summarise(n_stations = n(),
            mean_density_num = mean(num_crab / area_swept),
            var_density_num = var(num_crab / area_swept),
            mean_density_wt = mean(wt_crab_kg / area_swept),
            var_density_wt = var(wt_crab_kg / area_swept)) %>%
  # join to stratm area
  left_join(strata %>%
              filter(stratum %in% 11:55) %>%
              mutate(stratum = substring(stratum, 2, 2)) %>%
              group_by(stratum) %>%
              summarise(stratum_area_pi = sum(stratum_area_pi)),
            by = "stratum") %>%
  # get denisty estimates for the survey area
  group_by(survey_year, group) %>%
  mutate(w = stratum_area_pi / sum(stratum_area_pi)) %>%
  summarise(survey_area = sum(stratum_area_pi),
            mean_density_num = weighted.mean(mean_density_num, w = stratum_area_pi),
            var_density_num = sum(var_density_num / n_stations * w^2),
            mean_density_wt = weighted.mean(mean_density_wt, w = stratum_area_pi),
            var_density_wt = sum(var_density_wt / n_stations * w^2)) %>%
  # get survey abundance and biomass estimates
  mutate(abundance = survey_area * mean_density_num,
         var_abundance = survey_area^2 * var_density_num,
         cv_abund = sqrt(var_abundance) / abundance,
         biomass = survey_area * mean_density_wt,
         var_biomass = survey_area^2 * var_density_wt,
         cv_biomass = sqrt(var_biomass) / biomass) %>%
  dplyr::select(survey_year, group, abundance, cv_abund, biomass, cv_biomass) -> survey_est_weighted


## save output
survey_est_weighted %>%
  filter(survey_year != 2004) %>%
  write_csv("./PIGKC/output/survey_estimates_by_in_pi_district_weighted_.csv")

## extract re-model inputs
survey_est_weighted %>%
  filter(survey_year != 2004)-> pi_est

## export random effects model input data file
### extract data
#### model years
pi_est %>%
  filter(group == "male_mature") %>%
  pull(survey_year) -> yrs
#### starting year
start <- min(yrs)
#### ending year
end <- 2022
#### number of estimates
n <- nrow(filter(pi_est, group == "male_mature"))
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
      c("#Weighted biomass estimates mature males, survey area inside PI District", rep("", n - 1)),
      c(round(biomass, 2)),
      c("#Coefficients of variation for biomass estimates", rep("", n - 1)),
      c(round(cv, 2))) %>%
  write.table(., paste0("./PIGKC/model/", YEAR,"/2020d/re.dat"), 
              quote = F, row.names = F, col.names = F)






















# extract re-model inputs ----

## sum estimators across subareas to get totals for re-model
subarea_est %>%
  filter(subarea %in% c(6)) %>%
  
  #temporarily remove 2004
  filter(survey_year != 2004) %>%
  
  group_by(survey_year, group) %>%
  summarise(abundance = sum(abundance),
            se_abundance = sqrt(sum(var_abundance)),
            biomass = sum(biomass),
            se_biomass = sqrt(sum(var_biomass))) %>%
  # convert variances into a CV
  mutate(cv_abund = se_abundance / abundance,
         cv_biomass = se_biomass / biomass) -> pi_est

## export mature male estimates for subareas 
# pi_est %>%
#   filter(group == "male_mature") %>%
#   dplyr::select(-group) %>%
#   write_csv("./PIGKC/output/nmfs_slope_subarea_2_4_mature_male_timeseries.csv")
  
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
n <- nrow(filter(pi_est, group == "male_mature"))
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
      c("#Biomass estimates mature males, subarea 6", rep("", n - 1)),
      c(round(biomass, 2)),
      c("#Coefficients of variation for biomass estimates", rep("", n - 1)),
      c(round(cv, 2))) %>%
  write.table(., paste0("./PIGKC/model/", YEAR,"/mature_males_subarea_6_b/re.dat"), 
              quote = F, row.names = F, col.names = F)








  


  

  
## compute an weighted sum of abundance and biomass within subareas
stratum_est %>%
  group_by(survey_year, subarea, group) %>%
  # compute weighted mean density by subarea
  summarise(area_density_num = weighted.mean(mean_density_num, w = stratum_area),
            subarea_area = sum(stratum_area)) %>%
  mutate(abundance = subarea_area * area_density_num )




# compute a weighted sum of abundance and biomass within subareas
summarise(abundance = sum(abundance * w_subarea),
          var_abundance = sum(var_abundance * w_subarea^2),
          cv_abund = sqrt(var_abundance) / abundance,
          biomass = sum(biomass * w_subarea),
          var_biomass = sum(var_biomass * w_subarea^2),
          cv_biomass = sqrt(var_biomass) / biomass) -> subarea_est
## save output
write_csv(subarea_est, "./PIGKC/output/survey_estimates_by_subarea.csv")

## plots 
### biomass by year and subarea
subarea_est %>%
  ungroup() %>%
  mutate(subarea = factor(subarea),
         group = case_when(group == "female" ~ "Female",
                           group == "male_immature" ~ "Immature Males", 
                           group == "male_mature" ~ "Mature Males",
                           group == "male_legal" ~ "Legal Males"),
         group = factor(group, levels = c("Female", "Immature Males", 
                                          "Mature Males", "Legal Males"))) %>%
  ggplot()+
  geom_point(aes(x = survey_year, y = biomass / 1000, color = subarea))+
  geom_line(aes(x = survey_year, y = biomass / 1000, color = subarea, group = subarea))+
  scale_color_manual(values = cb_palette[2:7])+
  labs(x = NULL, y = "Biomass (t)", color = "Subarea")+
  facet_wrap(~group, ncol = 1, scales = "free_y")+
  theme(legend.position = "bottom") -> x
ggsave("./PIGKC/figures/survey_biomass_subarea.png", plot = x, height = 6, width = 4, units = "in")





  
  
  

