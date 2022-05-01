# notes ----
## compare race ebs trawl survey and adfg pot survey indices
## tyler jackson
## tyler.jackson@alaska.gov

# load ----
library(tidyverse)
library(sp); library(spdplyr)
library(sf)
library(viridis)

# load and clean race survey data ----

## race ebs trawl data
race_spec_raw <- read.csv("./SMBKC/data/survey_comparison/race_trawl_survey/race_ebs_specimen.csv", skip = 5)
race_bkc_strata <- read.csv("./SMBKC/data/survey_comparison/race_trawl_survey/race_ebs_strata.csv")

## race data cleanup
### specimen and haul data
race_spec_raw %>%
  # rename cols
  rename_all(tolower) %>%
  rename(year = akfin_survey_year,
         station = gis_station) %>%
  # remove bristol bay retows
  filter(haul_type != 17) -> race_spec
### strata data
race_bkc_strata %>%
  # clean names
  rename_all(tolower) %>%
  rename(station = station_id, 
         lat = latitude, lon = longitude) %>%
  # filter for st matthew district
  filter(grepl("St. Matthew", district)) %>%
  # compute station area in nmi2
  group_by(district) %>%
  add_count() %>%
  ungroup %>%
  mutate(station_area_nmi2 = total_area_sq_nm / n) %>%
  # select columns of interest
  dplyr::select(district, station, lat, lon, station_area_nmi2) -> race_strata

# load and clean adfg survey data ----

## log data from 2007 - present
adfg_log_data <- purrr::map(list.files("./SMBKC/data/survey_comparison/adfg_pot_survey", pattern = "Log", full.names = T),
                            read.csv) %>%
  do.call("rbind", .) %>%
  as_tibble() 

## log data from 1995 - 2004
early_adfg_log_data <- read.csv("./SMBKC/data/survey_comparison/adfg_pot_survey/pre2007_pilothouselog.csv") %>%
  as_tibble() 

## crab data from 2007 - present
adfg_crab_data <- purrr::map(list.files("./SMBKC/data/survey_comparison/adfg_pot_survey", pattern = "Crab", full.names = T),
                             read.csv) %>%
  do.call("rbind", .) %>%
  as_tibble() 

## crab data from 1995 - 2004
early_adfg_crab_data <- read.csv("./SMBKC/data/survey_comparison/adfg_pot_survey/pre2007_crabdump.csv") %>%
  as_tibble() 

## adfg station midpoints
adfg_stations <- read_csv("./SMBKC/data/survey_comparison/adfg_pot_survey/adfg_pot_survey_station_midpoints.csv") %>%
  rename_all(tolower)

## combine log data
early_adfg_log_data %>%
  # fix names in older data
  # convert dms to decimal degrees
  rename_all(tolower) %>%
  mutate(longitude = -wlondeg - wlonmin / 60,
         latitude = nlatdeg + nlatmin / 60) %>%
  dplyr::select(-nlatdeg, -nlatmin, -wlondeg, -wlonmin) %>%
  rename(rec_id = rec.id, gear_type = gear.type, set_date = setdate, set_time = settime, 
         depth = depth..fm., bottom_type = bottom.type, lift_date = pickdate, lift_time = picktime,
         gear_perf = gearperformance) %>%
  # join to newer data
  bind_rows(adfg_log_data %>%
              rename_all(tolower)) %>%
  # add survey year
  mutate(year = lubridate::year(lubridate::mdy(set_date))) -> adfg_log

## combine crab dump data
early_adfg_crab_data %>%
  # fix names in old data
  rename_all(tolower) %>%
  rename(rec_id = rec.id, lift_date = sampdate, sampfrac = sampfractn, dcode = species, condition = crabcondition, 
         clutch = clutchfullness, eggdev = eggdevelopment, clutchcon  = clutchcondition, tag_series = tagseries,
         tagnum = floyno) %>%
  # formate data
  mutate(lift_date = lubridate::mdy(lift_date)) %>%
  # join to newer data
  bind_rows(adfg_crab_data %>%
              rename_all(tolower) %>%
              mutate(lift_date = lubridate::ymd(lift_date))) %>%
  # add survey year
  mutate(year = lubridate::year(lift_date)) -> adfg_crab



# spatial data ----

### crs used for northen ebs
north_ebs_crs <- "+proj=aea +lat_1=57 +lat_2=63 +lat_0=59 +lon_0=-170 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"
### race ebs trawl survey grid
st_read("./SMBKC/data/map_data/EBSgrid.shp", quiet = T) %>%
  st_transform(crs = "+proj=longlat +datum=WGS84 +no_defs") -> ebs_grid
### high resolution land data
raster::getData("GADM", country = c("USA"), level = 1, path = "./SMBKC/data/map_data") %>%
  filter(NAME_1 == "Alaska") %>%
  st_as_sf() %>%
  st_transform(crs = north_ebs_crs) -> ak

# determine station overlap ----

## only intersecting the 96 incommon stations
adfg_stations %>%
  filter(incommon96 == 1) %>%
  st_as_sf(., coords = c("midpoint_longitude", "midpoint_latitude"), crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_intersects(ebs_grid) %>%  # subset ebs_grid for just R-24 
  unlist %>%
  unique %>%
  ebs_grid$STATION_ID[.] -> race_stations_intersecting_incommon96
## intersecting any pot survey station
adfg_stations %>%
  st_as_sf(., coords = c("midpoint_longitude", "midpoint_latitude"), crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_intersects(ebs_grid) %>%
  unlist %>%
  unique %>%
  ebs_grid$STATION_ID[.] -> race_stations_intersecting_any

# compute race per station cpue by stage ----

# calculated weight alpha / beta
a = 0.000502
b = 3.107158

race_spec %>%
  # specify size stages
  filter(sex == 1, 
         #!is.na(stage)) %>%
         length >= 90) %>%
  # compute number and weight of crab
  group_by(year, station, area_swept) %>%
  summarise(ncrab = sum(sampling_factor, na.rm = T),
            # cannot use calcuated weight field, inconsistent with other data sources
            wt_kg = sum((a * floor(length)^b) / 1000 * 1, na.rm = T)) %>%
  ungroup %>%
  # compute cpue per nmi2
  mutate(cpue_cnt = ncrab / area_swept,
         cpue_t = wt_kg / 1000 / area_swept) %>%
  # join to hauls that did not catch crab
  right_join(race_spec %>%
                  distinct(year, station, area_swept),
             by = c("year", "station", "area_swept")) %>%
  replace_na(list(ncrab = 0, wt_kg = 0, cpue_cnt = 0, cpue_t = 0)) %>%
  # join to stratum information
  left_join(race_strata, by = "station") %>%
  # remove data with missing district (not in st matts)
  filter(!is.na(district)) -> race_station_cpue

# total race survey biomass in st matt district ----

# total race survey biomass
race_station_cpue %>%
  # compute biomass
  # first by stratum (district - ie single stations vs corner stations)
  group_by(year, district) %>%
  summarise(cpue = mean(cpue_t),
            var_cpue = var(cpue_t) / n(),
            area = sum(station_area_nmi2),
            dist_biomass = cpue * area,
            dist_var = var_cpue * area^2) %>%
  # second sum strata, compute instead of var
  group_by(year) %>%
  summarise(biomass = sum(dist_biomass),
            se = sqrt(sum(dist_var))) %>%
  ungroup %>%
  # compute lognormal ci
  mutate(l95 = biomass * exp(qnorm(0.025) * log(1 + (se / biomass)^2)),
         u95 = biomass * exp(qnorm(0.975) * log(1 + (se / biomass)^2))) -> race_biomass_ts

# timeseries of proportion of race biomass in footprint of adfg 96 incommon stations ----

race_station_cpue %>%
  # remove stations not overlapping with adfg survey
  filter(station %in% race_stations_intersecting_incommon96) %>%
  # compute biomass
  # first by stratum (district - ie single stations vs corner stations)
  group_by(year, district) %>%
  summarise(cpue = mean(cpue_t),
            var_cpue = var(cpue_t) / n(),
            area = sum(station_area_nmi2),
            dist_biomass = cpue * area,
            dist_var = var_cpue * area^2) %>%
  # second sum strata, compute instead of var
  group_by(year) %>%
  summarise(biomass = sum(dist_biomass),
            se = sqrt(sum(dist_var))) %>%
  ungroup %>%
  # compute lognormal ci
  mutate(l95 = biomass * exp(qnorm(0.025) * log(1 + (se / biomass)^2)),
         u95 = biomass * exp(qnorm(0.975) * log(1 + (se / biomass)^2))) -> race_biomass_ts_in_adfg_incommon

# campare biomass index inside of adfg survey footprint to total
race_biomass_ts %>%
  dplyr::select(year, biomass) %>%
  rename(tot_biomass = biomass) %>%
  left_join(race_biomass_ts_in_adfg_incommon %>%
              dplyr::select(year, biomass) %>%
              rename(frac_biomass = biomass)) %>%
  # compute proportion of biomass
  mutate(prop = frac_biomass / tot_biomass) -> race_prop_incommon96

# plot of race survey biomass in stations within footprint of adfg 96 incommon stations
race_prop_incommon96 %>%
  group_by(year) %>%
  summarise(prop = sum(frac_biomass) / sum(tot_biomass)) %>%
  ggplot()+
  geom_point(aes(x = year, y = prop))+
  geom_line(aes(x = year, y = prop))+
  labs(x = NULL, y = "Prop Total NMFS Biomass", color = NULL)+
  expand_limits(y=0) + #scale_y_continuous(expand = c(0,0)) +
  theme_bw() -> x
##  !!TJ fraction of race within ADF&G? correct?
ggsave("./SMBKC/figures/prop_tot_biomass_in_96incommon_foot.png", plot = x, height = 3, width = 6, units = "in")  


# timeseries of proportion of race biomass in R-24 ----

race_station_cpue %>%
  filter(station == "R-24") %>%
  # scale to station abundance
  mutate(biomass_r24 = cpue_t * station_area_nmi2) %>%
  dplyr::select(year, biomass_r24) %>%
  # join to total biomass data
  left_join(race_biomass_ts %>%
              dplyr::select(year, biomass)) %>%
  # compute proportion
  mutate(prop = biomass_r24 / biomass) %>% 
  
  ggplot()+
  geom_point(aes(x = year, y = prop))+
  geom_line(aes(x = year, y = prop))+
  labs(x = NULL, y = "Prop Total NMFS Biomass in R24", color = NULL)+
  expand_limits(y=0) + #scale_y_continuous(expand = c(0,0)) +
  theme_bw() -> x

ggsave("./SMBKC/figures/prop_tot_biomass_in_r24.png", plot = x, height = 3, width = 6, units = "in")  


# timeseries of proportion of race biomass in pot survey or NOT in R-24 ----

race_prop_incommon96 %>%
  group_by(year) %>%
  summarise(prop_adfg = sum(frac_biomass) / sum(tot_biomass)) %>%
  # join to proportion of race biomass not in r24
  left_join(race_station_cpue %>%
              filter(station == "R-24") %>%
              # scale to station abundance
              mutate(biomass_r24 = cpue_t * station_area_nmi2) %>%
              dplyr::select(year, biomass_r24) %>%
              # join to total biomass data
              left_join(race_biomass_ts %>%
                          dplyr::select(year, biomass)) %>%
              # compute proportion
              mutate(prop_not_r24 = 1 - (biomass_r24 / biomass)) %>%
              dplyr::select(year, prop_not_r24)) %>%
  # pivot to long format
  pivot_longer(2:3, names_to = "index", values_to = "value") %>%
  # fix labels
  mutate(index = case_when(index == "prop_not_r24" ~ "RACE Grid (Excluding R-24)",
                           index == "prop_adfg" ~ "RACE Grid within Core Pot Survey")) %>%
  
  ggplot()+
  geom_point(aes(x = year, y = value, color = index), alpha = 0.5)+
  geom_line(aes(x = year, y = value, color = index), alpha = 0.5)+
  labs(x = NULL, y = "Proportion of Total Biomass", color = NULL)+
  scale_color_viridis_d(direction = 1)+
  theme_bw()+
  expand_limits(y=0) + #scale_y_continuous(expand = c(0,0)) +
  theme(legend.position = "bottom") -> x

ggsave("./SMBKC/figures/prop_tot_biomass_in_adfg_or_not_r24.png", plot = x, height = 3, width = 6, units = "in") 

# timerseries of adfg pot survey total cpue ----

adfg_crab %>%
  # filter for only bkc males of stages 1:3
  filter(dcode == 3,
         sex == 1, 
         size >= 90) %>%
  # join to number of pots fished at each station
  left_join(adfg_log %>%
              group_by(year, station) %>%
              count(name = "n_pots")) %>%
  # fill in sample fraction for 2004 (ie it was missing so all crab were measured)
  mutate(sampfrac = ifelse(year == 2004, 1, sampfrac)) %>%
  # compute cpue per station
  group_by(year, station, n_pots) %>%
  summarise(cpue_cnt = sum(sampfrac, na.rm = T) / mean(n_pots)) %>%
  ungroup %>%
  # add zero stations and haul information
  right_join(adfg_log %>%
               # remove strata, masked by station data with some discrepancies
               dplyr::select(-stratum) %>%
               left_join(adfg_stations) %>%
               distinct(year, station, incommon96, midpoint_longitude, midpoint_latitude),
             by = c("year", "station")) %>%
  replace_na(list(cpue_cnt = 0)) %>%
  # filter for core stations used th in the assessment
  filter(incommon96 == 1) %>%
  # compute cpue 
  group_by(year) %>%
  summarise(cpue = mean(cpue_cnt),
            se = var(cpue_cnt) / n()) %>%
  # compute lognormal ci
  mutate(l95 = cpue * exp(qnorm(0.025) * log(1 + (se / cpue)^2)),
         u95 = cpue * exp(qnorm(0.975) * log(1 + (se / cpue)^2))) -> adfg_cpue_ts

# survey cpue comparison ----

bind_rows(
  # pot survey
adfg_cpue_ts %>%
  dplyr::select(year, cpue) %>%
  rename(value = cpue) %>%
  mutate(data = "ADF&G Pot Survey"),
  # full race survey
race_biomass_ts %>%
  dplyr::select(year, biomass) %>%
  rename(value = biomass) %>%
  mutate(data = "RACE Total Biomass"),
  # race survey in pot survey footprint
race_biomass_ts_in_adfg_incommon %>%
  dplyr::select(year, biomass) %>%
  rename(value = biomass) %>%
  mutate(data = "RACE Total Biomass within Core Pot Survey")
) %>%
  # mean center the data, se = 1
  group_by(data) %>%
  mutate(scaled_index = scale(value)) %>%
  ggplot()+
  geom_point(aes(x = year, y = scaled_index, color = data))+
  geom_line(aes(x = year, y = scaled_index, color = data))+
  scale_color_viridis_d(direction = -1)+
  labs(x = NULL, y = "Scaled Index", color = NULL)+
  theme_bw()+
  #expand_limits(y=0) + #scale_y_continuous(expand = c(0,0)) +
  #theme(legend.position = "bottom") -> x
  theme(legend.position = c(1, 1), legend.justification = c(1, 1),
        legend.background = element_blank(), legend.key = element_blank()) -> x

ggsave("./SMBKC/figures/survey_ts_comparison.png", plot = x, height = 3, width = 6, units = "in")  

# map figures ----

## ebs grid 
ebs_grid %>%
  # add data to determine if its in the adfg survey footprint
  mutate(in_adfg = case_when(STATION_ID %in% race_stations_intersecting_any[!(race_stations_intersecting_any %in% race_stations_intersecting_incommon96)] ~ "Any Pot Stations",
                             STATION_ID %in% race_stations_intersecting_incommon96 ~ "96 Incommon Stations",
                             !(STATION_ID %in% race_stations_intersecting_any) ~ "Outside Pot Survey")) %>%
  # filter for st matthew district stations
  filter(STATION_ID %in% (race_bkc_strata %>%
                            filter(grepl("St. Matt", DISTRICT)) %>%
                            pull(STATION_ID))) %>%
  st_transform(crs = north_ebs_crs) -> map_grid

## adfg pot stations
adfg_stations %>%
  rename_all(tolower) %>%
  st_as_sf(., coords = c("midpoint_longitude", "midpoint_latitude"), crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_transform(crs = north_ebs_crs) -> map_pot_stations

## zoom to coords
st_sfc(st_point(c(-176, 58)), st_point(c(-170.5, 61.5)), crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_transform(., crs = st_crs(north_ebs_crs)) %>%
  st_coordinates() -> map_zoom

## plot map of survey stations
ggplot()+
  geom_sf(data = map_grid, aes(fill = in_adfg), color = 1)+
  geom_sf(data = ak, fill = "grey60", size = 0.4)+
  geom_sf(data = map_pot_stations, aes(shape = factor(incommon96)))+
  coord_sf(xlim = map_zoom[,"X"], ylim = map_zoom[,"Y"])+
  scale_shape_manual(values = c(1, 16), labels = c("Pot Station", "Core Pot Station"))+
  scale_fill_manual(values = c("skyblue1", "skyblue3", "grey80"),
                    labels = c("Overlapping Core Pot Survey", "Overlapping Pot Survey", "Non-overlapping Pot Survey"))+
  labs(fill = NULL, shape = NULL)+
  theme_bw()+
  theme(legend.position = c(0,0), legend.justification = c(0,0), legend.box = "horizontal",
        legend.box.background = element_rect(color = 1),
        legend.background = element_blank()) -> x

ggsave("./SMBKC/figures/pot_and_trawl_stations_map.png", plot = x, height = 6, width = 5, units = "in")  



