# notes ----
## Adjust AFSC EBS Slope Survey Area to PI District
## author: Tyler Jackson
## tyler.jackson@alaska.gov
## last updated: 2020/4/2

# load ----
source("./PIGKC/code/adfg_map_functions.R")

# compute are of polygon from a dataframe of each anchor point
## x - data frame 
f_get_area <- function(x){
  x %>%
    dplyr::select(long, lat) %>%
    coordinates(.) %>%
    Polygon(.) %>%
    list(.) %>%
    Polygons(., 1) %>%
    list(.) %>%
    SpatialPolygons(.) %>%
    rgeos::gArea()
}

# data ----

haul <- read_csv("./PIGKC/data/nmfs_slope_haul_2002_2016.csv")
names(haul) <- c("record_count", "load_date", "survey", "survey_year", "vessel", "cruise",
                 "haul", "haul_join", "stratum", "stratum_inpfc_area", "stratum_regulatory_name",
                 "stratum_description", "haul_type", "performance", "start_time", "duration", 
                 "distance_fished", "net_width", "net_measured", "net_height", "start_lat", 
                 "start_lon", "end_lat", "end_lon", "station_id", "gear_depth", "bottom_depth", 
                 "bottom_type", "surface_temp", "gear_temp", "wire_length", "gear", "asscessories", 
                 "subsample", "cruise_join", "audit_join", "statisfactory_performance", 
                 "performance_note")

## subarea polygons
subarea_1 <- f_shp_prep("./PIGKC/data/maps/bssa1", "bssa1")[[1]]
subarea_2 <- f_shp_prep("./PIGKC/data/maps/bssa2", "bssa2")[[1]]
subarea_3 <- f_shp_prep("./PIGKC/data/maps/bssa3", "bssa3")[[1]]
subarea_4 <- f_shp_prep("./PIGKC/data/maps/bssa4", "bssa4")[[1]]
subarea_5 <- f_shp_prep("./PIGKC/data/maps/bssa5", "bssa5")[[1]]

## Pribilof Island district boundary
f_shp_prep("./PIGKC/data/maps/pribilof_district", "Pribilof_District_Boundary") %>%
  # transform projection to match survey area
  f_transform_crs(to = survey_area[[2]]) %>%
  # save data object
  magrittr::extract2(1) -> pi_district

## survey strata
strata <- read_csv("./PIGKC/data/nmfs_slope_strata.csv")
## change names of strata data
names(strata) <- c("record_count", "load_date", "survey", "stratum", "stratum_area", "perimeter", 
                   "startum_inpfc_area", "min_depth", "max_depth", "description", "area_code",
                   "depth_code", "area_depth_code", "regulatory_area_name", "stratum_type")

## tibble to match polygon group with strata
tibble(group = factor(0.1:9.1),
       stratum = sort(rep(1:5, 2))) -> add_stratum


# compute whole stratum area ----
# subarea 5
subarea_5 %>%
  group_by(group) %>%
  nest() %>%
  mutate(area = purrr::map_dbl(data, f_get_area),
         subarea = 5) %>%
  dplyr::select(-data) %>%
  left_join(add_stratum, by = "group") %>%
  mutate(stratum = stratum + subarea * 10) -> whole_5

# subarea 4
subarea_4 %>%
  group_by(group) %>%
  nest() %>%
  mutate(area = purrr::map_dbl(data, f_get_area),
         subarea = 4) %>%
  dplyr::select(-data) %>%
  left_join(add_stratum, by = "group") %>%
  mutate(stratum = stratum + subarea * 10) -> whole_4

# subarea 1
subarea_1 %>%
  group_by(group) %>%
  nest() %>%
  mutate(area = purrr::map_dbl(data, f_get_area),
         subarea = 1) %>%
  dplyr::select(-data) %>%
  left_join(add_stratum, by = "group") %>%
  mutate(stratum = stratum + subarea * 10) -> whole_1


# compute area in PI district ----

# subarea 5
subarea_5 %>%
  filter(lat <= 58.65) %>%
  group_by(group) %>%
  nest() %>%
  mutate(area_pi = purrr::map_dbl(data, f_get_area),
         subarea = 5) -> pi_5
  
# subarea 4
subarea_4 %>%
  filter(lat <= 58.65) %>%
  group_by(group) %>%
  nest() %>%
  mutate(area_pi = purrr::map_dbl(data, f_get_area),
         subarea = 4) -> pi_4  
  
# subarea 1
subarea_1 %>%
  filter(lat >= 54.6 & long <= -168) %>%
  group_by(group) %>%
  nest() %>%
  mutate(area_pi = purrr::map_dbl(data, f_get_area),
         subarea = 1) -> pi_1

# combine cut polygon dataframes
bind_rows(pi_5, pi_4, pi_1) %>%
  dplyr::select(-data) -> pi

# bind cut areas polygon data
bind_rows(pi_5, pi_4, pi_1) %>%
  unnest(data) %>%
  # add uncut subarea polygon data
  bind_rows(subarea_2) %>%
  bind_rows(subarea_3) %>%
  ggplot()+
  geom_polygon(aes(long, lat, group = B5_, fill = factor(B5_)), show.legend = F)+
  geom_polygon(data = pi_district, aes(long, lat, group = group), fill = NA, color = "black")



# join areas and compute new area by stratum ----

bind_rows(whole_5, whole_4, whole_1) %>%
  full_join(pi, by = c("subarea", "group")) %>%
  group_by(subarea, stratum) %>%
  summarise(prop_in_prib_dist = sum(area_pi) / sum(area)) %>%
  ungroup() %>%
  dplyr::select(stratum, prop_in_prib_dist) %>%
  right_join(strata, by = "stratum") %>%
  slice(-1:-6) %>%
  # add proportion in PI for subareas 2, 3, and 6
  mutate(prop_in_prib_dist = ifelse(stratum %in% 21:35, 1, 
                             ifelse(stratum %in% 61:65, 0, prop_in_prib_dist)),
         stratum_area_pi = prop_in_prib_dist * stratum_area) %>%
  write_csv("./PIGKC/data/strata_area_prib_district.csv")
  

