# notes ----

## sdmTMB for AIGKC data
## tyler jackson  
## tyler.jackson@alaska.gov

# load ----

library(tidyverse)
library(sdmTMB)
library(sdmTMBextra)
library(DHARMa)
library(sf)
library(gmacsr); theme_set(theme_sleek())
library(ggeffects)

# data ----

## land boundary
land <- geodata::gadm("United States", level = 1, path = "./AIGKC/data/maps")
# eag 
st_as_sf(land) %>%
  st_shift_longitude() %>% 
  st_crop(c(xmin = 186, ymin = 51, xmax = 195.27, ymax = 55.5)) %>%
  st_transform(32602) %>%
  # utm units to km
  mutate(geometry = geometry / 1000) -> eag_land
# wag 
st_as_sf(land) %>%
  st_shift_longitude() %>% 
  st_crop(c(xmin = 170, ymin = 51, xmax = 186, ymax = 55.5)) %>%
  st_transform(32601) %>%
  # utm units to km
  mutate(geometry = geometry / 1000) -> wag_land

## observer pots
obs_raw <- read.csv("./AIGKC/data/observer/2025/linked_potsum_dump.csv", na.strings = -9)
## make core data
obs_raw %>%
  # remove missing key data
  filter(!is.na(crab_year),
         crab_year >= 1995,
         !is.na(sample_date),
         !is.na(permit_holder),
         !is.na(adfg),
         !is.na(soaktime),
         !is.na(depth),
         !is.na(gearcode),
         gearcode != "NA" ) %>%
  # filter for vessels that fished in more than 3 seasons
  group_by(adfg, subdistrict) %>%
  filter(length(unique(crab_year)) > 3) %>%
  ungroup %>%
  # remove odd gear types, combine others
  # remove 4, 13, 8 because sample sizes are low
  filter(!(gearcode %in% c(1:3, 14:23, 80, 81))) %>%
  mutate(gearcode = ifelse(gearcode == 9, 5,
                    ifelse(gearcode == 10, 6, 
                    ifelse(gearcode == 11, 7, gearcode))),
         soaktime = ifelse(crab_year %in% 1995:1996, soaktime * 24, soaktime)) %>%
  # biotwine is okay
  filter(biotwine_ok != "N") %>% ungroup %>%
  transmute(year = as.numeric(crab_year), subdistrict, lat = latitude, lon = as.numeric(longitude), depth, soaktime, spn, trip, sample_date, gearcode = factor(gearcode), adfg = factor(adfg), permit_holder, sublegal, tot_legal, cpue = tot_legal) %>%
  filter(!is.na(lon), !is.na(lat)) -> obs

# eag data 
obs %>% 
  filter(subdistrict == "EAG",
         lon < -164.7,
         lat < 53.65, lat > 51.75) %>%
  # remove area specific gear codes and quantiles
  # filter for middle quantiles of soaktime and depth by year
  filter(!gearcode %in% c(4, 13, 8)) %>%
  filter(soaktime > quantile(soaktime, 0.025), 
         soaktime < quantile(soaktime, 0.975),
         depth > quantile(depth, 0.01), 
         depth < quantile(depth, 0.99)) %>% ungroup %>%
  # convert to UTM projection
  st_as_sf(coords = c("lon", "lat"), crs = st_crs(land)) %>%
  st_shift_longitude() %>%
  st_transform(32602) %>% 
  # utm units to km 
  mutate(geometry = geometry / 1000) %>%
  mutate(lon = unlist(map(geometry,1)),
         lat = unlist(map(geometry,2))) %>%
  st_drop_geometry() %>%
  # add year as a factor
  mutate(fyear = factor(year)) -> eag

# wag data 
obs %>% 
  filter(subdistrict == "WAG",
         lat < 56) %>%
  # remove area specific gear codes and quantiles
  # filter for middle quantiles of soaktime and depth by year
  filter(!gearcode %in% c(4, 13, 25)) %>%
  filter(soaktime > quantile(soaktime, 0.025), 
         soaktime < quantile(soaktime, 0.975),
         depth > quantile(depth, 0.01), 
         depth < quantile(depth, 0.99)) %>% ungroup %>%
  # convert to UTM projection
  st_as_sf(coords = c("lon", "lat"), crs = st_crs(land)) %>%
  st_shift_longitude() %>%
  st_transform(32601) %>% 
  # utm units to km 
  mutate(geometry = geometry / 1000) %>%
  mutate(lon = unlist(map(geometry,1)),
         lat = unlist(map(geometry,2))) %>%
  st_drop_geometry() %>%
  # add year as a factor
  mutate(fyear = factor(year)) -> wag

# maps of fishing location by gear and vessel -----

# eag by vessel
eag %>%
  st_as_sf(coords = c("lon", "lat"), crs = st_crs(eag_land)) %>%
  ggplot()+
  geom_sf(data = eag_land)+
  geom_sf(aes(color = adfg), alpha = 0.5)+
  facet_wrap(~year, ncol = 4)+
  geom_text_npc(aes(npcx = "left", npcy = 0.9, label = year),
                check_overlap = T, size = 3)+
  labs(color = "Vessel")+
  theme(panel.spacing.x = unit(0, "lines"),
        panel.spacing.y = unit(0, "lines"),
        panel.border = element_rect(color = "grey30", fill = NA),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        legend.key.size = unit(0.25, "in")) -> x
ggsave("./AIGKC/figures/cpue_std/2025/jan/eag_vessel_map.png", plot = x, height = 8, width = 8, units = "in")


# eag by gear
eag %>%
  st_as_sf(coords = c("lon", "lat"), crs = st_crs(eag_land)) %>%
  ggplot()+
  geom_sf(data = eag_land)+
  geom_sf(aes(color = gearcode), alpha = 0.5)+
  facet_wrap(~year, ncol = 4)+
  geom_text_npc(aes(npcx = "left", npcy = 0.9, label = year),
                check_overlap = T, size = 3)+
  labs(color = "Vessel")+
  theme(panel.spacing.x = unit(0, "lines"),
        panel.spacing.y = unit(0, "lines"),
        panel.border = element_rect(color = "grey30", fill = NA),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        legend.key.size = unit(0.25, "in")) -> x
ggsave("./AIGKC/figures/cpue_std/2025/jan/eag_gear_map.png", plot = x, height = 8, width = 8, units = "in")


# wag by vessel
wag %>%
  st_as_sf(coords = c("lon", "lat"), crs = st_crs(wag_land)) %>%
  ggplot()+
  geom_sf(data = wag_land)+
  geom_sf(aes(color = adfg), alpha = 0.5)+
  facet_wrap(~year, ncol = 4)+
  geom_text_npc(aes(npcx = "left", npcy = 0.9, label = year),
                check_overlap = T, size = 3)+
  labs(color = "Vessel")+
  theme(panel.spacing.x = unit(0, "lines"),
        panel.spacing.y = unit(0, "lines"),
        panel.border = element_rect(color = "grey30", fill = NA),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        legend.key.size = unit(0.25, "in")) -> x
ggsave("./AIGKC/figures/cpue_std/2025/jan/wag_vessel_map.png", plot = x, height = 8, width = 8, units = "in")


# wag by gear
wag %>%
  st_as_sf(coords = c("lon", "lat"), crs = st_crs(wag_land)) %>%
  ggplot()+
  geom_sf(data = wag_land)+
  geom_sf(aes(color = gearcode), alpha = 0.5)+
  facet_wrap(~year, ncol = 4)+
  geom_text_npc(aes(npcx = "left", npcy = 0.9, label = year),
                check_overlap = T, size = 3)+
  labs(color = "Vessel")+
  theme(panel.spacing.x = unit(0, "lines"),
        panel.spacing.y = unit(0, "lines"),
        panel.border = element_rect(color = "grey30", fill = NA),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        legend.key.size = unit(0.25, "in")) -> x
ggsave("./AIGKC/figures/cpue_std/2025/jan/wag_gear_map.png", plot = x, height = 8, width = 8, units = "in")


# trend with soak time and space ----

# soak time by year
bind_rows(eag, wag) %>%
  ggplot()+
  geom_boxplot(aes(x = factor(year), y = soaktime), size = 0.25)+
  labs(x = NULL, y = "Soaktime (hrs)")+
  scale_x_discrete(labels = yraxis$labels, breaks = yraxis$breaks)+
  scale_y_continuous(labels = scales::comma)+
  facet_wrap(~subdistrict, ncol = 1, scales = "free") -> x
ggsave("./AIGKC/figures/cpue_std/2025/jan/soaktime_year.png", plot = x, height = 6, width = 5, units = "in")

eag %>%
  ggplot()+
  geom_point(aes(x = soaktime, y = cpue))+
  geom_smooth(aes(x = soaktime, y = cpue), method = "gam", se = F)+
  facet_wrap(~year, ncol = 4)+
  labs(x = "Soak Time (hrs)", y = "CPUE")+
  scale_y_continuous(breaks = seq(0, 300, 100), labels = seq(0, 300, 100))+
  geom_text_npc(aes(npcx = "right", npcy = 0.9, label = year),
                check_overlap = T, size = 3)+
  theme(panel.spacing.x = unit(0, "lines"),
        panel.spacing.y = unit(0, "lines"),
        panel.border = element_rect(color = "grey30", fill = NA),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        panel.background = element_blank(),
        legend.position = "bottom",
        legend.key.size = unit(0.25, "in")) -> x
ggsave("./AIGKC/figures/cpue_std/2025/jan/eag_soaktime_cpue_year.png", plot = x, height = 6, width = 5, units = "in")

wag %>%
  ggplot()+
  geom_point(aes(x = soaktime, y = cpue))+
  geom_smooth(aes(x = soaktime, y = cpue), method = "gam", se = F)+
  facet_wrap(~year, ncol = 4)+
  labs(x = "Soak Time (hrs)", y = "CPUE")+
  scale_y_continuous(breaks = seq(0, 300, 100), labels = seq(0, 300, 100))+
  geom_text_npc(aes(npcx = "right", npcy = 0.9, label = year),
                check_overlap = T, size = 3)+
  theme(panel.spacing.x = unit(0, "lines"),
        panel.spacing.y = unit(0, "lines"),
        panel.border = element_rect(color = "grey30", fill = NA),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        panel.background = element_blank(),
        legend.position = "bottom",
        legend.key.size = unit(0.25, "in")) -> x
ggsave("./AIGKC/figures/cpue_std/2025/jan/wag_soaktime_cpue_year.png", plot = x, height = 6, width = 5, units = "in")


# average soak time vs number of vessels
bind_rows(eag, wag) %>% 
  group_by(year, subdistrict) %>%
  summarise(vessels = length(unique(adfg)),
            soak = median(soaktime),
            pots = n()) %>%
  ggplot()+
  geom_point(aes(x = vessels, y = soak, color = year))+
  facet_wrap(~subdistrict, ncol = 1)+
  labs(x = "Number of Vessels", y = "Median Soaktime (hrs)", color = NULL) -> x
ggsave("./AIGKC/figures/cpue_std/2025/jan/soaktime_nvessels_year.png", plot = x, height = 6, width = 5, units = "in")


bind_rows(eag, wag) %>% 
  group_by(year, subdistrict) %>%
  summarise(vessels = length(unique(adfg)),
            soak = median(soaktime),
            pots = n()) %>%
  ggplot()+
  geom_point(aes(x = vessels, y = pots, color = year))+
  facet_wrap(~subdistrict, ncol = 1)+
  labs(x = "Number of Vessels", y = "Number of Pots", color = NULL)

# mesh ----

## eag
init_eag_mesh <- make_mesh(eag, xy_cols = c("lon", "lat"), n_knots = 150, type = "kmeans")
eag_mesh <- sdmTMBextra::add_barrier_mesh(
  spde_obj = init_eag_mesh,
  barrier_sf = eag_land,
  range_fraction = 0.1,
  plot = FALSE
)

# plot eag mesh
{as_tibble(eag_mesh$mesh$graph$tv) %>%
  mutate(tv = row_number()) %>%
  pivot_longer(1:3, names_to = "corner", values_to = "V") %>%
  left_join(eag_mesh$mesh$loc %>%
              as_tibble() %>% 
              mutate(V = row_number()) %>%
              transmute(V, lon = V1, lat = V2)) %>%
  transmute(tv, V, lon, lat) %>%
  nest_by(tv, .keep = F) %>% ungroup %>%
  mutate(data = purrr::map(data, function(data){
    data %>% bind_rows(data[1,]) 
  })) %>%
  unnest(data) %>%
  st_as_sf(coords = c("lon", "lat"), crs = st_crs(eag_land)) %>%
  group_by(tv) %>%  
  summarise(do_union = FALSE) %>% 
  st_cast("POLYGON")  %>%  
  st_cast("MULTIPOLYGON") -> eag_mesh_sf
  eag_mesh_sf %>%
  ggplot()+
  geom_point(data = eag, aes(x = lon, y = lat), color = "grey60", size = 0.2)+
  geom_sf(fill = NA, color = "grey70")+
  geom_sf(data = st_intersection(eag_land, st_as_sfc(st_bbox(tmp))), fill = "grey80", color = "black")+
  labs(x = NULL, y = NULL, title = "EAG")+
  theme(axis.text = element_blank(),
        #axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5)) -> eag_mesh_p
  }

## wag
init_wag_mesh <- make_mesh(wag, xy_cols = c("lon", "lat"), n_knots = 150, type = "kmeans")
wag_mesh <- sdmTMBextra::add_barrier_mesh(
  spde_obj = init_wag_mesh,
  barrier_sf = wag_land,
  range_fraction = 0.1,
  plot = FALSE
)

# plot wag mesh
as_tibble(wag_mesh$mesh$graph$tv) %>%
  mutate(tv = row_number()) %>%
  pivot_longer(1:3, names_to = "corner", values_to = "V") %>%
  left_join(wag_mesh$mesh$loc %>%
              as_tibble() %>% 
              mutate(V = row_number()) %>%
              transmute(V, lon = V1, lat = V2)) %>%
  transmute(tv, V, lon, lat) %>%
  nest_by(tv, .keep = F) %>% ungroup %>%
  mutate(data = purrr::map(data, function(data){
    data %>% bind_rows(data[1,]) 
  })) %>%
  unnest(data) %>%
  st_as_sf(coords = c("lon", "lat"), crs = st_crs(wag_land)) %>%
  group_by(tv) %>%  
  summarise(do_union = FALSE) %>% 
  st_cast("POLYGON")  %>%  
  st_cast("MULTIPOLYGON") -> wag_mesh_sf
  ggplot()+
  geom_point(data = wag, aes(x = lon, y = lat), color = "grey60", size = 0.2)+
  geom_sf(fill = NA, color = "grey70")+
  geom_sf(data = wag_land, fill = "grey80", color = "black")+
  labs(x = NULL, y = NULL, title = "WAG")+
  theme(axis.text = element_blank(),
        #axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5)) -> wag_mesh_p

ggsave("./AIGKC/figures/cpue_std/2025/may/mesh.png", plot = eag_mesh_p / wag_mesh_p, height = 6, width = 6, units = "in")

# eag fit ----

## Null
# eag_fit_null <- sdmTMB(
#   cpue ~ 0 + fyear,
#   data = eag,
#   mesh = eag_mesh,
#   time = "year",
#   spatial = "on",
#   spatiotemporal = "iid",
#   family = tweedie(link = "log")
# )
# saveRDS(eag_fit_null, "./AIGKC/output/cpue_std/2025/may/eag_fit_null.RDS")
eag_fit_null <- readRDS("./AIGKC/output/cpue_std/2025/may/eag_fit_null.RDS")

## Null + adfg
# eag_fit_1 <- sdmTMB(
#   cpue ~ 0 + fyear + (1|adfg),
#   data = eag,
#   mesh = eag_mesh,
#   time = "year",
#   spatial = "on",
#   spatiotemporal = "iid",
#   family = tweedie(link = "log")
# )
# saveRDS(eag_fit_1, "./AIGKC/output/cpue_std/2025/may/eag_fit_1.RDS")
eag_fit_1 <- readRDS("./AIGKC/output/cpue_std/2025/may/eag_fit_1.RDS")

## Null + adfg + gearcode
# eag_fit_2 <- sdmTMB(
#   cpue ~ 0 + fyear + gearcode + (1|adfg),
#   data = eag,
#   mesh = eag_mesh,
#   time = "year",
#   spatial = "on",
#   spatiotemporal = "iid",
#   family = tweedie(link = "log")
# )
# saveRDS(eag_fit_2, "./AIGKC/output/cpue_std/2025/may/eag_fit_2.RDS")
eag_fit_2 <- readRDS("./AIGKC/output/cpue_std/2025/may/eag_fit_2.RDS")

## Null + adfg + gearcode + soaktime
# eag_fit_3 <- sdmTMB(
#   cpue ~ 0 + fyear + s(soaktime) + gearcode + (1|adfg),
#   data = eag,
#   mesh = eag_mesh,
#   time = "year",
#   spatial = "on",
#   spatiotemporal = "iid",
#   family = tweedie(link = "log")
# )
# saveRDS(eag_fit_3, "./AIGKC/output/cpue_std/2025/may/eag_fit_3.RDS")
eag_fit_3 <- readRDS("./AIGKC/output/cpue_std/2025/may/eag_fit_3.RDS")

## Null + adfg + gearcode + soaktime:year
# eag_fit_3_soakyr <- sdmTMB(
#   cpue ~ 0 + fyear + (1 | adfg) + s(soaktime, by = fyear) + gearcode,
#   data = eag,
#   mesh = eag_mesh,
#   time = "year",
#   spatial = "on",
#   spatiotemporal = "iid",
#   family = tweedie(link = "log")
# )
# saveRDS(eag_fit_3_soakyr, "./AIGKC/output/cpue_std/2025/may/eag_fit_3_soakyr.RDS")
eag_fit_3_soakyr <- readRDS("./AIGKC/output/cpue_std/2025/may/eag_fit_3_soakyr.RDS")

## Null + adfg + gearcode + soaktime + depth
# eag_fit_full <- sdmTMB(
#   cpue ~ 0 + fyear + s(depth) + s(soaktime) + gearcode + (1|adfg),
#   data = eag,
#   mesh = eag_mesh,
#   time = "year",
#   spatial = "on",
#   spatiotemporal = "iid",
#   family = tweedie(link = "log")
# )
# saveRDS(eag_fit_full, "./AIGKC/output/cpue_std/2025/may/eag_fit_full.RDS")
eag_fit_full <- readRDS("./AIGKC/output/cpue_std/2025/may/eag_fit_full.RDS")


## Null + adfg + gearcode + soaktime + depth GLMM
# eag_fit_full_glmm <- sdmTMB(
#   cpue ~ 0 + fyear + s(depth) + s(soaktime) + gearcode + (1|adfg),
#   data = eag %>% mutate(fyear = factor(year)),
#   mesh = eag_mesh,
#   spatial = "off",
#   spatiotemporal = "off",
#   time = "year",
#   family = tweedie(link = "log")
# )
# saveRDS(eag_fit_full_glmm, "./AIGKC/output/cpue_std/2025/may/eag_fit_full_glmm.RDS")
eag_fit_full_glmm <- readRDS("./AIGKC/output/cpue_std/2025/may/eag_fit_full_glmm.RDS")

## Null + adfg + gearcode + soaktime + depth
# eag_fit_full_soakyr <- sdmTMB(
#   cpue ~ 0 + fyear + (1 | adfg) + s(depth) + s(soaktime, by = fyear) + gearcode,
#   data = eag,
#   mesh = eag_mesh,
#   time = "year",
#   spatial = "on",
#   spatiotemporal = "iid",
#   family = tweedie(link = "log")
# )
# saveRDS(eag_fit_full_soakyr, "./AIGKC/output/cpue_std/2025/may/eag_fit_full_soakyr.RDS")
eag_fit_full_soakyr <- readRDS("./AIGKC/output/cpue_std/2025/may/eag_fit_full_soakyr.RDS")

sanity(eag_fit_full_soakyr)

# wag fit ----

## Null
# wag_fit_null <- sdmTMB(
#   cpue ~ 0 + as.factor(year),
#   data = wag,
#   mesh = wag_mesh,
#   time = "year",
#   spatial = "on",
#   spatiotemporal = "iid",
#   family = tweedie(link = "log")
# )
# saveRDS(wag_fit_null, "./AIGKC/output/cpue_std/2025/may/wag_fit_null.RDS")
wag_fit_null <- readRDS("./AIGKC/output/cpue_std/2025/may/wag_fit_null.RDS")

## Null + adfg
# wag_fit_1 <- sdmTMB(
#   cpue ~ 0 + fyear + (1 | adfg),
#   data = wag,
#   mesh = wag_mesh,
#   time = "year",
#   spatial = "on",
#   spatiotemporal = "iid",
#   family = tweedie(link = "log")
# )
# saveRDS(wag_fit_1, "./AIGKC/output/cpue_std/2025/may/wag_fit_1.RDS")
wag_fit_1 <- readRDS("./AIGKC/output/cpue_std/2025/may/wag_fit_1.RDS")

## Null + adfg + gearcode
# wag_fit_2 <- sdmTMB(
#   cpue ~ 0 + fyear + (1 | adfg) + gearcode,
#   data = wag,
#   mesh = wag_mesh,
#   time = "year",
#   spatial = "on",
#   spatiotemporal = "iid",
#   family = tweedie(link = "log")
# )
# saveRDS(wag_fit_2, "./AIGKC/output/cpue_std/2025/may/wag_fit_2.RDS")
wag_fit_2 <- readRDS("./AIGKC/output/cpue_std/2025/may/wag_fit_2.RDS")

## Null + adfg + gearcode + soaktime
# wag_fit_3 <- sdmTMB(
#   cpue ~ 0 + fyear + (1 | adfg) + s(soaktime) + gearcode,
#   data = wag,
#   mesh = wag_mesh,
#   time = "year",
#   spatial = "on",
#   spatiotemporal = "iid",
#   family = tweedie(link = "log")
# )
# saveRDS(wag_fit_3, "./AIGKC/output/cpue_std/2025/may/wag_fit_3.RDS")
wag_fit_3 <- readRDS("./AIGKC/output/cpue_std/2025/may/wag_fit_3.RDS")

## Null + adfg + gearcode + soaktime:year
# wag_fit_3_soakyr <- sdmTMB(
#   cpue ~ 0 + fyear + (1 | adfg) + s(soaktime, by = fyear) + gearcode,
#   data = wag,
#   mesh = wag_mesh,
#   time = "year",
#   spatial = "on",
#   spatiotemporal = "iid",
#   family = tweedie(link = "log")
# )
# saveRDS(wag_fit_3_soakyr, "./AIGKC/output/cpue_std/2025/may/wag_fit_3_soakyr.RDS")
wag_fit_3_soakyr <- readRDS("./AIGKC/output/cpue_std/2025/may/wag_fit_3_soakyr.RDS")


## Null + adfg + gearcode + soaktime + depth
# wag_fit_full <- sdmTMB(
#   cpue ~ 0 + fyear + (1 | adfg) + s(depth) + s(soaktime) + gearcode,
#   data = wag,
#   mesh = wag_mesh,
#   time = "year",
#   spatial = "on",
#   spatiotemporal = "iid",
#   family = tweedie(link = "log")
# )
# saveRDS(wag_fit_full, "./AIGKC/output/cpue_std/2025/may/wag_fit_full.RDS")
wag_fit_full <- readRDS("./AIGKC/output/cpue_std/2025/may/wag_fit_full.RDS")


## Null + adfg + gearcode + soaktime + depth GLMM
# wag_fit_full_glmm <- sdmTMB(
#   cpue ~ 0 + fyear + s(depth) + s(soaktime) + gearcode + (1|adfg),
#   data = wag,
#   mesh = wag_mesh,
#   spatial = "off",
#   spatiotemporal = "off",
#   time = "year",
#   family = tweedie(link = "log")
# )
# saveRDS(wag_fit_full_glmm, "./AIGKC/output/cpue_std/2025/may/wag_fit_full_glmm.RDS")
wag_fit_full_glmm <- readRDS("./AIGKC/output/cpue_std/2025/may/wag_fit_full_glmm.RDS")


## Null + adfg + gearcode + soaktime + depth
wag_fit_full_soakyr <- sdmTMB(
  cpue ~ 0 + fyear + (1 | adfg) + s(depth) + s(soaktime, by = fyear) + gearcode,
  data = wag,
  mesh = wag_mesh,
  time = "year",
  spatial = "on",
  spatiotemporal = "iid",
  family = tweedie(link = "log"),
  control = sdmTMBcontrol(nlminb_loops = 2)
)
saveRDS(wag_fit_full_soakyr, "./AIGKC/output/cpue_std/2025/may/wag_fit_full_soakyr.RDS")
# wag_fit_full_soakyr <- readRDS("./AIGKC/output/cpue_std/2025/may/wag_fit_full_soakyr.RDS")



# dharma residuals ----

sanity(eag_fit_full)

## eag
# full st model iid
eag_full_tw_sim <- simulate(eag_fit_full, nsim = 500, type = "mle-mvn")
dharma_eag_full <- dharma_residuals(eag_full_tw_sim, eag_fit_full, return_DHARMa = TRUE)
# make qq plot of simulatied residuals
qq <- patchwork::wrap_elements(panel = ~plotQQunif(dharma_eag_full, testUniformity = F, testOutliers = F, testDispersion = F, cex = 0.2), clip = F)
# plot residuals against ranked model predictions
rf <- patchwork::wrap_elements(panel = ~plotResiduals(dharma_eag_full, smoothScatter = F, cex = 0.2, pch = 16), clip = F)
ggsave("./AIGKC/figures/cpue_std/2025/may/dharma_eag_full.png", plot = (qq + rf), height = 4, width = 8, units = "in")

# full st model iid soakyr
eag_full_tw_sim_soakyr <- simulate(eag_fit_full_soakyr, nsim = 500, type = "mle-mvn")
dharma_eag_full_soakyr <- dharma_residuals(eag_full_tw_sim_soakyr, eag_fit_full_soakyr, return_DHARMa = TRUE)
# make qq plot of simulatied residuals
qq <- patchwork::wrap_elements(panel = ~plotQQunif(dharma_eag_full_soakyr, testUniformity = F, testOutliers = F, testDispersion = F, cex = 0.2), clip = F)
# plot residuals against ranked model predictions
rf <- patchwork::wrap_elements(panel = ~plotResiduals(dharma_eag_full_soakyr, smoothScatter = F, cex = 0.2, pch = 16), clip = F)
ggsave("./AIGKC/figures/cpue_std/2025/may/dharma_eag_full_soakyr.png", plot = (qq + rf), height = 4, width = 8, units = "in")

# full glmm
eag_full_glmm_tw_sim <- simulate(eag_fit_full_glmm, nsim = 500, type = "mle-mvn")
dharma_eag_full_glmm <- dharma_residuals(eag_full_glmm_tw_sim, eag_fit_full_glmm, return_DHARMa = TRUE)
# make qq plot of simulatied residuals
qq <- patchwork::wrap_elements(panel = ~plotQQunif(dharma_eag_full_glmm, testUniformity = F, testOutliers = F, testDispersion = F, cex = 0.2), clip = F)
# plot residuals against ranked model predictions
rf <- patchwork::wrap_elements(panel = ~plotResiduals(dharma_eag_full_glmm, smoothScatter = F, cex = 0.2, pch = 16), clip = F)
ggsave("./AIGKC/figures/cpue_std/2025/may/dharma_eag_full_glmm.png", plot = (qq + rf), height = 4, width = 8, units = "in")


sanity(wag_fit_full_soakyr)


## wag
# full st model iid
wag_full_tw_sim <- simulate(wag_fit_full, nsim = 500, type = "mle-mvn")
dharma_wag_full <- dharma_residuals(wag_full_tw_sim, wag_fit_full, return_DHARMa = TRUE)
# make qq plot of simulatied residuals
qq <- patchwork::wrap_elements(panel = ~plotQQunif(dharma_wag_full, testUniformity = F, testOutliers = F, testDispersion = F, cex = 0.2), clip = F)
# plot residuals against ranked model predictions
rf <- patchwork::wrap_elements(panel = ~plotResiduals(dharma_wag_full, smoothScatter = F, cex = 0.2, pch = 16), clip = F)
ggsave("./AIGKC/figures/cpue_std/2025/may/dharma_wag_full.png", plot = (qq + rf), height = 4, width = 8, units = "in")

# full st model iid with soakyr
# wag_full_soakyr_sim <- simulate(wag_fit_full_soakyr, nsim = 500, type = "mle-mvn")
# dharma_wag_full_soakyr <- dharma_residuals(wag_full_soakyr_sim, wag_fit_full_soakyr, return_DHARMa = TRUE)
# # make qq plot of simulatied residuals
# qq <- patchwork::wrap_elements(panel = ~plotQQunif(dharma_wag_full_soakyr, testUniformity = F, testOutliers = F, testDispersion = F, cex = 0.2), clip = F)
# # plot residuals against ranked model predictions
# rf <- patchwork::wrap_elements(panel = ~plotResiduals(dharma_wag_full_soakyr, smoothScatter = F, cex = 0.2, pch = 16), clip = F)
# ggsave("./AIGKC/figures/cpue_std/2025/may/dharma_wag_full_soakyr.png", plot = (qq + rf), height = 4, width = 8, units = "in")

# full glmm
wag_full_glmm_tw_sim <- simulate(wag_fit_full_glmm, nsim = 500, type = "mle-mvn")
dharma_wag_full_glmm <- dharma_residuals(wag_full_glmm_tw_sim, wag_fit_full_glmm, return_DHARMa = TRUE)
# make qq plot of simulatied residuals
qq <- patchwork::wrap_elements(panel = ~plotQQunif(dharma_wag_full_glmm, testUniformity = F, testOutliers = F, testDispersion = F, cex = 0.2), clip = F)
# plot residuals against ranked model predictions
rf <- patchwork::wrap_elements(panel = ~plotResiduals(dharma_wag_full_glmm, smoothScatter = F, cex = 0.2, pch = 16), clip = F)
ggsave("./AIGKC/figures/cpue_std/2025/may/dharma_wag_full_glmm.png", plot = (qq + rf), height = 4, width = 8, units = "in")


# marginal effects ----

## eag
# gearcode
visreg::visreg(eag_fit_full, xvar = "gearcode", plot = F)$fit %>% 
  mutate(gearcode = case_when(gearcode == 5 ~ "5x5",
                              gearcode == 6 ~ "6x6",
                              gearcode == 7 ~ "7x7",
                              gearcode == 8 ~ "8x8",
                              gearcode == 12 ~ "Round")) %>%
  ggplot()+
  geom_errorbar(aes(x = gearcode, ymin = visregLwr, ymax = visregUpr), width = 0)+
  geom_point(aes(x = gearcode, y = visregFit), fill = "black")+
  labs(x = "Pot Size", y = "CPUE")  -> p1
ggsave("./AIGKC/figures/cpue_std/2025/may/eag_full_gearcode_effect.png", plot = p1, height = 3, width = 4, units = "in")

# soaktime - conditional effect
visreg::visreg(eag_fit_full, xvar = "soaktime", plot = F)$fit %>%
  ggplot()+
  geom_ribbon(aes(x = soaktime/24, ymin = visregLwr, ymax = visregUpr), fill = "grey90")+
  geom_line(aes(x = soaktime/24, y = visregFit))+
  scale_x_continuous(labels = scales::comma)+
  labs(x = "Soak Time (days)", y = "f(Soak Time)") -> p2
ggsave("./AIGKC/figures/cpue_std/2025/may/eag_full_soaktime_effect.png", plot = p2, height = 3, width = 4, units = "in")

#depth - conditional effect
visreg::visreg(eag_fit_full, xvar = "depth", plot = F)$fit %>%
  ggplot()+
  geom_ribbon(aes(x = depth, ymin = visregLwr, ymax = visregUpr), fill = "grey90")+
  geom_line(aes(x = depth, y = visregFit))+
  scale_x_continuous(labels = scales::comma)+
  labs(x = "Depth (fa)", y = "f(Depth)") -> p3
ggsave("./AIGKC/figures/cpue_std/2025/may/eag_full_depth_effect.png", plot = p3, height = 3, width = 4, units = "in")

ggsave("./AIGKC/figures/cpue_std/2025/may/eag_full_covar_effects.png", plot = (p1 + p2) / (p3 + plot_spacer()), height = 8, width = 8, units = "in")

# soaktime by fyear - conditional effect
eag_soakyr_grid <- expand_grid(as_tibble(eag_mesh$mesh$loc[, 1:2]) %>% rename_all(~c("lon", "lat")) %>% dplyr::slice(1),
                               depth = mean(eag_fit_full_soakyr$data$depth),
                               soaktime = seq(min(eag_fit_full_soakyr$data$soaktime), max(eag_fit_full_soakyr$data$soaktime), by = 10),
                               adfg = factor(names(sort(-table(eag_fit_full_soakyr$data$adfg)))[1]),
                               gearcode = factor(names(sort(-table(eag_fit_full_soakyr$data$gearcode)))[1]),
                               eag_fit_full_soakyr$data %>%
                                 distinct(fyear, year))

eag_soakyr_pred <- predict(eag_fit_full_soakyr, newdata = eag_soakyr_grid, return_tmb_object = T, se_fit = T)
eag_soakyr_pred$data %>%
  ggplot()+
  geom_ribbon(aes(x = soaktime, ymin = est - 1.96*est_se, ymax = est + 1.96*est_se), fill = "grey90")+
  geom_line(aes(x = soaktime, y = est))+
  geom_text_npc(aes(npcx = "right", npcy = 0.9, label = year),
                check_overlap = T, size = 3)+
  scale_x_continuous(labels = scales::comma)+
  labs(x = "Soak Time (hr)", y = "f(Soak Time)")+
  facet_wrap(~fyear)+
  theme(panel.spacing.x = unit(0, "lines"),
        panel.spacing.y = unit(0, "lines"),
        panel.border = element_rect(color = "grey30", fill = NA),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        #axis.text = element_blank(),
        # axis.ticks = element_blank(),
        panel.background = element_blank(),
        legend.position = "bottom",
        legend.key.size = unit(0.25, "in")) -> x
ggsave("./AIGKC/figures/cpue_std/2025/may/eag_full_soakyr_marginal_effect.png", plot = x, height = 8, width = 8, units = "in")


## wag
# gearcode
visreg::visreg(wag_fit_full, xvar = "gearcode", plot = F)$fit %>% 
  mutate(gearcode = case_when(gearcode == 5 ~ "5x5",
                              gearcode == 6 ~ "6x6",
                              gearcode == 7 ~ "7x7",
                              gearcode == 8 ~ "8x8",
                              gearcode == 12 ~ "Round")) %>%
  ggplot()+
  geom_errorbar(aes(x = gearcode, ymin = visregLwr, ymax = visregUpr), width = 0)+
  geom_point(aes(x = gearcode, y = visregFit), fill = "black")+
  labs(x = "Pot Size", y = "CPUE")  -> p1
ggsave("./AIGKC/figures/cpue_std/2025/may/wag_full_gearcode_effect.png", plot = p1, height = 3, width = 4, units = "in")

# soaktime - conditional effect
visreg::visreg(wag_fit_full, xvar = "soaktime", plot = F)$fit %>%
  ggplot()+
  geom_ribbon(aes(x = soaktime / 24, ymin = visregLwr, ymax = visregUpr), fill = "grey90")+
  geom_line(aes(x = soaktime / 24, y = visregFit))+
  scale_x_continuous(labels = scales::comma)+
  labs(x = "Soak Time (days)", y = "f(Soak Time)") -> p2
ggsave("./AIGKC/figures/cpue_std/2025/may/wag_full_soaktime_effect.png", plot = p2, height = 3, width = 4, units = "in")

#depth - conditional effect
visreg::visreg(wag_fit_full, xvar = "depth", plot = F)$fit %>%
  ggplot()+
  geom_ribbon(aes(x = depth, ymin = visregLwr, ymax = visregUpr), fill = "grey90")+
  geom_line(aes(x = depth, y = visregFit))+
  scale_x_continuous(labels = scales::comma)+
  labs(x = "Depth (fa)", y = "f(Depth)") -> p3
ggsave("./AIGKC/figures/cpue_std/2025/may/wag_full_depth_effect.png", plot = p3, height = 3, width = 4, units = "in")

ggsave("./AIGKC/figures/cpue_std/2025/may/wag_full_covar_effects.png", plot = (p1 + p2) / (p3 + plot_spacer()), height = 8, width = 8, units = "in")


# soaktime by fyear - conditional effect
wag_soakyr_grid <- expand_grid(as_tibble(wag_mesh$mesh$loc[, 1:2]) %>% rename_all(~c("lon", "lat")) %>% dplyr::slice(1),
                               depth = mean(wag_fit_full_soakyr$data$depth),
                               soaktime = seq(min(wag_fit_full_soakyr$data$soaktime), max(wag_fit_full_soakyr$data$soaktime), by = 10),
                               adfg = factor(names(sort(-table(wag_fit_full_soakyr$data$adfg)))[1]),
                               gearcode = factor(names(sort(-table(wag_fit_full_soakyr$data$gearcode)))[1]),
                               wag_fit_full_soakyr$data %>%
                                 distinct(fyear, year))

wag_soakyr_pred <- predict(wag_fit_full_soakyr, newdata = wag_soakyr_grid, return_tmb_object = T, se_fit = T)
wag_soakyr_pred$data %>%
  ggplot()+
  geom_ribbon(aes(x = soaktime, ymin = est - 1.96*est_se, ymax = est + 1.96*est_se), fill = "grey90")+
  geom_line(aes(x = soaktime, y = est))+
  geom_text_npc(aes(npcx = "right", npcy = 0.9, label = year),
                check_overlap = T, size = 3)+
  scale_x_continuous(labels = scales::comma)+
  labs(x = "Soak Time (hr)", y = "f(Depth)")+
  facet_wrap(~fyear)+
  theme(panel.spacing.x = unit(0, "lines"),
        panel.spacing.y = unit(0, "lines"),
        panel.border = element_rect(color = "grey30", fill = NA),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        #axis.text = element_blank(),
       # axis.ticks = element_blank(),
        panel.background = element_blank(),
        legend.position = "bottom",
        legend.key.size = unit(0.25, "in")) -> x
ggsave("./AIGKC/figures/cpue_std/2025/may/wag_full_soakyr_marginal_effect.png", plot = x, height = 8, width = 8, units = "in")


# vessel ranef ----

# eag
tibble(adfg = sort(unique(eag$adfg)),
       ranef = unlist(lme4::ranef(eag_fit_full))) %>%
  left_join(eag %>% count(adfg) ) %>%
  arrange(n) %>%
  mutate(adfg = factor(adfg, levels = .$adfg)) %>%
  ggplot()+
  geom_hline(yintercept = 0, linetype = 2)+
  geom_point(aes(x = adfg, y = ranef), fill = "black")+
  labs(x = "Vessel (ADFG Number)", y = "CPUE")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) -> x
ggsave("./AIGKC/figures/cpue_std/2025/may/eag_full_adfg_ranef.png", plot = x, height = 3, width = 4, units = "in")

# wag
tibble(adfg = sort(unique(wag$adfg)),
       ranef = unlist(lme4::ranef(wag_fit_full))) %>%
  left_join(wag %>% count(adfg) ) %>%
  arrange(n) %>%
  mutate(adfg = factor(adfg, levels = .$adfg)) %>%
  ggplot()+
  geom_hline(yintercept = 0, linetype = 2)+
  geom_point(aes(x = adfg, y = ranef), fill = "black")+
  labs(x = "Vessel (ADFG Number)", y = "CPUE")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) -> x
ggsave("./AIGKC/figures/cpue_std/2025/may/wag_full_adfg_ranef.png", plot = x, height = 3, width = 4, units = "in")

tibble(adfg = sort(unique(wag$adfg)),
       ranef = unlist(lme4::ranef(wag_fit_full_soakyr))) %>%
  left_join(wag %>% count(adfg) ) %>%
  arrange(n) %>%
  mutate(adfg = factor(adfg, levels = .$adfg)) %>%
  ggplot()+
  geom_hline(yintercept = 0, linetype = 2)+
  geom_point(aes(x = adfg, y = ranef), fill = "black")+
  labs(x = "Vessel (ADFG Number)", y = "CPUE")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) -> x
ggsave("./AIGKC/figures/cpue_std/2025/may/wag_full_soakyr_adfg_ranef.png", plot = x, height = 3, width = 4, units = "in")



# eag make predictions ----

as_tibble(eag_mesh$mesh$loc[, 1:2]) %>% rename_all(~c("lon", "lat")) -> eag_mesh_latlon

## eag
# prediction grid for full  model
eag_full_grid <- expand_grid(lon = seq(min(eag_mesh_latlon$lon), max(eag_mesh_latlon$lon), length.out = 25),
                             lat = seq(min(eag_mesh_latlon$lat), max(eag_mesh_latlon$lat), length.out = 15),
                       depth = mean(eag_fit_full$data$depth),
                       soaktime = mean(eag_fit_full$data$soaktime),
                       eag_fit_full$data %>%
                         distinct(fyear,year, gearcode, adfg))
# null predictions
eag_null_pred <- predict(eag_fit_null, 
                     newdata = eag_full_grid %>% distinct(lon, lat, fyear, year), 
                     return_tmb_object = T)

# eag1 predictions
eag1_pred <- predict(eag_fit_1, 
                     newdata = eag_full_grid %>% distinct(lon, lat, fyear, year, adfg), 
                     return_tmb_object = T)

# eag2 predictions
eag2_pred <- predict(eag_fit_2, 
                     newdata = eag_full_grid %>% distinct(lon, lat, fyear, year, adfg, gearcode), 
                     return_tmb_object = T)

# eag3 predictions
eag3_pred <- predict(eag_fit_3, 
                     newdata = eag_full_grid %>% distinct(lon, lat, fyear, year, adfg, gearcode, soaktime), 
                     return_tmb_object = T)

# full model predictions                        
eag_full_pred <- predict(eag_fit_full, newdata = eag_full_grid, return_tmb_object = T)
eag_full_pred$data$est_se <- predict(eag_fit_full, newdata = eag_full_grid, nsim = 500) %>% apply(., 1, sd)

# full soakyr model predictions                        
eag_full_soakyr_pred <- predict(eag_fit_full_soakyr, newdata = eag_full_grid, return_tmb_object = T, se_fit = F)
eag_full_soakyr_pred$data$est_se <- predict(eag_fit_full_soakyr, newdata = eag_full_grid, nsim = 500) %>% apply(., 1, sd)


# wag make predictions ----

as_tibble(wag_mesh$mesh$loc[, 1:2]) %>% rename_all(~c("lon", "lat")) -> wag_mesh_latlon

# prediction grid for full  model
wag_full_grid <- expand_grid(lon = seq(min(wag_mesh_latlon$lon), max(wag_mesh_latlon$lon), length.out = 55),
                             lat = seq(min(wag_mesh_latlon$lat), max(wag_mesh_latlon$lat), length.out = 25),
                         depth = mean(wag_fit_full$data$depth),
                         soaktime = mean(wag_fit_full$data$soaktime),
                         wag_fit_full$data %>%
                           distinct(fyear,year, gearcode, adfg))
# null predictions
wag_null_pred <- predict(wag_fit_null, 
                     newdata = wag_full_grid %>% distinct(lon, lat, fyear, year), 
                     return_tmb_object = T)

# wag1 predictions
wag1_pred <- predict(wag_fit_1, 
                     newdata = wag_full_grid %>% distinct(lon, lat, year, fyear, adfg), 
                     return_tmb_object = T)

# wag2 predictions
wag2_pred <- predict(wag_fit_2, 
                     newdata = wag_full_grid %>% distinct(lon, lat, year, fyear, adfg, gearcode), 
                     return_tmb_object = T)

# wag3 predictions
wag3_pred <- predict(wag_fit_3, 
                     newdata = wag_full_grid %>% distinct(lon, lat, year, fyear, adfg, gearcode, soaktime), 
                     return_tmb_object = T)

# full model predictions                        
wag_full_pred <- predict(wag_fit_full, newdata = wag_full_grid, return_tmb_object = T)
wag_full_pred$data$est_se <- predict(wag_fit_full, newdata = wag_full_grid, nsim = 500) %>% apply(., 1, sd)

# full soakyr model predictions                        
# wag_full_soakyr_pred <- predict(wag_fit_full_soakyr, newdata = wag_full_grid, return_tmb_object = T)
# wag_full_soakyr_pred$data$est_se <- predict(wag_fit_full_soakyr, newdata = wag_full_grid, nsim = 500) %>% apply(., 1, sd)


# spatial residual patterns ----

## eag
# add residuals from ar1 full model
eag$residuals <- residuals(eag_fit_full_soakyr) 

eag %>% 
  st_as_sf(coords = c("lon", "lat")) %>%
  st_join(eag_fit_full$data %>%
            distinct(lon, lat) %>%
            st_as_sf(coords = c("lon", "lat")) %>%
            st_make_grid(cellsize = c(20, 20)) %>%
            st_cast("MULTIPOLYGON") %>%
            st_sf() %>%
            mutate(cellid = row_number()), .) %>%
  group_by(year, cellid) %>%
  summarise(residual = sum(residuals)) %>%
  filter(!is.na(year)) %>%

ggplot()+
  geom_sf(data = eag_land)+
  geom_sf(aes(fill = residual), color = "grey70")+
  
 # coord_sf(expand = 0, ylim = c(NA, max(eag$lat)), xlim = c(NA, max(eag$lon)))+
  scale_fill_gradient2()+
  facet_wrap(~year, ncol = 4)+
  geom_text_npc(aes(npcx = "left", npcy = 0.9, label = year),
                check_overlap = T, size = 3)+
  labs(fill = "sum(Residual)")+
  theme(panel.spacing.x = unit(0, "lines"),
        panel.spacing.y = unit(0, "lines"),
        panel.border = element_rect(color = "grey30", fill = NA),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        legend.position = "bottom",
        legend.key.size = unit(0.25, "in")) -> x
ggsave("./AIGKC/figures/cpue_std/2025/may/eag_full_soakyr_spatial_residuals.png", plot = x, height = 8, width = 8, units = "in")


## wag
# add residuals from full model
wag$residuals <- residuals(wag_fit_full) 

wag %>% 
  st_as_sf(coords = c("lon", "lat")) %>%
  st_join(wag_fit_full$data %>%
            distinct(lon, lat) %>%
            st_as_sf(coords = c("lon", "lat")) %>%
            st_make_grid(cellsize = c(20, 20)) %>%
            st_cast("MULTIPOLYGON") %>%
            st_sf() %>%
            mutate(cellid = row_number()), .) %>%
  group_by(year, cellid) %>%
  summarise(residual = sum(residuals)) %>%
  filter(!is.na(year)) %>%
  
  ggplot()+
  geom_sf(data = wag_land)+
  geom_sf(aes(fill = residual), color = "grey70")+
  
  coord_sf(expand = 0, ylim = c(NA, max(wag$lat)), xlim = c(NA, max(wag$lon)))+
  scale_fill_gradient2()+
  facet_wrap(~year, ncol = 4)+
  geom_text_npc(aes(npcx = "left", npcy = 0.9, label = year),
                check_overlap = T, size = 3)+
  labs(fill = "sum(Residual)")+
  theme(panel.spacing.x = unit(0, "lines"),
        panel.spacing.y = unit(0, "lines"),
        panel.border = element_rect(color = "grey30", fill = NA),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        legend.position = "bottom",
        legend.key.size = unit(0.25, "in")) -> x
ggsave("./AIGKC/figures/cpue_std/2025/may/wag_full_spatial_residuals.png", plot = x, height = 8, width = 8, units = "in")

# spatial effects ----

## eag
# est
ggplot(eag_full_soakyr_pred$data)+
  geom_raster(aes(x = lon, y = lat, fill = exp(est)))+
  geom_sf(data = eag_mesh_sf, fill = NA, color = "grey70")+
  geom_point(data = eag, aes(x = lon, y = lat), size = 0.2)+
  geom_sf(data = eag_land)+
  scale_fill_viridis_c()+
  coord_sf(expand = 0, ylim = c(NA, max(eag_full_pred$data$lat)), xlim = c(NA, max(eag_full_pred$data$lon)))+
  facet_wrap(~year, ncol = 4)+
  labs(x = NULL, y = NULL)+
  geom_text_npc(aes(npcx = "left", npcy = 0.9, label = year),
                check_overlap = T, size = 3, color = "white")+
  theme(panel.spacing.x = unit(0, "lines"),
        panel.spacing.y = unit(0, "lines"),
        panel.border = element_rect(color = "grey30", fill = NA),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        legend.position = "bottom",
        legend.key.size = unit(0.25, "in")) -> x
ggsave("./AIGKC/figures/cpue_std/2025/may/eag_full_soakyr_spatial_effect.png", plot = x, height = 8, width = 8, units = "in")

# se
ggplot(eag_full_soakyr_pred$data)+
  geom_raster(aes(x = lon, y = lat, fill = est_se))+
  geom_sf(data = eag_mesh_sf, fill = NA, color = "grey70")+
  geom_point(data = eag, aes(x = lon, y = lat), size = 0.1)+
  geom_sf(data = eag_land)+
  scale_fill_viridis_c()+
  coord_sf(expand = 0, ylim = c(NA, max(eag_full_pred$data$lat)), xlim = c(NA, max(eag_full_pred$data$lon)))+
  facet_wrap(~year, ncol = 4)+
  labs(x = NULL, y = NULL, fill = "SE")+
  geom_text_npc(aes(npcx = "left", npcy = 0.9, label = year),
                check_overlap = T, size = 3, color = "white")+
  theme(panel.spacing.x = unit(0, "lines"),
        panel.spacing.y = unit(0, "lines"),
        panel.border = element_rect(color = "grey30", fill = NA),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        legend.position = "bottom",
        legend.key.size = unit(0.25, "in")) -> x
ggsave("./AIGKC/figures/cpue_std/2025/may/eag_full_soakyr_spatial_se.png", plot = x, height = 8, width = 8, units = "in")

# omega_s
eag_full_soakyr_pred$data %>%
  distinct(lon, lat, omega_s) %>%
  ggplot()+
  geom_raster(aes(x = lon, y = lat, fill = omega_s))+
  geom_sf(data = eag_land)+
  scale_fill_gradient2()+
  coord_sf(expand = 0, ylim = c(NA, max(eag_full_soakyr_pred$data$lat)), xlim = c(NA, max(eag_full_soakyr_pred$data$lon)))+
  labs(x = NULL, y = NULL)+
  theme(panel.spacing.x = unit(0, "lines"),
        panel.spacing.y = unit(0, "lines"),
        panel.border = element_rect(color = "grey30", fill = NA),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 5)) -> x
ggsave("./AIGKC/figures/cpue_std/2025/may/eag_full_soakyr_spatial_ranef.png", plot = x, height = 3, width = 6, units = "in")

# epsilon_st
ggplot(eag_full_soakyr_pred$data)+
  geom_raster(aes(x = lon, y = lat, fill = epsilon_st))+
  geom_sf(data = eag_land)+
  scale_fill_gradient2()+
  coord_sf(expand = 0, ylim = c(NA, max(eag_full_soakyr_pred$data$lat)), xlim = c(NA, max(eag_full_soakyr_pred$data$lon)))+
  facet_wrap(~year, ncol = 4)+
  labs(x = NULL, y = NULL)+
  geom_text_npc(aes(npcx = "left", npcy = 0.9, label = year),
                check_overlap = T, size = 3)+
  theme(panel.spacing.x = unit(0, "lines"),
        panel.spacing.y = unit(0, "lines"),
        panel.border = element_rect(color = "grey30", fill = NA),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        legend.position = "bottom",
        legend.key.size = unit(0.25, "in")) -> x
ggsave("./AIGKC/figures/cpue_std/2025/may/eag_full_soakyr_spatiotemporal_ranef.png", plot = x, height = 8, width = 8, units = "in")

# ggplot(eag_full_iid_pred$data)+
#   geom_raster(aes(x = lon, y = lat, fill = epsilon_st))+
#   geom_sf(data = eag_land)+
#   scale_fill_gradient2()+
#   coord_sf(expand = 0, ylim = c(NA, max(full_pred$data$lat)), xlim = c(NA, max(full_pred$data$lon)))+
#   facet_wrap(~year, ncol = 4)+
#   labs(x = NULL, y = NULL)+
#   geom_text_npc(aes(npcx = "left", npcy = 0.9, label = year),
#                 check_overlap = T, size = 3)+
#   theme(panel.spacing.x = unit(0, "lines"),
#         panel.spacing.y = unit(0, "lines"),
#         panel.border = element_rect(color = "grey30", fill = NA),
#         strip.background = element_blank(),
#         strip.text.x = element_blank(),
#         axis.text = element_blank(),
#         axis.ticks = element_blank(),
#         panel.background = element_blank(),
#         legend.position = "bottom",
#         legend.key.size = unit(0.25, "in")) -> x
# ggsave("./AIGKC/figures/cpue_std/2025/jan/eag_full_iid_spatiotemporal_ranef.png", plot = x, height = 8, width = 8, units = "in")
# 

## wag
# est
ggplot(wag_full_pred$data)+
  geom_raster(aes(x = lon, y = lat, fill = exp(est)))+
  geom_sf(data = wag_mesh_sf, fill = NA, color = "grey70")+
  geom_point(data = wag, aes(x = lon, y = lat), size = 0.2)+
 # geom_sf(data = wag_land)+
  scale_fill_viridis_c()+
  #coord_sf(expand = 0, ylim = c(NA, max(wag_mesh_sf$data$lat)), xlim = c(NA, max(wag_full_pred$data$lon)))+
  facet_wrap(~year, ncol = 4)+
  labs(x = NULL, y = NULL)+
  geom_text_npc(aes(npcx = "left", npcy = 0.9, label = year),
                check_overlap = T, size = 3, color = "white")+
  theme(panel.spacing.x = unit(0, "lines"),
        panel.spacing.y = unit(0, "lines"),
        panel.border = element_rect(color = "grey30", fill = NA),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        legend.position = "bottom",
        legend.key.size = unit(0.25, "in")) -> x
ggsave("./AIGKC/figures/cpue_std/2025/may/wag_full_spatial_effect.png", plot = x, height = 8, width = 8, units = "in")

# se
ggplot(wag_full_pred$data)+
  geom_raster(aes(x = lon, y = lat, fill = est_se))+
  geom_sf(data = wag_mesh_sf, fill = NA, color = "grey70")+
  geom_point(data = wag, aes(x = lon, y = lat), size = 0.1)+
  geom_sf(data = wag_land)+
  scale_fill_viridis_c()+
  coord_sf(expand = 0, ylim = c(NA, max(wag_full_pred$data$lat)), xlim = c(NA, max(wag_full_pred$data$lon)))+
  facet_wrap(~year, ncol = 4)+
  labs(x = NULL, y = NULL, fill = "SE")+
  geom_text_npc(aes(npcx = "left", npcy = 0.9, label = year),
                check_overlap = T, size = 3, color = "white")+
  theme(panel.spacing.x = unit(0, "lines"),
        panel.spacing.y = unit(0, "lines"),
        panel.border = element_rect(color = "grey30", fill = NA),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        legend.position = "bottom",
        legend.key.size = unit(0.25, "in")) -> x
ggsave("./AIGKC/figures/cpue_std/2025/may/wag_full_spatial_se.png", plot = x, height = 8, width = 8, units = "in")

# omega_s
wag_full_pred$data %>%
  distinct(lon, lat, omega_s) %>%
  ggplot()+
  geom_raster(aes(x = lon, y = lat, fill = omega_s))+
  #geom_sf(data = wag_mesh_sf, fill = NA, color = "grey70")+
  geom_sf(data = wag_land)+
  scale_fill_gradient2()+
  #coord_sf(expand = 0, ylim = c(NA, max(wag_full_pred$data$lat)), xlim = c(NA, max(wag_full_pred$data$lon)))+
  labs(x = NULL, y = NULL)+
  theme(panel.spacing.x = unit(0, "lines"),
        panel.spacing.y = unit(0, "lines"),
        panel.border = element_rect(color = "grey30", fill = NA),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 5)) -> x
ggsave("./AIGKC/figures/cpue_std/2025/may/wag_full_spatial_ranef.png", plot = x, height = 3, width = 5, units = "in")

# epsilon_st
ggplot(wag_full_pred$data)+
  geom_raster(aes(x = lon, y = lat, fill = epsilon_st))+
  geom_sf(data = wag_land)+
  #geom_sf(data = wag_mesh_sf, fill = NA, color = "grey70")+
  scale_fill_gradient2()+
  #coord_sf(expand = 0, ylim = c(NA, max(wag_full_pred$data$lat)), xlim = c(NA, max(wag_full_pred$data$lon)))+
  facet_wrap(~year, ncol = 4)+
  labs(x = NULL, y = NULL)+
  geom_text_npc(aes(npcx = "left", npcy = 0.9, label = year),
                check_overlap = T, size = 3)+
  theme(panel.spacing.x = unit(0, "lines"),
        panel.spacing.y = unit(0, "lines"),
        panel.border = element_rect(color = "grey30", fill = NA),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        legend.position = "bottom",
        legend.key.size = unit(0.25, "in")) -> x
ggsave("./AIGKC/figures/cpue_std/2025/may/wag_full_spatiotemporal_ranef.png", plot = x, height = 8, width = 8, units = "in")

# ggplot(wag_full_iid_pred$data)+
#   geom_raster(aes(x = lon, y = lat, fill = epsilon_st))+
#   geom_sf(data = wag_land)+
#   scale_fill_gradient2()+
#   coord_sf(expand = 0, ylim = c(NA, max(full_pred$data$lat)), xlim = c(NA, max(full_pred$data$lon)))+
#   facet_wrap(~year, ncol = 4)+
#   labs(x = NULL, y = NULL)+
#   geom_text_npc(aes(npcx = "left", npcy = 0.9, label = year),
#                 check_overlap = T, size = 3)+
#   theme(panel.spacing.x = unit(0, "lines"),
#         panel.spacing.y = unit(0, "lines"),
#         panel.border = element_rect(color = "grey30", fill = NA),
#         strip.background = element_blank(),
#         strip.text.x = element_blank(),
#         axis.text = element_blank(),
#         axis.ticks = element_blank(),
#         panel.background = element_blank(),
#         legend.position = "bottom",
#         legend.key.size = unit(0.25, "in")) -> x
# ggsave("./AIGKC/figures/cpue_std/2025/jan/wag_full_iid_spatiotemporal_ranef.png", plot = x, height = 8, width = 8, units = "in")


# eag extract index ----

# index on mesh grid
mesh_grid <- expand_grid(as_tibble(eag_mesh$mesh$loc[, 1:2]) %>% rename_all(~c("lon", "lat")),
                         depth = mean(eag_fit_full$data$depth),
                         soaktime = mean(eag_fit_full$data$soaktime),
                         adfg = factor(names(sort(-table(eag_fit_full$data$adfg)))[1]),
                         gearcode = factor(names(sort(-table(eag_fit_full$data$gearcode)))[1]),
                         eag_fit_full$data %>%
                           distinct(fyear, year))

# null index
get_index(predict(eag_fit_null, 
                  newdata = mesh_grid %>%
                    distinct(lon, lat, fyear, year), 
                  return_tmb_object = T), 
          area = 1, bias_correct = T) %>%
  mutate(index = exp(log_est - mean(log_est)),
         cv = se / log_est,
         l95 = index * exp(-1.96 * sqrt(log(1 + cv^2))),
         u95 = index * exp(1.96 * sqrt(log(1 + cv^2))),
         var = "Year",
         order = 1) -> eag_index_null_mesh

# null + adfg index
get_index(predict(eag_fit_1, 
                  newdata =  mesh_grid %>%
                    distinct(lon, lat, fyear, year, adfg), 
                  return_tmb_object = T), 
          area = 1, bias_correct = T) %>%
  mutate(index = exp(log_est - mean(log_est)),
         cv = se / log_est,
         l95 = index * exp(-1.96 * sqrt(log(1 + cv^2))),
         u95 = index * exp(1.96 * sqrt(log(1 + cv^2))),
         var = "+ (1 | Vessel)",
         order = 2) -> eag1_index_mesh

# null + adfg + gearcode index
get_index(predict(eag_fit_2, 
                  newdata =  mesh_grid %>%
                    distinct(lon, lat, fyear, year, adfg, gearcode), 
                  return_tmb_object = T), 
          area = 1, bias_correct = T) %>%
  mutate(index = exp(log_est - mean(log_est)),
         cv = se / log_est,
         l95 = index * exp(-1.96 * sqrt(log(1 + cv^2))),
         u95 = index * exp(1.96 * sqrt(log(1 + cv^2))),
         var = "+ Gear",
         order = 3) -> eag2_index_mesh

# null + adfg + gearcode + soaktime index
get_index(predict(eag_fit_3, 
                  newdata =  mesh_grid %>%
                    distinct(lon, lat, fyear, year, adfg, gearcode, soaktime), 
                  return_tmb_object = T), 
          area = 1, bias_correct = T) %>%
  mutate(index = exp(log_est - mean(log_est)),
         cv = se / log_est,
         l95 = index * exp(-1.96 * sqrt(log(1 + cv^2))),
         u95 = index * exp(1.96 * sqrt(log(1 + cv^2))),
         var = "+ s(soaktime)",
         order = 4) -> eag3_index_mesh

# null + adfg + gearcode + soaktime index soakyr
get_index(predict(eag_fit_3_soakyr, 
                  newdata =  mesh_grid %>%
                    distinct(lon, lat, fyear, year, adfg, gearcode, soaktime), 
                  return_tmb_object = T), 
          area = 1, bias_correct = T) %>%
  mutate(index = exp(log_est - mean(log_est)),
         cv = se / log_est,
         l95 = index * exp(-1.96 * sqrt(log(1 + cv^2))),
         u95 = index * exp(1.96 * sqrt(log(1 + cv^2))),
         var = "+ s(soaktime)",
         which = "soakyr",
         order = 4) -> eag3_soakyr_index_mesh

# full index
get_index(predict(eag_fit_full, 
                  newdata =  mesh_grid %>%
                    distinct(lon, lat, fyear, year, adfg, gearcode, soaktime, depth), 
                  return_tmb_object = T), 
          area = 1, bias_correct = T) %>%
  mutate(index = exp(log_est - mean(log_est)),
         cv = se / log_est,
         l95 = index * exp(-1.96 * sqrt(log(1 + cv^2))),
         u95 = index * exp(1.96 * sqrt(log(1 + cv^2))),
         var = "+ s(depth)",
         order = 5) -> eag_index_full_mesh

# full index soakyr
get_index(predict(eag_fit_full_soakyr, 
                  newdata =  mesh_grid %>%
                    distinct(lon, lat, fyear, year, adfg, gearcode, soaktime, depth), 
                  return_tmb_object = T), 
          area = 1, bias_correct = T) %>%
  mutate(index = exp(log_est - mean(log_est)),
         cv = se / log_est,
         l95 = index * exp(-1.96 * sqrt(log(1 + cv^2))),
         u95 = index * exp(1.96 * sqrt(log(1 + cv^2))),
         var = "+ s(depth)",
         which = "soakyr",
         order = 5) -> eag_index_full_soakyr_mesh

# write and read index by model
bind_rows(eag_index_null_mesh, eag1_index_mesh, eag2_index_mesh, eag3_index_mesh, eag3_soakyr_index_mesh, eag_index_full_mesh, eag_index_full_soakyr_mesh) %>%
  write_csv("./AIGKC/output/cpue_std/2025/may/eag_st_index_mesh.RDS")
eag_index_mesh <- read_csv("./AIGKC/output/cpue_std/2025/may/eag_st_index_mesh.RDS")

eag_index_full_mesh %>%
  transmute(year, index, cv, l95, u95) %>%
  write_csv("./AIGKC/output/cpue_std/2025/may/eag_st_index_mesh.csv")

eag_index_full_soakyr_mesh %>%
  transmute(year, index, cv, l95, u95) %>%
  write_csv("./AIGKC/output/cpue_std/2025/may/eag_index_full_soakyr_mesh.csv")


# step plot
tibble(mod1 = c(NA, "Year", "+ (1 | Vessel)", "+ Gear", "+ s(soaktime)"),
       mod2 = c("Year", "+ (1 | Vessel)", "+ Gear", "+ s(soaktime)", "+ s(depth)"),
       data = list(eag_index_mesh, eag_index_mesh, eag_index_mesh, eag_index_mesh, eag_index_mesh)) %>% #pull(data) %>% .[[4]] -> data
  mutate(data = purrr::pmap(list(mod1, mod2, data), function(mod1, mod2, data) {
    data %>%  #print(n = 1000)
      filter(order <= order[var == mod2]) %>% 
      mutate(dotted_mod = order == (order[var == mod2] - 1))  
  })) %>%
  unnest(data) %>%
  mutate(mod2 = factor(mod2, levels = c("Year", "+ (1 | Vessel)", "+ Gear", "+ s(soaktime)", "+ s(depth)"))) %>% 
  replace_na(list(which = "dummy")) %>%
  ggplot()+
  geom_line(data = . %>% filter(dotted_mod == F, var != mod2), aes(x = year, y = index, group = var), color = "grey90")+
  geom_line(data = . %>% filter(dotted_mod == T), aes(x = year, y = index, group = which, color = which), linetype = 2, show.legend = F)+
  geom_line(data = . %>% filter(var == mod2),  aes(x = year, y = index, group = which, color = which), show.legend = F)+
  facet_wrap(~mod2, ncol = 1)+
  labs(x = NULL, y = "Index")+
  scale_color_manual(values = c("black", cbpalette[2]))+
  geom_text_npc(aes(npcx = "left", npcy = 0.925, label = mod2),
                check_overlap = T, size = 4)+
  scale_x_continuous(breaks = gmacsr::yraxis$breaks, labels = gmacsr::yraxis$labels)+
  scale_y_continuous(breaks = seq(0.7, 1.9, 0.2))+
  theme(panel.spacing.x = unit(0, "lines"),
        panel.spacing.y = unit(0, "lines"),
        panel.border = element_rect(color = "grey30", fill = NA),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        panel.background = element_blank()) -> x
ggsave("./AIGKC/figures/cpue_std/2025/may/eag_step_index_mesh.png", plot = x, height = 8, width = 5, units = "in")


# extract index of GLMM and plot with nominal CPUE
## nominal index 
eag %>%
  group_by(year) %>%
  summarise(mean_cpue = mean(cpue),
            cv = sqrt(var(cpue) / n()) / mean_cpue) %>%
  mutate(index = mean_cpue / prod(mean_cpue)^(1/n()),
         l95 = index * exp(-1.96 * sqrt(log(1 + cv^2))),
         u95 = index * exp(1.96 * sqrt(log(1 + cv^2))),
         type = "Nominal") %>%
  transmute(year, type, index, cv, l95, u95) -> eag_nominal_index

# glmm index
get_index(predict(eag_fit_full_glmm, 
                  newdata =  mesh_grid %>%
                    distinct(fyear, year, adfg, gearcode, soaktime, depth), 
                  return_tmb_object = T), 
          area = 1, bias_correct = T) %>%
  mutate(index = exp(log_est - mean(log_est)),
         cv = se / log_est,
         l95 = index * exp(-1.96 * sqrt(log(1 + cv^2))),
         u95 = index * exp(1.96 * sqrt(log(1 + cv^2))),
         type = "GAMM") %>%
  transmute(year, type, index, cv, l95, u95) -> eag_index_full_glmm_mesh


## plot of full index with and without spatial effect
eag_index_full_mesh %>%
  mutate(type = "ST GAMM") %>%
  bind_rows(eag_index_full_soakyr_mesh %>%
              mutate(type = "ST GAMM + s(soaktime:year)")) %>%
  bind_rows(eag_nominal_index) %>%
  bind_rows(eag_index_full_glmm_mesh) %>%
  mutate(type = factor(type, levels = c("Nominal", "GAMM", "ST GAMM", "ST GAMM + s(soaktime:year)"))) %>%
  ggplot()+
  geom_line(aes(x = year, y = index, color = type))+
  labs(x = NULL, y = "Index", color = NULL)+
  scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
  scale_color_manual(values = cbpalette)+
  theme(legend.position = c(0, 1), 
        legend.justification = c(0, 1)) -> x
ggsave("./AIGKC/figures/cpue_std/2025/may/eag_full_index_comparison.png", plot = x, height = 3, width = 5, units = "in")


# wag extract index ----

# index on mesh grid
mesh_grid <- expand_grid(as_tibble(wag_mesh$mesh$loc[, 1:2]) %>% rename_all(~c("lon", "lat")),
                         depth = mean(wag_fit_full$data$depth),
                         soaktime = mean(wag_fit_full$data$soaktime),
                         adfg = factor(names(sort(-table(wag_fit_full$data$adfg)))[1]),
                         gearcode = factor(names(sort(-table(wag_fit_full$data$gearcode)))[1]),
                         wag_fit_full$data %>%
                           distinct(fyear, year))

# null index
get_index(predict(wag_fit_null, 
                  newdata = mesh_grid %>%
                    distinct(lon, lat, fyear, year), 
                  return_tmb_object = T), 
          area = 1, bias_correct = T) %>%
  mutate(index = exp(log_est - mean(log_est)),
         cv = se / log_est,
         l95 = index * exp(-1.96 * sqrt(log(1 + cv^2))),
         u95 = index * exp(1.96 * sqrt(log(1 + cv^2))),
         var = "Year",
         order = 1) -> wag_index_null_mesh

# null + adfg index
get_index(predict(wag_fit_1, 
                  newdata =  mesh_grid %>%
                    distinct(lon, lat, fyear, year, adfg), 
                  return_tmb_object = T), 
          area = 1, bias_correct = T) %>%
  mutate(index = exp(log_est - mean(log_est)),
         cv = se / log_est,
         l95 = index * exp(-1.96 * sqrt(log(1 + cv^2))),
         u95 = index * exp(1.96 * sqrt(log(1 + cv^2))),
         var = "+ (1 | Vessel)",
         order = 2) -> wag1_index_mesh

# null + adfg + gearcode index
get_index(predict(wag_fit_2, 
                  newdata =  mesh_grid %>%
                    distinct(lon, lat, fyear, year, adfg, gearcode), 
                  return_tmb_object = T), 
          area = 1, bias_correct = T) %>%
  mutate(index = exp(log_est - mean(log_est)),
         cv = se / log_est,
         l95 = index * exp(-1.96 * sqrt(log(1 + cv^2))),
         u95 = index * exp(1.96 * sqrt(log(1 + cv^2))),
         var = "+ Gear",
         order = 3) -> wag2_index_mesh

# null + adfg + gearcode + soaktime index
get_index(predict(wag_fit_3, 
                  newdata =  mesh_grid %>%
                    distinct(lon, lat, fyear, year, adfg, gearcode, soaktime), 
                  return_tmb_object = T), 
          area = 1, bias_correct = T) %>%
  mutate(index = exp(log_est - mean(log_est)),
         cv = se / log_est,
         l95 = index * exp(-1.96 * sqrt(log(1 + cv^2))),
         u95 = index * exp(1.96 * sqrt(log(1 + cv^2))),
         var = "+ s(soaktime)",
         order = 4) -> wag3_index_mesh

# null + adfg + gearcode + soaktime:year index
# get_index(predict(wag_fit_3_soakyr, 
#                   newdata =  mesh_grid %>%
#                     distinct(lon, lat, fyear, year, adfg, gearcode, soaktime), 
#                   return_tmb_object = T), 
#           area = 1, bias_correct = T) %>%
#   mutate(index = exp(log_est - mean(log_est)),
#          cv = se / log_est,
#          l95 = index * exp(-1.96 * sqrt(log(1 + cv^2))),
#          u95 = index * exp(1.96 * sqrt(log(1 + cv^2))),
#          var = "+ s(soaktime)",
#          which = "soakyr",
#          order = 4) -> wag3_soakyr_index_mesh

# full index
get_index(predict(wag_fit_full, 
                  newdata =  mesh_grid %>%
                    distinct(lon, lat, fyear, year, adfg, gearcode, soaktime, depth), 
                  return_tmb_object = T), 
          area = 1, bias_correct = T) %>%
  mutate(index = exp(log_est - mean(log_est)),
         cv = se / log_est,
         l95 = index * exp(-1.96 * sqrt(log(1 + cv^2))),
         u95 = index * exp(1.96 * sqrt(log(1 + cv^2))),
         var = "+ s(depth)",
         order = 5) -> wag_index_full_mesh

# full soakyr index
# get_index(predict(wag_fit_full_soakyr, 
#                   newdata =  mesh_grid %>%
#                     distinct(lon, lat, fyear, year, adfg, gearcode, soaktime, depth), 
#                   return_tmb_object = T), 
#           area = 1, bias_correct = T) %>%
#   mutate(index = exp(log_est - mean(log_est)),
#          cv = se / log_est,
#          l95 = index * exp(-1.96 * sqrt(log(1 + cv^2))),
#          u95 = index * exp(1.96 * sqrt(log(1 + cv^2))),
#          var = "+ s(depth)",
#          which = "soakyr",
#          order = 5) -> wag_index_full_soakyr_mesh

# write and read index by model
bind_rows(wag_index_null_mesh, wag1_index_mesh, wag2_index_mesh, wag3_index_mesh, wag_index_full_mesh) %>%
  write_csv("./AIGKC/output/cpue_std/2025/may/wag_st_index_mesh.RDS")
wag_index_mesh <- read_csv("./AIGKC/output/cpue_std/2025/may/wag_st_index_mesh.RDS")

wag_index_full_mesh %>%
  transmute(year, index, cv, l95, u95) %>%
  write_csv("./AIGKC/output/cpue_std/2025/may/wag_st_index_mesh.csv")
# wag_index_full_mesh <- read_csv("./AIGKC/output/cpue_std/2025/may/wag_st_index_mesh.csv")

# wag_index_full_soakyr_mesh %>%
#   transmute(year, index, cv, l95, u95) %>%
#   write_csv("./AIGKC/output/cpue_std/2025/may/wag_index_full_soakyr_mesh.csv")
# wag_index_full_soakyr_mesh <- read_csv("./AIGKC/output/cpue_std/2025/may/wag_index_full_soakyr_mesh.csv")


# step plot
tibble(mod1 = c(NA, "Year", "+ (1 | Vessel)", "+ Gear", "+ s(soaktime)"),
       mod2 = c("Year", "+ (1 | Vessel)", "+ Gear", "+ s(soaktime)", "+ s(depth)"),
       data = list(wag_index_mesh, wag_index_mesh, wag_index_mesh, wag_index_mesh, wag_index_mesh)) %>% #pull(data) %>% .[[4]] -> data
  mutate(data = purrr::pmap(list(mod1, mod2, data), function(mod1, mod2, data) {
    data %>%  #print(n = 1000)
      filter(order <= order[var == mod2]) %>% 
      mutate(dotted_mod = order == (order[var == mod2] - 1))  
  })) %>%
  unnest(data) %>%
  mutate(mod2 = factor(mod2, levels = c("Year", "+ (1 | Vessel)", "+ Gear", "+ s(soaktime)", "+ s(depth)"))) %>% 
  replace_na(list(which = "dummy")) %>%
  mutate(which = NA) %>%
  ggplot()+
  geom_line(data = . %>% filter(dotted_mod == F, var != mod2), aes(x = year, y = index, group = var), color = "grey90")+
  geom_line(data = . %>% filter(dotted_mod == T), aes(x = year, y = index, group = which, color = which), linetype = 2, show.legend = F)+
  geom_line(data = . %>% filter(var == mod2),  aes(x = year, y = index, group = which, color = which), show.legend = F)+
  facet_wrap(~mod2, ncol = 1)+
  labs(x = NULL, y = "Index")+
  scale_color_manual(values = c("black", cbpalette[2]))+
  geom_text_npc(aes(npcx = "left", npcy = 0.925, label = mod2),
                check_overlap = T, size = 4)+
  scale_x_continuous(breaks = gmacsr::yraxis$breaks, labels = gmacsr::yraxis$labels)+
  scale_y_continuous(breaks = seq(0.7, 1.9, 0.2))+
  theme(panel.spacing.x = unit(0, "lines"),
        panel.spacing.y = unit(0, "lines"),
        panel.border = element_rect(color = "grey30", fill = NA),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        panel.background = element_blank()) -> x
ggsave("./AIGKC/figures/cpue_std/2025/may/wag_step_index_mesh.png", plot = x, height = 8, width = 5, units = "in")

# extract index of GLMM and plot with nominal CPUE
## nominal index 
wag %>%
  group_by(year) %>%
  summarise(mean_cpue = mean(cpue),
            cv = sqrt(var(cpue) / n()) / mean_cpue) %>%
  mutate(index = mean_cpue / prod(mean_cpue)^(1/n()),
         l95 = index * exp(-1.96 * sqrt(log(1 + cv^2))),
         u95 = index * exp(1.96 * sqrt(log(1 + cv^2))),
         type = "Nominal") %>%
  transmute(year, type, index, cv, l95, u95) -> wag_nominal_index

# glmm index
get_index(predict(wag_fit_full_glmm, 
                  newdata =  mesh_grid %>%
                    distinct(fyear, year, adfg, gearcode, soaktime, depth), 
                  return_tmb_object = T), 
          area = 1, bias_correct = T) %>%
  mutate(index = exp(log_est - mean(log_est)),
         cv = se / log_est,
         l95 = index * exp(-1.96 * sqrt(log(1 + cv^2))),
         u95 = index * exp(1.96 * sqrt(log(1 + cv^2))),
         type = "GAMM") %>%
  transmute(year, type, index, cv, l95, u95) -> wag_index_full_glmm_mesh


## plot of full index 
wag_index_full_mesh %>%
  mutate(type = "ST GAMM") %>%
  bind_rows(wag_nominal_index) %>%
  bind_rows(wag_index_full_glmm_mesh) %>%
  mutate(type = factor(type, levels = c("Nominal", "GAMM", "ST GAMM"))) %>%
  ggplot()+
  geom_line(aes(x = year, y = index, color = type))+
  labs(x = NULL, y = "Index", color = NULL)+
  scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
  scale_color_manual(values = cbpalette)+
  theme(legend.position = c(0, 1), 
        legend.justification = c(0, 1)) -> x
ggsave("./AIGKC/figures/cpue_std/2025/may/wag_full_index_comparison.png", plot = x, height = 3, width = 5, units = "in")


# investigate eag vessel effect ----

# fit glmm
# eag_fit_1_glmm <- sdmTMB(
#   cpue ~ 0 + fyear + (1|adfg),
#   data = eag,
#   mesh = eag_mesh,
#   time = "year",
#   spatial = "off",
#   spatiotemporal = "off",
#   family = tweedie(link = "log")
# )
# saveRDS(eag_fit_1_glmm, "./AIGKC/output/cpue_std/2025/jan/eag_fit_1_glmm.RDS")
eag_fit_1_glmm <- readRDS("./AIGKC/output/cpue_std/2025/jan/eag_fit_1_glmm.RDS")

# fit glmm model with temporal re
# eag_fit_1_t <- sdmTMB(
#   cpue ~ 0 + fyear + (1|adfg),
#   data = eag,
#   mesh = eag_mesh,
#   time = "year",
#   spatial = "off",
#   spatiotemporal = "ar1",
#   family = tweedie(link = "log")
# )
# saveRDS(eag_fit_1_t, "./AIGKC/output/cpue_std/2025/jan/eag_fit_1_t.RDS")
eag_fit_1_t <- readRDS("./AIGKC/output/cpue_std/2025/jan/eag_fit_1_t.RDS")

# plot vessel ranef from these models
tibble(adfg = sort(unique(eag$adfg)),
       ranef = unlist(lme4::ranef(eag_fit_1_glmm)),
       model = "GLMM") %>%
  bind_rows(tibble(adfg = sort(unique(eag$adfg)),
                   ranef = unlist(lme4::ranef(eag_fit_1_t)),
                   model = "T AR1")) %>%
  bind_rows(tibble(adfg = sort(unique(eag$adfg)),
                   ranef = unlist(lme4::ranef(eag_fit_1)),
                   model = "ST AR1")) %>%
  
  ggplot()+
  geom_point(aes(x = adfg, y = ranef, color = model), alpha = 0.5)+
  geom_hline(yintercept = 0, linetype = 2)

# investigate index
# GLMM
get_index(predict(eag_fit_1_glmm, 
                  newdata =  mesh_grid %>%
                    distinct(lon, lat, fyear, year, adfg), 
                  return_tmb_object = T), 
          area = 1, bias_correct = T) %>%
  mutate(index = exp(log_est - mean(log_est)),
         cv = se / log_est,
         l95 = index * exp(-1.96 * sqrt(log(1 + cv^2))),
         u95 = index * exp(1.96 * sqrt(log(1 + cv^2))),
         model = "GLMM") -> eag1_index_mesh_glmm
# AR1 T
get_index(predict(eag_fit_1_t, 
                  newdata =  mesh_grid %>%
                    distinct(lon, lat, fyear, year, adfg), 
                  return_tmb_object = T), 
          area = 1, bias_correct = T) %>%
  mutate(index = exp(log_est - mean(log_est)),
         cv = se / log_est,
         l95 = index * exp(-1.96 * sqrt(log(1 + cv^2))),
         u95 = index * exp(1.96 * sqrt(log(1 + cv^2))),
         model = "AR1 T") -> eag1_index_mesh_t
# AR1 ST
eag1_index_mesh %>%
  dplyr::select(-var, -order) %>%
  mutate(model = "AR1 ST") -> eag1_index_mesh_st

# plot index
bind_rows(eag1_index_mesh_glmm, eag1_index_mesh_t, eag1_index_mesh_st) %>%
  ggplot()+
  geom_line(aes(x = year, y = index, color = model))+
  scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
  scale_color_manual(values = cbpalette)+
  labs(x = NULL, color = NULL, y = "Index")+
  theme(legend.justification = c(1, 1),
        legend.position = "inside") -> x
ggsave("./AIGKC/figures/cpue_std/2025/jan/eag_index_vessel_effect.png", plot = x, height = 3, width = 5, units = "in")

tibble(adfg = sort(unique(eag$adfg)),
       ranef = unlist(lme4::ranef(eag_fit_full))) %>%
  right_join(eag %>% count(year, adfg) ) %>%
  
  ggplot()+
  geom_point(aes(x = adfg, y = ranef))+
  geom_hline(aes(yintercept = 0), linetype = 2)+
  facet_wrap(~year)+
  labs(x = "Vessel", y = "RE")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 5)) -> x
ggsave("./AIGKC/figures/cpue_std/2025/jan/eag_full_adfg_ranef_year.png", plot = x, height = 8, width = 8, units = "in")  
  
# plot of ranef eag
tibble(adfg = sort(unique(eag$adfg)),
       ranef = unlist(lme4::ranef(eag_fit_1))) %>%
  right_join(eag %>% count(year, adfg) ) %>%
  group_by(year) %>%
  summarise(wt_vessel_effect = sum(ranef * n) / sum(n)) %>%
  left_join(eag1_index_mesh_st) %>%
  
  ggplot()+
  geom_line(aes(x = year, y = index))+
  geom_line(aes(x = year, y = wt_vessel_effect*10 + mean(wt_vessel_effect*10)))
  
  

  
  
  
  
  
  

