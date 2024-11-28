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
library(gmacsr)
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

## observer pots
obs_raw <- read.csv("./AIGKC/data/observer/linked_potsum_dump.csv", na.strings = -9)
## make core data
obs_raw %>%
  # remove missing key data
  filter(!is.na(crab_year),
         crab_year >= 1995,
         !is.na(sampdate),
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
  transmute(year = as.numeric(crab_year), subdistrict, lat = latitude, lon = as.numeric(longitude), depth, soaktime, spn, trip, sampdate, gearcode = factor(gearcode), adfg = factor(adfg), permit_holder, sublegal, tot_legal, cpue = sublegal + tot_legal) %>%
  filter(!is.na(lon), !is.na(lat)) -> obs

# eag data 
obs %>% 
  filter(subdistrict == "EAG",
         lon < -164.7) %>%
  # remove area specific gear codes and quantiles
  # filter for middle quantiles of soaktime and depth by year
  filter(!gearcode %in% c(4, 13, 8)) %>%
  group_by(year) %>%
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

# mesh ----

init_eag_mesh <- make_mesh(eag, xy_cols = c("lon", "lat"), cutoff = 10)
eag_mesh <- sdmTMBextra::add_barrier_mesh(
  spde_obj = init_eag_mesh,
  barrier_sf = eag_land,
  range_fraction = 0.1,
  plot = FALSE
)
plot(eag_mesh)

# eag fit ----

## Null
# eag_fit_null <- sdmTMB(
#   cpue ~ 0 + as.factor(year),
#   data = eag,
#   mesh = eag_mesh,
#   time = "year",
#   spatial = "on",
#   spatiotemporal = "ar1",
#   family = tweedie(link = "log")
# )
# saveRDS(eag_fit_null, "./AIGKC/output/cpue_std/2025/jan/eag_fit_null.RDS")
eag_fit_null <- readRDS("./AIGKC/output/cpue_std/2025/jan/eag_fit_null.RDS")

## Null + adfg
# eag_fit_1 <- sdmTMB(
#   cpue ~ adfg + 0 + fyear,
#   data = eag,
#   mesh = eag_mesh,
#   time = "year",
#   spatial = "on",
#   spatiotemporal = "ar1",
#   family = tweedie(link = "log")
# )
# saveRDS(eag_fit_1, "./AIGKC/output/cpue_std/2025/jan/eag_fit_1.RDS")
eag_fit_1 <- readRDS("./AIGKC/output/cpue_std/2025/jan/eag_fit_1.RDS")

## Null + adfg + gearcode
# eag_fit_2 <- sdmTMB(
#   cpue ~ adfg + gearcode + 0 + as.factor(year),
#   data = eag,
#   mesh = eag_mesh,
#   time = "year",
#   spatial = "on",
#   spatiotemporal = "ar1",
#   family = tweedie(link = "log")
# )
# saveRDS(eag_fit_2, "./AIGKC/output/cpue_std/2025/jan/eag_fit_2.RDS")
eag_fit_2 <- readRDS("./AIGKC/output/cpue_std/2025/jan/eag_fit_2.RDS")

## Null + adfg + gearcode + soaktime
# eag_fit_3 <- sdmTMB(
#   cpue ~ s(soaktime) + adfg + gearcode + 0 + as.factor(year),
#   data = eag,
#   mesh = eag_mesh,
#   time = "year",
#   spatial = "on",
#   spatiotemporal = "ar1",
#   family = tweedie(link = "log")
# )
# saveRDS(eag_fit_3, "./AIGKC/output/cpue_std/2025/jan/eag_fit_3.RDS")
eag_fit_3 <- readRDS("./AIGKC/output/cpue_std/2025/jan/eag_fit_3.RDS")

## Null + adfg + gearcode + soaktime + depth
eag_fit_full <- sdmTMB(
  cpue ~ s(depth) + s(soaktime) + adfg + gearcode + 0 + fyear,
  data = eag,
  mesh = eag_mesh,
  time = "year",
  spatial = "on",
  spatiotemporal = "ar1",
  family = tweedie(link = "log")
)
saveRDS(eag_fit_full, "./AIGKC/output/cpue_std/2025/jan/eag_fit_full.RDS")
eag_fit_full <- readRDS("./AIGKC/output/cpue_std/2025/jan/eag_fit_full.RDS")

## Null + adfg + gearcode + soaktime + depth
# eag_fit_full_iid <- sdmTMB(
#   cpue ~ s(depth) + s(soaktime) + adfg + gearcode + 0 + fyear,
#   data = eag %>% mutate(fyear = factor(year)),
#   mesh = eag_mesh,
#   spatial = "on",
#   time = "year",
#   family = tweedie(link = "log")
# )
# saveRDS(eag_fit_full_iid, "./AIGKC/output/cpue_std/2025/jan/eag_fit_full_iid.RDS")
# eag_fit_full_iid <- readRDS("./AIGKC/output/cpue_std/2025/jan/eag_fit_full_iid.RDS")

# dharma residuals ----

# full st model
eag_full_tw_sim <- simulate(eag_fit_full, nsim = 500, type = "mle-mvn")
dharma_eag_full <- dharma_residuals(eag_full_tw_sim, eag_fit_full, return_DHARMa = TRUE)
# make qq plot of simulatied residuals
qq <- patchwork::wrap_elements(panel = ~plotQQunif(dharma_eag_full, testUniformity = F, testOutliers = F, testDispersion = F, cex = 0.2), clip = F)
# plot residuals against ranked model predictions
rf <- patchwork::wrap_elements(panel = ~plotResiduals(dharma_eag_full, smoothScatter = F, cex = 0.2, pch = 16), clip = F)
ggsave("./AIGKC/figures/cpue_std/2025/jan/dharma_eag_full.png", plot = (qq + rf), height = 4, width = 8, units = "in")

# full st model
eag_full_iid_tw_sim <- simulate(eag_fit_full_iid, nsim = 500, type = "mle-mvn")
dharma_eag_full_iid <- dharma_residuals(eag_full_iid_tw_sim, eag_fit_full_iid, return_DHARMa = TRUE)
# make qq plot of simulatied residuals
qq <- patchwork::wrap_elements(panel = ~plotQQunif(dharma_eag_full_iid, testUniformity = F, testOutliers = F, testDispersion = F, cex = 0.2), clip = F)
# plot residuals against ranked model predictions
rf <- patchwork::wrap_elements(panel = ~plotResiduals(dharma_eag_full_iid, smoothScatter = F, cex = 0.2, pch = 16), clip = F)
ggsave("./AIGKC/figures/cpue_std/2025/jan/dharma_eag_full_iid.png", plot = (qq + rf), height = 4, width = 8, units = "in")



# marginal effects ----

# gearcode
ggeffect(eag_fit_full, terms = "gearcode[all]") %>% 
  as_tibble() %>%
  mutate(x = case_when(x == 5 ~ "5x5",
                       x == 6 ~ "6x6",
                       x == 7 ~ "7x7",
                       x == 12 ~ "Round")) %>%
  ggplot()+
  geom_errorbar(aes(x = x, ymin = conf.low, ymax = conf.high), width = 0)+
  geom_point(aes(x = x, y = predicted), fill = "black")+
  labs(x = "Pot Size", y = "CPUE")  -> p1
ggsave("./AIGKC/figures/cpue_std/2025/jan/eag_full_gearcode_effect.png", plot = p1, height = 3, width = 4, units = "in")

# adfg
ggeffect(eag_fit_full, terms = c("adfg[all]")) %>% as_tibble() %>%
  left_join(eag %>%
              rename(x = adfg) %>% count(x) ) %>%
  arrange(n) %>%
  mutate(x = factor(x, levels = .$x)) %>%
  ggplot()+
  geom_errorbar(aes(x = x, ymin = conf.low, ymax = conf.high), width = 0)+
  geom_point(aes(x = x, y = predicted), fill = "black")+
  labs(x = "Vessel (ADFG Number)", y = "CPUE")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) -> p2
ggsave("./AIGKC/figures/cpue_std/2025/jan/eag_full_adfg_effect.png", plot = p2, height = 3, width = 4, units = "in")

# soaktime - conditional effect
visreg::visreg(eag_fit_full, xvar = "soaktime", plot = F)$fit %>%
  ggplot()+
  geom_ribbon(aes(x = soaktime, ymin = visregLwr, ymax = visregUpr), fill = "grey90")+
  geom_line(aes(x = soaktime, y = visregFit))+
  scale_x_continuous(labels = scales::comma)+
  labs(x = "Soak Time (hrs)", y = "f(Soak Time)") -> p3
ggsave("./AIGKC/figures/cpue_std/2025/jan/eag_full_soaktime_effect.png", plot = p3, height = 3, width = 4, units = "in")

#depth - conditional effect
visreg::visreg(eag_fit_full, xvar = "depth", plot = F)$fit %>%
  ggplot()+
  geom_ribbon(aes(x = depth, ymin = visregLwr, ymax = visregUpr), fill = "grey90")+
  geom_line(aes(x = depth, y = visregFit))+
  scale_x_continuous(labels = scales::comma)+
  labs(x = "Depth (fa)", y = "f(Depth)") -> p4
ggsave("./AIGKC/figures/cpue_std/2025/jan/eag_full_depth_effect.png", plot = p4, height = 3, width = 4, units = "in")

ggsave("./AIGKC/figures/cpue_std/2025/jan/eag_full_covar_effects.png", plot = (p1 + p2) / (p3 + p4), height = 8, width = 8, units = "in")


# make predictions ----

# prediction grid for full  model
full_grid <- expand_grid(lon = seq(min(eag_fit_full$data$lon), max(eag_fit_full$data$lon), by = 10),
                       lat = seq(min(eag_fit_full$data$lat), max(eag_fit_full$data$lat), by = 10),
                       depth = mean(eag_fit_full$data$depth),
                       soaktime = mean(eag_fit_full$data$soaktime),
                       eag_fit_full$data %>%
                         distinct(fyear,year, gearcode, adfg))
# null predictions
null_pred <- predict(eag_fit_null, 
                     newdata = full_grid %>% distinct(lon, lat, year), 
                     return_tmb_object = T)

# eag1 predictions
eag1_pred <- predict(eag_fit_1, 
                     newdata = full_grid %>% distinct(lon, lat, year, adfg), 
                     return_tmb_object = T)

# eag2 predictions
eag2_pred <- predict(eag_fit_2, 
                     newdata = full_grid %>% distinct(lon, lat, year, adfg, gearcode), 
                     return_tmb_object = T)

# eag3 predictions
eag3_pred <- predict(eag_fit_3, 
                     newdata = full_grid %>% distinct(lon, lat, year, adfg, gearcode, soaktime), 
                     return_tmb_object = T)

# full model predictions                        
full_pred <- predict(eag_fit_full, newdata = full_grid, return_tmb_object = T)

# full_s model predictions                        
full_s_pred <- predict(eag_fit_full_s, newdata = full_grid, return_tmb_object = T)

# spatial effects ----

# est
ggplot(full_pred$data)+
  geom_raster(aes(x = lon, y = lat, fill = exp(est)))+
  geom_sf(data = eag_land)+
  scale_fill_viridis_c()+
  coord_sf(expand = 0, ylim = c(NA, max(full_pred$data$lat)), xlim = c(NA, max(full_pred$data$lon)))+
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
ggsave("./AIGKC/figures/cpue_std/2025/jan/eag_full_spatial_effect.png", plot = x, height = 8, width = 8, units = "in")

# omega_s
ggplot(full_pred$data)+
  geom_raster(aes(x = lon, y = lat, fill = omega_s))+
  geom_sf(data = eag_land)+
  scale_fill_gradient2()+
  coord_sf(expand = 0, ylim = c(NA, max(full_pred$data$lat)), xlim = c(NA, max(full_pred$data$lon)))+
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
ggsave("./AIGKC/figures/cpue_std/2025/jan/eag_full_spatial_ranef.png", plot = x, height = 8, width = 8, units = "in")

# epsilon_st
ggplot(full_pred$data)+
  geom_raster(aes(x = lon, y = lat, fill = epsilon_st))+
  geom_sf(data = eag_land)+
  scale_fill_gradient2()+
  coord_sf(expand = 0, ylim = c(NA, max(full_pred$data$lat)), xlim = c(NA, max(full_pred$data$lon)))+
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
ggsave("./AIGKC/figures/cpue_std/2025/jan/eag_full_spatiotemporal_ranef.png", plot = x, height = 8, width = 8, units = "in")

ggplot(full_s_pred$data)+
  geom_raster(aes(x = lon, y = lat, fill = epsilon_st))+
  geom_sf(data = eag_land)+
  scale_fill_gradient2()+
  coord_sf(expand = 0, ylim = c(NA, max(full_pred$data$lat)), xlim = c(NA, max(full_pred$data$lon)))+
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
ggsave("./AIGKC/figures/cpue_std/2025/jan/eag_full_s_spatiotemporal_ranef.png", plot = x, height = 8, width = 8, units = "in")


# extract index ----

# null index
index_null <- get_index(null_pred, area = 1, bias_correct = T)
# scale the index
index_null %>%
  mutate(index = exp(log_est - mean(log_est)),
         cv = se / log_est,
         l95 = index * exp(-1.96 * sqrt(log(1 + cv^2))),
         u95 = index * exp(1.96 * sqrt(log(1 + cv^2))),
         var = "Year",
         order = 1) -> eag_index_null

# null + adfg index
index_eag1 <- get_index(eag1_pred, area = 1, bias_correct = T)
# scale the index
index_eag1 %>%
  mutate(index = exp(log_est - mean(log_est)),
         cv = se / log_est,
         l95 = index * exp(-1.96 * sqrt(log(1 + cv^2))),
         u95 = index * exp(1.96 * sqrt(log(1 + cv^2))),
         var = "+ Vessel",
         order = 2) -> eag1_index

# null + adfg + gearcode index
index_eag2 <- get_index(eag2_pred, area = 1, bias_correct = T)
# scale the index
index_eag2 %>%
  mutate(index = exp(log_est - mean(log_est)),
         cv = se / log_est,
         l95 = index * exp(-1.96 * sqrt(log(1 + cv^2))),
         u95 = index * exp(1.96 * sqrt(log(1 + cv^2))),
         var = "+ Gear",
         order = 3) -> eag2_index

# null + adfg + gearcode + soaktime index
index_eag3 <- get_index(eag3_pred, area = 1, bias_correct = T)
# scale the index
index_eag3 %>%
  mutate(index = exp(log_est - mean(log_est)),
         cv = se / log_est,
         l95 = index * exp(-1.96 * sqrt(log(1 + cv^2))),
         u95 = index * exp(1.96 * sqrt(log(1 + cv^2))),
         var = "+ s(soaktime)",
         order = 4) -> eag3_index

# full index
index_full <- get_index(full_pred, area = 1, bias_correct = T)
# scale the index
index_full %>%
  mutate(index = exp(log_est - mean(log_est)),
         cv = se / log_est,
         l95 = index * exp(-1.96 * sqrt(log(1 + cv^2))),
         u95 = index * exp(1.96 * sqrt(log(1 + cv^2))),
         var = "+ s(depth)",
         order = 5) -> eag_index_full
# write and read index by model
# bind_rows(eag_index_null, eag1_index, eag2_index, eag3_index, eag_index_full) %>%
#   write_csv("./AIGKC/output/cpue_std/2025/jan/eag_st_index.RDS")
eag_index <- read_csv("./AIGKC/output/cpue_std/2025/jan/eag_st_index.RDS")

# step plot
tibble(mod1 = c(NA, "Year", "+ Vessel", "+ Gear", "+ s(soaktime)"),
       mod2 = c("Year", "+ Vessel", "+ Gear", "+ s(soaktime)", "+ s(depth)"),
       data = list(eag_index, eag_index, eag_index, eag_index, eag_index)) %>% #pull(data) %>% .[[2]] -> data
  mutate(data = purrr::pmap(list(mod1, mod2, data), function(mod1, mod2, data) {
    data %>%
      filter(order <= order[var == mod2]) %>% 
      mutate(dotted_mod = order == (order[var == mod2] - 1)) 
  })) %>%
  unnest(data) %>%
  mutate(mod2 = factor(mod2, levels = c("Year", "+ Vessel", "+ Gear", "+ s(soaktime)", "+ s(depth)"))) %>% 
  #print(n = 1000)
  
  ggplot()+
  geom_line(data = . %>% filter(dotted_mod == F, var != mod2), aes(x = year, y = index, group = var), color = "grey90")+
  geom_line(data = . %>% filter(dotted_mod == T), aes(x = year, y = index, group = var), linetype = 2, color = "grey40")+
  geom_line(data = . %>% filter(var == mod2),  aes(x = year, y = index, group = var))+
  facet_wrap(~mod2, ncol = 1)+
  labs(x = NULL, y = "Index")+
  geom_text_npc(aes(npcx = "right", npcy = 0.9, label = mod2),
                check_overlap = T, size = 4)+
  scale_x_continuous(breaks = gmacsr::yraxis$breaks, labels = gmacsr::yraxis$labels)+
  theme(panel.spacing.x = unit(0, "lines"),
        panel.spacing.y = unit(0, "lines"),
        panel.border = element_rect(color = "grey30", fill = NA),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        panel.background = element_blank()) -> x
ggsave("./AIGKC/figures/cpue_std/2025/jan/eag_step_index.png", plot = x, height = 8, width = 5, units = "in")

# full index
index_full_s <- get_index(full_s_pred, area = 1, bias_correct = T)
# scale the index
index_full_s %>%
  mutate(index = exp(log_est - mean(log_est)),
         cv = se / log_est,
         l95 = index * exp(-1.96 * sqrt(log(1 + cv^2))),
         u95 = index * exp(1.96 * sqrt(log(1 + cv^2))),
         var = "+ s(depth)",
         order = 5) -> eag_index_full_s


ggplot()+
  geom_line(data = eag_index_full, aes(x = year, y = index))+
  geom_line(data = eag_index_full_s, aes(x = year, y = index), color = 2)



#gam <- read_csv("./AIGKC/output/cpue_std/2025/sept/eag_index.csv")


eag_index %>%
  ggplot()+
  geom_line(aes(x = year, y = index, color = type))


eag %>% 
  count(year, gearcode) %>%
  group_by(year) %>% 
  mutate(prop = n / sum(n)) %>%
  ggplot()+
  geom_bar(aes(x = year, y = prop, fill = gearcode), stat = "identity", position = "stack")
  #geom_boxplot(aes(x = gearcode, y = cpue))+
  facet_wrap(~adfg)
  
eag %>%
  ggplot()+
  geom_point(aes(x = lon, y = lat, color = gearcode))+
  facet_wrap(~year)
