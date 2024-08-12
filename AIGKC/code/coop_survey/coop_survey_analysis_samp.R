# notes ----

## aigkc co-op survey
## tyler jackson
## 11/17/2023

# load ----

source("./AIGKC/code/aigkc_functions.R")
library(gamm4)
library(ggpmisc)
library(sf)
library(glmmTMB)
library(tweedie)

# ggplot# ggplot# ggplot year axis labels
yraxis <- tickr(tibble(yr = 1950:2100), yr, 2)

# data ----

survey_raw <- read_csv("./AIGKC/data/coop_survey/aigkcCrabData.csv")

# subareas sf object
sub <- readRDS("./AIGKC/data/maps/coop_survey_subareas/coop_survey_subareas.RDS")
# map data
ai <- raster::getData("GADM", country = c("USA"), level = 1, path = "./AIGKC/data/maps")
ai@data <- filter(ai@data, NAME_1 == "Alaska")


# data mgmt ----

# make pot data
survey_raw %>% 
  filter(grepl("large", mesh_size)) %>%
  # make unknown pot size its own group
  # mid location and mid depth
  mutate(pot_size = ifelse(is.na(pot_size), "Unknown", pot_size),
         lat = (start_lat + end_lat) / 2,
         lon = (start_lon + end_lon) / 2,
         depth = (depth_start + depth_out) / 2) %>% 
  # correct some longitudes
  # add string id
  mutate(lon = ifelse(lon < -180, 180 - (-180 - lon), lon),
         string_id = paste0(year, adfg, string, lon, lat)) %>%
  # get only 1 line per pot
  dplyr::select(-subsample_rate, -species, -sex, -size, -size_lower5mm, -legal) %>%
  distinct -> pots
           
# cpue data
survey_raw %>% 
  # filter for only large mesh pots
  filter(grepl("large", mesh_size),
         species == 923,
         sex == 1) %>%
  # make unknown pot size its own group
  # mid location and mid depth
  mutate(pot_size = ifelse(is.na(pot_size), "Unknown", pot_size),
         lat = (start_lat + end_lat) / 2,
         lon = (start_lon + end_lon) / 2,
         depth = (depth_start + depth_out) / 2) %>% 
  # correct some longitudes
  # add string id
  mutate(lon = ifelse(lon < -180, 180 - (-180 - lon), lon),
         string_id = paste0(year, adfg, string, lon, lat)) %>%
  # get pot cpue
  group_by(fishery, year, adfg, string, pot_size, pot, pot_id, soaktime, depth, lat, lon, string_id) %>% 
  summarise(n_males = sum(subsample_rate),
            n_legal = sum(subsample_rate * (legal == 1))) %>%
  ungroup %>%
  right_join(pots %>%
              dplyr::select(fishery, year, adfg, string, pot_size, pot, pot_id, string_id, soaktime, depth, lat, lon)) %>%
  # fill in zeros
  replace_na(list(n_males = 0, n_legal = 0)) -> pot_cpue

# add subarea   
pot_cpue %>%
  filter(!is.na(lon), !is.na(lat)) %>%
  transmute(lon, lat) %>%
  sf::st_as_sf(coords = c("lon", "lat")) %>%
  sf::st_set_crs(sf::st_crs(sub)) %>%
  mutate(subarea = sapply(sf::st_intersects(.,sub), function(z) if (length(z)==0) NA_integer_ else sub$subarea[z[1]])) %>%
  pull(subarea) %>%
  mutate(pot_cpue%>%
           filter(!is.na(lon), !is.na(lat)), subarea = .)  %>%
  # correct subarea when missing
  mutate(subarea = case_when(!is.na(subarea) ~ subarea,
                             (is.na(subarea) & lon > 170 & lon < 176) ~ "wag_west",
                             (is.na(subarea) & lon > -170.5 & lon < 0) ~ "eag_east",
                             (is.na(subarea) & lon > -171.6 & lon < -170.5) ~ "eag_central",
                             (is.na(subarea) & lon > -174 & lon < 171.6) ~ "eag_west")) -> pot_cpue

# plots pot locations ----

# string locations
pot_cpue %>%
  filter(fishery == "EAG") %>%
  st_as_sf(coords = c("lon", "lat")) %>%
  st_set_crs(st_crs(sub)) -> pts
# pot locations > 25 days
pot_cpue %>%
  filter(fishery == "EAG",
         soaktime > 25) %>%
  st_as_sf(coords = c("lon", "lat")) %>%
  st_set_crs(st_crs(sub)) -> pts_rm

# land
ai@data <- do.call("rbind", replicate(51, ai@data, simplify = FALSE))
st_as_sf(ai) -> land

# plot
ggplot()+
  geom_sf(data = land)+
  geom_sf(data = sub %>% filter(grepl("eag", subarea)), aes(fill = subarea), color = NA, alpha = 0.3, show.legend = F)+
  geom_sf(data = pts %>% mutate(Vessel = factor(adfg)), aes(shape = Vessel))+
  coord_sf(xlim = c(-173.9, -167.8), ylim = c(51.8, 53.5))+
  facet_wrap(~year, ncol = 2, dir = "v")+
  theme(legend.position = "bottom") -> p1

ggsave("./AIGKC/figures/coop_survey/2025/sept/eag_string_map.png",
       plot = p1,
       height = 8, width = 7, units = "in")

# plot
ggplot()+
  geom_sf(data = land)+
  geom_sf(data = sub %>% filter(grepl("eag", subarea)), aes(fill = subarea), color = NA, alpha = 0.3, show.legend = F)+
  geom_sf(data = pts %>% filter(year == 2022))+
  geom_sf(data = pts_rm, color = "firebrick")+
  coord_sf(xlim = c(-173.9, -167.8), ylim = c(51.8, 53.5))+
  facet_wrap(~year, ncol = 2, dir = "v")+
  theme(legend.position = "bottom") -> p1

ggsave("./AIGKC/figures/coop_survey/2025/sept/eag_string_map_removed_pots.png",
       plot = p1,
       height = 3, width = 5, units = "in")

# annual nominal index ----

# eag
pot_cpue %>%
  filter(fishery == "EAG",
         soaktime <= 25) %>%
  group_by(fishery, year, subarea, string_id, soaktime) %>%
  summarise(cpue = mean(n_males, na.rm = T),
            var = var(n_males, na.rm = T) / n(),
            n = n()) %>% 
  mutate(var = ifelse(n == 1, 0, var)) %>%
  group_by(fishery, subarea, year) %>%
  summarise(cpue = mean(cpue),
            var = sum(var) / (n()^2)) %>%
  ungroup %>%
  group_by(fishery, year) %>%
  summarise(cpue = mean(cpue),
            var = sum(var) / (n()^2)) %>%
  ungroup %>%
  mutate(index = cpue / (prod(cpue)^(1/n())),
         cv = sqrt(var) / cpue) -> survey_index

write_csv(survey_index, "./AIGKC/output/coop_survey/2025/sept/survey_index.csv")


# gamm based index ----

pot_cpue %>%
  mutate(subarea = factor(subarea),
         string_id = factor(string_id),
         year = factor(year)) %>%
  # filter for EAG
  filter(fishery == "EAG") -> fit_dat
# fit model
gamm4(n_males ~ year + s(depth) + s(soaktime), random = ~(1| subarea/string_id), family = negbin(1), data = fit_dat) -> step1
summary(step1$gam)
ml_theta <- theta.ml(step1$gam$model$n_males, fitted(step1$gam))
gamm4(n_males ~ year + s(depth) + s(soaktime), random = ~(1| subarea/string_id), family = negbin(ml_theta), data = fit_dat) -> step2
ml_theta <- theta.ml(step2$gam$model$n_males, fitted(step2$mer))
gamm4(n_males ~ year + s(depth) + s(soaktime), random = ~(1| subarea/string_id), family = negbin(ml_theta), data = fit_dat) -> step2
summary(step2$gam)

# dharma residuals
simr <- simulateResiduals(fittedModel = step2$mer, plot = F, n = 1000)
qq <- patchwork::wrap_elements(panel = ~plotQQunif(simr, testUniformity = F, testOutliers = F, testDispersion = F, cex = 0.2), clip = F)
rf <- patchwork::wrap_elements(panel = ~plotResiduals(simr, smoothScatter = F, cex = 0.2, pch = 16), clip = F)
ggsave("./AIGKC/figures/coop_survey/2025/sept/gamm_nb_dharma.png", plot = (qq / rf), height = 8, width = 5, units = "in")

# depth
plot(sm(getViz(step2$gam), select = 1))+
  l_points(color = "grey70", alpha = 1)+
  l_fitLine()+
  l_ciLine()+
  labs(x = "Depth", y = "f(Depth, 2.58)") -> dep
ggsave("./AIGKC/figures/coop_survey/2025/sept/gamm_nb_depth.png",
       plot = gridPrint(dep, ncol = 1),
       height = 3, width = 5, units = "in")

# soaktime
plot(sm(getViz(step2$gam), select = 2))+ 
  l_points(color = "grey70", alpha = 1)+
  l_fitLine()+
  l_ciLine()+
  labs(x = "Soak Time (Days)", y = "f(Soak Time, 3.72)") -> st
ggsave("./AIGKC/figures/coop_survey/2025/sept/gamm_nb_soaktime.png",
       plot = gridPrint(st, ncol = 1),
       height = 3, width = 5, units = "in")

# plot residuals as function of linear predictors
fit_dat %>%
  filter(!is.na(depth), !is.na(soaktime)) -> rdat
rdep <- patchwork::wrap_elements(panel = ~plotResiduals(simr, rdat$depth, smoothScatter = F, cex = 0.2, pch = 16), clip = F)
rst <- patchwork::wrap_elements(panel = ~plotResiduals(simr, rdat$soaktime, smoothScatter = F, cex = 0.2, pch = 16), clip = F)
ggsave("./AIGKC/figures/coop_survey/2025/sept/gamm_nb_dharma_vars.png", plot = (rdep / rst), height = 8, width = 5, units = "in")


# fit model with 'standardized' soaktime
std_dat <- fit_dat %>% filter(!is.na(depth), !is.na(soaktime), soaktime < 25)
gamm4(n_males ~ year + s(depth) + s(soaktime), random = ~(1| subarea/string_id), family = negbin(ml_theta), 
      data = std_dat) -> gamm_upd
ml_theta <- theta.ml(gamm_upd$gam$model$n_males, fitted(gamm_upd$mer))
gamm4(n_males ~ year + s(depth) + s(soaktime), random = ~(1| subarea/string_id), family = negbin(ml_theta), 
      data = std_dat) -> gamm_upd

# dharma residuals
simr <- simulateResiduals(fittedModel = gamm_upd$mer, plot = F, n = 1000)
qq <- patchwork::wrap_elements(panel = ~plotQQunif(simr, testUniformity = F, testOutliers = F, testDispersion = F, cex = 0.2), clip = F)
rf <- patchwork::wrap_elements(panel = ~plotResiduals(simr, smoothScatter = F, cex = 0.2, pch = 16), clip = F)
ggsave("./AIGKC/figures/coop_survey/2025/sept/gamm_upd_dharma.png", plot = (qq / rf), height = 8, width = 5, units = "in")

# switch to glmm
## neg binomial
glmmTMB(n_males ~ year + depth + soaktime + (1| subarea/string_id), 
        family = nbinom2,
        data = std_dat) -> glmm_nb

# dharma residuals
simr <- simulateResiduals(fittedModel = glmm_nb, plot = F, n = 1000)
qq <- patchwork::wrap_elements(panel = ~plotQQunif(simr, testUniformity = F, testOutliers = F, testDispersion = F, cex = 0.2), clip = F)
rf <- patchwork::wrap_elements(panel = ~plotResiduals(simr, smoothScatter = F, cex = 0.2, pch = 16), clip = F)
ggsave("./AIGKC/figures/coop_survey/2025/sept/glmm_nb_dharma.png", plot = (qq / rf), height = 8, width = 5, units = "in")


## tweedie
glmmTMB(n_males ~ year + depth + soaktime + (1| subarea/string_id), 
        family = tweedie,
        data = std_dat) -> glmm_tw

# dharma residuals
simr <- simulateResiduals(fittedModel = glmm_tw, plot = F, n = 1000)
qq <- patchwork::wrap_elements(panel = ~plotQQunif(simr, testUniformity = F, testOutliers = F, testDispersion = F, cex = 0.2), clip = F)
rf <- patchwork::wrap_elements(panel = ~plotResiduals(simr, smoothScatter = F, cex = 0.2, pch = 16), clip = F)
ggsave("./AIGKC/figures/coop_survey/2025/sept/glmm_tw_dharma.png", plot = (qq / rf), height = 8, width = 5, units = "in")

# plot effect

## depth
vis_dep <- visreg(fit = glmm_tw, "depth")
ggplot()+
  geom_point(data = vis_dep$res, aes(x = depth, y = visregRes), size = 0.3, color = "grey70")+
  geom_ribbon(data = vis_dep$fit, aes(x = depth, ymin = visregLwr, ymax = visregUpr), 
              fill = NA, color = 1, linetype = 2, alpha = 0.4)+
  geom_line(data = vis_dep$fit, aes(x = depth, y = visregFit))+
  labs(x = "Depth (fa)", y = "f(Depth)") -> x
ggsave("./AIGKC/figures/coop_survey/2025/sept/glmm_tw_depth.png",
       plot = x,
       height = 3, width = 5, units = "in")

## soaktime
vis_soak <- visreg(fit = glmm_tw, "soaktime")
ggplot()+
  geom_point(data = vis_soak$res, aes(x = soaktime, y = visregRes), size = 0.3, color = "grey70")+
  geom_ribbon(data = vis_soak$fit, aes(x = soaktime, ymin = visregLwr, ymax = visregUpr), 
              fill = NA, color = 1, linetype = 2, alpha = 0.4)+
  geom_line(data = vis_soak$fit, aes(x = soaktime, y = visregFit))+
  labs(x = "Soak Time (days)", y = "f(Soak Time)") -> x
ggsave("./AIGKC/figures/coop_survey/2025/sept/glmm_tw_soaktime.png",
       plot = x,
       height = 3, width = 5, units = "in")

# extract index ----

loc <- grep("year", rownames(summary(glmm_tw)$coefficients$cond))
yrs <- unique(fit_dat$year)
f_getCPUE_glmm(glmm_tw, loc, yrs) -> glmm_index

write_csv(glmm_index, "./AIGKC/output/coop_survey/2025/sept/glmm_survey_index.csv")

# plot of index ----

# design based index
survey_index %>%
  filter(fishery == "EAG") %>%
  transmute(year = factor(year), index, se = cv * index, 
            l95 = index + -1.96 * se, u95 = index + 1.96 * se,
            type = "Nominal") %>%
  # gamm based index
  bind_rows(glmm_index %>% mutate(type = "GLMM")) %>%
  mutate(year = as.numeric(as.character(year))) %>%
  add_row(year = 2020, type = c("Nominal", "GLMM")) %>%
  # plot
  ggplot()+
  geom_ribbon(aes(x = year, ymin = l95, ymax = u95, group = type, fill = type), alpha = 0.1)+
  geom_line(aes(x = year, y = index, group = type, color = type))+
  labs(x = NULL, y = "Survey Index", fill = NULL, color = NULL)+
  scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
  theme(legend.justification = c(1, 1),
        legend.position = c(1, 1)) -> p1

ggsave("./AIGKC/figures/coop_survey/2025/sept/index_plot.png",
       plot = p1,
       height = 3, width = 5, units = "in")


glmm_index %>% mutate(type = "Survey") %>%
  bind_rows(read_csv("./AIGKC/output/cpue_std/2024/may/post_eag_index.csv") %>%
              mutate(year = as.character(year),
                     type = "Observer")) %>% 
  
  ggplot()+
  geom_ribbon(aes(x = year, ymin = l95, ymax = u95, fill = type, group = type), alpha = 0.3)+
  geom_line(aes(x = year, y = index, color = type, group = type))+
  labs(x = NULL, y = "CPUE Index", fill = NULL, color = NULL)+
  scale_x_discrete(breaks = yraxis$breaks, labels = yraxis$labels)

obs_mod <- read_rds("./AIGKC/output/cpue_std/2024/may/post_eag_std_tw.rds")[[1]]
obs_data <- read_rds("./AIGKC/output/cpue_std/2024/may/post_eag_std_tw.rds")[[1]]$model
obs_mod_short <- update(obs_mod, data = obs_data %>% filter(crab_year %in% 2015:2023))
f_getCPUE_gam(obs_mod_short,
              where = 2:9,
              years = 2015:2023) -> obs_index


glmm_index %>% mutate(type = "Survey", year = as.numeric(as.character(year))) %>%
  bind_rows(read_csv("./AIGKC/output/cpue_std/2024/may/post_eag_index.csv") %>%
              mutate(type = "Observer")) %>%
  bind_rows(obs_index %>% mutate(type = "Observer 2015 - 2023")) %>%
  
  ggplot()+
  geom_ribbon(aes(x = year, ymin = l95, ymax = u95, fill = type, group = type), alpha = 0.3)+
  geom_line(aes(x = year, y = index, color = type, group = type))+
  labs(x = NULL, y = "CPUE Index", fill = NULL, color = NULL)+
  scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
  theme(legend.position = c(0, 1),
        legend.justification = c(0, 1)) -> x

ggsave("./AIGKC/figures/coop_survey/2025/sept/index_plot_w_obs.png",
       plot = x,
       height = 3, width = 5, units = "in")
  




  

# total male size composition ----

survey_raw %>% 
  filter(soaktime <= 25, sex == 1) %>%
  group_by(fishery, year, size) %>%
  summarise(n = sum(subsample_rate, na.rm = T),
            n_meas = n()) %>%
  mutate(total_n = sum(n),
         total_n_meas = sum(n_meas),
         prop = n / total_n) -> survey_size_comp
  
write_csv(survey_size_comp, "./AIGKC/output/coop_survey/survey_size_comp.csv")

# plot size composition EAG
survey_size_comp %>%
  # round size to 5 mm bins
  mutate(size = round(size / 5) * 5) %>%
  group_by(fishery, year, size) %>%
  summarise(prop = sum(prop)) %>%
  bind_rows(., 
            survey_size_comp %>%
              # round size to 5 mm bins
              mutate(size = round(size / 5) * 5) %>%
              group_by(fishery, year, size) %>%
              summarise(prop = sum(prop))  %>%
              mutate(size = size + 4.9999999,
                     edge = T)) %>%
  filter(fishery == "EAG") %>%
  ggplot()+
  geom_area(aes(x = size, y = prop), 
            position = position_dodge(width = 0), alpha = 0.5, show.legend = F)+
  geom_text_npc(aes(npcx = "left", npcy = 0.6, label = year),
                check_overlap = T, size = 3)+
  # geom_text_npc(aes(npcx = "right", npcy = 0.6, label = annotation),
  #               check_overlap = T, size = 3)+
  #scale_x_continuous(breaks = seq(0, 200, by = 20))+
  facet_wrap(~year, ncol = 1, dir = "v", )+
  labs(x = "\nCarapace Length (mm)", y = NULL, fill = NULL, color = NULL) +
  theme(panel.border= element_blank(),
        panel.spacing.y = unit(-0.5, "lines"),
        panel.spacing.x = unit(1, "lines"),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.line.y = element_blank(),
        axis.line.x = element_line(size = 0.1, color = "grey70"),
        axis.ticks.y = element_blank(),
        panel.background = element_blank()) -> p1

ggsave("./AIGKC/figures/coop_survey/2025/sept/size_comp_plot.png",
       plot = p1,
       height = 6, width = 5, units = "in")


# sample statistics ----

pot_cpue %>%
  filter(fishery == "EAG",
         soaktime <= 25) %>%
  mutate(vesstring = paste(adfg, string, year, sep = "_")) %>%
  group_by(year) %>%
  summarise(n_strings = length(unique(vesstring)),
            n_pots = n()) %>%
  left_join(survey_size_comp %>% filter(fishery == "EAG") %>% distinct(year, total_n_meas)) %>%
  transmute(year, n_strings, n_pots, total_n_meas) %>% ungroup %>%
  left_join(
  # join to proportion measured by size catagory
  survey_raw %>% 
  filter(fishery == "EAG", sex == 1, legal %in% 1:2) %>%
  group_by(fishery, year, legal) %>%
  summarise(n = sum(subsample_rate, na.rm = T),
            n_meas = n(),
            prop = round(n_meas / n, 2)) %>% ungroup %>%
  transmute(year, legal, prop) %>%
  pivot_wider(names_from = legal, values_from = prop) 
  ) %>%
  transmute(year, n_strings, n_pots, total_n_meas, legal = `1`, sublegal = `2`) %>%
  
  
  write_csv(., "./AIGKC/output/coop_survey/2025/sept/survey_sample_stats.csv")
  



# sampling issue ----

## oversampling pots with low cpue
pot_cpue %>%
  group_by(fishery, year, adfg, string_id) %>%
  add_count(name = "pots") %>%
  mutate(type = ifelse(pots > 8, "> 8 pots", "< 8 pots")) %>%
  
  ggplot()+
  geom_histogram(aes(x = n_males, y = ..density.., fill = type), alpha = 0.5, position = "identity")+
  facet_grid(vars(year), vars(fishery))+
  labs(x = "CPUE", y = "Density", fill = NULL)


