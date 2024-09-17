source("./gmacsr/gmacsr.R")
library(sf)
library(rnaturalearth); library(rnaturalearthdata)

# gmacs update and season swap ----

# eag
EAG23.1prev <- gmacs_read_allout("./AIGKC/models/2025/sept/EAG/23.1_prev/Gmacsall.out", "23.1 v2.01.M.10", version = "2.01.M.10")
EAG23.1gmacs <- gmacs_read_allout("./AIGKC/models/2025/sept/EAG/23.1_gmacs/Gmacsall.out", "23.1 v2.20.16")
EAG23.1season <- gmacs_read_allout("./AIGKC/models/2025/sept/EAG/23.1_season/Gmacsall.out", "23.1 season")
EAG23.1data <- gmacs_read_allout("./AIGKC/models/2025/sept/EAG/23.1_data/Gmacsall.out", "23.1 data")
# wag
WAG23.1prev <- gmacs_read_allout("./AIGKC/models/2025/sept/WAG/23.1_prev/Gmacsall.out", "23.1 v2.01.M.10", version = "2.01.M.10")
WAG23.1gmacs <- gmacs_read_allout("./AIGKC/models/2025/sept/WAG/23.1_gmacs/Gmacsall.out", "23.1 v2.20.16")
WAG23.1season <- gmacs_read_allout("./AIGKC/models/2025/sept/WAG/23.1_season/Gmacsall.out", "23.1 season")
WAG23.1data <- gmacs_read_allout("./AIGKC/models/2025/sept/WAG/23.1_data/Gmacsall.out", "23.1 data")

# likelihoods
gmacs_get_lik_type_pen(list(EAG23.1prev, EAG23.1gmacs, EAG23.1season, EAG23.1data)) %>%
  dplyr::slice(c(-8, -10:-11)) %>%
  write_csv("./AIGKC/output/models/2025/sept/eag_base_model_version_lik.csv")
gmacs_get_lik_type_pen(list(WAG23.1prev, WAG23.1gmacs, WAG23.1season, WAG23.1data)) %>%
  dplyr::slice(c(-8, -10:-11))  %>%
  write_csv("./AIGKC/output/models/2025/sept/wag_base_model_version_lik.csv")
  
# reference points
write_csv(gmacs_get_ref_points(list(EAG23.1prev, EAG23.1gmacs, EAG23.1season, EAG23.1data)), "./AIGKC/output/models/2025/sept/eag_base_model_version_ref.csv")
write_csv(gmacs_get_ref_points(list(WAG23.1prev, WAG23.1gmacs, WAG23.1season, WAG23.1data)), "./AIGKC/output/models/2025/sept/wag_base_model_version_ref.csv")

# 1993 observer data ----

potsum <- read_csv("./AIGKC/data/observer/2024_assessment/item1_linked_potsum_dump.csv")
obs_meas <- read_csv("./AIGKC/data/observer/2024_assessment/item12_linked_meas_dump.csv")

# land
ai <- raster::getData("GADM", country = c("USA"), level = 1, path = "./AIGKC/data/maps")
ai %>%
  st_as_sf() %>%
  filter(NAME_1 == "Alaska") %>%
  st_transform(3338) -> ai

# observer pots 1992-1994
potsum %>% 
  filter(subdistrict == "WAG", crab_year %in% 1992:1994) %>%
  filter(longitude < 0, longitude > -200, latitude < 55) %>%
  st_as_sf(coords = c("longitude", "latitude")) %>%
  st_set_crs("+proj=longlat +datum=WGS84 +no_defs") %>%
  st_transform(3338) -> pots
  
# observer pots 1993
potsum %>% 
  filter(subdistrict == "WAG", crab_year == 1993) %>%
  mutate(season = ifelse(sampdate < mdy("8/1/1993"), "1992/93", "1993/94")) %>%
  st_as_sf(coords = c("longitude", "latitude")) %>%
  st_set_crs("+proj=longlat +datum=WGS84 +no_defs") %>%
  st_transform(3338) -> pots1993

# compare observer pot location 1992-94
coord_lim <- st_bbox(pots$geometry)
ggplot()+
  geom_sf(data = ai)+
  geom_sf(data = pots, size = 0.2, color = cbpalette[2])+
  coord_sf(xlim = c(coord_lim[1], coord_lim[3]), ylim = c(coord_lim[2], coord_lim[4]))+
  facet_wrap(~crab_year, ncol = 1)+
  theme_bw() -> x
ggsave("./AIGKC/figures/models/2025/sept/1992_94_obs_pots.png", plot = x, height = 6, width = 5, units = "in")

# compare observer pot location by season 1993
tibble(x = c(-186, -174), y = c(53, 51)) %>%
  st_as_sf(coords = c("x", "y")) %>% 
  st_set_crs("+proj=longlat +datum=WGS84 +no_defs") %>%
  st_transform(3338) %>% st_bbox(.$geometry) -> coord_lim

ggplot()+
  geom_sf(data = ai)+
  geom_sf(data = pots1993, aes(color = season), size = 0.5)+
  coord_sf(xlim = c(coord_lim[1], coord_lim[3]), ylim = c(coord_lim[2], coord_lim[4]))+
  scale_color_manual(values = cbpalette[c(2, 5)])+
  labs(color = NULL)+
  theme_bw()+
  theme(legend.position = c(1, 1), 
        legend.justification = c(1, 1),
        legend.background = element_rect(fill = NULL, color = 1)) -> x
ggsave("./AIGKC/figures/models/2025/sept/1993_obs_pots.png", plot = x, height = 3, width = 5, units = "in")
  

# comparison of size comp data with an without removing gear code
# wag
gmacs_get_size_summary(list(WAG23.1season, WAG23.1data)) %>%
  filter(type == "total") %>%
  ggplot()+
  geom_bar(aes(x = size, y = obs, fill = model), position = "identity", stat = "identity", alpha = 0.5, width = 5)+
  geom_line(aes(x = size, y = pred, color = model, group = model))+
  labs(x = "Carapace Length (mm)", y = NULL, color = NULL, fill = NULL, title = "WAG Observed Total Composition")+
  geom_text_npc(aes(npcx = "right", npcy = 0.6, label = year), check_overlap = T, size = 3)+
  scale_fill_manual(values = cbpalette[4:3])+
  scale_color_manual(values = cbpalette[4:3])+
  facet_wrap(~year, ncol = 4, dir = "v")+
  theme(panel.spacing.x = unit(0.2, "lines"),
        panel.spacing.y = unit(0, "lines"),
        panel.border = element_blank(),
        axis.line.x = element_line(color = "grey70", size = 0.2),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 8),
        plot.title = element_text(hjust = 0.5),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        panel.background = element_blank(),
        title = element_text(hjust = 0.5)) -> x
ggsave("./AIGKC/figures/models/2025/sept/wag_total_comp.png", plot = x, height = 6, width = 7, units = "in") 

# eag
gmacs_get_size_summary(list(EAG23.1season, EAG23.1data)) %>%
  filter(type == "total") %>%
  ggplot()+
  geom_bar(aes(x = size, y = obs, fill = model), position = "identity", stat = "identity", alpha = 0.5, width = 5)+
  geom_line(aes(x = size, y = pred, color = model, group = model))+
  labs(x = "Carapace Length (mm)", y = NULL, color = NULL, fill = NULL, title = "EAG Observed Total Composition")+
  geom_text_npc(aes(npcx = "right", npcy = 0.6, label = year), check_overlap = T, size = 3)+
  scale_fill_manual(values = cbpalette[4:3])+
  scale_color_manual(values = cbpalette[4:3])+
  facet_wrap(~year, ncol = 4, dir = "v")+
  theme(panel.spacing.x = unit(0.2, "lines"),
        panel.spacing.y = unit(0, "lines"),
        panel.border = element_blank(),
        axis.line.x = element_line(color = "grey70", size = 0.2),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 8),
        plot.title = element_text(hjust = 0.5),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        panel.background = element_blank(),
        title = element_text(hjust = 0.5)) -> x
ggsave("./AIGKC/figures/models/2025/sept/eag_total_comp.png", plot = x, height = 6, width = 7, units = "in")

potsum %>%
  mutate(geartext = case_when(gearcode == -9 ~ "Unknown",
                              is.na(gearcode) ~ "Unknown",
                              gearcode == 1 ~ "Dungeness crab pot",
                              gearcode == 2 ~ "Pyramid pot",
                              gearcode == 3 ~ "Conical pot",
                              gearcode == 4 ~ "4' x 4'",
                              gearcode == 5 ~ "5' x 5'",
                              gearcode == 6 ~ "6' x 6'",
                              gearcode == 7 ~ "7' x 7'",
                              gearcode == 8 ~ "8' x 8'",
                              gearcode == 9 ~ "5.5' x 5.5'",
                              gearcode == 10 ~ "6.5' x 6.5'",
                              gearcode == 11 ~ "7.5' x 7.5'",
                              gearcode == 12 ~ "Large round pot",
                              gearcode == 13 ~ "10' x 10'",
                              gearcode == 14 ~ "9' x 9'",
                              gearcode == 15 ~ "8.5' x 8.5'",
                              gearcode == 16 ~ "9.5' x 9.5'",
                              gearcode == 17 ~ "8' x 9'",
                              gearcode == 18 ~ "8' x 10'",
                              gearcode == 19 ~ "9' x 10'",
                              gearcode == 20 ~ "7' x 8'",
                              gearcode == 21 ~ "Hair crab pot",
                              gearcode == 22 ~ "Snail pot",
                              gearcode == 23 ~ "Dome pot",
                              gearcode == 24 ~ "Research pot",
                              gearcode == 25 ~ "6.5' x 7'",
                              gearcode == 80 ~ "Cod pot",
                              gearcode == 81 ~ "Rectangular unkown size")) %>%
  count(subdistrict, crab_year, gearcode, geartext) %>%
  filter(subdistrict == "WAG", crab_year == 1993) 
  ggplot()+
  geom_bar(aes(x = geartext, y = n), stat = "identity")+
             facet_wrap(~crab_year)



# new total catch timeseries
gmacs_read_allout("./AIGKC/models/2025/sept/EAG/23.1_season/Gmacsall.out")$catch_fit_summary %>%
  filter(type == "All") %>%
  transmute(year, eag_sq = obs_catch) %>%
  full_join(gmacs_read_allout("./AIGKC/models/2025/sept/EAG/23.1_data/Gmacsall.out")$catch_fit_summary %>%
              filter(type == "All") %>%
              transmute(year, eag_upd = obs_catch)) %>%
  full_join(gmacs_read_allout("./AIGKC/models/2025/sept/WAG/23.1_season/Gmacsall.out")$catch_fit_summary %>%
              filter(type == "All") %>%
              transmute(year, wag_sq = obs_catch)) %>%
  full_join(gmacs_read_allout("./AIGKC/models/2025/sept/WAG/23.1_data/Gmacsall.out")$catch_fit_summary %>%
              filter(type == "All") %>%
              transmute(year, wag_upd = obs_catch)) %>%
  arrange(year) %>%
  write_csv("./AIGKC/output/models/2025/sept/updated_total_catch.csv")
gmacs_plot_catch(list(EAG23.1season, EAG23.1data), save_plot = F)[[3]]+
  ggtitle("EAG") + theme(plot.title = element_text(hjust = 0.5)) -> eag
gmacs_plot_catch(list(WAG23.1season, WAG23.1data), save_plot = F)[[3]]+
  ggtitle("WAG") + theme(plot.title = element_text(hjust = 0.5)) -> wag
ggsave("./AIGKC/figures/models/2025/sept/data_update_total_catch_fit.png", plot = eag / wag, height = 6, width = 7, units = "in") 




# recruit bias correction and initial conditions ----

EAG23.1 <- gmacs_read_allout("./AIGKC/models/2025/sept/EAG/23.1_data/Gmacsall.out", "23.1")
EAG23.1c <- gmacs_read_allout("./AIGKC/models/2025/sept/EAG/23.1c/Gmacsall.out", "23.1c")
EAG25.0 <- gmacs_read_allout("./AIGKC/models/2025/sept/EAG/25.0/Gmacsall.out", "25.0")

WAG23.1 <- gmacs_read_allout("./AIGKC/models/2025/sept/WAG/23.1_data/Gmacsall.out", "23.1")
WAG23.1c <- gmacs_read_allout("./AIGKC/models/2025/sept/WAG/23.1c/Gmacsall.out", "23.1c")
WAG25.0 <- gmacs_read_allout("./AIGKC/models/2025/sept/WAG/25.0/Gmacsall.out", "25.0")

## plot of mmb
gmacs_plot_mmb(list(EAG23.1, EAG23.1c, EAG25.0), save_plot = F)[[1]]+
  labs(title = "EAG")+theme(plot.title = element_text(hjust = 0.5)) -> eag
gmacs_plot_mmb(list(WAG23.1, WAG23.1c, WAG25.0), save_plot = F)[[1]]+
  labs(title = "WAG")+theme(plot.title = element_text(hjust = 0.5)) -> wag
ggsave("./AIGKC/figures/models/2025/sept/initial_conditions_mmb.png", plot = eag / wag, height = 5, width = 6, units = "in")

## plot of recruitment
gmacs_plot_recruitment(list(EAG23.1, EAG23.1c, EAG25.0), save_plot = F)+
  scale_y_continuous(limits = c(0, NA))+
  labs(title = "EAG")+theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position = c(0, 1), legend.justification = c(0, 1)) -> eag
gmacs_plot_recruitment(list(WAG23.1, WAG23.1c, WAG25.0), save_plot = F)+
  scale_y_continuous(limits = c(0, NA))+
  labs(title = "WAG")+theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position = c(0, 1), legend.justification = c(0, 1)) -> wag
ggsave("./AIGKC/figures/models/2025/sept/initial_conditions_recruitment.png", plot = eag / wag, height = 5, width = 6, units = "in")

gmacs_get_pars(list(EAG23.1, EAG23.1c, EAG25.0)) %>%
  filter(parameter %in% c("Log(R0):", "Log(Rinitial):", "Log(Rbar):"))


gmacs_get_lik(list(EAG23.1, EAG23.1c, EAG25.0))

gmacs_plot_index(list(EAG23.1, EAG23.1c, EAG25.0), save_plot = F)
EAG23.1c$penalties
EAG25.0$penalties

EAG23.1c$priorDensity
EAG25.0$priorDensity

# bootstrap neff ----

# plot of bootstrap stage 1 neff
boot_eag_ret <- lapply(grep("eag_retained_comp_boot", list.files("./AIGKC/output/observer", full.names = T), value = T), 
                       readRDS) %>% bind_rows %>%
  transmute(crab_year, fishery = "EAG", type = "Retained", n)
boot_eag_tot <- lapply(grep("eag_total_comp_boot", list.files("./AIGKC/output/observer", full.names = T), value = T), 
                       readRDS) %>% bind_rows %>%
  transmute(crab_year, fishery = "EAG", type = "Total", n)
boot_wag_ret <- lapply(grep("wag_retained_comp_boot", list.files("./AIGKC/output/observer", full.names = T), value = T), 
                       readRDS) %>% bind_rows %>%
  transmute(crab_year, fishery = "WAG", type = "Retained", n)
boot_wag_tot <- lapply(grep("wag_total_comp_boot", list.files("./AIGKC/output/observer", full.names = T), value = T), 
                       readRDS) %>% bind_rows %>%
  transmute(crab_year, fishery = "WAG", type = "Total", n)

boot_eag_ret %>%
  group_by(crab_year, fishery) %>%
  mutate(neff = mean(n)) %>%
  ggplot()+
  geom_violin(aes(x = factor(crab_year), y = n, group = crab_year), fill = cbpalette[4], color = "grey80", alpha = 0.3)+
  geom_line(aes(x = factor(crab_year), y = neff, group = 1))+
  labs(x = NULL, y = "Boot Effective N", title = "EAG Retained")+
  scale_y_continuous(labels = scales::comma)+
  scale_x_discrete(labels = yraxis$labels, breaks = yraxis$breaks)+
  coord_cartesian(ylim = c(0, 15000))+
  theme(plot.title = element_text(hjust = 0.5)) -> eag_ret_p

boot_eag_tot %>%
  group_by(crab_year, fishery) %>%
  mutate(neff = mean(n)) %>%
  ggplot()+
  geom_violin(aes(x = factor(crab_year), y = n, group = crab_year), fill = cbpalette[4], color = "grey80", alpha = 0.3)+
  geom_line(aes(x = factor(crab_year), y = neff, group = 1))+
  labs(x = NULL, y = "Boot Effective N", title = "EAG Total")+
  scale_y_continuous(labels = scales::comma)+
  scale_x_discrete(labels = yraxis$labels, breaks = yraxis$breaks)+
  coord_cartesian(ylim = c(0, 50000))+
  theme(plot.title = element_text(hjust = 0.5)) -> eag_tot_p

boot_wag_ret %>%
  group_by(crab_year, fishery) %>%
  mutate(neff = mean(n)) %>%
  ggplot()+
  geom_violin(aes(x = factor(crab_year), y = n, group = crab_year), fill = cbpalette[4], color = "grey80", alpha = 0.3)+
  geom_line(aes(x = factor(crab_year), y = neff, group = 1))+
  labs(x = NULL, y = "Boot Effective N", title = "WAG Retained")+
  scale_y_continuous(labels = scales::comma)+
  scale_x_discrete(labels = yraxis$labels, breaks = yraxis$breaks)+
  coord_cartesian(ylim = c(0, 15000))+
  theme(plot.title = element_text(hjust = 0.5)) -> wag_ret_p

boot_wag_tot %>%
  group_by(crab_year, fishery) %>%
  mutate(neff = mean(n)) %>%
  ggplot()+
  geom_violin(aes(x = factor(crab_year), y = n, group = crab_year), fill = cbpalette[4], color = "grey80", alpha = 0.3)+
  geom_line(aes(x = factor(crab_year), y = neff, group = 1))+
  labs(x = NULL, y = "Boot Effective N", title = "WAG Total")+
  scale_y_continuous(labels = scales::comma)+
  scale_x_discrete(labels = yraxis$labels, breaks = yraxis$breaks)+
  coord_cartesian(ylim = c(0, 50000))+
  theme(plot.title = element_text(hjust = 0.5)) -> wag_tot_p

ggsave("./AIGKC/figures/models/2025/sept/boot_neff.png", 
       plot = (eag_ret_p + eag_tot_p) / (wag_ret_p + wag_tot_p), height = 6, width = 9, units = "in")

# data weighting models 25.0a to 25.0b ----

EAG25.0 <- gmacs_read_allout("./AIGKC/models/2025/sept/EAG/25.0/Gmacsall.out", "25.0")
EAG25.0a <- gmacs_read_allout("./AIGKC/models/2025/sept/EAG/25.0a/Gmacsall.out", "25.0a")
EAG25.0b <- gmacs_read_allout("./AIGKC/models/2025/sept/EAG/25.0b/Gmacsall.out", "25.0b")
EAG25.0b2 <- gmacs_read_allout("./AIGKC/models/2025/sept/EAG/25.0b2/Gmacsall.out", "25.0b2")
EAG25.0c <- gmacs_read_allout("./AIGKC/models/2025/sept/EAG/25.0c/Gmacsall.out", "25.0c")
EAG25.0d <- gmacs_read_allout("./AIGKC/models/2025/sept/EAG/25.0d/Gmacsall.out", "25.0d")

WAG25.0 <- gmacs_read_allout("./AIGKC/models/2025/sept/WAG/25.0/Gmacsall.out", "25.0")
WAG25.0a <- gmacs_read_allout("./AIGKC/models/2025/sept/WAG/25.0a/Gmacsall.out", "25.0a")
WAG25.0b <- gmacs_read_allout("./AIGKC/models/2025/sept/WAG/25.0b/Gmacsall.out", "25.0b")
WAG25.0b2 <- gmacs_read_allout("./AIGKC/models/2025/sept/WAG/25.0b2/Gmacsall.out", "25.0b2")
WAG25.0c <- gmacs_read_allout("./AIGKC/models/2025/sept/WAG/25.0c/Gmacsall.out", "25.0c")
WAG25.0d <- gmacs_read_allout("./AIGKC/models/2025/sept/WAG/25.0d/Gmacsall.out", "25.0d")

# plot fits to data eag
gmacs_plot_catch(list(EAG25.0, EAG25.0a, EAG25.0b, EAG25.0b2, EAG25.0c, EAG25.0d), save_plot = F)
gmacs_get_index_summary(list(EAG25.0, EAG25.0a, EAG25.0b, EAG25.0b2, EAG25.0c, EAG25.0d)) %>%
  mutate(index = case_when(series == 1 ~ "Pre-Rationalized Observer",
                           series == 2 ~ "Post-Rationalized Observer",
                           series == 3 ~ "Early Fish Ticket"),
         index = factor(index, levels = c("Early Fish Ticket", "Pre-Rationalized Observer", "Post-Rationalized Observer"))) %>%
  mutate(obs_l95 = obs_index * exp(-1.96 * sqrt(log(1 + obs_cv^2))),
         obs_u95 = obs_index * exp(1.96 * sqrt(log(1 + obs_cv^2))),
         tot_l95 = obs_index * exp(-1.96 * sqrt(log(1 + tot_cv^2))),
         tot_u95 = obs_index * exp(1.96 * sqrt(log(1 + tot_cv^2)))) %>%
  ggplot()+
  geom_errorbar(aes(x = factor(year), ymin = tot_l95, ymax = tot_u95), width = 0, color = "grey70")+
  geom_errorbar(aes(x = factor(year), ymin = obs_l95, ymax = obs_u95), width = 0, color = "grey20")+
  geom_point(aes(x = factor(year), y = obs_index), color = "grey20")+
  geom_line(aes(x = factor(year), y = pred_index, group = model, color = model))+
  labs(x = NULL, color = NULL, y = "Standardized Index")+
  scale_y_continuous(labels = scales::comma)+
  scale_x_discrete(breaks = yraxis$breaks, labels = yraxis$labels)+
  scale_color_manual(values = cbpalette)+
  coord_cartesian(ylim = c(0, NA))+
  facet_wrap(~index, scales = "free_x", ncol = 2) -> x
ggsave(plot = x, filename = "./AIGKC/figures/models/2025/sept/eag_data_wt_index_fit.png", height = 6, width = 8, units = "in") 

gmacs_get_size_summary(list(EAG25.0, EAG25.0a,  EAG25.0b, EAG25.0b2, EAG25.0c, EAG25.0d)) %>% mutate(fishery = "EAG") %>%
  bind_rows(gmacs_get_size_summary(list(WAG25.0, WAG25.0a,  WAG25.0b, WAG25.0b2, WAG25.0c, WAG25.0d)) %>% mutate(fishery = "WAG")) %>%
  mutate(type = str_to_title(type)) %>%
  group_by(fishery, model, type, size) %>%
  summarise(obs = sum(obs), pred = sum(pred)) %>%
  ggplot()+
  geom_bar(aes(x = size, y = obs), fill = "grey70", position = "identity", stat = "identity", alpha = 0.1, width = 5)+
  geom_line(aes(x = size, y = pred, color = model, group = model))+
  labs(x = "Carapace Length (mm)", y = NULL, color = NULL, fill = NULL)+
  scale_color_manual(values = cbpalette)+
  facet_wrap(fishery~type, ncol = 2, dir = "v")+
  theme(panel.spacing.x = unit(0.2, "lines"),
        panel.spacing.y = unit(0, "lines"),
        panel.border = element_blank(),
        axis.line.x = element_line(color = "grey70", size = 0.2),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 8),
        plot.title = element_text(hjust = 0.5),
        strip.background = element_blank(),
        #strip.text.x = element_blank(),
        panel.background = element_blank(),
        title = element_text(hjust = 0.5)) -> x
ggsave("./AIGKC/figures/models/2025/sept/data_wt_agg_comp_fit.png", plot = x, height = 5, width = 6, units = "in")


gmacs_get_size_summary(list(EAG25.0, EAG25.0a,  EAG25.0b, EAG25.0b2, EAG25.0c, EAG25.0d)) %>%
  dplyr::select(-nsamp_est) %>%
  bind_cols(gmacs_get_effective_n(list(EAG25.0, EAG25.0a,  EAG25.0b, EAG25.0b2, EAG25.0c, EAG25.0d)) %>%
              expand_grid(., size = seq(103, 183, 5)) %>%
              transmute(nsamp_est)) %>%
  filter(type == "retained") %>%
  group_by(model, year) %>%
  summarise(obs_mean_size = weighted.mean(size, obs),
            pred_mean_size = weighted.mean(size, pred), 
            sd = sqrt(sum((size - pred_mean_size)^2 * pred) / sum(pred)) / sqrt(mean(nsamp_obs)),
            l95 = pred_mean_size + sd * qnorm(0.025),
            u95 = pred_mean_size + sd * qnorm(0.975),
            sd_est = sqrt(sum((size - pred_mean_size)^2 * pred) / sum(pred)) / sqrt(mean(nsamp_est)),
            l95_est = pred_mean_size + sd_est * qnorm(0.025),
            u95_est = pred_mean_size + sd_est * qnorm(0.975)) %>% ungroup %>%
  ggplot()+
  geom_ribbon(aes(x = year, ymin = l95_est, ymax = u95_est, group = 1), fill = "grey80", alpha = 0.5)+
  geom_ribbon(aes(x = year, ymin = l95, ymax = u95, group = 1), fill = "grey60", alpha = 0.5)+
  geom_line(aes(x = year, y = pred_mean_size, group = 1))+
  geom_vline(xintercept = 2004.5, linetype = 2)+
  geom_point(aes(x = year, y = obs_mean_size))+
  scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
  facet_wrap(~model, ncol = 2)+
  labs(x = NULL, y = paste0("Retained Mean CL (mm)")) -> x
ggsave("./AIGKC/figures/models/2025/sept/eag_data_wt_francis_plot_retained.png", plot = x, height = 7, width = 8, units = "in")

gmacs_get_size_summary(list(EAG25.0, EAG25.0a,  EAG25.0b, EAG25.0b2, EAG25.0c, EAG25.0d)) %>%
  dplyr::select(-nsamp_est) %>%
  bind_cols(gmacs_get_effective_n(list(EAG25.0, EAG25.0a,  EAG25.0b, EAG25.0b2, EAG25.0c, EAG25.0d)) %>%
              expand_grid(., size = seq(103, 183, 5)) %>%
              transmute(nsamp_est)) %>%
  filter(type == "total") %>%
  group_by(model, year) %>%
  summarise(obs_mean_size = weighted.mean(size, obs),
            pred_mean_size = weighted.mean(size, pred), 
            sd = sqrt(sum((size - pred_mean_size)^2 * pred) / sum(pred)) / sqrt(mean(nsamp_obs)),
            l95 = pred_mean_size + sd * qnorm(0.025),
            u95 = pred_mean_size + sd * qnorm(0.975),
            sd_est = sqrt(sum((size - pred_mean_size)^2 * pred) / sum(pred)) / sqrt(mean(nsamp_est)),
            l95_est = pred_mean_size + sd_est * qnorm(0.025),
            u95_est = pred_mean_size + sd_est * qnorm(0.975)) %>% ungroup %>%
  ggplot()+
  geom_ribbon(aes(x = year, ymin = l95_est, ymax = u95_est, group = 1), fill = "grey80", alpha = 0.5)+
  geom_ribbon(aes(x = year, ymin = l95, ymax = u95, group = 1), fill = "grey60", alpha = 0.5)+
  geom_line(aes(x = year, y = pred_mean_size, group = 1))+
  geom_vline(xintercept = 2004.5, linetype = 2)+
  geom_point(aes(x = year, y = obs_mean_size))+
  scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
  facet_wrap(~model, ncol = 2)+
  labs(x = NULL, y = paste0("Total Mean CL (mm)")) -> x
ggsave("./AIGKC/figures/models/2025/sept/eag_data_wt_francis_plot_total.png.png", plot = x, height = 7, width = 8, units = "in")

gmacs_get_effective_n(list(EAG25.0, EAG25.0a,  EAG25.0b, EAG25.0b2, EAG25.0c, EAG25.0d)) %>%
  mutate(type = case_when(mod_series == 1 ~ "Retained", 
                          mod_series == 2 ~ "Total")) %>%
  ggplot()+
  geom_line(aes(x = year, y = nsamp_est, color = model))+
  scale_color_manual(values = cbpalette)+
  scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
  scale_y_continuous(labels = scales::comma)+
  labs(x = NULL, y = "Effective Sample Size", color = NULL)+
  facet_wrap(~type, ncol = 1) -> x
ggsave("./AIGKC/figures/models/2025/sept/eag_data_wt_neff.png", plot = x, height = 5, width = 7, units = "in")
  
gmacs_get_derived_quantity_summary(list(EAG25.0, EAG25.0a,  EAG25.0b, EAG25.0b2, EAG25.0c, EAG25.0d)) %>% mutate(fishery = "EAG") %>%
  bind_rows(gmacs_get_derived_quantity_summary(list(WAG25.0, WAG25.0a, WAG25.0b, WAG25.0b2, WAG25.0c, WAG25.0d)) %>% mutate(fishery = "WAG")) %>%
  ggplot()+
  geom_line(aes(x = factor(year), y = recruit_male, group = model, color = model))+
  scale_x_discrete(breaks = yraxis$breaks, labels = yraxis$labels)+
  scale_y_continuous(labels = scales::comma, limits = c(0, NA))+
  labs(x = NULL, y = "Recruitment (1,000s)", color = NULL)+
  facet_wrap(~fishery, ncol = 1)+
  scale_color_manual(values = cbpalette) -> x
ggsave("./AIGKC/figures/models/2025/sept/data_wt_rec.png", plot = x, height = 5, width = 7, units = "in")

# plot MMB
eag_std <- gmacs_read_std("./AIGKC/models/2025/sept/EAG/25.0a/gmacs.std", sub_text = "log_ssb") %>%
  transmute(ssb_se = se / (1 / exp(est)), 
            ssb_lci = exp(est) + ssb_se * qnorm(0.05 / 2), 
            ssb_uci = exp(est) + ssb_se * qnorm(1 - 0.05 / 2))

wag_std <- gmacs_read_std("./AIGKC/models/2025/sept/WAG/25.0a/gmacs.std", sub_text = "log_ssb") %>%
  transmute(ssb_se = se / (1 / exp(est)), 
            ssb_lci = exp(est) + ssb_se * qnorm(0.05 / 2), 
            ssb_uci = exp(est) + ssb_se * qnorm(1 - 0.05 / 2))

tibble(ao = list(EAG25.0, EAG25.0a, EAG25.0b, EAG25.0b2, EAG25.0c, EAG25.0d),
       std = file.path("./AIGKC/models/2025/sept/EAG", 
                       c("25.0", "25.0a", "25.0b", "25.0b2", "25.0c", "25.0d"), 
                       "gmacs.std")) %>% #pull(ao) %>% .[[1]] -> ao
  mutate(out = purrr::map2(ao, std, function(ao, std) {
    se <- gmacs_read_std(std, sub_text = "log_ssb") %>% 
      transmute(ssb_se = se / (1 / exp(est)), 
                ssb_lci = exp(est) + ssb_se * qnorm(0.05 / 2), 
                ssb_uci = exp(est) + ssb_se * qnorm(1 - 0.05 / 2)) %>%
      bind_rows(gmacs_read_std(std, sub_text = "last_ssb") %>%
                  transmute(ssb_se = se, 
                            ssb_lci = est + ssb_se * qnorm(0.05 / 2), 
                            ssb_uci = est + ssb_se * qnorm(1 - 0.05 / 2)) ) %>%
      mutate(year = c(ao$mod_yrs, max(ao$mod_yrs)+1))
    ao$derived_quant_summary %>% 
      mutate(model = as.character(ao$model_name)) %>%
      add_row(year = max(ao$mod_yrs)+1,
              model = as.character(ao$model_name),
              ssb = ao$mmb_curr) %>%
      left_join(se)
    
  })) %>%
  transmute(out) %>% unnest %>%
  mutate(fishery = "EAG") %>%
  
  bind_rows(tibble(ao = list(WAG25.0, WAG25.0a, WAG25.0b, WAG25.0b2, WAG25.0c, WAG25.0d),
                   std = file.path("./AIGKC/models/2025/sept/WAG", 
                                   c("25.0", "25.0a", "25.0b", "25.0b2", "25.0c", "25.0d"), 
                                   "gmacs.std")) %>% #pull(ao) %>% .[[1]] -> ao
              mutate(out = purrr::map2(ao, std, function(ao, std) {
                se <- gmacs_read_std(std, sub_text = "log_ssb") %>% 
                  transmute(ssb_se = se / (1 / exp(est)), 
                            ssb_lci = exp(est) + ssb_se * qnorm(0.05 / 2), 
                            ssb_uci = exp(est) + ssb_se * qnorm(1 - 0.05 / 2)) %>%
                  bind_rows(gmacs_read_std(std, sub_text = "last_ssb") %>%
                              transmute(ssb_se = se, 
                                        ssb_lci = est + ssb_se * qnorm(0.05 / 2), 
                                        ssb_uci = est + ssb_se * qnorm(1 - 0.05 / 2)) ) %>%
                  mutate(year = c(ao$mod_yrs, max(ao$mod_yrs)+1))
                ao$derived_quant_summary %>% 
                  mutate(model = as.character(ao$model_name)) %>%
                  add_row(year = max(ao$mod_yrs)+1,
                          model = as.character(ao$model_name),
                          ssb = ao$mmb_curr) %>%
                  left_join(se)
                
              })) %>%
              transmute(out) %>% unnest %>%
              mutate(fishery = "WAG")) %>%
  
  ggplot()+
  geom_ribbon(data = ~filter(.x, model == "25.0a"), aes(x = factor(year), ymin = ssb_lci, ymax = ssb_uci, group = model), fill = "grey70", alpha = 0.2, show.legend = F)+
  geom_line(data = ~filter(.x, year != max(year)), aes(x = factor(year), y = ssb, group = model, color = model)) +
  geom_point(data = ~filter(.x, year == max(year)), aes(x = factor(year), y = ssb, color = model))+
  scale_x_discrete(breaks = yraxis$breaks, labels = yraxis$labels)+
  scale_y_continuous(labels = scales::comma, limits = c(0, NA))+
  scale_color_manual(values = cbpalette)+
  scale_fill_manual(values = cbpalette)+
  facet_wrap(~fishery, ncol = 1)+
  labs(x = NULL, y = paste0("MMB (t)"), color = NULL) -> x
ggsave("./AIGKC/figures/models/2025/sept/data_wt_mmb.png", plot = x, height = 5, width = 6, units = "in")

# gmacs_get_size_summary(list(EAG25.0, EAG25.0a, EAG25.1, EAG25.1a, EAG25.1a2)) %>%
#   filter(type == "retained") %>%
#   ggplot()+
#   geom_bar(aes(x = size, y = obs), fill = "grey70", position = "identity", stat = "identity", alpha = 0.5, width = 5)+
#   geom_line(aes(x = size, y = pred, color = model, group = model))+
#   labs(x = "Carapace Length (mm)", y = NULL, color = NULL, fill = NULL, title = "EAG Retained Size Composition")+
#   geom_text_npc(aes(npcx = "right", npcy = 0.6, label = year), check_overlap = T, size = 3)+
#   scale_color_manual(values = cbpalette)+
#   facet_wrap(~year, ncol = 4, dir = "v")+
#   theme(panel.spacing.x = unit(0.2, "lines"),
#         panel.spacing.y = unit(0, "lines"),
#         panel.border = element_blank(),
#         axis.line.x = element_line(color = "grey70", size = 0.2),
#         axis.ticks.y = element_blank(),
#         axis.text.y = element_blank(),
#         axis.text.x = element_text(size = 8),
#         plot.title = element_text(hjust = 0.5),
#         strip.background = element_blank(),
#         strip.text.x = element_blank(),
#         panel.background = element_blank(),
#         title = element_text(hjust = 0.5)) -> x
# ggsave("./AIGKC/figures/models/2025/sept/eag_retained_comp_fit.png", plot = x, height = 6, width = 6, units = "in")
# 

# plot fits to data wag
gmacs_plot_catch(list(WAG25.0, WAG25.0a, WAG25.0b, WAG25.0b2, WAG25.0c, WAG25.0d), save_plot = F)
gmacs_get_index_summary(list(WAG25.0, WAG25.0a, WAG25.0b, WAG25.0b2, WAG25.0c, WAG25.0d)) %>%
  mutate(index = case_when(series == 1 ~ "Pre-Rationalized Observer",
                           series == 2 ~ "Post-Rationalized Observer",
                           series == 3 ~ "Early Fish Ticket"),
         index = factor(index, levels = c("Early Fish Ticket", "Pre-Rationalized Observer", "Post-Rationalized Observer"))) %>%
  mutate(obs_l95 = obs_index * exp(-1.96 * sqrt(log(1 + obs_cv^2))),
         obs_u95 = obs_index * exp(1.96 * sqrt(log(1 + obs_cv^2))),
         tot_l95 = obs_index * exp(-1.96 * sqrt(log(1 + tot_cv^2))),
         tot_u95 = obs_index * exp(1.96 * sqrt(log(1 + tot_cv^2)))) %>%
  ggplot()+
  geom_errorbar(aes(x = factor(year), ymin = tot_l95, ymax = tot_u95), width = 0, color = "grey70")+
  geom_errorbar(aes(x = factor(year), ymin = obs_l95, ymax = obs_u95), width = 0, color = "grey20")+
  geom_point(aes(x = factor(year), y = obs_index), color = "grey20")+
  geom_line(aes(x = factor(year), y = pred_index, group = model, color = model))+
  labs(x = NULL, color = NULL, y = "Standardized Index")+
  scale_y_continuous(labels = scales::comma)+
  scale_x_discrete(breaks = yraxis$breaks, labels = yraxis$labels)+
  scale_color_manual(values = cbpalette)+
  coord_cartesian(ylim = c(0, NA))+
  facet_wrap(~index, scales = "free_x", ncol = 2) -> x
ggsave(plot = x, filename = "./AIGKC/figures/models/2025/sept/wag_data_wt_index_fit.png", height = 6, width = 8, units = "in") 

gmacs_get_size_summary(list(WAG25.0, WAG25.0a,  WAG25.0b, WAG25.0b2, WAG25.0c, WAG25.0d)) %>%
  dplyr::select(-nsamp_est) %>%
  bind_cols(gmacs_get_effective_n(list(WAG25.0, WAG25.0a,  WAG25.0b, WAG25.0b2, WAG25.0c, WAG25.0d)) %>%
              expand_grid(., size = seq(103, 183, 5)) %>%
              transmute(nsamp_est)) %>%
  filter(type == "retained") %>%
  group_by(model, year) %>%
  summarise(obs_mean_size = weighted.mean(size, obs),
            pred_mean_size = weighted.mean(size, pred), 
            sd = sqrt(sum((size - pred_mean_size)^2 * pred) / sum(pred)) / sqrt(mean(nsamp_obs)),
            l95 = pred_mean_size + sd * qnorm(0.025),
            u95 = pred_mean_size + sd * qnorm(0.975),
            sd_est = sqrt(sum((size - pred_mean_size)^2 * pred) / sum(pred)) / sqrt(mean(nsamp_est)),
            l95_est = pred_mean_size + sd_est * qnorm(0.025),
            u95_est = pred_mean_size + sd_est * qnorm(0.975)) %>% ungroup %>%
  ggplot()+
  geom_ribbon(aes(x = year, ymin = l95_est, ymax = u95_est, group = 1), fill = "grey80", alpha = 0.5)+
  geom_ribbon(aes(x = year, ymin = l95, ymax = u95, group = 1), fill = "grey60", alpha = 0.5)+
  geom_line(aes(x = year, y = pred_mean_size, group = 1))+
  geom_vline(xintercept = 2004.5, linetype = 2)+
  geom_point(aes(x = year, y = obs_mean_size))+
  scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
  facet_wrap(~model, ncol = 2)+
  labs(x = NULL, y = paste0("Retained Mean CL (mm)")) -> x
ggsave("./AIGKC/figures/models/2025/sept/wag_data_wt_francis_plot_retained.png", plot = x, height = 7, width = 8, units = "in")

gmacs_get_size_summary(list(WAG25.0, WAG25.0a,  WAG25.0b, WAG25.0b2, WAG25.0c, WAG25.0d)) %>%
  dplyr::select(-nsamp_est) %>%
  bind_cols(gmacs_get_effective_n(list(WAG25.0, WAG25.0a,  WAG25.0b, WAG25.0b2, WAG25.0c, WAG25.0d)) %>%
              expand_grid(., size = seq(103, 183, 5)) %>%
              transmute(nsamp_est)) %>%
  filter(type == "total") %>%
  group_by(model, year) %>%
  summarise(obs_mean_size = weighted.mean(size, obs),
            pred_mean_size = weighted.mean(size, pred), 
            sd = sqrt(sum((size - pred_mean_size)^2 * pred) / sum(pred)) / sqrt(mean(nsamp_obs)),
            l95 = pred_mean_size + sd * qnorm(0.025),
            u95 = pred_mean_size + sd * qnorm(0.975),
            sd_est = sqrt(sum((size - pred_mean_size)^2 * pred) / sum(pred)) / sqrt(mean(nsamp_est)),
            l95_est = pred_mean_size + sd_est * qnorm(0.025),
            u95_est = pred_mean_size + sd_est * qnorm(0.975)) %>% ungroup %>%
  ggplot()+
  geom_ribbon(aes(x = year, ymin = l95_est, ymax = u95_est, group = 1), fill = "grey80", alpha = 0.5)+
  geom_ribbon(aes(x = year, ymin = l95, ymax = u95, group = 1), fill = "grey60", alpha = 0.5)+
  geom_line(aes(x = year, y = pred_mean_size, group = 1))+
  geom_vline(xintercept = 2004.5, linetype = 2)+
  geom_point(aes(x = year, y = obs_mean_size))+
  scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
  facet_wrap(~model, ncol = 2)+
  labs(x = NULL, y = paste0("Total Mean CL (mm)")) -> x
ggsave("./AIGKC/figures/models/2025/sept/wag_data_wt_francis_plot_total.png", plot = x, height = 7, width = 8, units = "in")

gmacs_get_effective_n(list(WAG25.0, WAG25.0a,  WAG25.0b, WAG25.0b2, WAG25.0c, WAG25.0d)) %>%
  mutate(type = case_when(mod_series == 1 ~ "Retained", 
                          mod_series == 2 ~ "Total")) %>%
  ggplot()+
  geom_line(aes(x = year, y = nsamp_est, color = model))+
  scale_color_manual(values = cbpalette)+
  scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
  labs(x = NULL, y = "Effective Sample Size", color = NULL)+
  facet_wrap(~type, ncol = 1) -> x
ggsave("./AIGKC/figures/models/2025/sept/wag_data_wt_neff.png", plot = x, height = 5, width = 7, units = "in")

gmacs_plot_recruitment(list(WAG25.0, WAG25.0a,  WAG25.0b, WAG25.0b2, WAG25.0c, WAG25.0d), save_plot = F)+
  theme(legend.position = "right", legend.justification = c(0.5, 0.5)) -> x
ggsave("./AIGKC/figures/models/2025/sept/wag_data_wt_rec.png", plot = x, height = 3, width = 6, units = "in")

gmacs_plot_mmb(list(WAG25.0, WAG25.0a,  WAG25.0b, WAG25.0b2, WAG25.0c, WAG25.0d), save_plot = F)[[1]]+
  theme(legend.position = "right", legend.justification = c(0.5, 0.5)) -> x
ggsave("./AIGKC/figures/models/2025/sept/wag_data_wt_mmb.png", plot = x, height = 3, width = 6, units = "in")

# data conflict ----

# plot data conflict

gmacs_get_size_summary(list(EAG25.0b)) %>%
  filter(mod_series == 2, size >= 136, year >= 1995) %>%
  mutate(series = ifelse(year < 2005, 1, 2)) %>%
  group_by(year, series) %>%
  summarise(legal = sum(pred)) %>%
  group_by(series) %>%
  
  
  mutate(legal = legal / prod(legal)^(1 / n())) %>% 
  right_join(gmacs_get_index_summary(list(EAG25.0b)) %>%
               filter(series %in% 1:2) %>%
               transmute(year, series, obs_index, obs_cv, tot_cv, pred_index)) %>%
  
  mutate(obs_l95 = obs_index * exp(-1.96 * sqrt(log(1 + obs_cv^2))),
         obs_u95 = obs_index * exp(1.96 * sqrt(log(1 + obs_cv^2))),
         tot_l95 = obs_index * exp(-1.96 * sqrt(log(1 + tot_cv^2))),
         tot_u95 = obs_index * exp(1.96 * sqrt(log(1 + tot_cv^2))),
         fishery = "EAG") %>%
  
  bind_rows(gmacs_get_size_summary(list(WAG25.0b)) %>%
              filter(mod_series == 2, size >= 136, year >= 1995) %>%
              mutate(series = ifelse(year < 2005, 1, 2)) %>%
              group_by(year, series) %>%
              summarise(legal = sum(pred)) %>%
              group_by(series) %>%
              
              
              mutate(legal = legal / prod(legal)^(1 / n())) %>% 
              right_join(gmacs_get_index_summary(list(WAG25.0b)) %>%
                           filter(series %in% 1:2) %>%
                           transmute(year, series, obs_index, obs_cv, tot_cv, pred_index)) %>%
              
              mutate(obs_l95 = obs_index * exp(-1.96 * sqrt(log(1 + obs_cv^2))),
                     obs_u95 = obs_index * exp(1.96 * sqrt(log(1 + obs_cv^2))),
                     tot_l95 = obs_index * exp(-1.96 * sqrt(log(1 + tot_cv^2))),
                     tot_u95 = obs_index * exp(1.96 * sqrt(log(1 + tot_cv^2))),
                     fishery = "WAG")) %>%
  mutate(series = ifelse(series == 1, "Pre-Rationalization", "Post-Rationalization"),
         series = factor(series, levels = c("Pre-Rationalization", "Post-Rationalization"))) %>%
  
  ggplot()+
  geom_errorbar(aes(x = year, ymin = tot_l95, ymax = tot_u95), width = 0, color = "grey70")+
  geom_errorbar(aes(x = year, ymin = obs_l95, ymax = obs_u95), width = 0, color = "grey20")+
  geom_point(aes(x = year, y = obs_index))+
  geom_point(aes(x = year, y = legal), color = "3")+
  geom_line(aes(x = year, y = pred_index))+
  scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
  facet_wrap(fishery~series, scales = "free_x")+
  labs(x = NULL, y = "CPUE Index") -> x
ggsave("./AIGKC/figures/models/2025/sept/data_conflict_index.png", plot = x, height = 5, width = 6, units = "in")

# eag survey ----

EAG23.1c <- gmacs_read_allout("./AIGKC/models/2025/sept/EAG/23.1c/Gmacsall.out", "23.1c")
EAG25.0b <- gmacs_read_allout("./AIGKC/models/2025/sept/EAG/25.0b/Gmacsall.out", "25.0b")
EAG25.0c <- gmacs_read_allout("./AIGKC/models/2025/sept/EAG/25.0c/Gmacsall.out", "25.0c")
EAG25.1 <- gmacs_read_allout("./AIGKC/models/2025/sept/EAG/25.1/Gmacsall.out", "25.1")
EAG25.1b <- gmacs_read_allout("./AIGKC/models/2025/sept/EAG/25.1b/Gmacsall.out", "25.1b")
EAG25.1c <- gmacs_read_allout("./AIGKC/models/2025/sept/EAG/25.1c/Gmacsall.out", "25.1c")

gmacs_plot_catch(list(EAG23.1c, EAG25.0b, EAG25.0c, EAG25.1, EAG25.1b), save_plot = F)
gmacs_get_index_summary(list(EAG23.1c, EAG25.0b, EAG25.0c, EAG25.1, EAG25.1b)) %>%
  mutate(index = case_when(series == 1 ~ "Pre-Rationalized Observer",
                           series == 2 ~ "Post-Rationalized Observer",
                           series == 3 ~ "Early Fish Ticket",
                           series == 4 ~ "Cooperative Survey"),
         index = factor(index, levels = c("Early Fish Ticket", "Pre-Rationalized Observer", "Post-Rationalized Observer", "Cooperative Survey"))) %>%
  mutate(obs_l95 = obs_index * exp(-1.96 * sqrt(log(1 + obs_cv^2))),
         obs_u95 = obs_index * exp(1.96 * sqrt(log(1 + obs_cv^2))),
         tot_l95 = obs_index * exp(-1.96 * sqrt(log(1 + tot_cv^2))),
         tot_u95 = obs_index * exp(1.96 * sqrt(log(1 + tot_cv^2)))) %>%
  ggplot()+
  geom_errorbar(aes(x = year, ymin = tot_l95, ymax = tot_u95), width = 0, color = "grey70")+
  geom_errorbar(aes(x = year, ymin = obs_l95, ymax = obs_u95), width = 0, color = "grey20")+
  geom_point(aes(x = year, y = obs_index), color = "grey20")+
  geom_line(aes(x = year, y = pred_index, group = model, color = model))+
  labs(x = NULL, color = NULL, y = "Standardized Index")+
  scale_y_continuous(labels = scales::comma)+
  scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
  scale_color_manual(values = cbpalette)+
  coord_cartesian(ylim = c(0, NA))+
  facet_wrap(~index, scales = "free_x", ncol = 2) -> x
ggsave(plot = x, filename = "./AIGKC/figures/models/2025/sept/eag_survey_index_fit.png", height = 6, width = 8, units = "in") 


# compare size comp data
gmacs_get_size_summary(list(EAG25.1)) %>%
  filter(type == "total", year >= 2015) %>% 
  mutate(fleet = case_when(fleet == "Coop_Survey" ~ "Survey",
                        fleet == "Directed_Fishery" ~ "Observer")) %>%
  ggplot()+
  geom_bar(aes(x = size, y = obs, fill = fleet), position = "identity", stat = "identity", alpha = 0.5, width = 5)+
  labs(x = "Carapace Length (mm)", y = NULL, color = NULL, fill = NULL)+
  scale_color_manual(values = cbpalette)+
  facet_wrap(~year, ncol = 1)+
  scale_fill_manual(values = cbpalette[c(1, 3)])+
  theme(panel.spacing.x = unit(0.2, "lines"),
        panel.spacing.y = unit(0, "lines"),
        panel.border = element_blank(),
        axis.line.x = element_line(color = "grey70", size = 0.2),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 8),
        plot.title = element_text(hjust = 0.5),
        strip.background = element_blank(),
        #strip.text.x = element_blank(),
        panel.background = element_blank(),
        title = element_text(hjust = 0.5)) -> x
  ggsave("./AIGKC/figures/models/2025/sept/eag_survey_comp_compare.png", plot = x, height = 7, width = 4, units = "in")


gmacs_get_size_summary(list(EAG23.1c, EAG25.0b, EAG25.0c, EAG25.1, EAG25.1b)) %>%
  filter(fleet == "Directed_Fishery") %>%
  mutate(type = str_to_title(type)) %>%
  group_by(model, type, size) %>%
  summarise(obs = sum(obs), pred = sum(pred)) %>%
  ggplot()+
  geom_bar(aes(x = size, y = obs), fill = "grey70", position = "identity", stat = "identity", alpha = 0.1, width = 5)+
  geom_line(aes(x = size, y = pred, color = model, group = model))+
  labs(x = "Carapace Length (mm)", y = NULL, color = NULL, fill = NULL)+
  scale_color_manual(values = cbpalette)+
  facet_wrap(~type, ncol = 1)+
  theme(panel.spacing.x = unit(0.2, "lines"),
        panel.spacing.y = unit(0, "lines"),
        panel.border = element_blank(),
        axis.line.x = element_line(color = "grey70", size = 0.2),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 8),
        plot.title = element_text(hjust = 0.5),
        strip.background = element_blank(),
        #strip.text.x = element_blank(),
        panel.background = element_blank(),
        title = element_text(hjust = 0.5)) -> x
ggsave("./AIGKC/figures/models/2025/sept/eag_survey_comp_fit.png", plot = x, height = 4, width = 5, units = "in")

gmacs_get_size_summary(list(EAG23.1c, EAG25.0b, EAG25.0c, EAG25.1, EAG25.1b)) %>% 
  filter(fleet == "Coop_Survey") %>%
  ggplot()+
  geom_bar(aes(x = size, y = obs), fill = "grey60", position = "identity", stat = "identity", alpha = 0.1, width = 5)+
  geom_line(aes(x = size, y = pred, color = model, group = model))+
  labs(x = "Carapace Length (mm)", y = NULL, color = NULL, fill = NULL)+
  scale_color_manual(values = cbpalette)+
  facet_wrap(~year, ncol = 1)+
  theme(panel.spacing.x = unit(0.2, "lines"),
        panel.spacing.y = unit(0, "lines"),
        panel.border = element_blank(),
        axis.line.x = element_line(color = "grey70", size = 0.2),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 8),
        plot.title = element_text(hjust = 0.5),
        strip.background = element_blank(),
        #strip.text.x = element_blank(),
        panel.background = element_blank(),
        title = element_text(hjust = 0.5)) -> x
ggsave("./AIGKC/figures/models/2025/sept/eag_survey_comp_fit_survey.png", plot = x, height = 7, width = 4, units = "in") 

gmacs_get_size_summary(list(EAG23.1c, EAG25.0b, EAG25.0c, EAG25.1, EAG25.1b)) %>%
  dplyr::select(-nsamp_est) %>%
  bind_cols(gmacs_get_effective_n(list(EAG23.1c, EAG25.0b, EAG25.0c, EAG25.1, EAG25.1b)) %>%
              expand_grid(., size = seq(103, 183, 5)) %>%
              transmute(nsamp_est)) %>%
  filter(fleet == "Coop_Survey", type == "total") %>%
  group_by(model, year) %>%
  summarise(obs_mean_size = weighted.mean(size, obs),
            pred_mean_size = weighted.mean(size, pred), 
            sd = sqrt(sum((size - pred_mean_size)^2 * pred) / sum(pred)) / sqrt(mean(nsamp_obs)),
            l95 = pred_mean_size + sd * qnorm(0.025),
            u95 = pred_mean_size + sd * qnorm(0.975),
            sd_est = sqrt(sum((size - pred_mean_size)^2 * pred) / sum(pred)) / sqrt(mean(nsamp_est)),
            l95_est = pred_mean_size + sd_est * qnorm(0.025),
            u95_est = pred_mean_size + sd_est * qnorm(0.975)) %>% ungroup %>%
  ggplot()+
  geom_ribbon(aes(x = year, ymin = l95_est, ymax = u95_est, group = 1), fill = "grey80", alpha = 0.5)+
  geom_ribbon(aes(x = year, ymin = l95, ymax = u95, group = 1), fill = "grey60", alpha = 0.5)+
  geom_line(aes(x = year, y = pred_mean_size, group = 1))+
  #geom_vline(xintercept = 2004.5, linetype = 2)+
  geom_point(aes(x = year, y = obs_mean_size))+
  scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
  facet_wrap(~model, ncol = 2)+
  labs(x = NULL, y = paste0("Survey Mean CL (mm)")) -> x
ggsave("./AIGKC/figures/models/2025/sept/eag_survey_francis_plot_total.png.png", plot = x, height = 7, width = 8, units = "in")


gmacs_get_slx(list(EAG23.1c, EAG25.0b, EAG25.0c, EAG25.1, EAG25.1b)) %>%
  filter(fleet %in% c("Directed_Fishery", "Coop_Survey"),
         !(fleet == "Coop_Survey" & year < 2005)) %>% 
  mutate(block = ifelse(year <= 2004, "Pre-Rationalization", "Post-Rationalization"),
         block = factor(block, levels = c("Pre-Rationalization", "Post-Rationalization"))) %>%
  ggplot()+
  geom_line(aes(x = size, y = slx_capture, color = model, linetype = fleet))+
  scale_color_manual(values = cbpalette)+
  facet_wrap(~block)+
  labs(x = "Carapace Length (mm)", y = "Selectivity", linetype = NULL, color = NULL) -> x
ggsave("./AIGKC/figures/models/2025/sept/eag_survey_slx.png.png", plot = x, height = 3, width = 6, units = "in")


gmacs_plot_recruitment(list(EAG23.1c, EAG25.0b, EAG25.0c, EAG25.1, EAG25.1b), save_plot = F)+
  theme(legend.position = c(0, 1), legend.justification = c(0, 1)) -> x
ggsave("./AIGKC/figures/models/2025/sept/eag_survey_rec.png", plot = x, height = 3, width = 6, units = "in")

# plot MMB
tibble(ao = list(EAG23.1c, EAG25.0b, EAG25.0c, EAG25.1, EAG25.1b),
       std = file.path("./AIGKC/models/2025/sept/EAG", 
                       c("23.1c", "25.0b", "25.0c", "25.1", "25.1b"), 
                       "gmacs.std")) %>% #pull(ao) %>% .[[1]] -> ao
  mutate(out = purrr::map2(ao, std, function(ao, std) {
    se <- gmacs_read_std(std, sub_text = "log_ssb") %>% 
      transmute(ssb_se = se / (1 / exp(est)), 
                ssb_lci = exp(est) + ssb_se * qnorm(0.05 / 2), 
                ssb_uci = exp(est) + ssb_se * qnorm(1 - 0.05 / 2)) %>%
      bind_rows(gmacs_read_std(std, sub_text = "last_ssb") %>%
                  transmute(ssb_se = se, 
                            ssb_lci = est + ssb_se * qnorm(0.05 / 2), 
                            ssb_uci = est + ssb_se * qnorm(1 - 0.05 / 2)) ) %>%
      mutate(year = c(ao$mod_yrs, max(ao$mod_yrs)+1))
    ao$derived_quant_summary %>% 
      mutate(model = as.character(ao$model_name)) %>%
      add_row(year = max(ao$mod_yrs)+1,
              model = as.character(ao$model_name),
              ssb = ao$mmb_curr) %>%
      left_join(se)
    
  })) %>%
  transmute(out) %>% unnest %>%
  
  ggplot()+
  geom_ribbon(data = ~filter(.x, model %in% c("25.0b", "25.1")), aes(x = year, ymin = ssb_lci, ymax = ssb_uci, group = model, fill = model), alpha = 0.1, show.legend = F)+
  geom_line(data = ~filter(.x, year != max(year)), aes(x = year, y = ssb, group = model, color = model)) +
  geom_point(data = ~filter(.x, year == max(year)), aes(x = year, y = ssb, color = model))+
  scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels, expand = c(0.01, 0.01))+
  scale_y_continuous(labels = scales::comma, limits = c(0, NA))+
  scale_color_manual(values = cbpalette)+
  scale_fill_manual(values = cbpalette)+
  labs(x = NULL, y = paste0("MMB (t)"), color = NULL)+
  theme(legend.position = c(0, 0), legend.justification = c(0, 0)) -> x
ggsave("./AIGKC/figures/models/2025/sept/eag_survey_mmb.png", plot = x, height = 3, width = 6, units = "in")
 
# tables ----

# added CV
gmacs_get_pars(list(EAG23.1, EAG23.1c, EAG25.0, EAG25.0a, EAG25.0b, EAG25.0b2, EAG25.0c, EAG25.0d, EAG25.1, EAG25.1b)) %>%
  filter(grepl("add_cv", parameter)) %>%
  mutate(series = substring(parameter, 20, 20),
         series = case_when(series == 1 ~ "Pre-Rat. Observer CPUE",
                            series == 2 ~ "Post-Rat. Observer CPUE", 
                            series == 3 ~ "Early Fish Ticket CPUE"),
         est = exp(estimate)) %>%
  filter(!is.na(series)) %>%
  transmute(model, series, est = round(est, 2)) %>%
  pivot_wider(names_from = model, values_from = est) %>%
  mutate(fishery = "EAG") %>% dplyr::select(ncol(.), 1:(ncol(.)-1)) %>%
  write_csv("./AIGKC/output/models/2025/sept/eag_index_add_cv.csv")
  

gmacs_get_pars(list(WAG23.1, WAG23.1c, WAG25.0, WAG25.0a, WAG25.0b, WAG25.0b2, WAG25.0c, WAG25.0d)) %>%
  filter(grepl("add_cv", parameter)) %>%
  mutate(series = substring(parameter, 20, 20),
         series = case_when(series == 1 ~ "Pre-Rat. Observer CPUE",
                            series == 2 ~ "Post-Rat. Observer CPUE", 
                            series == 3 ~ "Early Fish Ticket CPUE"),
         est = exp(estimate)) %>%
  filter(!is.na(series)) %>%
  transmute(model, series, est = round(est, 2)) %>%
  pivot_wider(names_from = model, values_from = est) %>%
  mutate(fishery = "WAG") %>% dplyr::select(ncol(.), 1:(ncol(.)-1)) %>%
  write_csv("./AIGKC/output/models/2025/sept/wag_index_add_cv.csv")

# likelihoods
gmacs_get_lik(list(EAG23.1, EAG23.1c, EAG25.0, EAG25.0a, EAG25.0b, EAG25.0b2, EAG25.0c, EAG25.0d, EAG25.1, EAG25.1b)) %>%
  dplyr::slice(1:6, 16, 7:8, 17, 9, 12, 14, 15) %>%
  mutate(process = c("Retained Catch", "Total Catch", "GF Bycatch", "Pre-Rat. Obs CPUE", "Post Rat. Obs CPUE", "Fish Ticket CPUE",
                     "Coop. Survey CPUE", "Retained Size", "Total Size", "Coop. Survey Size", "Recruitment Penalties", "Tagging",
                     "Number of Parameters", "Objective Functions")) %>%
  write_csv("./AIGKC/output/models/2025/sept/eag_likelihood.csv")

gmacs_get_lik(list(WAG23.1, WAG23.1c, WAG25.0, WAG25.0a, WAG25.0b, WAG25.0b2, WAG25.0c, WAG25.0d)) %>%
  dplyr::slice(1:9, 12, 14:15) %>%
  mutate(process = c("Retained Catch", "Total Catch", "GF Bycatch", "Pre-Rat. Obs CPUE", "Post Rat. Obs CPUE", "Fish Ticket CPUE",
                     "Retained Size", "Total Size", "Recruitment Penalties", "Tagging",
                     "Number of Parameters", "Objective Functions")) %>%
  write_csv("./AIGKC/output/models/2025/sept/wag_likelihood.csv")

# reference points
gmacs_get_ref_points(list(EAG23.1, EAG23.1c, EAG25.0, EAG25.0a, EAG25.0b, EAG25.0b2, EAG25.0c, EAG25.0d, EAG25.1, EAG25.1b)) %>%
  write_csv("./AIGKC/output/models/2025/sept/eag_ref_point.csv")

gmacs_get_ref_points(list(WAG23.1, WAG23.1c, WAG25.0, WAG25.0a, WAG25.0b, WAG25.0b2, WAG25.0c, WAG25.0d)) %>%
  write_csv("./AIGKC/output/models/2025/sept/wag_ref_point.csv")



# retrospective issues ----

# cause of retrospective biasS
tibble(file = paste0("./AIGKC/models/2025/sept/EAG/25.0b/retrospectives/retro_", 1:10, "/Gmacsall.out"),
       model_name = as.character(2022:2013)) %>%
  mutate(allout = purrr::map2(file, model_name, gmacs_read_allout, version = "2.20.16")) -> eag_retro

gmacs_get_index_summary(eag_retro$allout) %>%
  filter(pred_index > 0) %>%
  gmacs_plot_index(data_summary = ., save_plot = F) %>% .[[2]]+
  labs(y = "CPUE Index", color = "Terminal Year")+
  theme(legend.position = "none") -> eag_retro_index

gmacs_plot_mmb(eag_retro$allout, save_plot = F, plot_proj = F)[[1]]+
  theme(legend.position = "none")+
  ggtitle("EAG")+theme(plot.title = element_text(hjust = 0.5)) -> eag_retro_mmb

gmacs_plot_recruitment(eag_retro$allout, save_plot = F)+
  theme(legend.position = "none") -> eag_retro_rec

tibble(file = paste0("./AIGKC/models/2025/sept/WAG/25.0b/retrospectives/retro_", 1:10, "/Gmacsall.out"),
       model_name = as.character(2022:2013)) %>%
  mutate(allout = purrr::map2(file, model_name, gmacs_read_allout, version = "2.20.16")) -> wag_retro

gmacs_get_index_summary(wag_retro$allout) %>%
  filter(pred_index > 0) %>%
  gmacs_plot_index(data_summary = ., save_plot = F) %>% .[[2]]+
  labs(y = "CPUE Index", color = "Terminal Year")+
  theme(legend.position = "none") -> wag_retro_index

gmacs_plot_mmb(wag_retro$allout, save_plot = F, plot_proj = F)[[1]]+
  theme(legend.position = "none")+
  ggtitle("WAG")+theme(plot.title = element_text(hjust = 0.5)) -> wag_retro_mmb

gmacs_plot_recruitment(wag_retro$allout, save_plot = F)+
  theme(legend.position = "none") -> wag_retro_rec

ggsave("./AIGKC/figures/models/2025/sept/retrospective_cause_25_0b.png",
       plot = (eag_retro_mmb / eag_retro_rec / eag_retro_index) | (wag_retro_mmb / wag_retro_rec / wag_retro_index), height = 7, width = 10, units = "in")



# plots of retrospectives
# EAG
r1 <- gmacs_do_retrospective("./AIGKC/models/2025/sept/EAG/23.1c/gmacs.dat", n_peel = 10, plot_only = T, save_plot = F)[[2]]+
  ggtitle("23.1c") + theme(plot.title = element_text(hjust = 0.5))
r2 <- gmacs_do_retrospective("./AIGKC/models/2025/sept/EAG/25.0b/gmacs.dat", n_peel = 10, plot_only = T, save_plot = F)[[2]]+
  ggtitle("25.0b") + theme(plot.title = element_text(hjust = 0.5))
r3 <- gmacs_do_retrospective("./AIGKC/models/2025/sept/EAG/25.1/gmacs.dat", n_peel = 8, plot_only = T, save_plot = F)[[2]]+
  ggtitle("25.1") + theme(plot.title = element_text(hjust = 0.5))
ggsave("./AIGKC/figures/models/2025/sept/retrospective_eag.png", plot = r1/r2/r3, height = 7, width = 7, units = "in")
# WAG
r1 <- gmacs_do_retrospective("./AIGKC/models/2025/sept/WAG/23.1c/gmacs.dat", n_peel = 10, plot_only = T, save_plot = F)[[2]]+
  ggtitle("23.1c") + theme(plot.title = element_text(hjust = 0.5))
r2 <- gmacs_do_retrospective("./AIGKC/models/2025/sept/WAG/25.0b/gmacs.dat", n_peel = 10, plot_only = T, save_plot = F)[[2]]+
  ggtitle("25.0b") + theme(plot.title = element_text(hjust = 0.5))
ggsave("./AIGKC/figures/models/2025/sept/retrospective_wag.png", plot = r1/r2, height = 5, width = 7, units = "in")
