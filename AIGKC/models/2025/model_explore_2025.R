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

# dirichlet and survey ----

EAG25.0 <- gmacs_read_allout("./AIGKC/models/2025/sept/EAG/25.0/Gmacsall.out", "25.0")
EAG25.0a <- gmacs_read_allout("./AIGKC/models/2025/sept/EAG/25.0a/Gmacsall.out", "25.0a")
EAG25.0ac <- gmacs_read_allout("./AIGKC/models/2025/sept/EAG/25.0a - Copy/Gmacsall.out", "25.0ac")
EAG25.0b <- gmacs_read_allout("./AIGKC/models/2025/sept/EAG/25.0b/Gmacsall.out", "25.0b")
EAG25.1 <- gmacs_read_allout("./AIGKC/models/2025/sept/EAG/25.1/Gmacsall.out", "25.1")
EAG25.1.2 <- gmacs_read_allout("./AIGKC/models/2025/sept/EAG/25.1.2/Gmacsall.out", "25.1.2")
EAG25.1b <- gmacs_read_allout("./AIGKC/models/2025/sept/EAG/25.1b/Gmacsall.out", "25.1b")
EAG25.1b2 <- gmacs_read_allout("./AIGKC/models/2025/sept/EAG/25.1b2/Gmacsall.out", "25.1b2")

WAG25.0 <- gmacs_read_allout("./AIGKC/models/2025/sept/WAG/25.0/Gmacsall.out", "25.0")
WAG25.1 <- gmacs_read_allout("./AIGKC/models/2025/sept/WAG/25.1/Gmacsall.out", "25.1")
WAG25.1.2 <- gmacs_read_allout("./AIGKC/models/2025/sept/WAG/25.1.2/Gmacsall.out", "25.1.2")

# plot fits to data eag
gmacs_plot_catch(list(EAG25.0, EAG25.0a, EAG25.1, EAG25.1a, EAG25.1a2), save_plot = F)

gmacs_plot_index(list(EAG25.0, EAG25.0a, EAG25.0ac, EAG25.0b, EAG25.1, EAG25.1.2,EAG25.1b, EAG25.1b2 ), save_plot = F)

, 
                          y_labs = c("Observer CPUE", "Observer CPUE", "Fish Ticket CPUE", "Survey CPUE"),
                          plot_dir = "./AIGKC/figures/models/2025/sept")

gmacs_get_size_summary(list(EAG25.0, EAG25.0a, EAG25.1, EAG25.1a, EAG25.1a2)) %>%
  filter(type == "retained") %>%
  ggplot()+
  geom_bar(aes(x = size, y = obs), fill = "grey70", position = "identity", stat = "identity", alpha = 0.5, width = 5)+
  geom_line(aes(x = size, y = pred, color = model, group = model))+
  labs(x = "Carapace Length (mm)", y = NULL, color = NULL, fill = NULL, title = "EAG Retained Size Composition")+
  geom_text_npc(aes(npcx = "right", npcy = 0.6, label = year), check_overlap = T, size = 3)+
  scale_color_manual(values = cbpalette)+
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
ggsave("./AIGKC/figures/models/2025/sept/eag_retained_comp_fit.png", plot = x, height = 6, width = 6, units = "in")

gmacs_get_size_summary(list(EAG25.0, EAG25.0a, EAG25.1, EAG25.1a, EAG25.1a2)) %>%
  filter(type == "total") %>%
  ggplot()+
  geom_bar(aes(x = size, y = obs), fill = "grey70", position = "identity", stat = "identity", alpha = 0.5, width = 5)+
  geom_line(aes(x = size, y = pred, color = model, group = model))+
  labs(x = "Carapace Length (mm)", y = NULL, color = NULL, fill = NULL, title = "EAG Total Size Composition")+
  geom_text_npc(aes(npcx = "right", npcy = 0.6, label = year), check_overlap = T, size = 3)+
  scale_color_manual(values = cbpalette)+
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
ggsave("./AIGKC/figures/models/2025/sept/eag_total_comp_fit.png", plot = x, height = 6, width = 6, units = "in")



# plot fits to data wag
gmacs_plot_catch(list(WAG25.0,  WAG25.1, WAG25.1.2), save_plot = F)

gmacs_plot_index(list(WAG25.0, WAG25.1, WAG25.1.2), save_plot = T, 
                 y_labs = c("Observer CPUE", "Observer CPUE", "Fish Ticket CPUE"),
                 plot_dir = "./AIGKC/figures/models/2025/sept")

gmacs_get_size_summary(list(EAG25.0, EAG25.0a, EAG25.1, EAG25.1a, EAG25.1a2)) %>%
  filter(type == "retained") %>%
  ggplot()+
  geom_bar(aes(x = size, y = obs), fill = "grey70", position = "identity", stat = "identity", alpha = 0.5, width = 5)+
  geom_line(aes(x = size, y = pred, color = model, group = model))+
  labs(x = "Carapace Length (mm)", y = NULL, color = NULL, fill = NULL, title = "EAG Retained Size Composition")+
  geom_text_npc(aes(npcx = "right", npcy = 0.6, label = year), check_overlap = T, size = 3)+
  scale_color_manual(values = cbpalette)+
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
ggsave("./AIGKC/figures/models/2025/sept/eag_retained_comp_fit.png", plot = x, height = 6, width = 6, units = "in")

gmacs_get_size_summary(list(EAG25.0, EAG25.0a, EAG25.1, EAG25.1a, EAG25.1a2)) %>%
  filter(type == "total") %>%
  ggplot()+
  geom_bar(aes(x = size, y = obs), fill = "grey70", position = "identity", stat = "identity", alpha = 0.5, width = 5)+
  geom_line(aes(x = size, y = pred, color = model, group = model))+
  labs(x = "Carapace Length (mm)", y = NULL, color = NULL, fill = NULL, title = "EAG Total Size Composition")+
  geom_text_npc(aes(npcx = "right", npcy = 0.6, label = year), check_overlap = T, size = 3)+
  scale_color_manual(values = cbpalette)+
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
ggsave("./AIGKC/figures/models/2025/sept/eag_total_comp_fit.png", plot = x, height = 6, width = 6, units = "in")





gmacs_plot_mmb(list(EAG23.1c, EAG25.0, EAG25.1, EAG25.1a), save_plot = F)
gmacs_plot_recruitment(list(EAG23.1c, EAG25.0, EAG25.1, EAG25.1a), save_plot = F)

# retrospective issues ----

# cause of retrospective biasS
tibble(file = paste0("./AIGKC/models/2024/may/EAG/23.1/retrospectives/retro_", 1:10, "/Gmacsall.out"),
       model_name = as.character(2022:2013)) %>%
  mutate(allout = purrr::map2(file, model_name, gmacs_read_allout, version = "2.01.M.10")) -> eag_retro

gmacs_get_index_summary(eag_retro$allout) %>%
  filter(pred_index > 0) %>%
  gmacs_plot_index(data_summary = ., save_plot = F) %>% .[[2]]+
  labs(y = "CPUE Index", color = "Terminal Year")+
  theme(legend.position = "none") -> retro_index

gmacs_plot_mmb(eag_retro$allout, save_plot = F, plot_proj = F)[[1]]+
  scale_x_discrete(limits = 2005:2023)+
  theme(legend.position = "none") -> retro_mmb

gmacs_plot_catch(eag_retro$allout, save_plot = F)


ggsave("./AIGKC/figures/models/2025/sept/retrospective_cause_23_1.png", plot = retro_mmb / retro_index, height = 5, width = 5, units = "in")

