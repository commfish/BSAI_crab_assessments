# 2026 AIGKC draft models
## tyler jackson
## tyler.jackson@alaska.gov
## 7/29/2025

# load ----

library(gmacsr)
library(sf)

# run initial models ----

# gmacs_do_exe(gmacs.dat = "./AIGKC/models/2026/nov/EAG/25.0a/gmacs.dat", pin = F, reweight = T, level = 0.001)
# gmacs_do_exe(gmacs.dat = "./AIGKC/models/2026/nov/EAG/26.0/gmacs.dat", pin = F, reweight = T, level = 0.001)
# gmacs_do_exe(gmacs.dat = "./AIGKC/models/2026/nov/EAG/26.0a/gmacs.dat", pin = F, reweight = T, level = 0.001)
# gmacs_do_exe(gmacs.dat = "./AIGKC/models/2026/nov/EAG/26.1/gmacs.dat", pin = F, reweight = T, level = 0.001)
# gmacs_do_exe(gmacs.dat = "./AIGKC/models/2026/nov/EAG/26.1a/gmacs.dat", pin = F, reweight = T, level = 0.001)
# gmacs_do_exe(gmacs.dat = "./AIGKC/models/2026/nov/EAG/26.1b/gmacs.dat", pin = F, reweight = T, level = 0.001)
# gmacs_do_exe(gmacs.dat = "./AIGKC/models/2026/nov/EAG/26.1c/gmacs.dat", pin = F, reweight = T, level = 0.001)

# gmacs_do_exe(gmacs.dat = "./AIGKC/models/2026/nov/WAG/25.0a/gmacs.dat", pin = F, reweight = T, level = 0.001)
# gmacs_do_exe(gmacs.dat = "./AIGKC/models/2026/nov/WAG/26.0/gmacs.dat", pin = F, reweight = T, level = 0.001)
# gmacs_do_exe(gmacs.dat = "./AIGKC/models/2026/nov/WAG/26.0a/gmacs.dat", pin = F, reweight = T, level = 0.001)
# gmacs_do_exe(gmacs.dat = "./AIGKC/models/2026/nov/WAG/26.1/gmacs.dat", pin = F, reweight = T, level = 0.001)
# gmacs_do_exe(gmacs.dat = "./AIGKC/models/2026/nov/WAG/26.1a/gmacs.dat", pin = F, reweight = T, level = 0.001)
# gmacs_do_exe(gmacs.dat = "./AIGKC/models/2026/nov/WAG/26.1b/gmacs.dat", pin = F, reweight = T, level = 0.001)
# gmacs_do_exe(gmacs.dat = "./AIGKC/models/2026/nov/WAG/26.1c/gmacs.dat", pin = F, reweight = T, level = 0.001)


# AI models run outside of R

# vessel dynamics ----

eag %>%
  filter(year >= 2005) %>%
  mutate(adfg = case_when(adfg == 103 ~ "Early Dawn",
                          adfg == 3645 ~ "Ballyhoo",
                          adfg == 5992 ~ "Aleutian No 1",
                          adfg == 8653 ~ "Kori Ann",
                          adfg == 20556 ~ "Erla N",
                          adfg == 35767 ~ "Patricia Lee",
                          adfg == 56111 ~ "Ocean Olympic",
                          adfg == 62436 ~ "Handler")) %>%
  ggplot()+
  geom_sf(data = eag_land)+
  geom_text_npc(aes(npcx = "left", npcy = 0.9, label = year), check_overlap = T, size = 3)+
  geom_sf(aes(color = adfg))+
  facet_wrap(~year, ncol = 4, dir = "v")+
  labs(color = NULL)+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        strip.text.x = element_blank(),
        panel.spacing = unit(0, "lines"))

get_mode <- function(x) {
  ux <- na.omit(unique(x))
  ux[which.max(tabulate(match(x, ux)))]
}


eag %>%
  #filter(year >= 2005) %>%
  mutate(adfg = case_when(adfg == 103 ~ "Early Dawn",
                          adfg == 3645 ~ "Ballyhoo",
                          adfg == 5992 ~ "Aleutian No 1",
                          adfg == 8653 ~ "Kori Ann",
                          adfg == 20556 ~ "Erla N",
                          adfg == 35767 ~ "Patricia Lee",
                          adfg == 56111 ~ "Ocean Olympic",
                          adfg == 62436 ~ "Handler")) %>%
  mutate(lon_bin = floor(lon / 20) * 20,
         lat_bin = floor(lat / 20) * 20) %>%
  group_by(year) %>%
  mutate(tot_effort = n()) %>%
  group_by(year, lon_bin, lat_bin) %>%
  summarise(adfg = get_mode(adfg),
            effort = n() / mean(tot_effort)) -> eag_tiles
  
  
eag_tiles %>%
  ggplot()+
  geom_text_npc(aes(npcx = "left", npcy = 0.9, label = year), check_overlap = T, size = 3)+
  geom_tile(aes(x = lon_bin, y = lat_bin, fill = effort))+
  geom_sf(data = eag_land)+
  facet_wrap(~year, ncol = 4, dir = "v")+
  labs(color = NULL)+
  scale_fill_gradientn(colors = topo.colors(5), values = c(0, 0.1, 0.2, 1),
                       breaks = c(0, 0.05, 0.1, 0.15))+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        strip.text.x = element_blank(),
        panel.spacing = unit(0, "lines"))

eag_tiles %>%
  ggplot()+
  geom_text_npc(aes(npcx = "left", npcy = 0.9, label = year), check_overlap = T, size = 3)+
  geom_tile(aes(x = lon_bin, y = lat_bin, fill = adfg))+
  geom_sf(data = eag_land)+
  facet_wrap(~year, ncol = 4, dir = "v")+
  labs(color = NULL)+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        strip.text.x = element_blank(),
        panel.spacing = unit(0, "lines"))




eag_tiles %>%
  mutate(tile = paste(lon_bin, lat_bin, sep = "_")) %>%
  st_drop_geometry() %>%
  ungroup  %>%  dplyr::select(-lon_bin, - lat_bin, -adfg) %>%
  pivot_wider(names_from = tile, values_from = effort, values_fill = 0) -> effort_wide



effort_matrix <- effort_wide %>% dplyr::select(-year)
pca <- prcomp(effort_matrix, scale. = TRUE)

# Plot years in PCA space
plot(pca$x[,1:2], type = "n")
text(pca$x[,1:2], labels = effort_wide$year)

plot(pca$x[,1] ~ effort_wide$year)




# differences in total size composition by vessel ----

read_csv("AIGKC/data/observer/directed_observer_size_comp_by_vessel.csv") %>%
  filter(size > 0, substring(fishery, 1, 2) == "OB", crab_year >= 2005) %>%
  group_by(crab_year, adfg) %>%
  mutate(total_n = sum(total)) %>%
  group_by(crab_year, adfg, size) %>%
  summarise(pmf = sum(total)/mean(total_n)) %>%
  ggplot()+
  geom_bar(aes(x = size, y = pmf, fill = factor(adfg)), color = NA,alpha = 0.2, width = 1, stat = "identity", position = "identity")+
  facet_wrap(~crab_year)


# load outputs ----

eag23.1c_v21 <- gmacs_read_allout(file = "./AIGKC/models/2026/nov/EAG/23.1c/Gmacsall.out", model_name = "23.1c_v21", version = "2.20.21")
eag23.1c <- gmacs_read_allout(file = "./AIGKC/models/2026/nov/EAG/23.1c_v31/Gmacsall.out", model_name = "23.1c")
eag25.0a <- gmacs_read_allout(file = "./AIGKC/models/2026/nov/EAG/25.0a/Gmacsall.out", model_name = "25.0a")
eag26.0 <- gmacs_read_allout(file = "./AIGKC/models/2026/nov/EAG/26.0/Gmacsall.out", model_name = "26.0")
eag26.0a <- gmacs_read_allout(file = "./AIGKC/models/2026/nov/EAG/26.0a/Gmacsall.out", model_name = "26.0a")
eag26.1 <- gmacs_read_allout(file = "./AIGKC/models/2026/nov/EAG/26.1/Gmacsall.out", model_name = "26.1")
eag26.1a <- gmacs_read_allout(file = "./AIGKC/models/2026/nov/EAG/26.1a/Gmacsall.out", model_name = "26.1a")
eag26.1b <- gmacs_read_allout(file = "./AIGKC/models/2026/nov/EAG/26.1b/Gmacsall.out", model_name = "26.1b")
eag26.1c <- gmacs_read_allout(file = "./AIGKC/models/2026/nov/EAG/26.1c/Gmacsall.out", model_name = "26.1c")


wag23.1c_v21 <- gmacs_read_allout(file = "./AIGKC/models/2026/nov/WAG/23.1c/Gmacsall.out", model_name = "23.1c_v21", version = "2.20.21")
wag23.1c <- gmacs_read_allout(file = "./AIGKC/models/2026/nov/WAG/23.1c_v31/Gmacsall.out", model_name = "23.1c")
wag25.0a <- gmacs_read_allout(file = "./AIGKC/models/2026/nov/WAG/25.0a/Gmacsall.out", model_name = "25.0a")
wag26.0 <- gmacs_read_allout(file = "./AIGKC/models/2026/nov/WAG/26.0/Gmacsall.out", model_name = "26.0")
wag26.0a <- gmacs_read_allout(file = "./AIGKC/models/2026/nov/WAG/26.0a/Gmacsall.out", model_name = "26.0a")
wag26.1 <- gmacs_read_allout(file = "./AIGKC/models/2026/nov/WAG/26.1/Gmacsall.out", model_name = "26.1")
wag26.1a <- gmacs_read_allout(file = "./AIGKC/models/2026/nov/WAG/26.1a/Gmacsall.out", model_name = "26.1a")
wag26.1b <- gmacs_read_allout(file = "./AIGKC/models/2026/nov/WAG/26.1b/Gmacsall.out", model_name = "26.1b")
wag26.1c <- gmacs_read_allout(file = "./AIGKC/models/2026/nov/WAG/26.1c/Gmacsall.out", model_name = "26.1c")

ai26.x <- gmacs_read_allout(file = "./AIGKC/models/2026/nov/AI/26.x/Gmacsall.out", model_name = "26.x")

# gmacs version ----

# likelihoods
gmacs_get_lik_type_pen(list(eag23.1c_v21, eag23.1c)) %>%
  dplyr::slice(c(-8, -10:-12)) %>%
  write_csv("./AIGKC/output/models/2026/nov/eag_base_model_version_lik.csv")
gmacs_get_lik_type_pen(list(wag23.1c_v21, wag23.1c)) %>%
  dplyr::slice(c(-8, -10:-12)) %>%
  write_csv("./AIGKC/output/models/2026/nov/wag_base_model_version_lik.csv")

# reference points
write_csv(gmacs_get_ref_points(list(eag23.1c_v21, eag23.1c)), "./AIGKC/output/models/2026/nov/eag_base_model_version_ref.csv")
write_csv(gmacs_get_ref_points(list(wag23.1c_v21, wag23.1c)), "./AIGKC/output/models/2026/nov/wag_base_model_version_ref.csv")



# initial conditions and catch weight ----

gmacs_get_catch_summary(list(eag23.1c, eag25.0a, eag26.0, eag26.0a)) %>%
  group_by(model, series) %>%
  summarise(RMSE = sqrt(sum(pred_catch - obs_catch)^2 / n()),
            MARE = mean(abs((obs_catch - pred_catch) / obs_catch)) ) %>%
  pivot_longer(c(RMSE, MARE), names_to = "stat", values_to = "value") -> tmp
gmacs_plot_catch(list(eag23.1c, eag25.0a, eag26.0, eag26.0a), save_plot = F) -> catch_tmp
catch_tmp[[1]]+theme(legend.position = "none") |
  tmp %>%
  filter(series == 1) %>%
  ggplot()+
  geom_bar(aes(x = model, y = value, fill = model), stat = "identity", show.legend = F)+
  labs(x = NULL, y = NULL, fill = NULL)+
  scale_fill_manual(values = cbpalette)+
  facet_wrap(~stat, scales = "free_y", dir = "h", ncol = 2)+
  theme(plot.title = element_text(hjust = 0.5)) + plot_layout(widths = c(3, 1)) -> ret
catch_tmp[[3]]+theme(legend.position = "none") |
  tmp %>%
  filter(series == 2) %>%
  ggplot()+
  geom_bar(aes(x = model, y = value, fill = model), stat = "identity", show.legend = F)+
  labs(x = NULL, y = NULL, fill = NULL)+
  scale_fill_manual(values = cbpalette)+
  facet_wrap(~stat, scales = "free_y", dir = "h", ncol = 2)+
  theme(plot.title = element_text(hjust = 0.5)) + plot_layout(widths = c(3, 1)) -> tot
catch_tmp[[4]]+theme(legend.position = "none") |
  tmp %>%
  filter(series == 3) %>%
  ggplot()+
  geom_bar(aes(x = model, y = value, fill = model), stat = "identity", show.legend = F)+
  labs(x = NULL, y = NULL, fill = NULL)+
  scale_fill_manual(values = cbpalette)+
  facet_wrap(~stat, scales = "free_y", dir = "h", ncol = 2)+
  theme(plot.title = element_text(hjust = 0.5)) + plot_layout(widths = c(3, 1)) -> gf
ggsave("AIGKC/figures/models/2026/nov/eag_catch_fit_init_conditions.png", 
       plot = ret / tot / gf + plot_annotation(title = "EAG Catch Series", theme = theme(plot.title = element_text(hjust = 0.5))),
       width = 11, height = 9, units = "in")

gmacs_get_catch_summary(list(wag23.1c, wag25.0a, wag26.0, wag26.0a)) %>%
  group_by(model, series) %>%
  summarise(RMSE = sqrt(sum(pred_catch - obs_catch)^2 / n()),
            MARE = mean(abs((obs_catch - pred_catch) / obs_catch)) ) %>%
  pivot_longer(c(RMSE, MARE), names_to = "stat", values_to = "value") -> tmp
gmacs_plot_catch(list(wag23.1c, wag25.0a, wag26.0, wag26.0a), save_plot = F) -> catch_tmp
catch_tmp[[1]]+theme(legend.position = "none") |
  tmp %>%
  filter(series == 1) %>%
  ggplot()+
  geom_bar(aes(x = model, y = value, fill = model), stat = "identity", show.legend = F)+
  labs(x = NULL, y = NULL, fill = NULL)+
  scale_fill_manual(values = cbpalette)+
  facet_wrap(~stat, scales = "free_y", dir = "h", ncol = 2)+
  theme(plot.title = element_text(hjust = 0.5)) + plot_layout(widths = c(3, 1)) -> ret
catch_tmp[[3]]+theme(legend.position = "none") |
  tmp %>%
  filter(series == 2) %>%
  ggplot()+
  geom_bar(aes(x = model, y = value, fill = model), stat = "identity", show.legend = F)+
  labs(x = NULL, y = NULL, fill = NULL)+
  scale_fill_manual(values = cbpalette)+
  facet_wrap(~stat, scales = "free_y", dir = "h", ncol = 2)+
  theme(plot.title = element_text(hjust = 0.5)) + plot_layout(widths = c(3, 1)) -> tot
catch_tmp[[4]]+theme(legend.position = "none") |
  tmp %>%
  filter(series == 3) %>%
  ggplot()+
  geom_bar(aes(x = model, y = value, fill = model), stat = "identity", show.legend = F)+
  labs(x = NULL, y = NULL, fill = NULL)+
  scale_fill_manual(values = cbpalette)+
  facet_wrap(~stat, scales = "free_y", dir = "h", ncol = 2)+
  theme(plot.title = element_text(hjust = 0.5)) + plot_layout(widths = c(3, 1)) -> gf
ggsave("AIGKC/figures/models/2026/nov/wag_catch_fit_init_conditions.png", 
       plot = ret / tot / gf + plot_annotation(title = "WAG Catch Series", theme = theme(plot.title = element_text(hjust = 0.5))),
       width = 11, height = 9, units = "in")

# plot of recruitment
gmacs_get_derived_quantity_summary(list(eag23.1c)) %>%
  transmute(model, year, recruit_male = round(recruit_male)) %>%
  bind_cols(gmacs_read_std("AIGKC/models/2026/nov/EAG/23.1c/gmacs.std") %>% 
              filter(grepl("sd_log_recruits", par)) %>%
              transmute(se = se / (1 / exp(est)))) %>% 
  bind_rows(
    gmacs_get_derived_quantity_summary(list(eag25.0a)) %>%
      transmute(model, year, recruit_male = round(recruit_male)) %>%
      bind_cols(gmacs_read_std("AIGKC/models/2026/nov/EAG/25.0a/gmacs.std") %>% 
                  filter(grepl("sd_log_recruits", par)) %>%
                  transmute(se = se / (1 / exp(est))))
  ) %>%
  bind_rows(
    gmacs_get_derived_quantity_summary(list(eag26.0)) %>%
      transmute(model, year, recruit_male = round(recruit_male)) %>%
      bind_cols(gmacs_read_std("AIGKC/models/2026/nov/EAG/26.0/gmacs.std") %>% 
                  filter(grepl("sd_log_recruits", par)) %>%
                  transmute(se = NA))
  ) %>%
  bind_rows(
    gmacs_get_derived_quantity_summary(list(eag26.0a)) %>%
      transmute(model, year, recruit_male = round(recruit_male)) %>%
      bind_cols(gmacs_read_std("AIGKC/models/2026/nov/EAG/26.0a/gmacs.std") %>% 
                  filter(grepl("sd_log_recruits", par)) %>%
                  transmute(se = NA))
  ) %>%
  ggplot()+
  geom_ribbon(aes(x = as.numeric(as.factor(year)), ymin = recruit_male + se * qnorm(0.05 / 2), ymax = recruit_male + se * qnorm(1 - 0.05 / 2), fill = model),
              alpha = 0.2, show.legend = F)+
  geom_line(aes(x = factor(year), y = recruit_male, group = model, color = model))+
  scale_x_discrete(breaks = yraxis$breaks, labels = yraxis$labels)+
  scale_y_continuous(labels = scales::comma, limits = c(NA, 8000))+
  scale_color_manual(values = cbpalette)+
  scale_fill_manual(values = c("grey70", NA, NA, NA))+
  labs(x = NULL, y = paste0("Recruitment (1,000)"), color = NULL, title = "EAG")+
  theme(legend.position = c(0, 1),
        legend.justification = c(0, 1),
        plot.title = element_text(hjust = 0.5)) -> eag_rec


gmacs_get_derived_quantity_summary(list(wag23.1c)) %>%
  transmute(model, year, recruit_male = round(recruit_male)) %>%
  bind_cols(gmacs_read_std("AIGKC/models/2026/nov/WAG/23.1c/gmacs.std") %>% 
              filter(grepl("sd_log_recruits", par)) %>%
              transmute(se = se / (1 / exp(est)))) %>% 
  bind_rows(
    gmacs_get_derived_quantity_summary(list(wag25.0a)) %>%
      transmute(model, year, recruit_male = round(recruit_male)) %>%
      bind_cols(gmacs_read_std("AIGKC/models/2026/nov/WAG/25.0a/gmacs.std") %>% 
                  filter(grepl("sd_log_recruits", par)) %>%
                  transmute(se = se / (1 / exp(est))))
  ) %>%
  
  bind_rows(
    gmacs_get_derived_quantity_summary(list(wag26.0)) %>%
      transmute(model, year, recruit_male = round(recruit_male)) %>%
      bind_cols(gmacs_read_std("AIGKC/models/2026/nov/WAG/26.0/gmacs.std") %>% 
                  filter(grepl("sd_log_recruits", par)) %>%
                  transmute(se = NA))
  ) %>%
  
  bind_rows(
    gmacs_get_derived_quantity_summary(list(wag26.0a)) %>%
      transmute(model, year, recruit_male = round(recruit_male)) %>%
      bind_cols(gmacs_read_std("AIGKC/models/2026/nov/WAG/26.0a/gmacs.std") %>% 
                  filter(grepl("sd_log_recruits", par)) %>%
                  transmute(se = NA))
  ) %>%
  
  ggplot()+
  geom_ribbon(aes(x = as.numeric(as.factor(year)), ymin = recruit_male + se * qnorm(0.05 / 2), ymax = recruit_male + se * qnorm(1 - 0.05 / 2), fill = model),
              alpha = 0.2, show.legend = F)+
  geom_line(aes(x = factor(year), y = recruit_male, group = model, color = model))+
  scale_x_discrete(breaks = yraxis$breaks, labels = yraxis$labels)+
  scale_y_continuous(labels = scales::comma, limits = c(NA, 8000))+
  scale_color_manual(values = cbpalette)+
  scale_fill_manual(values = c("grey70", NA, NA, NA))+
  labs(x = NULL, y = paste0("Recruitment (1,000)"), color = NULL, title = "WAG")+
  theme(legend.position = c(0, 1),
        legend.justification = c(0, 1),
        plot.title = element_text(hjust = 0.5)) -> wag_rec

ggsave("AIGKC/figures/models/2026/nov/recruitment_init_condition.png", plot = eag_rec / wag_rec + plot_layout(guides = "collect"), width = 7, height = 6, units = "in")

# plot of mmb

gmacs_get_derived_quantity_summary(list(eag23.1c)) %>%
  transmute(model, year, ssb) %>%
  bind_cols(gmacs_read_std("AIGKC/models/2026/nov/EAG/23.1c/gmacs.std") %>% 
              filter(grepl("sd_log_ssb", par)) %>%
              transmute(se = se / (1 / exp(est)))) %>% 
  bind_rows(
    gmacs_get_derived_quantity_summary(list(eag25.0a)) %>%
      transmute(model, year, ssb) %>%
      bind_cols(gmacs_read_std("AIGKC/models/2026/nov/EAG/25.0a/gmacs.std") %>% 
                  filter(grepl("sd_log_ssb", par)) %>%
                  transmute(se = se / (1 / exp(est))))
  ) %>%
  
  bind_rows(
    gmacs_get_derived_quantity_summary(list(eag26.0)) %>%
      transmute(model, year, ssb) %>%
      bind_cols(gmacs_read_std("AIGKC/models/2026/nov/EAG/26.0/gmacs.std") %>% 
                  filter(grepl("sd_log_ssb", par)) %>%
                  transmute(se = NA))
  ) %>%
  
  bind_rows(
    gmacs_get_derived_quantity_summary(list(eag26.0a)) %>%
      transmute(model, year, ssb) %>%
      bind_cols(gmacs_read_std("AIGKC/models/2026/nov/EAG/26.0a/gmacs.std") %>% 
                  filter(grepl("sd_log_ssb", par)) %>%
                  transmute(se = NA))
  ) %>%
  
  ggplot()+
  geom_ribbon(aes(x = as.numeric(as.factor(year)), ymin = ssb + se * qnorm(0.05 / 2), ymax = ssb + se * qnorm(1 - 0.05 / 2), fill = model),
              alpha = 0.2, show.legend = F)+
  geom_line(aes(x = factor(year), y = ssb, group = model, color = model))+
  scale_x_discrete(breaks = yraxis$breaks, labels = yraxis$labels)+
  scale_y_continuous(labels = scales::comma, limits = c(0, 22000))+
  scale_color_manual(values = cbpalette)+
  scale_fill_manual(values = c("grey70", NA, NA, NA))+
  labs(x = NULL, y = paste0("MMB (t)"), color = NULL, title = "EAG")+
  theme(legend.position = c(1, 1),
        legend.justification = c(1, 1),
        plot.title = element_text(hjust = 0.5)) -> eag_mmb


gmacs_get_derived_quantity_summary(list(wag23.1c)) %>%
  transmute(model, year, ssb) %>%
  bind_cols(gmacs_read_std("AIGKC/models/2026/nov/WAG/23.1c/gmacs.std") %>% 
              filter(grepl("sd_log_ssb", par)) %>%
              transmute(se = se / (1 / exp(est)))) %>%
  bind_rows(
    gmacs_get_derived_quantity_summary(list(wag25.0a)) %>%
      transmute(model, year, ssb) %>%
      bind_cols(gmacs_read_std("AIGKC/models/2026/nov/WAG/25.0a/gmacs.std") %>% 
                  filter(grepl("sd_log_ssb", par)) %>%
                  transmute(se = se / (1 / exp(est))))
  ) %>%
  
  bind_rows(
    gmacs_get_derived_quantity_summary(list(wag26.0)) %>%
      transmute(model, year, ssb) %>%
      bind_cols(gmacs_read_std("AIGKC/models/2026/nov/WAG/26.0/gmacs.std") %>% 
                  filter(grepl("sd_log_ssb", par)) %>%
                  transmute(se = NA))
  ) %>%
  
  bind_rows(
    gmacs_get_derived_quantity_summary(list(wag26.0a)) %>%
      transmute(model, year, ssb) %>%
      bind_cols(gmacs_read_std("AIGKC/models/2026/nov/WAG/26.0a/gmacs.std") %>% 
                  filter(grepl("sd_log_ssb", par)) %>%
                  transmute(se = NA))
  ) %>%
  ggplot()+
  geom_ribbon(aes(x = as.numeric(as.factor(year)), ymin = ssb + se * qnorm(0.05 / 2), ymax = ssb + se * qnorm(1 - 0.05 / 2), fill = model),
              alpha = 0.2, show.legend = F)+
  geom_line(aes(x = factor(year), y = ssb, group = model, color = model))+
  scale_x_discrete(breaks = yraxis$breaks, labels = yraxis$labels)+
  scale_y_continuous(labels = scales::comma, limits = c(0, 22000))+
  scale_color_manual(values = cbpalette)+
  scale_fill_manual(values = c("grey70", NA, NA, NA))+
  labs(x = NULL, y = paste0("MMB (t)"), color = NULL, title = "WAG")+
  theme(legend.position = c(1, 1),
        legend.justification = c(1, 1),
        plot.title = element_text(hjust = 0.5)) -> wag_mmb

ggsave("AIGKC/figures/models/2026/nov/mmb_init_condition.png", plot = eag_mmb / wag_mmb + plot_layout(guides = "collect"), width = 7, height = 6, units = "in")


# time varying selectivity ----


# plot to index
# eag
gmacs_get_index_summary(list(eag26.0a, eag26.1, eag26.1a, eag26.1b, eag26.1c)) %>%
  filter(series == 1) %>% 
  group_by(model) %>%
  summarise(RMSE = sqrt(sum(pred_index - obs_index)^2 / n()),
            MARE = mean(abs((obs_index - pred_index) / obs_index)) ) %>%
  pivot_longer(c(RMSE, MARE), names_to = "stat", values_to = "value") -> tmp
gmacs_plot_index(list(eag26.0a, eag26.1, eag26.1a, eag26.1b, eag26.1c), save_plot = F)[[1]]+theme(legend.position = "none")  -> catch_tmp
tmp %>%
  ggplot()+
  geom_bar(aes(x = model, y = value, fill = model), stat = "identity", show.legend = F)+
  labs(x = NULL, y = NULL, fill = NULL)+
  scale_fill_manual(values = cbpalette)+
  facet_wrap(~stat, scales = "free_y", dir = "h", ncol = 2)+
  theme(plot.title = element_text(hjust = 0.5)) -> stat_tmp #+ plot_layout(widths = c(3, 1)) 
ggsave("AIGKC/figures/models/2026/nov/eag_index_fit_slx.png", 
       plot = (catch_tmp / stat_tmp) + plot_layout(heights = c(2, 1)),
       width = 7, height = 6, units = "in")

# wag
gmacs_get_index_summary(list(wag26.0a, wag26.1, wag26.1a, wag26.1b, wag26.1c)) %>%
  filter(series == 1) %>%
  group_by(model) %>%
  summarise(RMSE = sqrt(sum(pred_index - obs_index)^2 / n()),
            MARE = mean(abs((obs_index - pred_index) / obs_index)) ) %>%
  pivot_longer(c(RMSE, MARE), names_to = "stat", values_to = "value") -> tmp
gmacs_plot_index(list(wag26.0a, wag26.1, wag26.1a, wag26.1b, wag26.1c), save_plot = F)[[1]]+theme(legend.position = "none")  -> catch_tmp
tmp %>%
  ggplot()+
  geom_bar(aes(x = model, y = value, fill = model), stat = "identity", show.legend = F)+
  labs(x = NULL, y = NULL, fill = NULL)+
  scale_fill_manual(values = cbpalette)+
  facet_wrap(~stat, scales = "free_y", dir = "h", ncol = 2)+
  theme(plot.title = element_text(hjust = 0.5)) -> stat_tmp #+ plot_layout(widths = c(3, 1)) 
ggsave("AIGKC/figures/models/2026/nov/wag_index_fit_slx.png", 
       plot = (catch_tmp / stat_tmp) + plot_layout(heights = c(2, 1)),
       width = 7, height = 6, units = "in")

# table of added cv
gmacs_get_pars(list(eag26.0a, eag26.1, eag26.1a, eag26.1b, eag26.1c)) %>%
  filter(grepl("add_cvt_survey_1", parameter)) %>%
  mutate(estimate = exp(estimate),
         subdistrict = "EAG") %>%
  bind_rows(gmacs_get_pars(list(wag26.0a, wag26.1, wag26.1a, wag26.1b, wag26.1c)) %>%
              filter(grepl("add_cvt_survey_1", parameter)) %>%
              mutate(estimate = exp(estimate),
                     subdistrict = "WAG")) %>%
  transmute(subdistrict, model, est = round(estimate, 2)) %>%
  pivot_wider(names_from = model, values_from = est) %>%
  write_csv("AIGKC/output/models/2026/nov/add_cv_slx.csv")

# plot of catchability
gmacs_get_index_summary(list(eag26.0a, eag26.1, eag26.1a, eag26.1b, eag26.1c)) %>%
  mutate(subdistrict = "EAG") %>%
  bind_rows(gmacs_get_index_summary(list(wag26.0a, wag26.1, wag26.1a, wag26.1b, wag26.1c)) %>%
              mutate(subdistrict = "WAG")) %>%
  filter(series == 1) %>%
  ggplot()+
  geom_line(aes(x = year, y = q, color = model), alpha = 0.5)+
  geom_point(aes(x = year, y = q, color = model), alpha = 0.5)+
  labs(x = NULL, y = "Catachability", color = NULL, main = "EAG")+
  scale_color_manual(values = cbpalette)+
  scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
  facet_wrap(~subdistrict, ncol = 1) -> x
ggsave("AIGKC/figures/models/2026/nov/catchability_slx.png", plot = x, width = 6, height = 6, units = "in")


# plot selectivity
# pre rationalization
gmacs_get_slx(list(eag26.0a, eag26.1, eag26.1a, eag26.1b, eag26.1c)) %>% mutate(subdistrict = "EAG") %>%
  bind_rows(gmacs_get_slx(list(wag26.0a, wag26.1, wag26.1a, wag26.1b, wag26.1c)) %>%
              mutate(subdistrict = "WAG")) %>%
  
  filter(fleet == "Directed_Fishery", year < 2005) %>%
  ggplot()+
  geom_line(aes(x = size, y = slx_capture, color = model, group = model))+
  facet_wrap(~subdistrict)+
  scale_color_manual(values = cbpalette)+
  labs(x = "Carapace Length (mm)", y = "Selectivity", color = NULL) -> x
ggsave("AIGKC/figures/models/2026/nov/pre_rat_selectivity_slx.png", plot = x, width = 6, height = 3, units = "in")

# post rationalization
gmacs_get_slx(list(eag26.0a, eag26.1, eag26.1a, eag26.1b, eag26.1c)) %>% mutate(subdistrict = "EAG") %>%
  bind_rows(gmacs_get_slx(list(wag26.0a, wag26.1, wag26.1a, wag26.1b, wag26.1c)) %>%
              mutate(subdistrict = "WAG")) %>%
  
  filter(fleet == "Directed_Fishery", year >= 2005) %>%
  ggplot()+
  geom_line(aes(x = size, y = slx_capture, color = year, group = year))+
  scale_color_gradient2(low = cbpalette[7], mid = cbpalette[1], high = cbpalette[2], midpoint = mean(2005:2024))+
  facet_grid(cols = vars(subdistrict), rows = vars(model))+
  labs(x = "Carapace Length (mm)", y = "Selectivity", color = NULL) -> x
ggsave("AIGKC/figures/models/2026/nov/post_rat_selectivity_slx.png", plot = x, width = 7, height = 8, units = "in")

# francis weights
tibble(subdistrict = c(rep("EAG", 5), rep("WAG", 5)),
       data = list(eag26.0a, eag26.1, eag26.1a, eag26.1b, eag26.1c, wag26.0a, wag26.1, wag26.1a, wag26.1b, wag26.1c)) %>% #pull(model) %>% .[[1]] -> model
  mutate(f_wts = purrr::map(data, function(data) {
   tibble(model = data$model_name,
          retained = data$francis_weights[1],
          total = data$francis_weights[2])
  })) %>%
  dplyr::select(-data) %>%
  unnest(f_wts) %>%
  pivot_wider(names_from = subdistrict, values_from = c(retained, total)) %>%
  dplyr::select(1, 2, 4, 3, 5) %>%
  write_csv("AIGKC/output/models/2026/nov/francis_slx.csv")


gmacs_get_size_summary(list(eag26.0a, eag26.1, eag26.1a, eag26.1b, eag26.1c)) %>%
  mutate(wt = round(nsamp_est / nsamp_obs, 4)) %>%
  distinct(mod_series, wt)

# size comp aggregate
gmacs_get_size_summary(list(eag26.0a, eag26.1, eag26.1a, eag26.1b, eag26.1c)) %>% mutate(subdistrict = "EAG") %>%
  bind_rows(gmacs_get_size_summary(list(wag26.0a, wag26.1, wag26.1a, wag26.1b, wag26.1c)) %>% mutate(subdistrict = "WAG")) %>%
  group_by(model, subdistrict, type, size) %>%
  summarise(obs = sum(obs),
            pred = sum(pred),
            nobs = sum(nsamp_obs),
            nest = sum(nsamp_est)) %>% ungroup %>%
  mutate(obs = ifelse(obs == 0, NA, obs),
         pred = ifelse(pred == 0, NA, pred),
         type = str_to_title(type)) %>%
  group_by(type, subdistrict) %>%
  mutate(nnote = paste0("N = ", prettyNum(nobs, ","), 
                        "\n26.0a = ", prettyNum(nest[model == "26.0a"], ","),
                        "\n26.1 = ", prettyNum(nest[model == "26.1"], ","),
                        "\n26.1a = ", prettyNum(nest[model == "26.1a"], ","),
                        "\n26.1b = ", prettyNum(nest[model == "26.1b"], ","),
                        "\n26.1c = ", prettyNum(nest[model == "26.1c"], ",")
                        )) %>%
  
  ggplot()+
  geom_bar(aes(x = size, y = obs), stat = "identity", position = "identity", color = NA, fill = "grey90", width = 5, alpha = 0.5)+
  geom_line(aes(x = size, y = pred, color = model))+
  geom_point(aes(x = size, y = pred, color = model))+
  geom_text_npc(aes(npcx = "left", npcy = 0.9, label = nnote), check_overlap = T, size = 2.5)+
  scale_y_continuous(expand = expansion(mult = c(0, 0.1), add = c(0, 0)))+
  labs(x = "Carapace Length (mm)", y = NULL, color = NULL, fill = NULL)+
  scale_color_manual(values = cbpalette)+
  theme(panel.spacing.x = unit(0, "lines"),
        panel.spacing.y = unit(0, "lines"),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_text(hjust = 0.5),
        strip.background = element_blank(),
        panel.background = element_blank()) +
  facet_grid(cols = vars(subdistrict), rows = vars(type)) -> x
ggsave("AIGKC/figures/models/2026/nov/agg_comp_slx.png", plot = x, width = 7, height = 5, units = "in")

# size comp osa residuals
osa_res <- function(model) {
  data <- gmacs_get_size_summary(list(model))
  data %>%
    filter(!is.na(osa_residual)) %>%
    mutate(sign = ifelse(osa_residual > 0, "+", "-"),
           type = str_to_title(type)) %>%
    ggplot()+
    geom_point(aes(x = factor(year), y = size, size = abs(osa_residual), fill = sign), shape = 21)+
    scale_fill_manual(values = c("white", "grey30"))+
    labs(x = NULL, y = "Carapace Length (mm)", fill = NULL, size = "abs(Residual)")+
    scale_x_discrete(breaks = yraxis$breaks, labels = yraxis$labels)+
    facet_wrap(~type) -> dot_plot
  data %>%
    filter(!is.na(osa_residual)) %>%
    mutate(theor_q = stats::qqnorm(osa_residual, plot.it = FALSE)$x) %>%
    ggplot()+
    geom_abline()+
    geom_point(aes(x = theor_q, y = osa_residual, color = factor(size)))+
    scale_color_manual(values = cbpalette, guide = guide_legend(ncol = 2))+
    labs(color = NULL, x = "Theoretical Quantiles", y = "Sample quantiles")+
    facet_wrap(~type) + theme(strip.text.x = element_blank()) -> qq_plot
  return(dot_plot / qq_plot)
}
# eag
ggsave("AIGKC/figures/models/2026/nov/osa_res_eag26.0a.png", plot = osa_res(eag26.0a), width = 9, height = 6, units = "in")
ggsave("AIGKC/figures/models/2026/nov/osa_res_eag26.1.png", plot = osa_res(eag26.1), width = 9, height = 6, units = "in")
ggsave("AIGKC/figures/models/2026/nov/osa_res_eag26.1a.png", plot = osa_res(eag26.1a), width = 9, height = 6, units = "in")
ggsave("AIGKC/figures/models/2026/nov/osa_res_eag26.1b.png", plot = osa_res(eag26.1b), width = 9, height = 6, units = "in")
ggsave("AIGKC/figures/models/2026/nov/osa_res_eag26.1c.png", plot = osa_res(eag26.1c), width = 9, height = 6, units = "in")
# wag
ggsave("AIGKC/figures/models/2026/nov/osa_res_wag26.0a.png", plot = osa_res(wag26.0a), width = 9, height = 6, units = "in")
ggsave("AIGKC/figures/models/2026/nov/osa_res_wag26.1.png", plot = osa_res(wag26.1), width = 9, height = 6, units = "in")
ggsave("AIGKC/figures/models/2026/nov/osa_res_wag26.1a.png", plot = osa_res(wag26.1a), width = 9, height = 6, units = "in")
ggsave("AIGKC/figures/models/2026/nov/osa_res_wag26.1b.png", plot = osa_res(wag26.1b), width = 9, height = 6, units = "in")
ggsave("AIGKC/figures/models/2026/nov/osa_res_wag26.1c.png", plot = osa_res(wag26.1c), width = 9, height = 6, units = "in")

# plot growth 

# growth observations from base model
eag_tag_obs <- gmacs_read_dat("./AIGKC/models/2026/nov/EAG/23.1c_v31/EAG_23_1c.dat", model_name = "23.1c")$growth

# get predicted mean molt increment
tibble(ao = list(eag26.0a, eag26.1, eag26.1a, eag26.1b, eag26.1c)) %>%
  mutate(model = purrr::map_chr(ao, function(ao){ao$model_name}),
         pred_molt_inc = purrr::map(ao, function(ao) {
           
           # number of yrs at liberty
           max_yrs <- max(eag_tag_obs$years_at_liberty)
           
           tibble(yrs_lib = 1:max_yrs, 
                  X = list(ao$size_transition[[1]])) %>%
             mutate(pred_molt = purrr::map2(X, yrs_lib, function(X, yrs_lib) {
               XX <- X
               if(yrs_lib > 1) {
               for (i in 2:yrs_lib) { XX <- XX %*% X  }
               }
               
               XX %>%
                 as_tibble()%>%
                 rowid_to_column(var = "pre_molt") %>%
                 mutate(pre_molt = ao$size_mid_points[pre_molt]) %>%
                 pivot_longer(2:ncol(.), names_to = "post_molt", values_to = "prob") %>%
                 mutate(post_molt = as.numeric(post_molt)) %>%
                 group_by(pre_molt) %>%
                 summarise(pred_inc = weighted.mean(post_molt - pre_molt, prob)) -> out
               return(out)

             })) %>%
             transmute(yrs_lib, pred_molt) %>%
             unnest(pred_molt)
         })) %>%
  transmute(model, pred_molt_inc) %>% unnest(pred_molt_inc) -> pred_molt_inc
# observed mean molt inc
eag_tag_obs %>%
  mutate(pre_molt = eag26.1a$size_mid_points[release_class],
         size_recapture = eag26.1a$size_mid_points[recapture_class]) %>%
  group_by(years_at_liberty, pre_molt) %>%
  summarise(obs_inc = weighted.mean(size_recapture - mean(pre_molt), nsamp),
            nsamp = sum(nsamp)) %>% ungroup %>%
  rename(yrs_lib = years_at_liberty) %>%
  # join to predictions
  right_join(pred_molt_inc) %>%
  mutate(yrs_lib = case_when(yrs_lib == 1 ~ "1 Year",
                             yrs_lib > 1 ~ paste0(yrs_lib, " Years"))) %>%
  
  ggplot()+
  geom_point(aes(x = pre_molt, y = obs_inc, size = nsamp), shape = 21)+
  geom_line(aes(x = pre_molt, y = pred_inc, color = model))+
  labs(x = "Pre-Molt Carapace Length (mm)", y = "Mean Molt Increment", color = NULL, size = "Sample Size", title = "EAG Tagging")+
  scale_color_manual(values = cbpalette)+
  facet_wrap(~yrs_lib, scales = "free")+
  theme(plot.title = element_text(hjust = 0.5)) -> x
ggsave("AIGKC/figures/models/2026/nov/eag_tagging_slx.png", plot = x, width = 9, height = 6, units = "in")

# growth observations from base model
wag_tag_obs <- gmacs_read_dat("./AIGKC/models/2026/nov/wag/23.1c_v31/wag_23_1c.dat", model_name = "23.1c")$growth

# get predicted mean molt increment
tibble(ao = list(wag26.0a, wag26.1, wag26.1a, wag26.1b, wag26.1c)) %>%
  mutate(model = purrr::map_chr(ao, function(ao){ao$model_name}),
         pred_molt_inc = purrr::map(ao, function(ao) {
           
           # number of yrs at liberty
           max_yrs <- max(wag_tag_obs$years_at_liberty)
           
           tibble(yrs_lib = 1:max_yrs, 
                  X = list(ao$size_transition[[1]])) %>%
             mutate(pred_molt = purrr::map2(X, yrs_lib, function(X, yrs_lib) {
               XX <- X
               if(yrs_lib > 1) {
                 for (i in 2:yrs_lib) { XX <- XX %*% X  }
               }
               
               XX %>%
                 as_tibble()%>%
                 rowid_to_column(var = "pre_molt") %>%
                 mutate(pre_molt = ao$size_mid_points[pre_molt]) %>%
                 pivot_longer(2:ncol(.), names_to = "post_molt", values_to = "prob") %>%
                 mutate(post_molt = as.numeric(post_molt)) %>%
                 group_by(pre_molt) %>%
                 summarise(pred_inc = weighted.mean(post_molt - pre_molt, prob)) -> out
               return(out)
               
             })) %>%
             transmute(yrs_lib, pred_molt) %>%
             unnest(pred_molt)
         })) %>%
  transmute(model, pred_molt_inc) %>% unnest(pred_molt_inc) -> pred_molt_inc
# observed mean molt inc
wag_tag_obs %>%
  mutate(pre_molt = wag26.1a$size_mid_points[release_class],
         size_recapture = wag26.1a$size_mid_points[recapture_class]) %>%
  group_by(years_at_liberty, pre_molt) %>%
  summarise(obs_inc = weighted.mean(size_recapture - mean(pre_molt), nsamp),
            nsamp = sum(nsamp)) %>% ungroup %>%
  rename(yrs_lib = years_at_liberty) %>%
  # join to predictions
  right_join(pred_molt_inc) %>%
  mutate(yrs_lib = case_when(yrs_lib == 1 ~ "1 Year",
                             yrs_lib > 1 ~ paste0(yrs_lib, " Years"))) %>%
  
  ggplot()+
  geom_point(aes(x = pre_molt, y = obs_inc, size = nsamp), shape = 21)+
  geom_line(aes(x = pre_molt, y = pred_inc, color = model))+
  labs(x = "Pre-Molt Carapace Length (mm)", y = "Mean Molt Increment", color = NULL, size = "Sample Size", title = "WAG Tagging")+
  scale_color_manual(values = cbpalette)+
  facet_wrap(~yrs_lib, scales = "free")+
  theme(plot.title = element_text(hjust = 0.5)) -> x
ggsave("AIGKC/figures/models/2026/nov/wag_tagging_slx.png", plot = x, width = 9, height = 6, units = "in")




# likelihood ----

# base models
gmacs_get_lik(list(eag23.1c, eag25.0a)) %>%
  filter(!process %in% c(paste0("rec_pen_", 1:3), "tagging_1", "growth_1")) %>%
  bind_rows(gmacs_get_lik_type_pen(list(eag23.1c, eag25.0a)) %>%
              filter(!(process %in% c("catch", "index", "size", "total", "Sex_ratio")))) %>%
  dplyr::slice(c(1:8, 11:12, 14:16, 9:10)) %>%
  mutate(process = c("Retained Catch", "Total Catch", "Groundfish Bycatch", "Obs CPUE 1995 - 2004", "Obs CPUE 2005 - 2024",
                     "FT CPUE 1985 - 1998", "Retained Size Composition", "Total Size Composition", "Recruitment", "Tagging",
                     "Penalties", "Priors", "Initial Conditions", "N Parameters", "Total NLL")) %>%
  rename_all(~c("process", "eag23.1c", "eag25.0a")) %>%
  mutate(space = NA) %>%
  left_join(gmacs_get_lik(list(wag23.1c, wag25.0a)) %>%
              filter(!process %in% c(paste0("rec_pen_", 1:3), "tagging_1", "growth_1")) %>%
              bind_rows(gmacs_get_lik_type_pen(list(eag23.1c, eag25.0a)) %>%
                          filter(!(process %in% c("catch", "index", "size", "total", "Sex_ratio")))) %>%
              dplyr::slice(c(1:8, 11:12, 14:16, 9:10)) %>%
              mutate(process = c("Retained Catch", "Total Catch", "Groundfish Bycatch", "Obs CPUE 1995 - 2004", "Obs CPUE 2005 - 2024",
                                 "FT CPUE 1985 - 1998", "Retained Size Composition", "Total Size Composition", "Recruitment", "Tagging",
                                 "Penalties", "Priors", "Initial Conditions", "N Parameters", "Total NLL")) %>%
              rename_all(~c("process", "wag23.1c", "wag25.0a"))) %>%
  write_csv("./AIGKC/output/models/2026/nov/base_lik_table.csv")

# eag
gmacs_get_lik(list(eag26.0, eag26.0a, eag26.1, eag26.1a, eag26.1b, eag26.1c)) %>%
  filter(!process %in% c(paste0("rec_pen_", 1:3), "tagging_1", "growth_1")) %>%
  bind_rows(gmacs_get_lik_type_pen(list(eag26.0, eag26.0a, eag26.1, eag26.1a, eag26.1b, eag26.1c)) %>%
              filter(!(process %in% c("catch", "index", "size", "total", "Sex_ratio")))) %>%
  dplyr::slice(c(1:7, 10:11, 13:15, 8:9)) %>%
  mutate(process = c("Retained Catch", "Total Catch", "Groundfish Bycatch", "Obs CPUE 1995 - 2024",
                     "FT CPUE 1985 - 1998", "Retained Size Composition", "Total Size Composition", "Recruitment", "Tagging",
                     "Penalties", "Priors", "Initial Conditions", "N Parameters", "Total NLL")) %>%
  write_csv("./AIGKC/output/models/2026/nov/eag_lik_table.csv") 

# wag
gmacs_get_lik(list(wag26.0, wag26.0a, wag26.1, wag26.1a, wag26.1b, wag26.1c)) %>%
  filter(!process %in% c(paste0("rec_pen_", 1:3), "tagging_1", "growth_1")) %>%
  bind_rows(gmacs_get_lik_type_pen(list(wag26.0, wag26.0a, wag26.1, wag26.1a, wag26.1b, wag26.1c)) %>%
              filter(!(process %in% c("catch", "index", "size", "total", "Sex_ratio")))) %>%
  dplyr::slice(c(1:7, 10:11, 13:15, 8:9)) %>%
  mutate(process = c("Retained Catch", "Total Catch", "Groundfish Bycatch", "Obs CPUE 1995 - 2024",
                     "FT CPUE 1985 - 1998", "Retained Size Composition", "Total Size Composition", "Recruitment", "Tagging",
                     "Penalties", "Priors", "Initial Conditions", "N Parameters", "Total NLL")) %>%
  write_csv("./AIGKC/output/models/2026/nov/wag_lik_table.csv") 


# aleutian island combined model ----

gmacs_get_lik(list(ai26.x)) %>%
  print(n = 1000)


# directed fishery catch
gmacs_plot_catch(list(ai26.x), save_plot = F) -> tmp
tmp[[2]] + ggtitle(label = "EAG") + theme(plot.title = element_text(hjust = 0.5)) + labs(y = "Retained Catch (1,000s)") -> p1
tmp[[1]] + labs(y = "Retained Catch (t)") -> p2
tmp[[5]] + labs(y = "Total Catch (t)") -> p3
tmp[[4]] + ggtitle(label = "WAG") + theme(plot.title = element_text(hjust = 0.5)) + labs(y = "Retained Catch (1,000s)") -> p4
tmp[[3]] + labs(y = "Retained Catch (t)") -> p5
tmp[[6]] + labs(y = "Total Catch (t)") -> p6
wrap_plots(list(p1, p2, p3, p4, p5, p6), nrow = 3, ncol = 2, byrow = F, guides = "collect")
# bycatch
tmp[[7]] + labs(y = "Bycatch (t)")

# index
gmacs_plot_index(list(ai26.x), save_plot = F) -> tmp
tmp[[1]] + ggtitle(label = "EAG") + theme(plot.title = element_text(hjust = 0.5)) + labs(y = "Observer CPUE Index") -> p1
tmp[[2]] + labs(y = "Fish Ticket CPUE Index") -> p2
tmp[[3]] + ggtitle(label = "WAG") + theme(plot.title = element_text(hjust = 0.5)) + labs(y = "Observer CPUE Index") -> p3
tmp[[4]] + labs(y = "Fish Ticket CPUE Index") -> p4
wrap_plots(list(p1, p2, p3, p4), nrow = 2, ncol = 2, byrow = F, guides = "collect")

# size
gmacs_plot_size_fit_summary(list(ai26.x), save_plot = F)


gmacs_get_slx(list(ai26.x)) %>%
  filter(fleet == "Directed_Fishery", year %in% c(2000, 2006)) %>%
  ggplot()+
  geom_line(aes(x = size, y = slx_capture, group = year))+
  facet_wrap(~sex)

# recruitment
std <- gmacs_read_std("./AIGKC/models/2026/nov/AI/26.x/gmacs.std")
gmacs_get_derived_quantity_summary(list(ai26.x)) %>%
  bind_cols(std %>% 
              filter(grepl("sd_log_recruits", par)) %>%
              transmute(rec_se = se / (1 / exp(est)),
                        rec_lci = exp(est) + rec_se * qnorm(0.05 / 2),
                        rec_uci = exp(est) + rec_se * qnorm(1 - 0.05 / 2)) %>%
              dplyr::slice(1:(nrow(.)/2))) %>%
  transmute(year, model, subdistrict = "EAG", recruits = recruit_male, l95 = rec_lci, u95 = rec_uci) %>%
  bind_rows(gmacs_get_derived_quantity_summary(list(ai26.x)) %>%
              bind_cols(std %>% 
                          filter(grepl("sd_log_recruits", par)) %>%
                          transmute(rec_se = se / (1 / exp(est)),
                                    rec_lci = exp(est) + rec_se * qnorm(0.05 / 2),
                                    rec_uci = exp(est) + rec_se * qnorm(1 - 0.05 / 2)) %>%
                          dplyr::slice((nrow(.)/2 + 1):nrow(.))) %>%
              transmute(year, model, subdistrict = "WAG", recruits = recruit_female, l95 = rec_lci, u95 = rec_uci)) %>%
  ggplot()+
  geom_line(aes(x = factor(year), y = recruits, group = model, color = model))+
  geom_ribbon(aes(x = factor(year), ymin = l95, ymax = u95, group = model, fill = model), alpha = 0.2, show.legend = F)+
  scale_x_discrete(breaks = yraxis$breaks, labels = yraxis$labels)+
  scale_y_continuous(labels = scales::comma, limits = c(NA, NA))+
  labs(x = NULL, y = "Recruitment (1000s)", color = NULL)+
  scale_color_manual(values = cbpalette)+
  scale_fill_manual(values = cbpalette)+
  facet_wrap(~subdistrict, ncol = 1)+
  theme(legend.position = c(1, 1),
        legend.justification = c(1, 1))


gmacs_plot_mmb(list(ai26.x), save_plot = F)

gmacs_get_derived_quantity_summary(list(ai26.x, eag26.0a, wag26.0a)) %>%
  group_by(model, year) %>%
  mutate(ssb = sum(ssb)) %>%
  ggplot()+
  geom_line(aes(x = year, y = ssb, color = model))

gmacs_get_lik(list(ai26.x, wag26.0a)) %>% print(n = 1000)

