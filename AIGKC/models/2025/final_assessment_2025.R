# 2025 AIGKC final assessment
## tyler jackson
## tyler.jackson@alaska.gov
## 5/6/2025

# load ----

library(gmacsr)

# run initial models ----
# gmacs_do_exe(gmacs.dat = "./AIGKC/models/2025/may/EAG/23.1c/gmacs.dat", pin = F, reweight = T, level = 0.001)
# gmacs_do_exe(gmacs.dat = "./AIGKC/models/2025/may/EAG/23.1c_complete/gmacs.dat", pin = F, reweight = T, level = 0.001)
# gmacs_do_exe(gmacs.dat = "./AIGKC/models/2025/may/EAG/25.0b/gmacs.dat", pin = F, reweight = T, level = 0.001)
# gmacs_do_exe(gmacs.dat = "./AIGKC/models/2025/may/EAG/25.0a/gmacs.dat", pin = F, reweight = T, level = 0.001)
# gmacs_do_exe(gmacs.dat = "./AIGKC/models/2025/may/EAG/25.0/gmacs.dat", pin = F, reweight = T, level = 0.001)

# gmacs_do_exe(gmacs.dat = "./AIGKC/models/2025/may/WAG/23.1c/gmacs.dat", pin = F, reweight = T, level = 0.001)
# gmacs_do_exe(gmacs.dat = "./AIGKC/models/2025/may/WAG/25.0b/gmacs.dat", pin = F, reweight = T, level = 0.001)


# jittering ----

gmacs_do_jitter(gmacs.dat = "./AIGKC/models/2025/may/WAG/23.1c/gmacs.dat",
                sd = 0.3, iter = 500, model_name = "23.1c", version = "2.20.20")
gmacs_do_jitter(gmacs.dat = "./AIGKC/models/2025/may/WAG/25.0b/gmacs.dat",
                sd = 0.3, iter = 500, model_name = "25.0b", version = "2.20.20")

gmacs_do_jitter(gmacs.dat = "./AIGKC/models/2025/may/EAG/23.1c/gmacs.dat",
                sd = 0.3, iter = 500, model_name = "23.1c", version = "2.20.20")
gmacs_do_jitter(gmacs.dat = "./AIGKC/models/2025/may/EAG/25.0b/gmacs.dat",
                sd = 0.3, iter = 500, model_name = "25.0b", version = "2.20.20")

read_csv("./AIGKC/models/2025/may/EAG/23.1c/output/23.1c_jitter_sd_0.3.csv") %>%
  mutate(obj_function = round(obj_function, 3)) %>%
  group_by(obj_function) %>%
  summarise(catch_lik = mean(catch_lik), 
            index_lik = mean(index_lik), 
            size_lik = mean(size_lik), 
            mmb_curr = mean(mmb_curr),  
            bmsy = mean(bmsy),   
            ofl = mean(ofl),
    n = n(), prop = n / 500) %>%
  write_csv("./AIGKC/output/models/2025/may/jitter_eag_23.1c.csv")

read_csv("./AIGKC/models/2025/may/EAG/25.0b/output/25.0b_jitter_sd_0.3.csv") %>%
  mutate(obj_function = round(obj_function, 3)) %>%
  group_by(obj_function) %>%
  summarise(catch_lik = mean(catch_lik), 
            index_lik = mean(index_lik), 
            size_lik = mean(size_lik), 
            mmb_curr = mean(mmb_curr),  
            bmsy = mean(bmsy),   
            ofl = mean(ofl),
            n = n(), prop = n / 500) %>%
  write_csv("./AIGKC/output/models/2025/may/jitter_eag_25.0b.csv")

# WAG 
read_csv("./AIGKC/models/2025/may/WAG/23.1c/output/23.1c_jitter_sd_0.3.csv") %>%
  mutate(obj_function = round(obj_function, 3)) %>%
  group_by(obj_function) %>%
  summarise(catch_lik = mean(catch_lik), 
            index_lik = mean(index_lik), 
            size_lik = mean(size_lik), 
            mmb_curr = mean(mmb_curr),  
            bmsy = mean(bmsy),   
            ofl = mean(ofl),
            n = n(), prop = n / 500) %>%
  write_csv("./AIGKC/output/models/2025/may/jitter_wag_23.1c.csv")

read_csv("./AIGKC/models/2025/may/WAG/25.0b/output/25.0b_jitter_sd_0.3.csv") %>%
  mutate(obj_function = round(obj_function, 3)) %>%
  group_by(obj_function) %>%
  summarise(catch_lik = mean(catch_lik), 
            index_lik = mean(index_lik), 
            size_lik = mean(size_lik), 
            mmb_curr = mean(mmb_curr),  
            bmsy = mean(bmsy),   
            ofl = mean(ofl),
            n = n(), prop = n / 500) %>%
  write_csv("./AIGKC/output/models/2025/may/jitter_wag_25.0b.csv")

# load outputs ----

# wag
wag23.1c_v16 <- gmacs_read_allout("./AIGKC/models/2025/may/WAG/23.1c_v16/Gmacsall.out", model_name = "23.1c v16", version = "2.20.16")
wag23.1c <- gmacs_read_allout("./AIGKC/models/2025/may/WAG/23.1c/Gmacsall.out", model_name = "23.1c", version = "2.20.21")
wag25.0b <- gmacs_read_allout("./AIGKC/models/2025/may/WAG/25.0b/Gmacsall.out", model_name = "25.0b", version = "2.20.21")

# eag
eag23.1c_v16 <- gmacs_read_allout("./AIGKC/models/2025/may/EAG/23.1c_v16/Gmacsall.out", model_name = "23.1c v16", version = "2.20.16")
eag23.1c <- gmacs_read_allout(file = "./AIGKC/models/2025/may/EAG/23.1c/Gmacsall.out", model_name = "23.1c", version = "2.20.21")
eag25.0b <- gmacs_read_allout("./AIGKC/models/2025/may/EAG/25.0b/Gmacsall.out", model_name = "25.0b", version = "2.20.21")

eag23.1c_comp <- gmacs_read_allout(file = "./AIGKC/models/2025/may/EAG/23.1c_complete/Gmacsall.out", model_name = "23.1c Complete", version = "2.20.21")

# gmacs version table ----

gmacs_get_lik(list(eag23.1c_v16, eag23.1c)) %>%
  rename_all(~c("process", "eag_16", "eag_21")) %>%
  mutate(space = NA) %>%
  left_join(gmacs_get_lik(list(wag23.1c_v16, wag23.1c)) %>%
              rename_all(~c("process", "wag_16", "wag_21"))) %>%
  mutate(process = gsub("_1|_2|_3", "", process)) %>%
  group_by(process) %>% summarise_all(sum) %>%
  filter(process != "growth") %>%
  write_csv("./AIGKC/output/models/2025/may/version_lik_table.csv")

# likelihood table ----

gmacs_get_lik(list(eag23.1c, eag25.0b)) %>%
  filter(!process %in% c(paste0("rec_pen_", 1:3), "tagging_1", "growth_1")) %>%
  bind_rows(gmacs_get_lik_type_pen(list(eag23.1c, eag25.0b)) %>%
              filter(!(process %in% c("catch", "index", "size", "total", "Sex_ratio")))) %>%
  dplyr::slice(c(1:8, 11:15, 9:10)) %>%
  mutate(process = c("Retained Catch", "Total Catch", "Groundfish Bycatch", "Obs CPUE 1995 - 2004", "Obs CPUE 2005 - 2024",
                     "FT CPUE 1985 - 1998", "Retained Size Composition", "Total Size Composition", "Recruitment", "Tagging",
                     "Penalties", "Priors", "Initial Conditions", "N Parameters", "Total NLL")) %>%
  rename_all(~c("process", "eag23.1c", "eag25.0b")) %>%
  mutate(space = NA) %>%
  left_join(gmacs_get_lik(list(wag23.1c, wag25.0b)) %>%
              filter(!process %in% c(paste0("rec_pen_", 1:3), "tagging_1", "growth_1")) %>%
              bind_rows(gmacs_get_lik_type_pen(list(eag23.1c, eag25.0b)) %>%
                          filter(!(process %in% c("catch", "index", "size", "total", "Sex_ratio")))) %>%
              dplyr::slice(c(1:8, 11:15, 9:10)) %>%
              mutate(process = c("Retained Catch", "Total Catch", "Groundfish Bycatch", "Obs CPUE 1995 - 2004", "Obs CPUE 2005 - 2024",
                                 "FT CPUE 1985 - 1998", "Retained Size Composition", "Total Size Composition", "Recruitment", "Tagging",
                                 "Penalties", "Priors", "Initial Conditions", "N Parameters", "Total NLL")) %>%
              rename_all(~c("process", "wag23.1c", "wag25.0b"))) %>%
  write_csv("./AIGKC/output/models/2025/may/lik_table.csv")

# parameters table ----

gmacs_get_pars(list(wag23.1c)) %>%
  filter(phase > 0) %>%
  transmute(model, parameter, estimate, standard_error) %>%
  pivot_wider(names_from = model, values_from = c(estimate, standard_error)) %>%
  rename_all(~c("parameter", "est", "se")) %>%
  mutate(est_se = paste0(ifelse(est < 0.01, formatC(est, format = "e", digits = 2), round(est, 2)),
                         " (", ifelse(se < 0.001, formatC(se, format = "e", digits = 2), round(se, 3)), ")")) %>%
  transmute(parameter, wag23.1c = est_se) %>%
  filter(!grepl("dev", parameter)) %>% 
  mutate(parameter = c("ln R$_{0}$", "Rec Dist Scale", "Growth $\\alpha$", "Growth $\\beta$", "Growth $\\sigma$", "Molt probability $\\mu$", "Molt probability $cv$",
                       "Sel ln $S_{50}$ Pre-Rat", "Sel ln $S_{\\Delta}$ Pre-Rat", "Sel ln $S_{50}$ Post-Rat", "Sel ln $S_{\\Delta}$ Post-Rat", 
                       "Ret ln $R_{50}$", "Ret ln $R_{\\Delta}$",
                       "ln $\\bar{F}$ Directed Fishery", "ln $\\bar{F}$ Groundfish Fisheries", 
                       "Obs CPUE $q$ 1995-2004", "Obs CPUE $q$ 2005-2024", "FT CPUE $q$ 1985-1998", 
                       "ln extra $cv$ Obs CPUE 1995-2004", "ln extra $cv$ Obs CPUE 2005-2024", "ln extra $cv$ FT CPUE 1985-1998")) -> wag23.1cpar

gmacs_get_pars(list(wag25.0b)) %>%
  filter(phase > 0) %>%
  transmute(model, parameter, estimate, standard_error) %>%
  pivot_wider(names_from = model, values_from = c(estimate, standard_error)) %>%
  rename_all(~c("parameter", "est", "se")) %>%
  mutate(est_se = paste0(ifelse(est < 0.01, formatC(est, format = "e", digits = 2), round(est, 2)),
                         " (", ifelse(se < 0.001, formatC(se, format = "e", digits = 2), round(se, 3)), ")")) %>%
  transmute(parameter, wag25.0b = est_se) %>%
  filter(!grepl("dev|logN", parameter)) %>%
  mutate(parameter = c("ln R$_{ini}$", "ln $\\bar{R}$", "Rec Dist Scale", "Growth $\\alpha$", "Growth $\\beta$", "Growth $\\sigma$", "Molt probability $\\mu$", "Molt probability $cv$",
                       "Sel ln $S_{50}$ Pre-Rat", "Sel ln $S_{\\Delta}$ Pre-Rat", "Sel ln $S_{50}$ Post-Rat", "Sel ln $S_{\\Delta}$ Post-Rat", 
                       "Ret ln $R_{50}$", "Ret ln $R_{\\Delta}$",
                       "ln $\\bar{F}$ Directed Fishery", "ln $\\bar{F}$ Groundfish Fisheries",
                       "Obs CPUE $q$ 1995-2004", "Obs CPUE $q$ 2005-2024", "FT CPUE $q$ 1985-1998", 
                       "ln extra $cv$ Obs CPUE 1995-2004", "ln extra $cv$ Obs CPUE 2005-2024", "ln extra $cv$ FT CPUE 1985-1998")) -> wag25.0bpar

gmacs_get_pars(list(eag23.1c)) %>%
  filter(phase > 0) %>%
  transmute(model, parameter, estimate, standard_error) %>%
  pivot_wider(names_from = model, values_from = c(estimate, standard_error)) %>%
  rename_all(~c("parameter", "est", "se")) %>%
  mutate(est_se = paste0(ifelse(est < 0.01, formatC(est, format = "e", digits = 2), round(est, 2)),
                         " (", ifelse(se < 0.001, formatC(se, format = "e", digits = 2), round(se, 3)), ")")) %>%
  transmute(parameter, eag23.1c = est_se) %>%
  filter(!grepl("dev", parameter)) %>% 
  mutate(parameter = c("ln R$_{0}$", "Rec Dist Scale", "Growth $\\alpha$", "Growth $\\beta$", "Growth $\\sigma$", "Molt probability $\\mu$", "Molt probability $cv$",
                       "Sel ln $S_{50}$ Pre-Rat", "Sel ln $S_{\\Delta}$ Pre-Rat", "Sel ln $S_{50}$ Post-Rat", "Sel ln $S_{\\Delta}$ Post-Rat", 
                       "Ret ln $R_{50}$", "Ret ln $R_{\\Delta}$",
                       "ln $\\bar{F}$ Directed Fishery", "ln $\\bar{F}$ Groundfish Fisheries", 
                       "Obs CPUE $q$ 1995-2004", "Obs CPUE $q$ 2005-2024", "FT CPUE $q$ 1985-1998", 
                       "ln extra $cv$ Obs CPUE 1995-2004", "ln extra $cv$ Obs CPUE 2005-2024", "ln extra $cv$ FT CPUE 1985-1998")) -> eag23.1cpar

gmacs_get_pars(list(eag25.0b)) %>%
  filter(phase > 0) %>%
  transmute(model, parameter, estimate, standard_error) %>%
  pivot_wider(names_from = model, values_from = c(estimate, standard_error)) %>%
  rename_all(~c("parameter", "est", "se")) %>%
  mutate(est_se = paste0(ifelse(est < 0.01, formatC(est, format = "e", digits = 2), round(est, 2)),
                         " (", ifelse(se < 0.001, formatC(se, format = "e", digits = 2), round(se, 3)), ")")) %>%
  transmute(parameter, eag25.0b = est_se) %>%
  filter(!grepl("dev|logN", parameter)) %>% 
  mutate(parameter = c("ln R$_{ini}$", "ln $\\bar{R}$", "Rec Dist Scale", "Growth $\\alpha$", "Growth $\\beta$", "Growth $\\sigma$", "Molt probability $\\mu$", "Molt probability $cv$",
                       "Sel ln $S_{50}$ Pre-Rat", "Sel ln $S_{\\Delta}$ Pre-Rat", "Sel ln $S_{50}$ Post-Rat", "Sel ln $S_{\\Delta}$ Post-Rat", 
                       "Ret ln $R_{50}$", "Ret ln $R_{\\Delta}$",
                       "ln $\\bar{F}$ Directed Fishery", "ln $\\bar{F}$ Groundfish Fisheries", 
                       "Obs CPUE $q$ 1995-2004", "Obs CPUE $q$ 2005-2024", "FT CPUE $q$ 1985-1998", 
                       "ln extra $cv$ Obs CPUE 1995-2004", "ln extra $cv$ Obs CPUE 2005-2024", "ln extra $cv$ FT CPUE 1985-1998")) -> eag25.0bpar

eag23.1cpar %>% full_join(eag25.0bpar %>% mutate(space = NA)) %>% full_join(wag23.1cpar) %>% full_join(wag25.0bpar) %>% 
  dplyr::slice(1, 22:23, 2:21) %>% 
  write_csv("./AIGKC/output/models/2025/may/parameter_est.csv")

# mmb and recruitment table ----

# mmb 
gmacs_get_derived_quantity_summary(list(eag23.1c)) %>%
  transmute(year, ssb = round(ssb)) %>%
  bind_cols(gmacs_read_std("AIGKC/models/2025/may/EAG/23.1c/gmacs.std") %>%
              filter(grepl("sd_log_ssb", par)) %>%
              transmute(se = se / (1 / exp(est)))) %>%
  transmute(year, eag23.1c = paste0(prettyNum(round(ssb), big.mark = ","), " (",prettyNum(round(se), big.mark = ","),")")) -> eag23.1c_ssb
gmacs_get_derived_quantity_summary(list(eag25.0b)) %>%
  transmute(year, ssb = round(ssb)) %>%
  bind_cols(gmacs_read_std("AIGKC/models/2025/may/EAG/25.0b/gmacs.std") %>%
              filter(grepl("sd_log_ssb", par)) %>%
              transmute(se = se / (1 / exp(est)))) %>%
  transmute(year, eag25.0b = paste0(prettyNum(round(ssb), big.mark = ","), " (",prettyNum(round(se), big.mark = ","),")")) -> eag25.0b_ssb
gmacs_get_derived_quantity_summary(list(wag23.1c)) %>%
  transmute(year, ssb = round(ssb)) %>%
  bind_cols(gmacs_read_std("AIGKC/models/2025/may/WAG/23.1c/gmacs.std") %>%
              filter(grepl("sd_log_ssb", par)) %>%
              transmute(se = se / (1 / exp(est)))) %>%
  transmute(year, wag23.1c = paste0(prettyNum(round(ssb), big.mark = ","), " (",prettyNum(round(se), big.mark = ","),")")) -> wag23.1c_ssb
gmacs_get_derived_quantity_summary(list(wag25.0b)) %>%
  transmute(year, ssb = round(ssb)) %>%
  bind_cols(gmacs_read_std("AIGKC/models/2025/may/WAG/25.0b/gmacs.std") %>%
              filter(grepl("sd_log_ssb", par)) %>%
              transmute(se = se / (1 / exp(est)))) %>%
  transmute(year, wag25.0b = paste0(prettyNum(round(ssb), big.mark = ","), " (",prettyNum(round(se), big.mark = ","),")")) -> wag25.0b_ssb

eag23.1c_ssb %>% full_join(eag25.0b_ssb %>% mutate(space = NA)) %>% full_join(wag23.1c_ssb) %>% full_join(wag25.0b_ssb) %>%
  write_csv("./AIGKC/output/models/2025/may/mmb_est.csv")

# recruitment
gmacs_get_derived_quantity_summary(list(eag23.1c)) %>%
  transmute(year, recruit_male = round(recruit_male)) %>%
  bind_cols(gmacs_read_std("AIGKC/models/2025/may/EAG/23.1c/gmacs.std") %>% 
              filter(grepl("sd_log_recruits", par)) %>%
              transmute(se = se / (1 / exp(est)))) %>%
  transmute(year, eag23.1c = paste0(prettyNum(round(recruit_male), big.mark = ","), " (",prettyNum(round(se), big.mark = ","),")")) -> eag23.1c_rec
gmacs_get_derived_quantity_summary(list(eag25.0b)) %>%
  transmute(year, recruit_male = round(recruit_male)) %>%
  bind_cols(gmacs_read_std("AIGKC/models/2025/may/EAG/25.0b/gmacs.std") %>%
              filter(grepl("sd_log_recruits", par)) %>%
              transmute(se = se / (1 / exp(est)))) %>%
  transmute(year, eag25.0b = paste0(prettyNum(round(recruit_male), big.mark = ","), " (",prettyNum(round(se), big.mark = ","),")")) -> eag25.0b_rec
gmacs_get_derived_quantity_summary(list(wag23.1c)) %>%
  transmute(year, recruit_male = round(recruit_male)) %>%
  bind_cols(gmacs_read_std("AIGKC/models/2025/may/WAG/23.1c/gmacs.std") %>%
              filter(grepl("sd_log_recruits", par)) %>%
              transmute(se = se / (1 / exp(est)))) %>%
  transmute(year, wag23.1c = paste0(prettyNum(round(recruit_male), big.mark = ","), " (",prettyNum(round(se), big.mark = ","),")")) -> wag23.1c_rec
gmacs_get_derived_quantity_summary(list(wag25.0b)) %>%
  transmute(year, recruit_male = round(recruit_male)) %>%
  bind_cols(gmacs_read_std("AIGKC/models/2025/may/WAG/25.0b/gmacs.std") %>%
              filter(grepl("sd_log_recruits", par)) %>%
              transmute(se = se / (1 / exp(est)))) %>%
  transmute(year, wag25.0b = paste0(prettyNum(round(recruit_male), big.mark = ","), " (",prettyNum(round(se), big.mark = ","),")")) -> wag25.0b_rec

eag23.1c_rec %>% full_join(eag25.0b_rec %>% mutate(space = NA)) %>% full_join(wag23.1c_rec) %>% full_join(wag25.0b_rec) %>%
  write_csv("./AIGKC/output/models/2025/may/recruitment_est.csv")

# reference points table ----

gmacs_get_ref_points(list(eag23.1c, eag25.0b, wag23.1c, wag25.0b)) %>%
  bind_cols(tibble(subdistrict = c("EAG", NA, "WAG", NA)), .) %>% as.data.frame %>%
  write_csv("AIGKC/output/models/2025/may/reference_points_table.csv")

# plot of model data -----

eagrange <- gmacs_plot_data_range(list(wag23.1c), save_plot = F)$plot[[1]]+
  labs(title = "EAG") + theme(plot.title = element_text(hjust = 0.5))
wagrange <- gmacs_plot_data_range(list(eag23.1c), save_plot = F)$plot[[1]]+
  labs(title = "WAG") + theme(plot.title = element_text(hjust = 0.5))
ggsave("AIGKC/figures/models/2025/may/data_range.png", plot = eagrange/wagrange, height = 7, width = 6, units = "in")



# catch and tac ----

# wag plot
gmacs_get_catch_summary(list(wag23.1c)) %>%
  filter(series %in% 1:2, units == "Mass") %>%
  transmute(year, type, obs_catch) %>%
  pivot_wider(names_from = type, values_from = obs_catch) %>% 
  mutate(`Discard M` = (All - Retained) * 0.2) %>%
  left_join(read_csv("AIGKC/data/tac/aigkc_tac.csv") %>% rename(year = crab_year) %>% filter(fishery == "WAG")) %>%
  mutate(tac_t = tac_mil_lb * 1e6 * 0.000453592) %>%
  pivot_longer(c(Retained, `Discard M`), names_to = "type", values_to = "catch") %>%
  ggplot()+
  geom_bar(aes(x = factor(year), y = catch, fill = type), stat = "identity", color = 1, width = 1)+
  geom_line(aes(x = factor(year), y = tac_t, group = 1), color = "firebrick", size = 1)+
  scale_fill_manual(values = c("grey50", "grey90"))+
  labs(x = NULL, y = "Catch (t)", fill = NULL, title = "WAG")+
  scale_x_discrete(breaks = yraxis$breaks, labels = yraxis$labels)+
  scale_y_continuous(labels = scales::comma)+
  theme(legend.position = c(1, 1), legend.justification = c(1, 1), plot.title = element_text(hjust = 0.5)) -> x
ggsave("AIGKC/figures/models/2025/may/wag_catch_time_series.png", plot = x, height = 4, width = 6, units = "in")


# eag plot
gmacs_get_catch_summary(list(eag23.1c)) %>%
  filter(series %in% 1:2, units == "Mass") %>%
  transmute(year, type, obs_catch) %>%
  pivot_wider(names_from = type, values_from = obs_catch) %>% 
  mutate(`Discard M` = (All - Retained) * 0.2) %>%
  left_join(read_csv("AIGKC/data/tac/aigkc_tac.csv") %>% rename(year = crab_year) %>% filter(fishery == "EAG")) %>%
  mutate(tac_t = tac_mil_lb * 1e6 * 0.000453592) %>%
  pivot_longer(c(Retained, `Discard M`), names_to = "type", values_to = "catch") %>%
  ggplot()+
  geom_bar(aes(x = factor(year), y = catch, fill = type), stat = "identity", color = 1, width = 1)+
  geom_line(aes(x = factor(year), y = tac_t, group = 1), color = "firebrick", size = 1)+
  scale_fill_manual(values = c("grey50", "grey90"))+
  labs(x = NULL, y = "Catch (t)", fill = NULL, title = "EAG")+
  scale_x_discrete(breaks = yraxis$breaks, labels = yraxis$labels)+
  scale_y_continuous(labels = scales::comma)+
  theme(legend.position = c(1, 1), legend.justification = c(1, 1), plot.title = element_text(hjust = 0.5)) -> x
ggsave("AIGKC/figures/models/2025/may/eag_catch_time_series.png", plot = x, height = 4, width = 6, units = "in")

# fit to catch ----

## wag
wag_catch <- gmacs_plot_catch(list(wag23.1c, wag25.0b), save_plot = F)
(wag_catch[[2]] + wag_catch[[1]]) / (wag_catch[[3]] + wag_catch[[4]]) + 
  plot_layout(guides = "collect") +
  plot_annotation(title = "WAG",
                  theme = theme(plot.title = element_text(hjust = 0.5))) -> x
ggsave("AIGKC/figures/models/2025/may/wag_catch_fit.png", plot = x, width = 11, height = 8, units = "in")

## eag
eag_catch <- gmacs_plot_catch(list(eag23.1c, eag25.0b), save_plot = F)
(eag_catch[[2]] + eag_catch[[1]]) / (eag_catch[[3]] + eag_catch[[4]]) + 
  plot_layout(guides = "collect") +
  plot_annotation(title = "EAG",
                  theme = theme(plot.title = element_text(hjust = 0.5))) -> x
ggsave("AIGKC/figures/models/2025/may/eag_catch_fit.png", plot = x, width = 11, height = 8, units = "in")

# fit to index ----

## wag
wag_index <- gmacs_plot_index(list(wag23.1c, wag25.0b), save_plot = F)
(wag_index[[3]] + ylab("Fish Ticket CPUE")) / (wag_index[[1]] + ylab("Observer CPUE")) / (wag_index[[2]] + ylab("Observer CPUE")) + 
  plot_layout(guides = "collect") +
  plot_annotation(title = "WAG",
                  theme = theme(plot.title = element_text(hjust = 0.5))) -> x
ggsave("AIGKC/figures/models/2025/may/wag_index_fit.png", plot = x, width = 6, height = 7, units = "in")

## eag
eag_index <- gmacs_plot_index(list(eag23.1c, eag25.0b), save_plot = F)
(eag_index[[3]] + ylab("Fish Ticket CPUE")) / (eag_index[[1]] + ylab("Observer CPUE")) / (eag_index[[2]] + ylab("Observer CPUE")) + 
  plot_layout(guides = "collect") +
  plot_annotation(title = "EAG",
                  theme = theme(plot.title = element_text(hjust = 0.5))) -> x
ggsave("AIGKC/figures/models/2025/may/eag_index_fit.png", plot = x, width = 6, height = 7, units = "in")

# fit to size data ----

# wag
gmacs_plot_size_comp(list(wag23.1c, wag25.0b), save_plot = F, add_n = F, add_n_est = F)[[1]]+
  labs(x = "Carapace Width (mm)", title = "WAG Retained") -> x
ggsave("AIGKC/figures/models/2025/may/wag_retained_comp_fit.png", plot = x, width = 6, height = 6, units = "in")
gmacs_plot_size_comp(list(wag23.1c, wag25.0b), save_plot = F, add_n = F, add_n_est = F)[[2]]+
  labs(x = "Carapace Width (mm)", title = "WAG Total") -> x
ggsave("AIGKC/figures/models/2025/may/wag_total_comp_fit.png", plot = x, width = 6, height = 6, units = "in")

# wag osa residuals
wrap_plots(gmacs_plot_osa_residuals(list(wag23.1c), size_lab = "Carapace Length (mm)", save_plot = F)[[1]],
           gmacs_plot_osa_residuals(list(wag23.1c), size_lab = "Carapace Length (mm)", save_plot = F)[[2]], 
           ncol = 1)+
  plot_annotation(tag_levels = "a") -> x
ggsave("AIGKC/figures/models/2025/may/wag_osa_residuals_23_1c.png", plot = x, width = 10, height = 7, units = "in")
wrap_plots(gmacs_plot_osa_residuals(list(wag25.0b), size_lab = "Carapace Length (mm)", save_plot = F)[[1]],
           gmacs_plot_osa_residuals(list(wag25.0b), size_lab = "Carapace Length (mm)", save_plot = F)[[2]], 
           ncol = 1)+
  plot_annotation(tag_levels = "a") -> x
ggsave("AIGKC/figures/models/2025/may/wag_osa_residuals_25_0b.png", plot = x, width = 10, height = 7, units = "in")

# eag
gmacs_plot_size_comp(list(eag23.1c, eag25.0b), save_plot = F, add_n = F, add_n_est = F)[[1]]+
  labs(x = "Carapace Width (mm)", title = "EAG Retained") -> x
ggsave("AIGKC/figures/models/2025/may/eag_retained_comp_fit.png", plot = x, width = 6, height = 6, units = "in")
gmacs_plot_size_comp(list(eag23.1c, eag25.0b), save_plot = F, add_n = F, add_n_est = F)[[2]]+
  labs(x = "Carapace Width (mm)", title = "EAG Total") -> x
ggsave("AIGKC/figures/models/2025/may/eag_total_comp_fit.png", plot = x, width = 6, height = 6, units = "in")

# eag osa residuals
## 23.1c
wrap_plots(gmacs_plot_osa_residuals(list(eag23.1c), size_lab = "Carapace Length (mm)", save_plot = F)[[1]],
           gmacs_plot_osa_residuals(list(eag23.1c), size_lab = "Carapace Length (mm)", save_plot = F)[[2]], 
           ncol = 1)+
  plot_annotation(tag_levels = "a") -> x
ggsave("AIGKC/figures/models/2025/may/eag_osa_residuals_23_1c.png", plot = x, width = 10, height = 7, units = "in")
## 25.0b
wrap_plots(gmacs_plot_osa_residuals(list(eag25.0b), size_lab = "Carapace Length (mm)", save_plot = F)[[1]],
           gmacs_plot_osa_residuals(list(eag25.0b), size_lab = "Carapace Length (mm)", save_plot = F)[[2]], 
           ncol = 1)+
  plot_annotation(tag_levels = "a") -> x
ggsave("AIGKC/figures/models/2025/may/eag_osa_residuals_25_0b.png", plot = x, width = 10, height = 7, units = "in")

# sample sizes
gmacs_get_effective_n(list(eag23.1c, eag25.0b)) %>%
  mutate(subdistrict = "EAG") %>% 
  bind_rows(gmacs_get_effective_n(list(wag23.1c, wag25.0b)) %>%
              mutate(subdistrict = "WAG")) %>%
  mutate(type = c("Retained Composition", "Total Compositon")[mod_series]) %>%
  ggplot()+
  geom_line(aes(x = year, y = nsamp_est, color = model))+
  facet_grid(rows = vars(subdistrict), cols = vars(type), scales = "free_y")+
  labs(x = NULL, y = "Re-weighted N", color = NULL)+
  scale_color_manual(values = cbpalette)


# n matrix ----

gmacs_get_n_matrix(list(eag23.1c, eag25.0b)) %>%
  #filter(year >= 1981) %>%
  ggplot()+
  geom_bar(aes(x = as.numeric(size), y = males, fill = model), stat = "identity", position = "identity", color = NA, width = 5, alpha = 0.5)+
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.1), add = c(0, 0)))+
  geom_text_npc(aes(npcx = "right", npcy = 0.6, label = year), check_overlap = T, size = 3)+
  facet_wrap(~year, ncol = 3, dir = "v")+
  scale_fill_manual(values = cbpalette)+
  labs(x = "Carapace Length (mm)", y = "N matrix", fill = NULL)+
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
        panel.background = element_blank()) -> x
ggsave("AIGKC/figures/models/2025/may/eag_nmatrix.png", plot = x, width = 6, height = 8, units = "in")

gmacs_get_n_matrix(list(wag23.1c, wag25.0b)) %>%
  #filter(year >= 1981) %>%
  ggplot()+
  geom_bar(aes(x = as.numeric(size), y = males, fill = model), stat = "identity", position = "identity", color = NA, width = 5, alpha = 0.5)+
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.1), add = c(0, 0)))+
  geom_text_npc(aes(npcx = "right", npcy = 0.6, label = year), check_overlap = T, size = 3)+
  facet_wrap(~year, ncol = 3, dir = "v")+
  scale_fill_manual(values = cbpalette)+
  labs(x = "Carapace Length (mm)", y = "N matrix", fill = NULL)+
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
        panel.background = element_blank()) -> x
ggsave("AIGKC/figures/models/2025/may/wag_nmatrix.png", plot = x, width = 6, height = 8, units = "in")


# plot size trans ----

eag23.1c$growth_transition$matrix %>%
  as_tibble() %>%
  rowid_to_column() %>%
  rename(to = rowid) %>%
  mutate(to = seq(103, 183, 5)[to]) %>%
  pivot_longer(2:ncol(.), names_to = "from", values_to = "p", names_transform = list(from = as.numeric)) %>% filter(p != 0) %>%
  mutate(model = "23.1c") %>%
  bind_rows(
    eag25.0b$growth_transition$matrix %>%
      as_tibble() %>%
      rowid_to_column() %>%
      rename(to = rowid) %>%
      mutate(to = seq(103, 183, 5)[to]) %>%
      pivot_longer(2:ncol(.), names_to = "from", values_to = "p", names_transform = list(from = as.numeric)) %>% filter(p != 0) %>%
      mutate(model = "25.0b")) %>%
  ggplot()+
  geom_violin(aes(x = from, y = to, group = paste0(from, model), weight = p, fill = model), position = "identity", color = NA, alpha = 0.5)
  labs(x = bquote(Shell~Height[t]~(mm)), y = bquote(Shell~Height[t+1]~(mm))) 


# selectivity ----

gmacs_get_slx(list(wag23.1c, wag25.0b)) %>%
  mutate(subdistrict = "WAG") %>%
  bind_rows(gmacs_get_slx(list(eag23.1c, eag25.0b)) %>%
              mutate(subdistrict = "EAG")) %>%
  filter(year %in% 2004:2005, fleet == "Directed_Fishery") %>%
  mutate(year = case_when(year == 2004 ~ "Pre-Rationalization",
                          year == 2005 ~ "Post-Rationalization"),
         year = factor(year, levels = c("Pre-Rationalization", "Post-Rationalization"))) %>%
  ggplot()+
  geom_line(aes(x = size, y = slx_capture, color = model, linetype = year))+
  facet_wrap(~subdistrict)+
  labs(x = "Carapace Width (mm)", y = "Selectivity", linetype = NULL, color = NULL)+
  scale_color_manual(values = cbpalette) -> x
ggsave("AIGKC/figures/models/2025/may/selectivity.png", plot = x, width = 7, height = 4, units = "in")


# recruitment ----

full_join(eag23.1c_rec, eag25.0b_rec) %>%
  pivot_longer(2:3, names_to = "model", values_to = "val") %>%
  separate(val, into = c("rec", "se"), sep = " ") %>%
  mutate(model = gsub("eag", "", model),
         rec = as.numeric(gsub(",", "", rec)),
         se = as.numeric(gsub("[()]|,", "", se))) %>%
  ggplot()+
  geom_ribbon(aes(x = as.numeric(as.factor(year)), ymin = rec + se * qnorm(0.05 / 2), ymax = rec + se * qnorm(1 - 0.05 / 2), fill = model),
              alpha = 0.2, show.legend = F)+
  geom_line(aes(x = factor(year), y = rec, group = model, color = model))+
  scale_x_discrete(breaks = yraxis$breaks, labels = yraxis$labels)+
  scale_y_continuous(labels = scales::comma, limits = c(NA, 8000))+
  scale_color_manual(values = cbpalette)+
  scale_fill_manual(values = c("grey70", NA))+
  labs(x = NULL, y = paste0("Recruitment (1,000)"), color = NULL, title = "EAG")+
  theme(legend.position = c(0, 1),
        legend.justification = c(0, 1),
        plot.title = element_text(hjust = 0.5)) -> eag_rec

full_join(wag23.1c_rec, wag25.0b_rec) %>%
  pivot_longer(2:3, names_to = "model", values_to = "val") %>%
  separate(val, into = c("rec", "se"), sep = " ") %>%
  mutate(model = gsub("eag", "", model),
         rec = as.numeric(gsub(",", "", rec)),
         se = as.numeric(gsub("[()]|,", "", se))) %>%
  ggplot()+
  geom_ribbon(aes(x = as.numeric(as.factor(year)), ymin = rec + se * qnorm(0.05 / 2), ymax = rec + se * qnorm(1 - 0.05 / 2), fill = model),
              alpha = 0.2, show.legend = F)+
  geom_line(aes(x = factor(year), y = rec, group = model, color = model))+
  scale_x_discrete(breaks = yraxis$breaks, labels = yraxis$labels)+
  scale_y_continuous(labels = scales::comma, limits = c(NA, 8000))+
  scale_color_manual(values = cbpalette)+
  scale_fill_manual(values = c("grey70", NA))+
  labs(x = NULL, y = paste0("Recruitment (1,000)"), color = NULL, title = "WAG")+
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5)) -> wag_rec

ggsave("AIGKC/figures/models/2025/may/recruitment.png", plot = eag_rec / wag_rec, width = 7, height = 6, units = "in")


# uncertainty
gmacs_get_pars(list(eag23.1c, eag25.0b)) %>%
  filter(grepl("Rec_dev", parameter)) %>%
  mutate(year = as.numeric(gsub("[^0-9]", "", parameter))) %>%
  ggplot()+
  geom_vline(xintercept = c(1987, 2021), linetype = 2)+
  geom_point(aes(x = year, y = standard_error, color = model))+
  labs(x = NULL, y = "Rec. Dev. SE", color = NULL, title = "EAG")+
  scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
  scale_color_manual(values = cbpalette)+
  theme(plot.title = element_text(hjust = 0.5)) -> x

gmacs_get_pars(list(wag23.1c, wag25.0b)) %>%
  filter(grepl("Rec_dev", parameter)) %>%
  mutate(year = as.numeric(gsub("[^0-9]", "", parameter))) %>%
  ggplot()+
  geom_vline(xintercept = c(1987, 2021), linetype = 2)+
  geom_point(aes(x = year, y = standard_error, color = model))+
  labs(x = NULL, y = "Rec. Dev. SE", color = NULL, title = "WAG")+
  scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
  scale_color_manual(values = cbpalette)+
  theme(plot.title = element_text(hjust = 0.5))  -> y

ggsave(file.path("AIGKC/figures/models/2025/may","rec_dev_se.png"), 
       plot = x/y + plot_layout(guides = "collect"), height = 5, width = 7, units = "in")


# mmb ----

gmacs_get_ref_points(list(eag23.1c, eag25.0b)) %>%
  mutate(year = max(eag23.1c_ssb$year)+ 1) 

full_join(eag23.1c_ssb, eag25.0b_ssb) %>%
  pivot_longer(2:3, names_to = "model", values_to = "val") %>%
  separate(val, into = c("ssb", "se"), sep = " ") %>%
  mutate(model = gsub("eag", "", model),
         ssb = as.numeric(gsub(",", "", ssb)),
         se = as.numeric(gsub("[()]|,", "", se))) %>%
  ggplot()+
  geom_ribbon(aes(x = as.numeric(as.factor(year)), ymin = ssb + se * qnorm(0.05 / 2), ymax = ssb + se * qnorm(1 - 0.05 / 2), fill = model),
              alpha = 0.2, show.legend = F)+
  geom_line(aes(x = factor(year), y = ssb, group = model, color = model))+
  geom_point(data = gmacs_get_ref_points(list(eag23.1c, eag25.0b)) %>%
               mutate(year = as.character(max(eag23.1c_ssb$year)+ 1)),
             aes(x = factor(year), y = mmb, color = model))+
  scale_x_discrete(breaks = yraxis$breaks, labels = yraxis$labels)+
  scale_y_continuous(labels = scales::comma, limits = c(0, 22000))+
  scale_color_manual(values = cbpalette)+
  scale_fill_manual(values = c("grey70", NA))+
  labs(x = NULL, y = paste0("MMB (t)"), color = NULL, title = "EAG")+
  theme(legend.position = c(1, 1),
        legend.justification = c(1, 1),
        plot.title = element_text(hjust = 0.5)) -> eag_mmb

full_join(wag23.1c_ssb, wag25.0b_ssb) %>%
  pivot_longer(2:3, names_to = "model", values_to = "val") %>%
  separate(val, into = c("ssb", "se"), sep = " ") %>%
  mutate(model = gsub("wag", "", model),
         ssb = as.numeric(gsub(",", "", ssb)),
         se = as.numeric(gsub("[()]|,", "", se))) %>%
  ggplot()+
  geom_ribbon(aes(x = as.numeric(as.factor(year)), ymin = ssb + se * qnorm(0.05 / 2), ymax = ssb + se * qnorm(1 - 0.05 / 2), fill = model),
              alpha = 0.2, show.legend = F)+
  geom_line(aes(x = factor(year), y = ssb, group = model, color = model))+
  geom_point(data = gmacs_get_ref_points(list(wag23.1c, wag25.0b)) %>%
               mutate(year = as.character(max(wag23.1c_ssb$year)+ 1)),
             aes(x = factor(year), y = mmb, color = model))+
  scale_x_discrete(breaks = yraxis$breaks, labels = yraxis$labels)+
  scale_y_continuous(labels = scales::comma, limits = c(0, 22000))+
  scale_color_manual(values = cbpalette)+
  scale_fill_manual(values = c("grey70", NA))+
  labs(x = NULL, y = paste0("MMB (t)"), color = NULL, title = "WAG")+
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5)) -> wag_mmb

ggsave("AIGKC/figures/models/2025/may/mature_male_biomass.png", plot = eag_mmb / wag_mmb, width = 7, height = 6, units = "in")

gmacs_plot_mmb(list(eag23.1c, eag23.1c_comp), save_plot = F)

# fishing mortality ----

gmacs_get_f(list(wag23.1c, wag25.0b)) %>%
  mutate(subdistrict = "WAG") %>%
  group_by(subdistrict, model, fleet, year) %>%
  summarise(F = sum(F)) %>%
  bind_rows(gmacs_get_f(list(eag23.1c, eag25.0b)) %>%
              mutate(subdistrict = "EAG") %>%
              group_by(subdistrict, model, fleet, year) %>%
              summarise(F = sum(F))) %>%
  mutate(fleet = gsub("_", " ", fleet)) %>%
  ggplot()+
  geom_line(aes(x = factor(year), y = F, color = model, group = model))+
  facet_grid(rows = vars(fleet), cols = vars(subdistrict), scales = "free_y")+
  scale_x_discrete(breaks = tickr(tibble(yr = 1900:3000), var = yr, 10)$breaks, 
                     labels = tickr(tibble(yr = 1900:3000), var = yr, 10)$labels)+
  labs(x = NULL, y = "F", color = NULL)+
  scale_color_manual(values = cbpalette)+
  theme(legend.position = c(0, 1), legend.justification = c(0, 1)) -> x
ggsave("AIGKC/figures/models/2025/may/fishing_mortality.png", plot = x, width = 7, height = 4, units = "in")


# kobe ----

gmacs_plot_kobe(all_out = list(eag25.0b), save_plot = F)[[1]]+
  labs(title = "EAG")+
  theme(plot.title = element_text(hjust = 0.5)) -> ekobe
gmacs_plot_kobe(all_out = list(wag25.0b), save_plot = F)[[1]]+
  labs(title = "WAG")+
  theme(plot.title = element_text(hjust = 0.5)) -> wkobe
ggsave("AIGKC/figures/models/2025/may/kobe_25.0b.png", plot = ekobe + wkobe, width = 8, height = 4, units = "in")



# retrospective analysis ----

# run analysis
gmacs_do_retrospective("./AIGKC/models/2025/may/EAG/23.1c/gmacs.dat", n_peel = 10)
gmacs_do_retrospective("./AIGKC/models/2025/may/EAG/25.0b/gmacs.dat", n_peel = 10)
gmacs_do_retrospective("./AIGKC/models/2025/may/WAG/23.1c/gmacs.dat", n_peel = 10)
gmacs_do_retrospective("./AIGKC/models/2025/may/WAG/25.0b/gmacs.dat", n_peel = 10)

# make plots
gmacs_do_retrospective("./AIGKC/models/2025/may/EAG/23.1c/gmacs.dat", n_peel = 10, plot_only = T, save_plot = F)[[2]]+
  labs(title = "EAG 23.1c")+theme(plot.title = element_text(hjust = 0.5)) -> retro_e23.1c
gmacs_do_retrospective("./AIGKC/models/2025/may/EAG/25.0b/gmacs.dat", n_peel = 10, plot_only = T, save_plot = F)[[2]]+
  labs(title = "EAG 25.0b")+theme(plot.title = element_text(hjust = 0.5)) -> retro_e25.0b
gmacs_do_retrospective("./AIGKC/models/2025/may/WAG/23.1c/gmacs.dat", n_peel = 10, plot_only = T, save_plot = F)[[2]]+
  labs(title = "WAG 23.1c")+theme(plot.title = element_text(hjust = 0.5)) -> retro_w23.1c
gmacs_do_retrospective("./AIGKC/models/2025/may/WAG/25.0b/gmacs.dat", n_peel = 10, plot_only = T, save_plot = F)[[2]]+
  labs(title = "WAG 25.0b")+theme(plot.title = element_text(hjust = 0.5)) -> retro_w25.0b

wrap_plots(retro_e23.1c, retro_e25.0b,
           retro_w23.1c, retro_w25.0b)+
  plot_layout(guides = 'collect') -> x
ggsave("AIGKC/figures/models/2025/may/retrospectives.png", plot = x, width = 12, height = 8, units = "in")

gmacs_get_derived_quantity_summary(file = list.files("./AIGKC/models/2025/may/WAG/23.1c/retrospectives", "Gmacsall.out", full.names = T, recursive = T),
                                   model_name = c(1, 10, 2:9)) %>%
  group_by(model) %>%
  mutate(terminal_year = max(year)) %>%
  
  ggplot()+
  geom_line(aes(x = year, y = recruit_male, color = factor(terminal_year)))

gmacs_get_derived_quantity_summary(file = list.files("./AIGKC/models/2025/may/EAG/23.1c/retrospectives", "Gmacsall.out", full.names = T, recursive = T),
                                   model_name = c(1, 10, 2:9)) %>%
  group_by(model) %>%
  mutate(terminal_year = max(year)) %>%
  
  ggplot()+
  geom_line(aes(x = year, y = recruit_male, color = factor(terminal_year)))



# stock status ----

read_csv("./AIGKC/data/safe/ofl_basis.csv") %>%
  
  group_by(season, area) %>%
  summarise_if(is.numeric, mean) %>%
  
  bind_rows(read_csv("./AIGKC/data/safe/ofl_basis.csv") %>%
              group_by(season, area) %>%
              summarise_if(is.numeric, mean) %>%
              group_by(season) %>%
              summarise_if(is.numeric, sum) %>%
              mutate(area = "Combined")
              ) %>%
  mutate(ass_yr = as.numeric(substring(season, 1, 4)),
         status = mmb / bmsy) %>% 
  
  ggplot()+
  geom_point(aes(x = ass_yr, y = mmb/bmsy, color = area))+
  geom_line(aes(x = ass_yr, y = mmb/bmsy, color = area, group = area))+
  geom_hline(yintercept = c(1, 0.5), linetype = c(2, 1))+
  scale_y_continuous(limits = c(0.25, NA))+
  scale_x_continuous(breaks = tickr(tibble(yr = 2017:2025), yr, 2)$breaks, labels = tickr(tibble(yr = 2017:2025), yr, 2)$labels)+
  labs(x = NULL, y = expression(B/B[35*"%"]), color = NULL)+
  scale_color_manual(values = c("black", "grey40", "grey70"))+
  theme(legend.position = c(1, 1), legend.justification = c(1, 1)) -> x
ggsave("AIGKC/figures/models/2025/may/stock_status_history.png", plot = x, width = 6, height = 4, units = "in")

# last season mmb
gmacs_get_derived_quantity_summary(list(wag23.1c, eag23.1c)) %>%
  filter(year == 2024) %>%
  summarise(ssb = sum(ssb) / 1000) %>% as.data.frame()

# mmb prj
gmacs_get_ref_points(list(wag23.1c, eag23.1c)) %>%
  summarise(mmb = sum(mmb) / 1000) %>% as.data.frame()

# msst
gmacs_get_ref_points(list(wag23.1c, eag23.1c)) %>%
  summarise(msst = sum(b35) / 1000 / 2) %>% as.data.frame()

# tac 
(3760000 + 1120000) * 0.000453592 / 1000

# retained and total catch
read_csv("./AIGKC/data/observer/2025/")
  


# wag exploitation rate ----

# LMA on June 30th ~ next fishing season catch

gmacs_get_size_summary(list(wag23.1c)) %>%
  filter(mod_series == 1) %>% transmute(year, size, obs) %>%
  left_join(gmacs_get_catch_summary(list(wag23.1c)) %>%
              filter(series == 1) %>% transmute(year, ret_catch_t = obs_catch)) %>%
  left_join(eag23.1c$wt_at_size %>% distinct(size, wt) ) %>%
  filter(size >= 138) %>%
  mutate(ret_catch_n = (ret_catch_t * obs) / wt) %>%
  group_by(year) %>%
  summarise(ret_catch_n = sum(ret_catch_n)) %>%
  left_join(gmacs_get_n_matrix(list(wag23.1c)) %>%
               mutate(year = year + 1) %>%
               filter(size >= 138) %>%
               group_by(year) %>%
              # numbers in thousands and weights in t cancel out
               summarise(lma = sum(total)) ) %>%
  mutate(expl_catch = ret_catch_n / lma, 
         subdistrict = "WAG") %>%
  bind_rows(gmacs_get_size_summary(list(eag23.1c)) %>%
              filter(mod_series == 1) %>% transmute(year, size, obs) %>%
              left_join(gmacs_get_catch_summary(list(eag23.1c)) %>%
                          filter(series == 1) %>% transmute(year, ret_catch_t = obs_catch)) %>%
              left_join(eag23.1c$wt_at_size %>% distinct(size, wt)) %>%
              filter(size >= 138) %>%
              mutate(ret_catch_n = (ret_catch_t * obs) / wt) %>%
              group_by(year) %>%
              summarise(ret_catch_n = sum(ret_catch_n)) %>%
              left_join(gmacs_get_n_matrix(list(eag23.1c)) %>%
                           mutate(year = year + 1) %>%
                           filter(size >= 138) %>%
                           group_by(year) %>%
                           # numbers in thousands and weights in t cancel out
                           summarise(lma = sum(total)) ) %>%
              mutate(expl_catch = ret_catch_n / lma, 
                     subdistrict = "EAG") ) %>% 
  
  ggplot()+
  geom_line(aes(x = year, y = expl_catch, color = subdistrict))+
  geom_hline(yintercept = 0.25, linetype = 2)+
  scale_x_continuous(labels = yraxis$labels, breaks = yraxis$breaks)+
  scale_color_manual(values = c("black", "grey70"))+
  labs(color = NULL, x = NULL, y = "Exploitation on LMA")+
  theme(legend.position = c(1, 1), legend.justification = c(1, 1)) -> x
ggsave("AIGKC/figures/models/2025/may/lma_exploitation_rate_backcalc_ret.png", plot = x, height = 4, width = 6, units = "in")

# n matrix season difference
wag23.1c_nseas <- gmacs_read_allout("./AIGKC/models/2025/may/WAG/23.1c_nseas/Gmacsall.out", model_name = "N Mat Jul 1, YY", version = "2.20.21")

gmacs_get_n_matrix(list(wag23.1c, wag23.1c_nseas)) %>%
  mutate(model = ifelse(model == "23.1c", "N Mat Jul 30, YY+1", model)) %>%
  filter(size >= 138) %>%
  group_by(year, model) %>%
  summarise(LMA = sum(total) ) %>%
  ggplot()+
  geom_line(aes(x = year, y = LMA, color = model))


read_csv("AIGKC/data/tac/legacy_model_abundance_output.csv") %>%
  bind_rows(gmacs_get_n_matrix(list(wag23.1c)) %>%
              mutate(year = year + 1,
                     assessment = 2025) %>%
              filter(size >= 138) %>%
              group_by(year, assessment) %>%
              # numbers in thousands and weights in t cancel out
              summarise(lma = sum(total) / 1000,
                        subdistrict = "WAG") ) %>%
  bind_rows(gmacs_get_n_matrix(list(eag23.1c)) %>%
              mutate(year = year + 1,
                     assessment = 2025) %>%
              filter(size >= 138) %>%
              group_by(year, assessment) %>%
              # numbers in thousands and weights in t cancel out
              summarise(lma = sum(total) / 1000,
                        subdistrict = "EAG") ) %>%
  
  right_join(read_csv("AIGKC/data/observer/2025/retained_catch.csv") %>%
              rename(year = crab_year,
                     subdistrict = fishery)) %>%
  mutate(exploit = tot_retained_n / 1e6 / lma) %>%
  filter(year >= 1985) %>%
  
  ggplot()+
  geom_line(aes(x = year, y = exploit, color = factor(assessment)))+
  geom_hline(yintercept = 0.25, linetype = 2)+
  scale_x_continuous(labels = yraxis$labels, breaks = yraxis$breaks)+
  scale_color_manual(values = cbpalette)+
  labs(color = NULL, x = NULL, y = "Exploitation on LMA")+
  theme(legend.position = c(1, 1), legend.justification = c(1, 1))+
  facet_wrap(~subdistrict) -> x
ggsave("AIGKC/figures/models/2025/may/exploitation_hindsight.png", plot = x, height = 4, width = 8, units = "in")


read_csv("AIGKC/data/tac/legacy_model_abundance_output.csv") %>%
  bind_rows(gmacs_get_n_matrix(list(wag23.1c)) %>%
              mutate(year = year + 1,
                     assessment = 2025) %>%
              filter(size >= 138) %>%
              group_by(year, assessment) %>%
              # numbers in thousands and weights in t cancel out
              summarise(lma = sum(total) / 1000,
                        subdistrict = "WAG") ) %>%
  bind_rows(gmacs_get_n_matrix(list(eag23.1c)) %>%
              mutate(year = year + 1,
                     assessment = 2025) %>%
              filter(size >= 138) %>%
              group_by(year, assessment) %>%
              # numbers in thousands and weights in t cancel out
              summarise(lma = sum(total) / 1000,
                        subdistrict = "EAG") ) %>%

  filter(year >= 1985) %>%
  
  ggplot()+
  geom_line(aes(x = year, y = lma, color = factor(assessment)))+
  scale_x_continuous(labels = yraxis$labels, breaks = yraxis$breaks)+
  scale_color_manual(values = cbpalette)+
  labs(color = NULL, x = NULL, y = "LMA (mil)")+
  theme(legend.position = c(1, 1), legend.justification = c(1, 1))+
  facet_wrap(~subdistrict) -> x
ggsave("AIGKC/figures/models/2025/may/lma_hindsight.png", plot = x, height = 4, width = 8, units = "in")


read_csv("AIGKC/data/tac/legacy_model_abundance_output.csv") %>%
  bind_rows(gmacs_get_derived_quantity_summary(list(wag23.1c)) %>%
              transmute(year = year + 1,
                        recruits = recruit_male / 1000,
                        assessment = 2025, 
                        subdistrict = "WAG") ) %>%
  bind_rows(gmacs_get_derived_quantity_summary(list(eag23.1c)) %>%
              transmute(year = year + 1,
                        recruits = recruit_male / 1000,
                        assessment = 2025, 
                        subdistrict = "EAG") ) %>%
  
  filter(year >= 1985) %>%
  
  ggplot()+
  geom_line(aes(x = year, y = recruits, color = factor(assessment)))+
  scale_x_continuous(labels = yraxis$labels, breaks = yraxis$breaks)+
  scale_color_manual(values = cbpalette)+
  labs(color = NULL, x = NULL, y = "Recruitment (mil)")+
  theme(legend.position = c(1, 1), legend.justification = c(1, 1))+
  facet_wrap(~subdistrict) -> x
ggsave("AIGKC/figures/models/2025/may/rec_hindsight.png", plot = x, height = 4, width = 8, units = "in")

# MCMC ----

# do mcmc runs
gmacs_do_ofl_dist("./AIGKC/models/2025/may/EAG/23.1c/gmacs.dat", n_replicates = 100000, n_draws = 100, save_plot = F)
gmacs_do_ofl_dist("./AIGKC/models/2025/may/WAG/23.1c/gmacs.dat", n_replicates = 100000, n_draws = 100, save_plot = F)

eagmc <- gmacs_read_mcoutREF("./AIGKC/models/2025/may/EAG/23.1c/ofl_distribution/mcoutREF.rep", model_name = "EAG") 
wagmc <- gmacs_read_mcoutREF("./AIGKC/models/2025/may/WAG/23.1c/ofl_distribution/mcoutREF.rep", model_name = "WAG") 
bind_rows(eagmc, wagmc) %>%
  ggplot()+
  geom_density(aes(x = ofl / 1000, fill = model), color = "grey30", alpha = 0.5)+
  scale_x_continuous(labels = scales::comma)+
  labs(x = "OFL (1,000 t)", y = "Probability Denisty", fill = NULL)+
  scale_fill_manual(values = cbpalette)+
  theme(legend.position = c(1, 1),
        legend.justification = c(1, 1)) -> x

bind_rows(eagmc %>% mutate(mmb_est = eag23.1c$bmsy *  eag23.1c$b_bmsy,
                           mc_status = mmb_est / bmsy), 
          
          wagmc %>% mutate(mmb_est = wag23.1c$bmsy * wag23.1c$b_bmsy,
                           mc_status = mmb_est / bmsy)) %>%
  ggplot()+
  stat_ecdf(aes(x = mc_status, geom = "step", group = model, color = model))+
  geom_vline(xintercept = 0.5, linetype = 2, color = "firebrick")+
  geom_text(aes(x = 0.48, y = 0.5, label = "Overfished"), size = 4, angle = 90)+
  labs(x = bquote(MMB[2024] ~"/"~B["35%"]), y = "Cumulative Density", color = NULL)+
  scale_color_manual(values = cbpalette)+
  theme(legend.position = c(0.2, 1),
        legend.justification = c(0.2, 1)) -> y

ggsave("AIGKC/figures/models/2025/may/ofl_distribution.png", plot = x+y, height = 4, width = 7, units = "in")
