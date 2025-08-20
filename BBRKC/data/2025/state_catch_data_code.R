# process catch data from ADF&G for BBRKC
# katie.palof@alaska.gov

# data from Tyler Jackson - April 2025
# updated 8-5-2025
# found in BSAI_crab_assessments/BBRKC/data/2025

#devtools::install_github("commfish/BSAIcrabR", force = TRUE)
library(BSAIcrabR)
library(tidyverse)

# folder 
folder <- "bbrkc_2025 (8 1 2025)"

# retained catch summary 
ret_catch <- read_csv(paste0(here::here(), "/BBRKC/data/2025/", folder, "/retained_catch.csv")) 

ret_catch %>% 
  filter(crab_year >= 2020)


#total catch summary
tot_catch <- read_csv(paste0(here::here(), "/BBRKC/data/2025/", folder, "/total_catch.csv")) 

tot_catch %>% 
  filter(crab_year >= 2022, target_stock == "BBRKC") %>% 
  group_by(group)
         
# males
tot_catch %>% 
  as.data.frame() %>% 
  mutate(sex = ifelse(group == "female", 2, 1)) %>% 
  group_by(target_stock, crab_year, sex) %>% 
  summarise(t_catch_wt = round(as.numeric(sum(total_catch_wt)), 2)) %>% 
  mutate(t_catch_wt = round(t_catch_wt, 2)) %>% 
  filter(sex == 1) -> temp2

temp2 %>% 
  filter(crab_year >= 2020) # total male pot fishery 


#females
tot_catch %>% 
  as.data.frame() %>% 
  mutate(sex = ifelse(group == "female", 2, 1)) %>% 
  group_by(target_stock, crab_year, sex) %>% 
  summarise(t_catch_wt = as.numeric(sum(total_catch_wt))) %>% 
  mutate(t_catch_wt = round(t_catch_wt, 2)) %>% 
  filter(sex == 2) -> temp3

temp3 %>% 
  filter(crab_year >= 2020)


# tanner crab total catch
tot_catch %>% 
  as.data.frame() %>% 
  group_by(target_stock, crab_year, group) %>% 
  filter(target_stock != "BBRKC") %>% 
  mutate(tstock = ifelse(target_stock == "EBT", 1, ifelse(target_stock == "WBT", 1, 0 ))) %>% 
  filter(tstock == 1) %>% 
  group_by(crab_year, group) %>% 
  summarise(t_catch_wt = as.numeric(sum(total_catch_wt))) %>% 
  mutate(t_catch_wt = round(t_catch_wt, 2)) %>% as.data.frame()
  

## total size comp -------------------
# 
tot_catch_comp <- read_csv(paste0(here::here(), "/BBRKC/data/2025/", folder, "/directed_total_composition.csv"))
head(tot_catch_comp)

# males
tot_catch_comp %>% 
  filter(sex == 1) %>% 
  filter(size >= 65) %>% 
  mutate(size_bin = ifelse(size > 160, 160, floor(size/5)*5)) %>% 
  group_by(crab_year, size_bin, .drop = F) %>% 
  summarise(y_total = sum(total, na.rm = T)) %>% 
  # pull in missing bins
  full_join(expand_grid(size_bin = seq(65, 160, by = 5), 
                        crab_year = seq(1990, 2024, by = 1)), 
                        by = c("crab_year", "size_bin")) %>% 
  replace_na(list(y_total = 0)) %>% 
  #normalize to sum to 1
  group_by(crab_year) %>% 
  mutate(prop =  sprintf('%.4f', y_total / sum(y_total)),
         nsamp = min(0.05*sum(y_total), 100),
         ncrab = paste0("#", sum(y_total))) %>%
  arrange(crab_year, size_bin) %>% 
  ungroup() %>%
  # pivot to wide format
  dplyr::select(-y_total) %>%
  pivot_wider(names_from = "size_bin", values_from = "prop") %>% 
  arrange(crab_year) %>% 
  dplyr::select(crab_year, nsamp, grep("\\d", names(.), value = T), ncrab) %>% 
  write_delim(here::here("BBRKC/data/2025/item4a_directed_fishery_size_comp_total_males.txt"), delim = "\t", col_names = F, na = "")

# females 
tot_catch_comp %>% 
  filter(sex == 2) %>% 
  filter(size >= 65) %>% 
  mutate(size_bin = ifelse(size > 140, 140, floor(size/5)*5)) %>% 
  group_by(crab_year, size_bin, .drop = F) %>% 
  summarise(y_total = sum(total, na.rm = T)) %>% 
  # pull in missing bins
  full_join(expand_grid(size_bin = seq(65, 140, by = 5), 
                        crab_year = seq(1990, 2024, by = 1)), 
            by = c("crab_year", "size_bin")) %>% 
  replace_na(list(y_total = 0)) %>% 
  #normalize to sum to 1
  group_by(crab_year) %>% 
  mutate(prop =  sprintf('%.4f', y_total / sum(y_total)),
         nsamp = min(round(0.05*sum(y_total), 2), 50),
         ncrab = paste0("#", sum(y_total))) %>%
  arrange(crab_year, size_bin) %>% 
  ungroup() %>%
  # pivot to wide format
  dplyr::select(-y_total) %>%
  pivot_wider(names_from = "size_bin", values_from = "prop") %>% 
  arrange(crab_year) %>% 
  dplyr::select(crab_year, nsamp, grep("\\d", names(.), value = T), ncrab) %>% 
  write_delim(here::here("BBRKC/data/2025/item4b_directed_fishery_size_comp_total_females.txt"), delim = "\t", col_names = F, na = "")

# retained catch comp  ------------------
ret_catch_comp <- read_csv(paste0(here::here(), "/BBRKC/data/2025/", folder, "/retained_catch_composition.csv"))
head(ret_catch_comp)

ret_catch_comp %>% 
  #filter(sex == 1) %>% 
  filter(size >= 65) %>% 
  mutate(size_bin = ifelse(size > 160, 160, floor(size/5)*5)) %>% 
  group_by(crab_year, size_bin, .drop = F) %>% 
  summarise(y_total = sum(total, na.rm = T)) %>% 
  # pull in missing bins
  full_join(expand_grid(size_bin = seq(65, 160, by = 5), 
                        crab_year = seq(1990, 2024, by = 1)), 
            by = c("crab_year", "size_bin")) %>% 
  replace_na(list(y_total = 0)) %>% 
  #normalize to sum to 1
  group_by(crab_year) %>% 
  mutate(prop =  sprintf('%.4f', y_total / sum(y_total)),
         nsamp = min(round(0.05*sum(y_total),2), 100),
         ncrab = paste0("#", sum(y_total))) %>%
  arrange(crab_year, size_bin) %>% 
  ungroup() %>%
  # pivot to wide format
  dplyr::select(-y_total) %>%
  pivot_wider(names_from = "size_bin", values_from = "prop") %>% 
  arrange(crab_year) %>% 
  dplyr::select(crab_year, nsamp, grep("\\d", names(.), value = T), ncrab) %>% 
  write_delim(here::here("BBRKC/data/2025/item5_directed_fishery_size_comp_retained_males.txt"), delim = "\t", col_names = F, na = "")


# tanner bycatch size comps -----------------
## total size comp -------------------
# 
tanner_catch_comp <- read_csv(paste0(here::here(), "/BBRKC/data/2025/", folder, "/tanner_bycatch_composition.csv"))
head(tanner_catch_comp)

# sample size for 2024
tanner_catch_comp %>% 
  filter(crab_year == 2024)
# 1 crab is NOT enough to do size comps.
# males
tanner_catch_comp %>% 
  filter(sex == 1) %>% 
  filter(size >= 65) %>% 
  mutate(size_bin = ifelse(size > 160, 160, floor(size/5)*5)) %>% 
  group_by(crab_year, size_bin, .drop = F) %>% 
  summarise(y_total = sum(total, na.rm = T)) %>% 
  # pull in missing bins
  full_join(expand_grid(size_bin = seq(65, 160, by = 5), 
                        crab_year = seq(1990, 2024, by = 1)), 
            by = c("crab_year", "size_bin")) %>% 
  replace_na(list(y_total = 0)) %>% 
  #normalize to sum to 1
  group_by(crab_year) %>% 
  mutate(prop =  sprintf('%.4f', y_total / sum(y_total)),
         nsamp = min(0.05*sum(y_total), 25),
         ncrab = paste0("#", sum(y_total))) %>%
  arrange(crab_year, size_bin) %>% 
  ungroup() %>%
  # pivot to wide format
  dplyr::select(-y_total) %>%
  pivot_wider(names_from = "size_bin", values_from = "prop") %>% 
  arrange(crab_year) %>% 
  dplyr::select(crab_year, nsamp, grep("\\d", names(.), value = T), ncrab) %>% 
  write_delim(here::here("BBRKC/data/2025/item6a_tanner_e166_size_comp_males.txt"), delim = "\t", col_names = F, na = "")




### old -----------------------------
ret_catch_comp %>% 
  # filter only for animals in the model
  filter(size >= 65) %>%
  # create length comp bin
  mutate(cl_bin = ifelse(size > 180, 180, floor(size / 5) * 5), cl_bin) %>%  #,
         #cl_bin = ifelse(SEX_TEXT == "female" & cl_bin >= 140, 140, cl_bin)) %>%
  # get abundance estimates by bin
  group_by(crab_year, cl_bin) %>%
  summarise(N_hat = sum(total)) %>%
  # get total abundance estimate of all
  group_by(crab_year) %>%
  mutate(total = sum(N_hat)) %>% ungroup %>%
  # compute proportion
  mutate(p = round(N_hat / total, 4)) -> size_comp

# males 1975	3	1	1	1	0	0	150	
size_comp %>% 
  as.data.frame() %>% 
  #filter(SEX_TEXT == "male") %>%
  mutate(season = 3, fleet = 1, sex = 1, type = 1, shell = 0, maturity = 0, Nsamp = 150, p =  sprintf('%.4f', p)) %>%
  dplyr::select(crab_year, season, fleet, sex, type, shell, maturity, Nsamp, cl_bin, p) %>%
  mutate(p = as.numeric(p)) -> hemp #%>% 
  pivot_wider(names_from = cl_bin, values_from = p) %>%
  mutate_at(c(8:30), ~replace_na(.,0)) %>% 
  
  rbind(c("#year", "season", "fleet", "sex", "type", "shell", "maturity", "nsamp", "datavector", rep(NA, 20)), .) %>%
  write_delim(paste0(here::here(), '/BBRKC/data/2025/male_race_size_comp_180.txt'), delim = "\t", col_names = F, na = "")
#
