# notes ----

# aigkc gmacs .dat file prep
# tyler jackson
# 4/11/2024

# load ----

source("./AIGKC/code/aigkc_functions.R")

# taus ----

# eag
read_csv("./AIGKC/data/observer/item10_season_dates.csv") %>%
  # calculate mid date
  mutate(midfish = f_mid_date(start_date, end_date),
         jul1 = mdy(paste0("7/1/", crab_year)),
         feb15 = mdy(paste0("2/15/", crab_year+1)),
         midfish = as_date(ifelse(midfish > feb15, feb15, midfish))) %>%
  # compute taus
  transmute(fishery, 
            `#inst_n` = 0,
            jul1_midfish = as.numeric(midfish - jul1) / 365,
            inst_c = 0,
            midfish_feb15 = as.numeric(feb15 - midfish) / 365,
            inst_bycatch = 0, 
            rest = 0.37260274,
            yr = paste0("#", crab_year)) %>%
  # filter for eag
  filter(fishery == "EAG") %>%
  dplyr::select(-fishery) %>% 
  mutate_at(1:6, function(x){sprintf('%.3f', x)}) %>%
  write_delim(., "./AIGKC/data/gmacs/2024_may/eag_tau_1981_present.txt", delim = "\t")

# wag
read_csv("./AIGKC/data/observer/item10_season_dates.csv") %>%
  # calculate mid date
  mutate(midfish = f_mid_date(start_date, end_date),
         jul1 = mdy(paste0("7/1/", crab_year)),
         feb15 = mdy(paste0("2/15/", crab_year+1)),
         midfish = as_date(ifelse(midfish > feb15, feb15, midfish))) %>%
  # compute taus
  transmute(fishery, 
            `#inst_n` = 0,
            jul1_midfish = as.numeric(midfish - jul1) / 365, 
            inst_c = 0,
            midfish_feb15 = as.numeric(feb15 - midfish) / 365,
            inst_bycatch = 0, 
            rest = 0.37260274,
            yr = paste0("#", crab_year)) %>%
  # filter for eag
  filter(fishery == "WAG") %>%
  dplyr::select(-fishery) %>%
  mutate_at(1:6, function(x){sprintf('%.3f', x)}) %>%
  write_delim(., "./AIGKC/data/gmacs/2024_jan/wag_tau_1981_present.txt", delim = "\t")

# ai
# read_csv("./AIGKC/data/observer/item10_season_dates.csv") %>%
#   # calculate mid date
#   mutate(midfish = f_mid_date(start_date, end_date),
#          jul1 = mdy(paste0("7/1/", crab_year)),
#          feb15 = mdy(paste0("2/15/", crab_year+1)),
#          midfish = as_date(ifelse(midfish > feb15, feb15, midfish))) %>%
#   # compute taus
#   transmute(fishery, 
#             `#inst_n` = 0,
#             jul1_midfish = as.numeric(midfish - jul1) / 365,
#             inst_c = 0,
#             midfish_feb15 = as.numeric(feb15 - midfish) / 365,
#             inst_bycatch = 0, 
#             rest = 0.37260274,
#             yr = paste0("#", crab_year)) %>%
#   group_by(yr) %>%
#   summarise_all(mean) %>%
#   dplyr::select(3:8, 1) %>%
#   
#   write_delim(., "./AIGKC/data/gmacs/2024_jan/ai_tau_1981_present_tj.txt", delim = "\t")
  

# retained catch ----

# eag
read_csv("./AIGKC/data/observer/item3_retained_catch.csv") %>%
  filter(fishery == "EAG") %>%
  transmute(`#year` = crab_year,
            seas = 3, fleet = 1, sex = 1,
            obs = round(t, 2),
            cv = 0.0316, type = 1, units = 1, mult = 1, effort = 0, discard_mortality = 0.2) %>%
  arrange(`#year`) %>%
  write_delim(., "./AIGKC/data/gmacs/2024_may/eag_retained_1985_present.txt", delim = "\t")

# wag
read_csv("./AIGKC/data/observer/item3_retained_catch.csv") %>%
  filter(fishery == "WAG") %>%
  transmute(`#year` = crab_year,
            seas = 3, fleet = 1, sex = 1,
            obs = round(t, 2),
            cv = 0.0316, type = 1, units = 1, mult = 1, effort = 0, discard_mortality = 0.2) %>%
  arrange(`#year`) %>%
  write_delim(., "./AIGKC/data/gmacs/2024_may/wag_retained_1985_present.txt", delim = "\t")

# total catch ----

# eag
read_csv("./AIGKC/data/observer/item4_directed_total_catch.csv") %>%
  filter(fishery == "EAG",
         group %in% c("sublegal", "tot_legal")) %>%
  group_by(fishery, crab_year) %>%
  summarise(total_catch_t = sum(total_catch_t)) %>% ungroup %>%
  # join to cv
  left_join(.,
    
    read_csv("./AIGKC/data/observer/item5_nonzero_obs_pots.csv") %>%
      filter(fishery == "EAG") %>%
      # compute graded weight
      mutate(w = (m_nz * 250) / max(m_nz), 
             cv = sqrt(exp(1 / (2 * w)) - 1)) %>%
      transmute(crab_year, cv)
    
  ) %>%
  transmute(`#year` = crab_year,
            seas = 3, fleet = 1, sex = 1,
            obs = round(total_catch_t, 2),
            cv = round(cv, 3), type = 0, units = 1, mult = 1, effort = 0, discard_mortality = 0.2) %>%
  arrange(`#year`) %>% 
  # save gmacs output
  write_delim(., "./AIGKC/data/gmacs/2024_may/eag_total_catch.txt", delim = "\t")

# wag
read_csv("./AIGKC/data/observer/item4_directed_total_catch.csv") %>%
  filter(fishery == "WAG",
         group %in% c("sublegal", "tot_legal")) %>%
  group_by(fishery, crab_year) %>% 
  summarise(total_catch_t = sum(total_catch_t)) %>% ungroup %>%
  # join to cv
  left_join(.,
            
            read_csv("./AIGKC/data/observer/item5_nonzero_obs_pots.csv") %>%
              filter(fishery == "WAG") %>%
              # compute graded weight
              mutate(w = (m_nz * 250) / max(m_nz), 
                     cv = sqrt(exp(1 / (2 * w)) - 1)) %>%
              transmute(crab_year, cv)
            
  ) %>%
  transmute(`#year` = crab_year,
            seas = 3, fleet = 1, sex = 1,
            obs = round(total_catch_t, 2),
            cv = round(cv, 3), type = 0, units = 1, mult = 1, effort = 0, discard_mortality = 0.2) %>%
  arrange(`#year`) %>% 
  # save gmacs output
  write_delim(., "./AIGKC/data/gmacs/2024_may/wag_total_catch.txt", delim = "\t")



# retained size composition ----

# eag
read_csv("./AIGKC/data/observer/item8_retained_size_comp.csv") %>%
  # add length bin
  f_add_len_bin(., .$size) %>%
  # compute total measured per year
  group_by(crab_year, fishery) %>%
  mutate(nmeas = sum(total)) %>%
  # compute total measured by bin
  group_by(crab_year, fishery, bin) %>%
  summarise(n = sum(total),
            nmeas = mean(nmeas)) %>%
  ungroup %>%
  mutate(prop  = n / nmeas) %>%
  # join to stage 1 neff
  group_by(fishery, crab_year) %>%
  mutate(neff = min(nmeas * 0.05, 100)) %>% ungroup %>%
  left_join(read_csv("./AIGKC/data/observer/item6_vessel_days.csv")) %>%
  # gmacs retained composition
  filter(fishery == "EAG") %>% ungroup %>%
  arrange(bin) %>%
  transmute(`#year` = crab_year, 
            seas = 3,
            fleet = 1, 
            sex = 1,
            type = 1,
            shell = 0,
            maturity = 0, 
            nsamp = neff,
            nsamp = n_days,
            bin = paste0("l", bin), 
            prop = sprintf("%.5f", prop)) %>%
  pivot_wider(names_from = bin, values_from = prop) %>%
  arrange(`#year`) %>%
  replace(is.na(.), "0.00000") %>%
  # save gmacs output
  write_delim(., "./AIGKC/data/gmacs/2024_may/eag_retained_composition.txt", delim = "\t")

# wag
read_csv("./AIGKC/data/observer/item8_retained_size_comp.csv") %>%
  # add length bin
  f_add_len_bin(., .$size) %>%
  # compute total measured per year
  group_by(crab_year, fishery) %>%
  mutate(nmeas = sum(total)) %>%
  # compute total measured by bin
  group_by(crab_year, fishery, bin) %>%
  summarise(n = sum(total),
            nmeas = mean(nmeas)) %>%
  ungroup %>%
  mutate(prop  = n / nmeas) %>%
  # join to stage 1 neff
  left_join(read_csv("./AIGKC/data/observer/item6_vessel_days.csv")) %>%
  # gmacs retained composition
  filter(fishery == "WAG") %>%
  arrange(bin, crab_year) %>%
  transmute(`#year` = crab_year, 
            seas = 3,
            fleet = 1, 
            sex = 1,
            type = 1,
            shell = 0,
            maturity = 0, 
            nsamp = n_days,
            bin = paste0("l", bin), 
            prop = sprintf("%.5f", prop)) %>%
  pivot_wider(names_from = bin, values_from = prop) %>%
  replace(is.na(.), "0.00000") %>%
  arrange(`#year`) %>%
  # save gmacs output
  write_delim(., "./AIGKC/data/gmacs/2024_may/wag_retained_composition.txt", delim = "\t")

# retained size composition, no minus bin ----

# eag
read_csv("./AIGKC/data/observer/item8_retained_size_comp.csv") %>%
  filter(size > 100) %>%
  # add length bin
  f_add_len_bin(., .$size) %>%
  # compute total measured per year
  group_by(crab_year, fishery) %>%
  mutate(nmeas = sum(total)) %>%
  # compute total measured by bin
  group_by(crab_year, fishery, bin) %>%
  summarise(n = sum(total),
            nmeas = mean(nmeas)) %>%
  ungroup %>%
  mutate(prop  = n / nmeas) %>%
  # join to stage 1 neff
  group_by(fishery, crab_year) %>%
  mutate(neff = min(nmeas * 0.05, 100)) %>% ungroup %>%
  left_join(read_csv("./AIGKC/data/observer/item6_vessel_days.csv")) %>%
  # gmacs retained composition
  filter(fishery == "EAG") %>% ungroup %>%
  arrange(bin) %>%
  transmute(`#year` = crab_year, 
            seas = 3,
            fleet = 1, 
            sex = 1,
            type = 1,
            shell = 0,
            maturity = 0, 
            nsamp = neff,
            nsamp = n_days,
            bin = paste0("l", bin), 
            prop = sprintf("%.5f", prop)) %>%
  pivot_wider(names_from = bin, values_from = prop) %>%
  arrange(`#year`) %>%
  replace(is.na(.), "0.00000") %>%
  # save gmacs output
  write_delim(., "./AIGKC/data/gmacs/2024_may/eag_retained_composition_trunc.txt", delim = "\t")

# wag
read_csv("./AIGKC/data/observer/item8_retained_size_comp.csv") %>%
  filter(size > 100) %>%
  # add length bin
  f_add_len_bin(., .$size) %>%
  # compute total measured per year
  group_by(crab_year, fishery) %>%
  mutate(nmeas = sum(total)) %>%
  # compute total measured by bin
  group_by(crab_year, fishery, bin) %>%
  summarise(n = sum(total),
            nmeas = mean(nmeas)) %>%
  ungroup %>%
  mutate(prop  = n / nmeas) %>%
  # join to stage 1 neff
  group_by(fishery, crab_year) %>%
  mutate(neff = min(nmeas * 0.05, 100)) %>% ungroup %>%
  left_join(read_csv("./AIGKC/data/observer/item6_vessel_days.csv")) %>%
  # gmacs retained composition
  filter(fishery == "WAG") %>% ungroup %>%
  arrange(bin) %>%
  transmute(`#year` = crab_year, 
            seas = 3,
            fleet = 1, 
            sex = 1,
            type = 1,
            shell = 0,
            maturity = 0, 
            nsamp = neff,
            nsamp = n_days,
            bin = paste0("l", bin), 
            prop = sprintf("%.5f", prop)) %>%
  pivot_wider(names_from = bin, values_from = prop) %>%
  arrange(`#year`) %>%
  replace(is.na(.), "0.00000") %>%
  # save gmacs output
  write_delim(., "./AIGKC/data/gmacs/2024_may/Wag_retained_composition_trunc.txt", delim = "\t")



# total size composition ----

# eag
read_csv("./AIGKC/data/observer/item9_directed_observer_size_comp.csv") %>%
  filter(sex == 1,
         !is.na(size)) %>%
  # add length bin
  f_add_len_bin(., .$size) %>%
  # compute total measured per year
  group_by(crab_year, fishery) %>%
  mutate(nmeas = sum(total)) %>%
  # compute total measured by bin
  group_by(crab_year, fishery, bin) %>%
  summarise(n = sum(total),
            nmeas = mean(nmeas)) %>%
  ungroup %>%
  mutate(prop  = n / nmeas) %>%
  group_by(crab_year, fishery) %>%
  mutate(neff = round(min(nmeas * 0.05, 100))) %>%
  # join to stage 1 neff
  left_join(read_csv("./AIGKC/data/observer/item7_observed_vessel_days.csv")) %>%
  # gmacs retained composition
  filter(fishery == "EAG") %>% ungroup %>%
  arrange(bin, crab_year) %>%
  transmute(`#year` = crab_year, 
            seas = 3,
            fleet = 1, 
            sex = 1,
            type = 0,
            shell = 0,
            maturity = 0, 
            nsamp = neff,
            nsamp = n_days,
            bin = paste0("l", bin), 
            prop = sprintf("%.5f", prop)) %>%
  pivot_wider(names_from = bin, values_from = prop) %>%
  replace(is.na(.), "0.00000") %>%
  # save gmacs output
  write_delim(., "./AIGKC/data/gmacs/2024_may/eag_total_composition.txt", delim = "\t")

# wag
read_csv("./AIGKC/data/observer/item9_directed_observer_size_comp.csv") %>%
  filter(sex == 1,
         !is.na(size)) %>%
  # add length bin
  f_add_len_bin(., .$size) %>%
  # compute total measured per year
  group_by(crab_year, fishery) %>%
  mutate(nmeas = sum(total)) %>%
  # compute total measured by bin
  group_by(crab_year, fishery, bin) %>%
  summarise(n = sum(total),
            nmeas = mean(nmeas)) %>%
  ungroup %>%
  mutate(prop  = n / nmeas) %>%
  # join to stage 1 neff
  left_join(read_csv("./AIGKC/data/observer/item7_observed_vessel_days.csv")) %>%
  # gmacs retained composition
  filter(fishery == "WAG") %>%
  arrange(bin, crab_year) %>%
  transmute(`#year` = crab_year, 
            seas = 3,
            fleet = 1, 
            sex = 1,
            type = 0,
            shell = 0,
            maturity = 0, 
            nsamp = as.character(n_days),
            bin = paste0("l", bin), 
            prop = sprintf("%.5f", prop)) %>%
  pivot_wider(names_from = bin, values_from = prop) %>%
  replace(is.na(.), "0.00000") %>% 
  # save gmacs output
  write_delim(., "./AIGKC/data/gmacs/2024_may/wag_total_composition.txt", delim = "\t")

# total size composition, no minus bin ----

# eag
read_csv("./AIGKC/data/observer/item9_directed_observer_size_comp.csv") %>%
  filter(sex == 1,
         size > 100,
         !is.na(size)) %>%
  # add length bin
  f_add_len_bin(., .$size) %>%
  # compute total measured per year
  group_by(crab_year, fishery) %>%
  mutate(nmeas = sum(total)) %>%
  # compute total measured by bin
  group_by(crab_year, fishery, bin) %>%
  summarise(n = sum(total),
            nmeas = mean(nmeas)) %>%
  ungroup %>%
  mutate(prop  = n / nmeas) %>%
  group_by(crab_year, fishery) %>%
  mutate(neff = round(min(nmeas * 0.05, 100))) %>%
  # join to stage 1 neff
  left_join(read_csv("./AIGKC/data/observer/item7_observed_vessel_days.csv")) %>%
  # gmacs retained composition
  filter(fishery == "EAG") %>% ungroup %>%
  arrange(bin, crab_year) %>%
  transmute(`#year` = crab_year, 
            seas = 3,
            fleet = 1, 
            sex = 1,
            type = 0,
            shell = 0,
            maturity = 0, 
            nsamp = neff,
            nsamp = n_days,
            bin = paste0("l", bin), 
            prop = sprintf("%.5f", prop)) %>%
  pivot_wider(names_from = bin, values_from = prop) %>%
  replace(is.na(.), "0.00000") %>%
  # save gmacs output
  write_delim(., "./AIGKC/data/gmacs/2024_may/eag_total_composition_trunc.txt", delim = "\t")



# wag
read_csv("./AIGKC/data/observer/item9_directed_observer_size_comp.csv") %>%
  filter(sex == 1,
         size > 100,
         !is.na(size)) %>%
  # add length bin
  f_add_len_bin(., .$size) %>%
  # compute total measured per year
  group_by(crab_year, fishery) %>%
  mutate(nmeas = sum(total)) %>%
  # compute total measured by bin
  group_by(crab_year, fishery, bin) %>%
  summarise(n = sum(total),
            nmeas = mean(nmeas)) %>%
  ungroup %>%
  mutate(prop  = n / nmeas) %>%
  group_by(crab_year, fishery) %>%
  mutate(neff = round(min(nmeas * 0.05, 100))) %>%
  # join to stage 1 neff
  left_join(read_csv("./AIGKC/data/observer/item7_observed_vessel_days.csv")) %>%
  # gmacs retained composition
  filter(fishery == "WAG") %>% ungroup %>%
  arrange(bin, crab_year) %>%
  transmute(`#year` = crab_year, 
            seas = 3,
            fleet = 1, 
            sex = 1,
            type = 0,
            shell = 0,
            maturity = 0, 
            nsamp = neff,
            nsamp = n_days,
            bin = paste0("l", bin), 
            prop = sprintf("%.5f", prop)) %>%
  pivot_wider(names_from = bin, values_from = prop) %>%
  replace(is.na(.), "0.00000") %>%
  # save gmacs output
  write_delim(., "./AIGKC/data/gmacs/2024_may/wag_total_composition_trunc.txt", delim = "\t")


# observer index ----

# eag
## gam legal male, pre-rationalized
read_csv("./AIGKC/output/cpue_std/2024/may/pre_eag_index.csv") %>%
  transmute(`#index` = 1,
            year, season = 3, fleet = 1, sex = 1, maturity = 0, 
            index = round(index, 4), cv = round(se / index, 4), unit = 2, timing = 0.5) %>%
  arrange(year) %>%
  # save gmacs output
  write_delim(., "./AIGKC/data/gmacs/2024_may/eag_pre_std_cpue.txt", delim = "\t")
## gam legal male, post-rationalized
read_csv("./AIGKC/output/cpue_std/2024/may/post_eag_index.csv") %>%
  transmute(`#index` = 2,
            year, season = 3, fleet = 1, sex = 1, maturity = 0, 
            index = round(index, 4), cv = round(se / index, 4), unit = 2, timing = 0.5) %>%
  arrange(year) %>%
  # save gmacs output
  write_delim(., "./AIGKC/data/gmacs/2024_may/eag_post_std_cpue.txt", delim = "\t")

# wag
## gam legal male, pre-rationalized
read_csv("./AIGKC/output/cpue_std/2024/may/pre_wag_index.csv") %>%
  transmute(`#index` = 1,
            year, season = 3, fleet = 1, sex = 1, maturity = 0, 
            index = round(index, 4), cv = round(se / index, 4), unit = 2, timing = 0.5) %>%
  arrange(year) %>%
  # save gmacs output
  write_delim(., "./AIGKC/data/gmacs/2024_may/wag_pre_std_cpue.txt", delim = "\t")
## gam legal male, post-rationalized
read_csv("./AIGKC/output/cpue_std/2024/may/post_wag_index.csv") %>%
  transmute(`#index` = 2,
            year, season = 3, fleet = 1, sex = 1, maturity = 0, 
            index = round(index, 4), cv = round(se / index, 4), unit = 2, timing = 0.5) %>%
  arrange(year) %>%
  # save gmacs output
  write_delim(., "./AIGKC/data/gmacs/2024_may/wag_post_std_cpue.txt", delim = "\t")
  

# fish ticket index ----

# eag
read_csv("./AIGKC/output/cpue_std/eag_fish_tickets_85_98_std_index_may2024.csv") %>%
  transmute(`#index` = 3,
            year, season = 3, fleet = 1, sex = 1, maturity = 0, 
            index = round(index, 4), cv = round(se / index, 4), unit = 2, timing = 0.5) %>%
  arrange(year) %>%
  # save gmacs output
  write_delim(., "./AIGKC/data/gmacs/2024_may/eag_fish_tickets_85_98_std_index.txt", delim = "\t")

# wag
read_csv("./AIGKC/output/cpue_std/wag_fish_tickets_85_98_std_index_may2024.csv") %>%
  transmute(`#index` = 3,
            year, season = 3, fleet = 1, sex = 1, maturity = 0, 
            index = round(index, 4), cv = round(se / index, 4), unit = 2, timing = 0.5) %>%
  arrange(year) %>%
  # save gmacs output
  write_delim(., "./AIGKC/data/gmacs/2024_may/wag_fish_tickets_85_98_std_index.txt", delim = "\t")

# survey index ----

# eag
## gamm total male
read_csv("./AIGKC/output/coop_survey/gamm_survey_index.csv") %>%
  transmute(`#index` = 4,
            year, season = 3, fleet = 3, sex = 1, maturity = 0, 
            index = round(index, 4), cv = round(se/index, 4), unit = 2, timing = 0.5) %>%
  arrange(year) %>%
  # save gmacs output
  write_delim(., "./AIGKC/data/gmacs/2024_jan/eag_coop_survey_cpue.txt", delim = "\t")


# survey size comp ----
# eag
read_csv("./AIGKC/output/coop_survey/survey_size_comp.csv") %>%
  filter(size >= 100) %>%
  # add length bin
  f_add_len_bin(., .$size) %>%
  # compute total measured by bin
  group_by(year, fishery, bin) %>%
  summarise(n = sum(n),
            n_meas = sum(n_meas),
            prop = sum(prop),
            total_n = mean(total_n),
            tot_n_meas = mean(total_n_meas)) %>%
  ungroup %>%
  # join to stage 1 neff
  group_by(fishery, year) %>%
  mutate(neff = min(tot_n_meas * 0.05, 100)) %>% ungroup %>%
  left_join(read_csv("./AIGKC/output/coop_survey/survey_sample_stats.csv") %>%
              mutate(fishery = "EAG") %>%
              transmute(year, fishery, n_pots)) %>%
  # gmacs retained composition
  filter(fishery == "EAG") %>% ungroup %>%
  arrange(bin) %>%
  transmute(`#year` = year, 
            seas = 3,
            fleet = 3, 
            sex = 1,
            type = 0,
            shell = 0,
            maturity = 0, 
            nsamp = n_pots,
            bin = paste0("l", bin), 
            prop = sprintf("%.5f", prop)) %>%
  pivot_wider(names_from = bin, values_from = prop) %>%
  arrange(`#year`) %>%
  replace(is.na(.), "0.00000") %>%
  # save gmacs output
  write_delim(., "./AIGKC/data/gmacs/2024_jan/eag_coop_survey_composition.txt", delim = "\t")

