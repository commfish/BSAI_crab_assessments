# notes ----

# aigkc gmacs .dat file prep
# tyler jackson
# 11/29/2024

# load ----
library(gmacsr)

#source("./AIGKC/code/aigkc_functions.R")

# current dat file ----

# eag
eag_dat <- gmacs_read_dat("AIGKC/models/2025/sept/EAG/23.1c/EAG_23.1c.dat")

# wag
wag_dat <- gmacs_read_dat("AIGKC/models/2025/sept/WAG/23.1c/WAG_23_1c.dat")

# dimensions ----

wag_dat$terminal_year <- 2024

# taus ----

# eag
read_csv("./AIGKC/data/observer/season_dates.csv") %>%
  filter(fishery == "EAG") %>%
  # calculate mid date
  mutate(midfish = start_date + (end_date - start_date) / 2,
         jul1 = mdy(paste0("7/1/", crab_year)),
         feb15 = mdy(paste0("2/15/", crab_year+1)),
         midfish = as_date(ifelse(midfish > feb15, feb15, midfish))) %>%
  # compute taus
  transmute(year = crab_year,
            season_1 = 0,
            season_2 = as.numeric(midfish - jul1) / 365,
            season_3 = 0,
            season_4 = as.numeric(feb15 - midfish) / 365,
            season_5 = 0.37260274,
            season_6 = 0)  %>%
  bind_rows(eag_dat$tau %>% filter(year < 1981), .) -> eag_dat$tau

# wag
read_csv("./AIGKC/data/observer/2025/season_dates.csv") %>%
  filter(fishery == "WAG") %>%
  # calculate mid date
  mutate(midfish = start_date + (end_date - start_date) / 2,
         jul1 = mdy(paste0("7/1/", crab_year)),
         feb15 = mdy(paste0("2/15/", crab_year+1)),
         midfish = as_date(ifelse(midfish > feb15, feb15, midfish))) %>%
  # compute taus
  transmute(year = crab_year,
            season_1 = 0,
            season_2 = as.numeric(midfish - jul1) / 365,
            season_3 = 0,
            season_4 = as.numeric(feb15 - midfish) / 365,
            season_5 = 0.37260274,
            season_6 = 0) %>%
  bind_rows(wag_dat$tau %>% filter(year < 1981), .) -> wag_dat$tau

# catch data ----

## retained catch

### eag
# 1981 - 1984
eag_dat$catch %>% filter(type == 1, units == 2) %>%
# 1985 - present
  bind_rows(read_csv("./AIGKC/data/observer/retained_catch.csv") %>%
              filter(fishery == "EAG") %>%
              transmute(year = crab_year,
                        season = 3, fleet = 1, sex = 1,
                        obs = round(t, 2),
                        cv = 0.0316, type = 1, units = 1, mult = 1, effort = 0, disc_m = 0.2) %>%
              arrange(year) ) -> eag_retained

### wag
# 1981 - 1984
wag_dat$catch %>% filter(type == 1, units == 2) %>%
# 1985 - present
  bind_rows(read_csv("./AIGKC/data/observer/2025/retained_catch.csv") %>%
              filter(fishery == "WAG") %>% 
              transmute(year = crab_year,
                        season = 3, fleet = 1, sex = 1,
                        obs = round(tot_retained_wt, 2),
                        cv = 0.0316, type = 1, units = 1, mult = 1, effort = 0, disc_m = 0.2) %>%
              arrange(year) ) -> wag_retained

## total catch

### eag
read_csv("./AIGKC/data/observer/total_catch.csv") %>%
  filter(substring(fishery, 1, 2) == "OB",
         group %in% c("sublegal_male", "legal_male")) %>%
  group_by(fishery, crab_year) %>%
  summarise(total_catch_t = sum(total_catch_wt)) %>% ungroup %>%
  # join to cv
  left_join(.,
            
            read_csv("./AIGKC/data/observer/nonzero_obs_pots.csv") %>%
              filter(substring(fishery, 1, 2) == "OB") %>%
              # compute graded weight
              mutate(w = (m_nz * 250) / max(m_nz), 
                     cv = sqrt(exp(1 / (2 * w)) - 1)) %>%
              transmute(crab_year, cv)
            
  ) %>%
  transmute(year = crab_year,
            season = 3, fleet = 1, sex = 1,
            obs = round(total_catch_t, 2),
            cv = round(cv, 3), type = 0, units = 1, mult = 1, effort = 0, disc_m = 0.2) %>%
  arrange(year) -> eag_total

### wag
read_csv("./AIGKC/data/observer/2025/total_catch.csv") %>%
  filter(fishery == "WAG",
         group %in% c("sublegal_male", "legal_male")) %>% 

  group_by(fishery, crab_year) %>%
  summarise(total_catch_t = sum(total_catch_wt)) %>% ungroup %>% 
  # join to cv
  left_join(.,
            
            read_csv("./AIGKC/data/observer/2025/nonzero_obs_pots.csv") %>%
              filter(subdistrict == "WAG") %>%
              # compute graded weight
              mutate(w = (m_nz * 250) / max(m_nz), 
                     cv = sqrt(exp(1 / (2 * w)) - 1)) %>%
              transmute(crab_year, cv)
            
  ) %>%
  transmute(year = crab_year,
            season = 3, fleet = 1, sex = 1,
            obs = round(total_catch_t, 2),
            cv = round(cv, 3), type = 0, units = 1, mult = 1, effort = 0, disc_m = 0.2) %>%
  arrange(year) -> wag_total


## gf bycatch  

# bycatch data pre 2009
bycatch_raw_pre2009 <- read_csv("./AIGKC/data/groundfish/Crab Bycatch Estimates 1991-2009.csv")
# bycatch detail 2009 - present
bycatch_raw <- read_csv("./AIGKC/data/groundfish/Crab Bycatch Estimates.csv")
# est
## pre-2009
bycatch_raw_pre2009 %>%
  janitor::clean_names() %>%
  # crab year
  mutate(crab_year = as.numeric(substring(crab_year, 1, 4))) %>%
  # determine eag / wag
  mutate(fishery = case_when(reporting_area_code %in% c(518, 519, 541) ~ "EAG",
                             reporting_area_code %in% c(542, 543) ~ "WAG")) %>%
  filter(!is.na(fishery)) %>%
  # get mortality estimates by year and gear
  group_by(crab_year, fishery, gear) %>%
  summarise(disc_t = sum(estimate_wt_sum) / 1000) %>% ungroup %>%
  mutate(h_m = case_when(gear == "FIXED" ~ 0.5,
                         gear == "TRAWL" ~ 0.8)) %>%
  group_by(crab_year, fishery) %>%
  summarise(tot_disc_t = sum(disc_t), 
            m = weighted.mean(h_m, w = disc_t)) %>% ungroup %>%
  bind_rows(# 2009 - present
    bycatch_raw %>%
      janitor::clean_names() %>%
      rename(fishery = fishery_code) %>%
      filter(!is.na(fishery)) %>%
      # get mortality estimates by year and gear
      group_by(crab_year, fishery, agency_gear_code) %>%
      summarise(disc_t = sum( estimate_wt_kg_crab) / 1000) %>% ungroup %>%
      mutate(gear = case_when(agency_gear_code %in% c("HAL", "POT", "JIG") ~ "FIXED",
                              agency_gear_code %in% c("NPT", "PTR") ~ "TRAWL")) %>%
      group_by(crab_year, fishery, gear, disc_t) %>% ungroup %>%
      mutate(h_m = case_when(gear == "FIXED" ~ 0.5,
                             gear == "TRAWL" ~ 0.8)) %>%
      group_by(crab_year, fishery) %>%
      summarise(tot_disc_t = sum(disc_t), 
                m = weighted.mean(h_m, w = disc_t)) %>% ungroup) -> gf_bycatch_est
# eag 
gf_bycatch_est %>%
  filter(fishery == "EAG",
         tot_disc_t > 0) %>%
  transmute(year = crab_year,
            season = 3, fleet = 2, sex = 1, 
            obs = round(tot_disc_t, 4),
            cv = 1.3108,
            type = 2, units = 1,
            mult = 1,
            effort = 0, 
            disc_m = round(m, 2)) -> eag_bycatch

# wag 
gf_bycatch_est %>%
  filter(fishery == "WAG",
         tot_disc_t > 0) %>%
  transmute(year = crab_year,
            season = 3, fleet = 2, sex = 1, 
            obs = round(tot_disc_t, 4),
            cv = 1.3108,
            type = 2, units = 1,
            mult = 1,
            effort = 0, 
            disc_m = round(m, 2)) -> wag_bycatch

# joining catch data frames

## eag
### dimensions
eag_dat$n_catch_series <- 3
eag_dat$n_catch_rows <- c(nrow(eag_retained), nrow(eag_total), nrow(eag_bycatch))
eag_dat$catch <- bind_rows(eag_retained, eag_total, eag_bycatch)

## wag
### dimensions
wag_dat$n_catch_series <- 3
wag_dat$n_catch_rows <- c(nrow(wag_retained), nrow(wag_total), nrow(wag_bycatch))
wag_dat$catch <- bind_rows(wag_retained, wag_total, wag_bycatch)

# index ----

# eag
# # spatiotemporal model index
# read_csv("./AIGKC/output/cpue_std/2025/jan/eag_st_ar1_index.csv") %>%
#   transmute(series = 1,
#             year, 
#             season = 3, fleet = 1, sex = 1, maturity = 0, 
#             obs = round(index, 3), cv = round(cv, 3), units = 2, timing = 0.5) -> st_index

# wag

## cpue index 
### pre rationalized not updated
read_csv("./AIGKC/output/cpue_std/2024/may/pre_wag_index2.csv") %>%
  transmute(series = 1,
            year, season = 3, fleet = 1, sex = 1, maturity = 0, 
            obs = round(index, 4), cv = round(se / index, 4), units = 2, timing = 0.5) -> wag_pre_cpue_index
### post rationalized updated
read_csv("./AIGKC/output/cpue_std/2025/may/post_wag_index.csv") %>%
  transmute(series = 2,
            year, season = 3, fleet = 1, sex = 1, maturity = 0, 
            obs = round(index, 4), cv = round(se / index, 4), units = 2, timing = 0.5) -> wag_post_cpue_index
## fish ticket index not updated
read_csv("./AIGKC/output/cpue_std/wag_fish_tickets_85_98_std_index_may2024.csv") %>%
  transmute(series = 3,
            year, season = 3, fleet = 1, sex = 1, maturity = 0, 
            obs = round(index, 4), cv = round(se / index, 4), units = 2, timing = 0.5) -> wag_ft_index



## dimensions
wag_dat$n_index_series <- 3
wag_dat$n_index_rows <- nrow(wag_pre_cpue_index) + nrow(wag_post_cpue_index) + nrow(wag_ft_index)
wag_dat$index <- bind_rows(wag_pre_cpue_index, wag_post_cpue_index, wag_ft_index)

# size composition (base neff) ----

## retained
read_csv("./AIGKC/data/observer/2025/retained_size_comp.csv") %>%
  filter(size > 100) %>%
  # add length bin
  mutate(bin = ceiling(size / 5) * 5 - 2,
         bin = ifelse(bin < 103, 103, bin),
         bin = ifelse(bin > 183, 183, bin),
         bin = factor(bin, levels = seq(103, 183, 5))) %>%
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
  left_join(read_csv("./AIGKC/data/observer/2025/vessel_days.csv") %>%
              transmute(crab_year, fishery, neff = round(n_days))) %>% ungroup -> retained_comp

## total comp
read_csv("./AIGKC/data/observer/2025/directed_observer_size_comp.csv") %>%
  filter(size > 100) %>%
  # add length bin
  mutate(bin = ceiling(size / 5) * 5 - 2,
         bin = ifelse(bin < 103, 103, bin),
         bin = ifelse(bin > 183, 183, bin),
         bin = factor(bin, levels = seq(103, 183, 5))) %>%
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
  left_join(read_csv("./AIGKC/data/observer/2025/observed_vessel_days.csv") %>%
              transmute(crab_year, fishery, neff = round(n_days))) %>% ungroup -> total_comp

# wag
retained_comp %>%
  filter(fishery == "WAG") %>%
  transmute(org_series = 1, year = crab_year,
            season = 3, fleet = 1, sex = 1, type = 1, shell = 0, maturity = 0,
            nsamp = neff, size = bin, obs =  sprintf("%.5f", prop)) -> wag_retained_comp
total_comp %>%
  filter(fishery == "WAG") %>%
  transmute(org_series = 2, year = crab_year,
            season = 3, fleet = 1, sex = 1, type = 0, shell = 0, maturity = 0,
            nsamp = neff, size = bin, obs =  sprintf("%.5f", prop)) -> wag_total_comp
  
## dimensions
wag_dat$n_size_series <- 2
wag_dat$n_size_rows <- c(length(unique(wag_retained_comp$year)), length(unique(wag_total_comp$year)))
wag_dat$n_size_bin_series <- c(17, 17)
wag_dat$size_comp <- bind_rows(wag_retained_comp, wag_total_comp)


# size composition (bootstrap neff) ----


read_csv("./AIGKC/data/observer/2025/retained_size_comp.csv") %>%
  filter(size > 100) %>%
  # add length bin
  mutate(bin = ceiling(size / 5) * 5 - 2,
         bin = ifelse(bin < 103, 103, bin),
         bin = ifelse(bin > 183, 183, bin)) %>%
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
  left_join(read_csv("./AIGKC/output/observer/length_comp_boot/retained_neff_boot_table_wag.csv") %>%
              transmute(crab_year, fishery, neff = round(neff_mean)),
            by = join_by(crab_year, fishery)) %>%
  mutate(neff = min(neff, 2000)) %>% ungroup -> retained_comp



read_csv("./AIGKC/data/observer/2025/directed_observer_size_comp.csv") %>%
  filter(size > 100,
         !(fishery == "EAG" & crab_year != 1993)) %>%
  # add length bin
  mutate(bin = ceiling(size / 5) * 5 - 2,
         bin = ifelse(bin < 103, 103, bin),
         bin = ifelse(bin > 183, 183, bin)) %>%
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
  left_join(read_csv("./AIGKC/output/observer/length_comp_boot/total_neff_boot_table_wag.csv") %>%
              transmute(crab_year, fishery, neff = round(neff_mean))) %>%
  mutate(neff = min(neff, 2000)) %>% ungroup -> total_comp


# wag
retained_comp %>%
  filter(fishery == "WAG") %>%
  transmute(org_series = 1, year = crab_year,
            season = 3, fleet = 1, sex = 1, type = 1, shell = 0, maturity = 0,
            nsamp = neff, size = bin, obs =  sprintf("%.5f", prop)) -> wag_retained_comp
total_comp %>%
  filter(fishery == "WAG") %>%
  transmute(org_series = 2, year = crab_year,
            season = 3, fleet = 1, sex = 1, type = 0, shell = 0, maturity = 0,
            nsamp = neff, size = bin, obs =  sprintf("%.5f", prop)) -> wag_total_comp

## dimensions
wag_dat$n_size_series <- 2
wag_dat$n_size_rows <- c(length(unique(wag_retained_comp$year)), length(unique(wag_total_comp$year)))
wag_dat$n_size_bin_series <- c(17, 17)
wag_dat$size_comp <- bind_rows(wag_retained_comp, wag_total_comp)

# output dat file ----

# wag
gmacs_write_dat(wag_dat, "./AIGKC/models/2025/may/WAG/23.1c/WAG_23_1c.dat")
gmacs_write_dat(wag_dat, "./AIGKC/models/2025/may/WAG/25.0b/WAG_25_0b.dat")
