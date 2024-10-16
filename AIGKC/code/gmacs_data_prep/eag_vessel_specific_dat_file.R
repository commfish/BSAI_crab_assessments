# notes ----

## .dat file for aigkc assessment with vessels as fleets
## tyler jackson

# library ----

source("./AIGKC/code/aigkc_functions.R")
library(gmacsr)

# base dat file ----

base_dat <- gmacs_read_dat("./AIGKC/models/2025/sept/EAG/25.0b/EAG_25_0b.dat", model_name = "25.0b", version = "2.20.16")

# change setup ----

dat <- base_dat
dat$model_name <- "25.x"

# number of fleets
dat$n_fleets <- base_dat$n_fleets + 6
dat$fleets_names <- c(base_dat$fleets_names, "EarlyDawn", "AleutNo1", "ErlaN", "PatLee", "AKTrojan", "Other")

# catch data ----

# retained catch
base_dat$catch %>%
  filter(type == 1, fleet == 1, year < 2005) %>%
  bind_rows(read_csv("AIGKC/data/observer/retained_catch_by_vessel.csv") %>%
              mutate(fleet = case_when(adfg_number == 103 ~ 3,
                                       adfg_number == 5992 ~ 4,
                                       adfg_number == 20556 ~ 5,
                                       adfg_number == 35767 ~ 6,
                                       adfg_number == 37887 ~ 7,
                                       adfg_number == 999999 ~ 8)) %>%
              filter(fishery == "EAG") %>%
              transmute(year = crab_year, season = 3, fleet, sex = 1, obs = t, cv = 0.0316, type = 1, units = 2, mult = 1, effort = 0, disc_m = 0.2) %>%
              arrange(fleet, year)) -> retained

# total catch
base_dat$catch %>%
  filter(type == 0, fleet == 1, year < 2005) %>% 
  bind_rows(read_csv("AIGKC/data/observer/total_catch_by_vessel.csv") %>%
              filter(grepl("O", fishery),
                     group != "female") %>%
              group_by(crab_year, adfg) %>%
              summarise(t = sum(total_catch_wt)) %>% ungroup %>%
              filter(t > 0) %>%
              mutate(fleet = case_when(adfg == 103 ~ 3,
                                       adfg == 5992 ~ 4,
                                       adfg == 20556 ~ 5,
                                       adfg == 35767 ~ 6,
                                       adfg == 37887 ~ 7,
                                       adfg == 999999 ~ 8)) %>%
              transmute(year = crab_year, season = 3, fleet, sex = 1, obs = t, cv = 0.1, type = 0, units = 2, mult = 1, effort = 0, disc_m = 0.2) %>%
              arrange(fleet, year)) -> total
# bycatch
base_dat$catch %>%
  filter(fleet == 2) -> bycatch

dat$catch <- bind_rows(retained, total, bycatch) %>% arrange(type, fleet, year)

dat$n_catch_series <- 15
dat$catch %>%
  count(fleet, type) %>% pull(n) -> dat$n_catch_rows

# index data ----

base_dat$index %>%
  filter(series %in% c(1, 3), fleet == 1, year < 2005) %>%
  mutate(series = ifelse(series == 3, 2, series)) %>%
  bind_rows(read_csv("AIGKC/data/observer/retained_catch_by_vessel.csv") %>%
              filter(fishery == "EAG") %>%
              mutate(fleet = case_when(adfg_number == 103 ~ 3,
                                       adfg_number == 5992 ~ 4,
                                       adfg_number == 20556 ~ 5,
                                       adfg_number == 35767 ~ 6,
                                       adfg_number == 37887 ~ 7,
                                       adfg_number == 999999 ~ 8)) %>%
              filter(!(fleet %in% 7:8)) %>%
              transmute(series = fleet, year = crab_year, season = 3, fleet, sex = 1, maturity = 0, obs = n / effort, cv = 0.02, units = 2, timing = 0.5) %>%
              group_by(series) %>%
              mutate(obs = obs / (prod(obs)^(1/n()))) %>%
              arrange(series, year)) -> dat$index
dat$n_index_rows <- nrow(dat$index)           
dat$n_index_series <- length(unique(dat$index$series))    
dat$index_type <- rep(2, dat$n_index_series)
              
              
              
# size comp data ----

# retained
base_dat$size_comp %>% 
  filter(type == 1, year < 2005) %>%
  bind_rows(read_csv("AIGKC/data/observer/retained_size_comp_by_vessel.csv") %>%
              filter(size > 100,
                     grepl("O", fishery)) %>%
              f_add_len_bin(., .$size) %>%
              mutate(bin = as.character(bin - 2.5)) %>%
              mutate(fleet = case_when(adfg == 103 ~ 3,
                                       adfg == 5992 ~ 4,
                                       adfg == 20556 ~ 5,
                                       adfg == 35767 ~ 6,
                                       adfg == 37887 ~ 7,
                                       adfg == 999999 ~ 8)) %>%
              group_by(crab_year, fishery, fleet) %>%
              mutate(prop = round(total / sum(total), 4)) %>% 
              group_by(crab_year, fishery, fleet, bin) %>%
              summarise(prop = sum(prop)) %>% ungroup %>%
              transmute(org_series = fleet - 1,
                        year = crab_year, season = 3, fleet, sex = 1, type = 1, shell = 0, maturity = 0, nsamp = 500, size = bin, obs = prop) %>%
              arrange(org_series, year, size) ) -> retained_comp


# total
base_dat$size_comp %>% 
  filter(type == 0, year < 2005) %>%
  mutate(org_series = max(retained_comp$org_series) + 1) %>%
  bind_rows(read_csv("AIGKC/data/observer/directed_observer_size_comp_by_vessel.csv") %>%
              filter(size > 100,
                     grepl("O", fishery)) %>%
              f_add_len_bin(., .$size) %>%
              mutate(bin = as.character(bin - 2.5)) %>%
              mutate(fleet = case_when(adfg == 103 ~ 3,
                                       adfg == 5992 ~ 4,
                                       adfg == 20556 ~ 5,
                                       adfg == 35767 ~ 6,
                                       adfg == 37887 ~ 7,
                                       adfg == 999999 ~ 8)) %>%
              group_by(crab_year, fishery, fleet) %>%
              mutate(prop = round(total / sum(total), 4)) %>% 
              group_by(crab_year, fishery, fleet, bin) %>%
              summarise(prop = sum(prop)) %>% ungroup %>%
              transmute(org_series = fleet +6,
                        year = crab_year, season = 3, fleet, sex = 1, type = 0, shell = 0, maturity = 0, nsamp = 500, size = bin, obs = prop) %>%
              arrange(org_series, year, size) ) -> total_comp

dat$size_comp <- bind_rows(retained_comp, total_comp)
dat$n_size_series <- length(unique(dat$size_comp$org_series))
dat$n_size_rows <- dat$size_comp %>%
  distinct(org_series, year) %>%
  count(org_series) %>% pull(n)
dat$n_size_bin_series <- rep(17, dat$n_size_series)


# write ----

gmacs_write_dat(dat, file = "./AIGKC/models/2025/sept/EAG/25.x/EAG_25_x.dat")















