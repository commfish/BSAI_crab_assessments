# notes ----

# aigkc groundfish bycatch data analysis
# tyler jackson
# 9/4/2024

# load ----

source("./AIGKC/code/aigkc_functions.R")

# data ----

# bycatch data pre 2009
bycatch_raw_pre2009 <- read_csv("./AIGKC/data/groundfish/gkc_bycatch_est_1991_2008.csv")

# bycatch detail 2009 - present
bycatch_raw_eag <- read_csv("./AIGKC/data/groundfish/gkc_bycatch_eag_detail.csv", skip = 7)
bycatch_raw_wag <- read_csv("./AIGKC/data/groundfish/gkc_bycatch_wag_detail.csv", skip = 7)


# summary ----

# pre-2009
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
            m = weighted.mean(h_m, w = disc_t)) %>% ungroup -> est_pre_2009


# 2009 - present
bycatch_raw_eag %>%
  janitor::clean_names() %>%
  mutate(fishery = "EAG") %>%
  # bind eag and wag
  bind_rows(bycatch_raw_wag %>%
              janitor::clean_names() %>%
              mutate(fishery = "WAG")) %>%
  filter(!is.na(fishery)) %>%
  # get mortality estimates by year and gear
  group_by(crab_year, fishery, agency_gear_code) %>%
  summarise(disc_t = sum(estimated_crab_weight_kg) / 1000) %>% ungroup %>%
  mutate(gear = case_when(agency_gear_code %in% c("HAL", "POT", "JIG") ~ "FIXED",
                            agency_gear_code %in% c("NPT", "PTR") ~ "TRAWL")) %>%
  group_by(crab_year, fishery, gear, disc_t) %>% ungroup %>%
  mutate(h_m = case_when(gear == "FIXED" ~ 0.5,
                         gear == "TRAWL" ~ 0.8)) %>%
  group_by(crab_year, fishery) %>%
  summarise(tot_disc_t = sum(disc_t), 
            m = weighted.mean(h_m, w = disc_t)) %>% ungroup -> est_2009_present

# combine
bind_rows(est_pre_2009, est_2009_present) %>%
  write_csv(., "./AIGKC/output/gf_bycatch/gf_bycatch_est.csv")
est <- read_csv("./AIGKC/output/gf_bycatch/gf_bycatch_est.csv")

# gmacs input
## eag
bind_rows(est_pre_2009, est_2009_present) %>%
  filter(fishery == "EAG",
         tot_disc_t > 0) %>%
  transmute(`#year` = crab_year,
            seas = 3, fleet = 2, sex = 1, 
            obs = sprintf("%.4f", tot_disc_t),
            cv = 1.3108,
            type = 2, units = 1,
            mult = 1,
            effort = 0, discard_motality = round(m, 2)) %>% 
  write_delim(., "./AIGKC/data/gmacs/2025_may/eag_gf_bycatch.txt")
## wag
bind_rows(est_pre_2009, est_2009_present) %>%
  filter(fishery == "WAG",
         tot_disc_t > 0) %>%
  transmute(`#year` = crab_year,
            seas = 3, fleet = 2, sex = 1, 
            obs = sprintf("%.4f", tot_disc_t),
            cv = 1.3108,
            type = 2, units = 1,
            mult = 1,
            effort = 0, discard_motality = round(m, 2)) %>%
  write_delim(., "./AIGKC/data/gmacs/2025_may/wag_gf_bycatch.txt")


  