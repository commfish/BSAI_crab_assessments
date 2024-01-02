# notes ----

# aigkc groundfish bycatch data analysis
# tyler jackson
# 9/6/2023

# load ----

source("./AIGKC/code/aigkc_functions.R")

# data ----

# bycatch data pre 2009
bycatch_raw_pre2009 <- read_csv("./AIGKC/data/groundfish/gkc_bycatch_est_1991_2009.csv")

# bycatch detail 2009 - present
bycatch_raw_eag <- read_csv("./AIGKC/data/groundfish/gkc_bycatch_eag_detail.csv", skip = 6)
bycatch_raw_wag <- read_csv("./AIGKC/data/groundfish/gkc_bycatch_wag_detail.csv", skip = 6)


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
  mutate(mort_t = case_when(gear == "FIXED" ~ 0.5 * disc_t,
                            gear == "TRAWL" ~ 0.8 * disc_t)) %>%
  # get mortality estimates by year
  group_by(crab_year, fishery) %>%
  summarise(mort_t = sum(mort_t)) %>% ungroup -> est_pre_2009

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
  mutate(mort_t = case_when(agency_gear_code %in% c("HAL", "POT", "JIG") ~ 0.5 * disc_t,
                            agency_gear_code %in% c("NPT", "PTR") ~ 0.8 * disc_t)) %>%
  # get mortality estimates by year
  group_by(crab_year, fishery) %>%
  summarise(mort_t = sum(mort_t)) -> est_2009_present

# combine
bind_rows(est_pre_2009, est_2009_present) %>%
  write_csv(., "./AIGKC/output/gf_bycatch/gf_bycatch_est.csv")
est <- read_csv("./AIGKC/output/gf_bycatch/gf_bycatch_est.csv")

# gmacs input
## eag
bind_rows(est_pre_2009, est_2009_present) %>%
  filter(fishery == "EAG",
         mort_t > 0) %>%
  transmute(`#year` = crab_year,
            seas = 3, fleet = 2, sex = 1, 
            obs = sprintf("%.4f", mort_t),
            cv = 1.3108,
            type = 2, units = 1,
            mult = 1.538461538,
            effort = 0, discard_motality = 0.65) %>% 
  write_delim(., "./AIGKC/data/gmacs/2024_jan/eag_gf_bycatch_tj.txt")
## wag
bind_rows(est_pre_2009, est_2009_present) %>%
  filter(fishery == "WAG",
         mort_t > 0) %>%
  transmute(`#year` = crab_year,
            seas = 3, fleet = 2, sex = 1, 
            obs = sprintf("%.4f", mort_t),
            cv = 1.3108,
            type = 2, units = 1,
            mult = 1.538461538,
            effort = 0, discard_motality = 0.65) %>%
  write_delim(., "./AIGKC/data/gmacs/2024_jan/wag_gf_bycatch_tj.txt")
## ai
est %>%
  group_by(crab_year) %>%
  summarize(mort_t = sum(mort_t)) %>%
  filter(mort_t > 0) %>%
  transmute(`#year` = crab_year,
            seas = 3, fleet = 3, sex = 1, 
            obs = sprintf("%.4f", mort_t),
            cv = 1.3108,
            type = 2, units = 1,
            mult = 1.538461538,
            effort = 0, discard_motality = 0.65) %>%
  write_delim(., "./AIGKC/data/gmacs/2024_jan/ai_gf_bycatch_tj.txt")


