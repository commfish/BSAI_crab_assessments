# notes ----
## figures 15 and 35 bbrkc safe
## Tyler Jackson (original code)
## 8/23/2022 / 8-22-23 (k.palof) / 8-16-24(kjp)/ 8-18-25 (kjp)

# load ----

library(tidyverse)
library(patchwork)
#library(FNGr); theme_set(theme_sleek())

cur_yr <- 2025 # update annually
folder <- "bbrkc_25f" # update annually
.FIGS     = c(paste0("./BBRKC/", folder, "/doc/figures/"))
# y axis minor ticks
yr_axis = tickr(tibble(yr = 1975:2050), yr, 5)

# data ----

library(crabpack)

## Connect to pull data ----------------
channel <- "API"
#channel <- crabpack::get_connected()

## Pull specimen data
specimen_data <- crabpack::get_specimen_data(species = "RKC",
                                             region = "EBS",
                                             years = c(1975:2025),
                                             channel = "API")


survey_size_comp <- crabpack::calc_bioabund(crab_data = specimen_data, 
                                            species = "RKC", 
                                            region = "EBS", 
                                            district = "BB", 
                                            bin_1mm = TRUE, 
                                            sex = TRUE)

# speperate out specimen and haul data -
specimen <- specimen_data$specimen
hauls <- specimen_data$haul

hauls %>% 
  filter(DISTRICT == "BB") -> hauls_bb

hauls %>% 
  filter(DISTRICT == "NORTH") -> hauls_north

# ERIN IGNORE THIS ONE bristol bay female abundance by shell condition and clutch fullness, figure 15----

# every station towed in each year
hauls_bb %>% 
  distinct(YEAR, STATION_ID, TOTAL_AREA) %>% 
  expand_grid(SEX = 2, CLUTCH_SIZE = 0:6) -> hauls_bb

# estimates in 1 mm bins --
specimen %>% 
  filter(DISTRICT == "BB", 
         SEX == 2, 
         CLUTCH_SIZE %in% 0:6,
         SHELL_CONDITION %in% 1:2,
         SIZE_1MM > 89) %>% 
  # not going to deal with retows right now (but this is where that would go)
  # compute calculated weight by line item 
  mutate(wt = SAMPLING_FACTOR * CALCULATED_WEIGHT_1MM) %>%
  # compute station cpue by clutch size
  group_by(YEAR, STATION_ID, TOTAL_AREA, CLUTCH_SIZE) %>%
  summarise(n = sum(SAMPLING_FACTOR),
            wt_t = sum(wt) * 1e-6,
            cpue_n = n / mean(AREA_SWEPT),
            cpue_t = wt_t / mean(AREA_SWEPT)) %>%
  # join to all hauls, fill in zero catches
  right_join(hauls_bb) %>%
  replace_na(list(n = 0, cpue_n = 0, wt_t = 0, cpue_t = 0)) %>%
  
  # expand to abundance and biomass
  # group_by(akfin_survey_year, sex, clutch_size) %>%
  # summarise(N_hat = mean(cpue_n) * bb_area,
  #           se_N = sqrt((var(cpue_n) / n()) * bb_area^2),
  #           B_hat = mean(cpue_t) * bb_area,
  #           se_B = sqrt((var(cpue_t) / n()) * bb_area^2))
  
  # replace with above section when nmfs corrects their timeseries (area should be constant across years)
  
  group_by(YEAR, SEX, CLUTCH_SIZE) %>%
  summarise(N_hat = mean(cpue_n) * mean(TOTAL_AREA),
            se_N = sqrt((var(cpue_n) / n()) * mean(TOTAL_AREA)^2),
            B_hat = mean(cpue_t) * mean(TOTAL_AREA),
            se_B = sqrt((var(cpue_t) / n()) * mean(TOTAL_AREA)^2)) -> clutch_est

## safe, figure 15 - female abundance by clutch size
clutch_est %>% 
  # remove immature
  filter(CLUTCH_SIZE != 0) %>%
  # add clutch text
  mutate(clutch = case_when(CLUTCH_SIZE == 1 ~ "Barren",
                            CLUTCH_SIZE == 2 ~ "1/8 Full",
                            CLUTCH_SIZE == 3 ~ "1/4 Full",
                            CLUTCH_SIZE == 4 ~ "1/2 Full",
                            CLUTCH_SIZE == 5 ~ "3/4 Full",
                            CLUTCH_SIZE == 6 ~ "Full"),
         clutch = factor(clutch, levels = c("Full", "3/4 Full", "1/2 Full", "1/4 Full", "1/8 Full", "Barren"))) %>%
  # compute total mature females
  group_by(YEAR) %>%
  mutate(total_N_hat = sum(N_hat)) %>%
  # compute proportion by clutch size 
  group_by(YEAR, CLUTCH_SIZE) %>%
  mutate(proportion = N_hat / total_N_hat) -> clutch_data

# jie's figure 15
clutch_data %>%
  mutate(clutch_num = case_when(CLUTCH_SIZE == 1 ~ 0,
                                CLUTCH_SIZE == 2 ~ 0.125,
                                CLUTCH_SIZE == 3 ~ 0.25,
                                CLUTCH_SIZE == 4 ~ 0.5,
                                CLUTCH_SIZE == 5 ~ 0.75,
                                CLUTCH_SIZE == 6 ~ 1)) %>%
  group_by(YEAR) %>%
  summarise(mean_clutch = weighted.mean(clutch_num[CLUTCH_SIZE %in% 2:6], w = N_hat[CLUTCH_SIZE %in% 2:6]),
            prop_barren = proportion[CLUTCH_SIZE == 1]) %>%
  #add data for mean by time perios
  mutate(period = ifelse(YEAR < 1991, 1, 2)) %>%
  group_by(period) %>%
  mutate(mean_clutch_period = mean(mean_clutch)) %>%
  ggplot()+
  geom_point(aes(x = YEAR, y = mean_clutch), color = "red")+
  geom_line(aes(x = YEAR, y = mean_clutch), linetype = 2, color = "red")+
  geom_line(aes(x = YEAR, y = mean_clutch_period), linetype = 2, color = "blue")+
  geom_point(aes(x = YEAR, y = prop_barren), shape = 18)+
  geom_line(aes(x = YEAR, y = prop_barren))+
  scale_x_continuous(labels = yr_axis$labels, breaks = yr_axis$breaks)+
  labs(x = NULL, y = "Proportion") -> x
ggsave(paste0(.FIGS, "figure15.png"), plot = x, height= 3, width = 5, units = "in")

# alternative figure 15
clutch_data %>%
  ggplot()+
  geom_bar(aes(x = YEAR, y = proportion, fill = factor(clutch)), stat = "identity")+
  scale_fill_brewer(palette = "Spectral")+
  scale_x_continuous(labels = yr_axis$labels, breaks = yr_axis$breaks)+
  coord_cartesian(expand = 0)+
  labs(x = NULL, y = "Proportion", fill = NULL) -> x
ggsave(paste0(.FIGS, "figure_15_alternate.png"), plot = x, height= 3, width = 5, units = "in")
