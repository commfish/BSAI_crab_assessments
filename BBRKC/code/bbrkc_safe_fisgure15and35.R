# notes ----
## figures 15 and 35 bbrkc safe
## Tyler Jackson
## 8/23/2022 / 8-22-23 (k.palof)

# load ----

library(tidyverse)
library(patchwork)
library(FNGr); theme_set(theme_sleek())

cur_yr <- 2023 # update annually
folder <- "bbrkc_23f" # update annually
.FIGS     = c(paste0("./BBRKC/", folder, "/doc/figures/"))
# y axis minor ticks
yr_axis = tickr(tibble(yr = 1975:2050), yr, 5)

# data ----

# specimen data (haul data dump)
read.csv("./BBRKC/data/2023/survey/EBSCrab_Haul.csv", skip = 5) %>%
  rename_all(tolower) -> specimen

# strata file (strata dump) - do NOT presort by BB when pull it from AKFIN
# AKFIN/Crab Data/"Lookups/Translations"/"Stata/Station Characteristics" - 1975 to 2023, RED KING CRAB
read.csv("./BBRKC/data/2023/survey/EBSCRAB - Strata Report_all.csv") %>%
  rename_all(tolower) %>%
  rename_at(1, ~"station_id") -> strata

# refine strata information ----

## list of bristol bay stations
strata %>%
  filter(district == "Bristol Bay") %>%
  distinct(station_id) %>% pull -> bb_stations

## survey area in bristol bay nmi2
bb_area <- length(bb_stations) * 401 #each full station is 401 nmi2

## list of northern unstratified stations
strata %>%
  filter(survey_year == 2021) %>%
  #count(district)
  filter(district == "Northern Unstratified") %>%
  distinct(station_id) %>% pull -> nu_stations

## survey area in northern unstratified nmi2
nu_area <- length(nu_stations) * 401 #each full station is 401 nmi2

# bristol bay female abundance by shell condition and clutch fullness, figure 15----

# every station towed in each year
strata %>%
  filter(district == "Bristol Bay") %>%
  distinct(survey_year, station_id, total_area_sq_nm) %>%
  dplyr::rename(akfin_survey_year = survey_year, gis_station = station_id) %>%
  expand_grid(sex = 2, clutch_size = 0:6) -> hauls

# estimates in 1 mm bins
specimen %>%
  # filter for component of the population of interest
  filter(gis_station %in% bb_stations,
         haul_type %in% c(3, 0, 4, 17),  # will likely want to remove haul types 0 and 4 after nmfs corrects their timeseries
         sex == 2,
         clutch_size %in% 0:6,
         shell_condition %in% 1:2,
         length_1mm > 89) %>%
  # handle retows
  group_by(akfin_survey_year, gis_station) %>%
  nest %>% ungroup %>%
  mutate(retow = purrr::map(data, function(data) {
    
    # no retow, no action
    if(!(17 %in% data$haul_type)) { out <- data}
    
    # retow, remove female data from original tows, male data from retows
    if(17 %in% data$haul_type) {
      
      data %>%
        filter(!(sex == 2 & haul_type == 3),
               !(sex == 1 & haul_type == 17)) -> out
      
    }
    
    return(out)
    
    
  })) %>%
  dplyr::select(-data) %>%
  unnest(retow) %>%
  # compute calculated weight by line item
  mutate(wt = sampling_factor * calculated_weight_1mm) %>%
  # compute station cpue by clutch size
  group_by(akfin_survey_year, gis_station, area_swept, clutch_size) %>%
  summarise(n = sum(sampling_factor),
            wt_t = sum(wt) * 1e-6,
            cpue_n = n / mean(area_swept),
            cpue_t = wt_t / mean(area_swept)) %>%
  # join to all hauls, fill in zero catches
  right_join(hauls) %>%
  replace_na(list(n = 0, cpue_n = 0, wt_t = 0, cpue_t = 0)) %>%
  
  # expand to abundance and biomass
  # group_by(akfin_survey_year, sex, clutch_size) %>%
  # summarise(N_hat = mean(cpue_n) * bb_area,
  #           se_N = sqrt((var(cpue_n) / n()) * bb_area^2),
  #           B_hat = mean(cpue_t) * bb_area,
  #           se_B = sqrt((var(cpue_t) / n()) * bb_area^2))
  
  # replace with above section when nmfs corrects their timeseries (area should be constant across years)
  
  group_by(akfin_survey_year, sex, clutch_size) %>%
  summarise(N_hat = mean(cpue_n) * mean(total_area_sq_nm),
            se_N = sqrt((var(cpue_n) / n()) * mean(total_area_sq_nm)^2),
            B_hat = mean(cpue_t) * mean(total_area_sq_nm),
            se_B = sqrt((var(cpue_t) / n()) * mean(total_area_sq_nm)^2)) -> clutch_est
  
## safe, figure 15 - female abundance by clutch size

# proportion by abundance of clutch size
clutch_est %>%
  # remove immature
  filter(clutch_size != 0) %>%
  # add clutch text
  mutate(clutch = case_when(clutch_size == 1 ~ "Barren",
                            clutch_size == 2 ~ "1/8 Full",
                            clutch_size == 3 ~ "1/4 Full",
                            clutch_size == 4 ~ "1/2 Full",
                            clutch_size == 5 ~ "3/4 Full",
                            clutch_size == 6 ~ "Full"),
         clutch = factor(clutch, levels = c("Full", "3/4 Full", "1/2 Full", "1/4 Full", "1/8 Full", "Barren"))) %>%
  # compute total mature females
  group_by(akfin_survey_year) %>%
  mutate(total_N_hat = sum(N_hat)) %>%
  # compute proportion by clutch size 
  group_by(akfin_survey_year, clutch_size) %>%
  mutate(proportion = N_hat / total_N_hat) -> clutch_data

# jie's figure 15
clutch_data %>%
  mutate(clutch_num = case_when(clutch_size == 1 ~ 0,
                                clutch_size == 2 ~ 0.125,
                                clutch_size == 3 ~ 0.25,
                                clutch_size == 4 ~ 0.5,
                                clutch_size == 5 ~ 0.75,
                                clutch_size == 6 ~ 1)) %>%
  group_by(akfin_survey_year) %>%
  summarise(mean_clutch = weighted.mean(clutch_num[clutch_size %in% 2:6], w = N_hat[clutch_size %in% 2:6]),
            prop_barren = proportion[clutch_size == 1]) %>%
  #add data for mean by time perios
  mutate(period = ifelse(akfin_survey_year < 1991, 1, 2)) %>%
  group_by(period) %>%
  mutate(mean_clutch_period = mean(mean_clutch)) %>%
  ggplot()+
  geom_point(aes(x = akfin_survey_year, y = mean_clutch), color = "red")+
  geom_line(aes(x = akfin_survey_year, y = mean_clutch), linetype = 2, color = "red")+
  geom_line(aes(x = akfin_survey_year, y = mean_clutch_period), linetype = 2, color = "blue")+
  geom_point(aes(x = akfin_survey_year, y = prop_barren), shape = 18)+
  geom_line(aes(x = akfin_survey_year, y = prop_barren))+
  scale_x_continuous(labels = yr_axis$labels, breaks = yr_axis$breaks)+
  labs(x = NULL, y = "Proportion") -> x
ggsave(paste0(.FIGS, "figure15.png"), plot = x, height= 3, width = 5, units = "in")
  

# alternative figure 15
clutch_data %>%
  ggplot()+
  geom_bar(aes(x = akfin_survey_year, y = proportion, fill = factor(clutch)), stat = "identity")+
  scale_fill_brewer(palette = "Spectral")+
  scale_x_continuous(labels = yr_axis$labels, breaks = yr_axis$breaks)+
  coord_cartesian(expand = 0)+
  labs(x = NULL, y = "Proportion", fill = NULL) -> x
ggsave(paste0(.FIGS, "figure_15_alternate.png"), plot = x, height= 3, width = 5, units = "in")

 
# compute male abundance estimates in northern area and bristol bay ----
# ---------------need to restart at line 47
# every station towed in each year
strata %>%
  filter(district %in% c("Bristol Bay", "Northern Unstratified")) %>%
  distinct(survey_year, district, station_id, total_area_sq_nm) %>%
  dplyr::rename(akfin_survey_year = survey_year, gis_station = station_id) %>%
  expand_grid(sex = 1, length_1mm = 0:250) -> hauls

  # estimates in 1 mm bins
  specimen %>%
    filter(gis_station %in% c(bb_stations, nu_stations),
           haul_type %in% c(3, 0, 4, 17),
           sex %in% 1) %>%
    # handle retows
    group_by(akfin_survey_year, gis_station) %>%
    nest %>% ungroup %>%
    
    mutate(retow = purrr::map(data, function(data) {
      
      # no retow, no action
      if(!(17 %in% data$haul_type)) { out <- data}
      
      # retow, remove female data from original tows, male data from retows
      if(17 %in% data$haul_type) {
        
        data %>%
          filter(!(sex == 2 & haul_type == 3),
                 !(sex == 1 & haul_type == 17)) -> out
        
      }
      
      return(out)
      
      
    })) %>%
    dplyr::select(-data) %>%
    unnest(retow) %>%
    # compute station cpue
    
    group_by(akfin_survey_year, gis_station, area_swept, sex, length_1mm, calculated_weight_1mm) %>%
    summarise(n = sum(sampling_factor),
              wt_t = n * mean(calculated_weight_1mm) * 1e-6,
              cpue_n = n / mean(area_swept),
              cpue_t = wt_t / mean(area_swept)) %>%
    
    # join to all hauls, fill in zero catches
    right_join(hauls) %>%
    replace_na(list(n = 0, cpue_n = 0, wt_t = 0, cpue_t = 0)) %>%
  
    group_by(akfin_survey_year, district, sex, length_1mm) -> temp
  temp %>%   
  summarise(N_hat = mean(cpue_n) * mean(total_area_sq_nm),
              se_N = sqrt((var(cpue_n) / n()) * mean(total_area_sq_nm)^2),
              B_hat = mean(cpue_t) * mean(total_area_sq_nm),
              se_B = sqrt((var(cpue_t) / n()) * mean(total_area_sq_nm)^2)) -> temp2 #est_1mm
est_1mm <- temp2

## jie's figures
### figure 35a
est_1mm %>%
  filter(length_1mm >= 120,
         akfin_survey_year >= 1985) %>%
  group_by(akfin_survey_year, district) %>%
  summarise(B_hat = sum(B_hat)) %>% ungroup %>%
  add_row(akfin_survey_year = 2020, district = c("Bristol Bay", "Northern Unstratified")) %>%
  pivot_wider(names_from = district, values_from = B_hat) %>%
  mutate(`Proportion of North (North)/(North+BB)` = `Northern Unstratified` / (`Bristol Bay` + `Northern Unstratified`) * 6e5) %>%
  #dplyr::rename(`BB biomass` = `Bristol Bay`,
  #       `North biomass` = `Northern Unstratified`) %>%
  pivot_longer(2:4, names_to = "stat", values_to = "value") %>%
   
  ggplot()+
  geom_point(aes(x = akfin_survey_year, y = value, color = stat, shape = stat))+
  geom_line(aes(x = akfin_survey_year, y =value, color = stat, group = stat))+
  scale_y_continuous(sec.axis = sec_axis(trans = ~./600000, name = "Proportion"), labels = scales::comma, limits = c(0, 80000))+
  scale_x_continuous(labels = yr_axis$labels, breaks = yr_axis$breaks)+
  scale_color_manual(values = c("red", "blue", "black"))+
  scale_shape_manual(values = c(15, 8, 16))+
  labs(x = NULL, y = "Mature male biomass (t)", color = NULL, shape = NULL)+
  theme(legend.justification = c(0,1),
        legend.position = c(0,1.05))-> x
ggsave(paste0(.FIGS, "figure_35a.png"), plot = x, height= 3, width = 5, units = "in")



### figure 35b
est_1mm %>%
  filter(length_1mm >= 135,
         akfin_survey_year >= 1985) %>%
  group_by(akfin_survey_year, district) %>%
  summarise(B_hat = sum(B_hat)) %>% ungroup %>%
  add_row(akfin_survey_year = 2020, district = c("Bristol Bay", "Northern Unstratified")) %>%
  pivot_wider(names_from = district, values_from = B_hat) %>%
  mutate(`Proportion of North (North)/(North+BB)` = `Northern Unstratified` / (`Bristol Bay` + `Northern Unstratified`) * 6e5) %>%
  #rename(`BB biomass` = `Bristol Bay`,
  #       `North biomass` = `Northern Unstratified`) %>%
  pivot_longer(2:4, names_to = "stat", values_to = "value") %>%
  
  ggplot()+
  geom_point(aes(x = akfin_survey_year, y = value, color = stat, shape = stat))+
  geom_line(aes(x = akfin_survey_year, y =value, color = stat, group = stat))+
  scale_y_continuous(sec.axis = sec_axis(trans = ~./600000, name = "Proportion"), labels = scales::comma, limits = c(0, 60000))+
  scale_x_continuous(labels = yr_axis$labels, breaks = yr_axis$breaks)+
  scale_color_manual(values = c("red", "blue", "black"))+
  scale_shape_manual(values = c(15, 8, 16))+
  labs(x = NULL, y = "Legal male biomass (t)", color = NULL, shape = NULL)+
  theme(legend.justification = c(0,1),
        legend.position = c(0,1.025))  -> x
ggsave(paste0(.FIGS, "figure_35b.png"), plot = x, height= 3, width = 5, units = "in")

#ggsave(paste0(.FIGS, "figure_15_alternate.png"), plot = x, height= 3, width = 5, units = "in")
# alternative figure
est_1mm %>%
  filter(length_1mm >= 120,
         akfin_survey_year >= 1985) %>%
  group_by(akfin_survey_year, district) %>%
  summarise(mmb = sum(B_hat)) %>%
  left_join(est_1mm %>%
              filter(length_1mm >= 135,
                     akfin_survey_year >= 1985) %>%
              group_by(akfin_survey_year, district) %>%
              summarise(lmb = sum(B_hat))) %>%
  group_by(akfin_survey_year) %>%
  mutate(prop_mmb = mmb / sum(mmb),
         prop_lmb = lmb / sum(lmb)) -> fig35_data

# plot a
fig35_data %>%
  filter(district == "Bristol Bay") %>%
  dplyr::select(akfin_survey_year, district, mmb, lmb) %>%
  pivot_longer(c(mmb, lmb)) %>%
  ggplot(aes(x = akfin_survey_year, y = value, linetype = name, shape = name))+
  geom_point()+
  geom_line()+
  scale_linetype_manual(values = c(2, 1), labels = c("Legal Males", "Mature males"))+
  scale_shape_manual(values = c(1, 16), labels = c("Legal Males", "Mature males"))+
  scale_x_continuous(labels = yr_axis$labels, breaks = yr_axis$breaks)+
  scale_y_continuous(labels = scales::comma)+
  labs(x = NULL, y = "Bristol Bay \n Biomass (t)", linetype = NULL, shape = NULL)+
  theme(legend.justification = c(1,1), legend.position = c(1,1)) -> a

# plot b
fig35_data %>%
  filter(district == "Northern Unstratified") %>%
  dplyr::select(akfin_survey_year, district, mmb, lmb) %>%
  pivot_longer(c(mmb, lmb)) %>%
  ggplot(aes(x = akfin_survey_year, y = value, linetype = name, shape = name))+
  geom_point()+
  geom_line()+
  scale_linetype_manual(values = c(2, 1), labels = c("Legal Males", "Mature males"))+
  scale_shape_manual(values = c(1, 16), labels = c("Legal Males", "Mature males"))+
  scale_x_continuous(labels = yr_axis$labels, breaks = yr_axis$breaks)+
  scale_y_continuous(labels = scales::comma)+
  labs(x = NULL, y = "Northern Unstratified \n Biomass (t)", linetype = NULL, shape = NULL)+
  theme(legend.justification = c(0,1), legend.position = c(0,1)) -> b

# plot c
fig35_data %>%
  filter(district == "Northern Unstratified") %>%
  dplyr::select(akfin_survey_year, district, prop_mmb, prop_lmb) %>%
  pivot_longer(c(prop_mmb, prop_lmb)) %>%
  ggplot(aes(x = akfin_survey_year, y = value, linetype = name, shape = name))+
  geom_point()+
  geom_line()+
  scale_linetype_manual(values = c(2, 1), labels = c("Legal Males", "Mature males"))+
  scale_shape_manual(values = c(1, 16), labels = c("Legal Males", "Mature males"))+
  scale_x_continuous(labels = yr_axis$labels, breaks = yr_axis$breaks)+
  scale_y_continuous(limit = c(0, 0.25))+
  labs(x = NULL, y = "Proportion Total Biomass in \n North Unstratified District", linetype = NULL, shape = NULL)+
  theme(legend.justification = c(0,1), legend.position = c(0,1)) -> c

ggsave(paste0(.FIGS, "figure_35_alternate.png"), plot = a/b/c, height = 8, width = 5, units = "in")


# compute female abundance estimates in northern area and bristol bay ----
# every station towed in each year
strata %>%
  filter(district %in% c("Bristol Bay", "Northern Unstratified")) %>%
  distinct(survey_year, district, station_id, total_area_sq_nm) %>%
  rename(akfin_survey_year = survey_year, gis_station = station_id) %>%
  expand_grid(sex = 2, length_1mm = 0:250) -> hauls

# estimates in 1 mm bins
specimen %>%
  filter(gis_station %in% c(bb_stations, nu_stations),
         haul_type %in% c(3, 0, 4, 17),
         sex %in% 2) %>%
  # handle retows
  group_by(akfin_survey_year, gis_station) %>%
  nest %>% ungroup %>%
  
  mutate(retow = purrr::map(data, function(data) {
    
    # no retow, no action
    if(!(17 %in% data$haul_type)) { out <- data}
    
    # retow, remove female data from original tows, male data from retows
    if(17 %in% data$haul_type) {
      
      data %>%
        filter(!(sex == 2 & haul_type == 3),
               !(sex == 1 & haul_type == 17)) -> out
      
    }
    
    return(out)
    
    
  })) %>%
  dplyr::select(-data) %>%
  unnest(retow) %>%
  # compute station cpue
  
  group_by(akfin_survey_year, gis_station, area_swept, sex, length_1mm, calculated_weight_1mm) %>%
  summarise(n = sum(sampling_factor),
            wt_t = n * mean(calculated_weight_1mm) * 1e-6,
            cpue_n = n / mean(area_swept),
            cpue_t = wt_t / mean(area_swept)) %>%
  
  # join to all hauls, fill in zero catches
  right_join(hauls) %>%
  replace_na(list(n = 0, cpue_n = 0, wt_t = 0, cpue_t = 0)) %>%
  
  group_by(akfin_survey_year, district, sex, length_1mm) %>%
  summarise(N_hat = mean(cpue_n) * mean(total_area_sq_nm),
            se_N = sqrt((var(cpue_n) / n()) * mean(total_area_sq_nm)^2),
            B_hat = mean(cpue_t) * mean(total_area_sq_nm),
            se_B = sqrt((var(cpue_t) / n()) * mean(total_area_sq_nm)^2)) -> est_1mm


## jie's figures
### figure 35a - Abundance of females <90mm NOT working 
est_1mm %>%
  filter(length_1mm <= 90,
         akfin_survey_year >= 1985) %>%
  group_by(akfin_survey_year, district) %>%
  summarise(N_hat = sum(N_hat)/1000000) %>% ungroup %>%
  add_row(akfin_survey_year = 2020, district = c("Bristol Bay", "Northern Unstratified")) %>%
  pivot_wider(names_from = district, values_from = N_hat) %>%
  mutate(`Proportion of North (North)/(North+BB)` = `Northern Unstratified` / (`Bristol Bay` + `Northern Unstratified`) * 100) %>%
  rename(`BB biomass` = `Bristol Bay`,
         `North biomass` = `Northern Unstratified`) %>%
  pivot_longer(2:4, names_to = "stat", values_to = "value") %>%
  
  ggplot()+
  geom_point(aes(x = akfin_survey_year, y = value, color = stat, shape = stat))+
  geom_line(aes(x = akfin_survey_year, y =value, color = stat, group = stat))+
  scale_y_continuous(sec.axis = sec_axis(trans = ~./100, name = "Proportion"), labels = scales::comma)+
  scale_x_continuous(labels = yr_axis$labels, breaks = yr_axis$breaks)+
  scale_color_manual(values = c("red", "blue", "black"))+
  scale_shape_manual(values = c(15, 8, 16))+
  labs(x = NULL, y = "Female abundance <90mm (millions of crab)", color = NULL, shape = NULL)+
  theme(legend.justification = c(0,1),
        legend.position = c(0,1.00))-> x
ggsave("./BBRKC/bbrkc_22f/figures/FEMALES_figure_35a.png", plot = x, height= 3, width = 5, units = "in")

### figure 35b
est_1mm %>%
  filter(length_1mm >= 90,
         akfin_survey_year >= 1985) %>%
  group_by(akfin_survey_year, district) %>%
  summarise(N_hat = sum(N_hat)/1000000) %>% ungroup %>%
  add_row(akfin_survey_year = 2020, district = c("Bristol Bay", "Northern Unstratified")) %>%
  pivot_wider(names_from = district, values_from = N_hat) %>%
  mutate(`Proportion of North (North)/(North+BB)` = `Northern Unstratified` / (`Bristol Bay` + `Northern Unstratified`)*100) %>%
  rename(`BB biomass` = `Bristol Bay`,
         `North biomass` = `Northern Unstratified`) %>%
  pivot_longer(2:4, names_to = "stat", values_to = "value") %>%
  
  ggplot()+
  geom_point(aes(x = akfin_survey_year, y = value, color = stat, shape = stat))+
  geom_line(aes(x = akfin_survey_year, y =value, color = stat, group = stat))+
  scale_y_continuous(sec.axis = sec_axis(trans = ~./100, name = "Proportion"), labels = scales::comma)+
  scale_x_continuous(labels = yr_axis$labels, breaks = yr_axis$breaks)+
  scale_color_manual(values = c("red", "blue", "black"))+
  scale_shape_manual(values = c(15, 8, 16))+
  labs(x = NULL, y = "Mature female abundance (millions of crab)", color = NULL, shape = NULL)+
  theme(legend.justification = c(0,1),
        legend.position = c(0,1.025))  -> x
ggsave("./BBRKC/bbrkc_22f/figures/FEMALES_figure_35b.png", plot = x, height= 3, width = 5, units = "in")
