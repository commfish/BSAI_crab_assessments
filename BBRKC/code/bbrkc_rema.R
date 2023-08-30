# notes ----
# REMA for BBRKC MMB 
# tyler jackson
# 5/2/2023 / 8-28-2023

# load ----

# KATIE INSTALLS REMA HERE
# https://afsc-assessments.github.io/rema/
#devtools::install_github("afsc-assessments/rema", dependencies = TRUE, build_vignettes = TRUE)

library(rema)
library(tidyverse)

# graphic options
theme_sleek <- function(base_size = 12, base_family = "Times") {
  
  windowsFonts(Times=windowsFont("TT Times New Roman"))
  
  half_line <- base_size/2
  
  theme_light(base_size = base_size, base_family = base_family) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.ticks.length = unit(half_line / 2.2, "pt"),
      strip.background = element_rect(fill = NA, colour = NA),
      strip.text.x = element_text(colour = "black"),
      strip.text.y = element_text(colour = "black"),
      #axis.text = element_text(colour = "grey30"),
      #axis.title = element_text(colour = "grey30"),
      #legend.title = element_text(colour = "grey30"),#, size = rel(0.9)
      panel.border = element_rect(fill = NA),#, colour = "grey70", size = 1),
      legend.key.size = unit(0.9, "lines"),
      #legend.text = element_text(size = rel(0.7)),#, colour = "grey30"),
      legend.key = element_rect(colour = NA, fill = NA),
      legend.background = element_rect(colour = NA, fill = NA)#,
      #plot.title = element_text(colour = "grey30"),#, size = rel(1)
      #plot.subtitle = element_text(colour = "grey30")#, size = rel(.85)
    )
  
}

# Depends on dplyr
tickr <- function(
    data, # dataframe
    var, # column of interest
    to # break point definition
) {
  
  VAR <- enquo(var) # makes VAR a dynamic variable
  
  data %>%
    dplyr::filter(!is.na(!!VAR)) %>%
    distinct(!!VAR) %>%
    mutate(labels = ifelse(!!VAR %in% seq(to * round(min(!!VAR) / to), max(!!VAR), to),
                           !!VAR, "")) %>%
    dplyr::select(breaks = UQ(VAR), labels)
}
theme_set(theme_sleek())
yraxis <- tickr(tibble(yr = 1975:2100), yr, 5)

# custom function ----

f_plot_rema_fit <- function(fits, confint = T) {
  
  colors = c("#999999", "#E69F00", "#56B4E9", "#009E73", 
             "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  
  # extract models
  nmod = length(fits)
  purrr::map(fits, function(x) {
    tidy_rema(x)$biomass_by_strata %>%
      mutate(model = x$model_name)
  }) -> reps
  
  if(confint == T) {
    do.call("rbind", reps) %>%
      ggplot()+
      geom_ribbon(aes(x = year, ymin = pred_lci, ymax = pred_uci, fill = model_name), alpha = 0.3)+
      geom_line(aes(x = year, y = pred, color = model_name))+
      geom_errorbar(aes(x = year, ymin = obs_lci, ymax = obs_uci), width = 0.2)+
      geom_point(aes(x = year, y = obs), shape = 21, fill = "white")+
      scale_y_continuous(labels = scales::comma)+
      scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
      scale_fill_manual(values = colors[1:nmod])+
      scale_color_manual(values = colors[1:nmod])+
      labs(x = NULL, y = "MMB (t)", fill = NULL, color = NULL)+
      theme(legend.position = c(1,1),
            legend.justification = c(1,1)) -> p
  }
  if(confint == F) {
    do.call("rbind", reps) %>%
      ggplot()+
      geom_line(aes(x = year, y = pred, color = model_name))+
      geom_errorbar(aes(x = year, ymin = obs_lci, ymax = obs_uci), width = 0.2)+
      geom_point(aes(x = year, y = obs), shape = 21, fill = "white")+
      scale_y_continuous(labels = scales::comma)+
      scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
      scale_fill_manual(values = colors[1:nmod])+
      scale_color_manual(values = colors[1:nmod])+
      labs(x = NULL, y = "MMB (t)", color = NULL)+
      theme(legend.position = c(1,1),
            legend.justification = c(1,1)) -> p
  }
  
  return(p)
  
}

# data ----

# specimen data (haul data dump)
#read.csv("./BBRKC/data/2022/survey/rkc_specimen.csv", skip = 5) %>%
read.csv("./BBRKC/data/2023/survey/EBSCrab_Haul.csv", skip = 5) %>%
  rename_all(tolower) -> specimen

# strata file (strata dump)
#read.csv("./BBRKC/data/2022/survey/rkc_strata.csv") %>%
read.csv("./BBRKC/data/2023/survey/EBSCRAB - Strata Report_all.csv") %>%
  rename_all(tolower) %>%
  rename_at(1, ~"station_id") -> strata

# estimate mmb timeseries ----

## list of bristol bay stations
strata %>%
  filter(district == "Bristol Bay") %>%
  distinct(station_id) %>% pull -> bb_stations

## survey area in bristol bay nmi2
bb_area <- length(bb_stations) * 401 #each full station is 401 nmi2

## every station towed in each year
strata %>%
  filter(district == "Bristol Bay") %>%
  distinct(survey_year, station_id, total_area_sq_nm) %>%
  dplyr::rename(akfin_survey_year = survey_year, gis_station = station_id) -> hauls

## estimates
specimen %>%
  # filter for component of the population of interest
  filter(gis_station %in% bb_stations,
         haul_type %in% c(3, 0, 4, 17),  # will likely want to remove haul types 0 and 4 after nmfs corrects their timeseries
         sex == 1,
         length_1mm >= 120) %>%
  # handle retows
  # rejoin to zero catch retows
  right_join(specimen %>%
               distinct(akfin_survey_year, gis_station, haul_type, area_swept)) %>%
  group_by(akfin_survey_year, gis_station) %>%
  nest %>% ungroup %>% 
  mutate(retow = purrr::map(data, function(data) {
    
    # no retow, no action
    if(!(17 %in% data$haul_type)) { out <- data}
    
    # retow, remove female data from original tows
    if(17 %in% data$haul_type) {
      
      data %>%
        filter(!(sex == 1 & haul_type == 17),
               !(is.na(sex) & haul_type == 3),
               !(sex == 2 & haul_type == 3)) -> out
      
    }
    
    return(out)
    
    
  })) %>%
  dplyr::select(-data) %>%
  unnest(retow) %>%
  # compute station cpue by clutch size
  group_by(akfin_survey_year, gis_station, haul_type) %>%
  summarise(wt_crab = sum(sampling_factor * calculated_weight_1mm) / 1e6,
            cpue_wt = wt_crab / mean(area_swept)) %>%
  # join to all hauls, fill in zero catches
  right_join(hauls) %>%
  replace_na(list(wt_crab= 0, cpue_wt = 0)) %>%
  # scale to abundance
  group_by(akfin_survey_year) %>%
  summarise(biomass_t = mean(cpue_wt) * mean(total_area_sq_nm),
            biomass_t_cv = (sqrt((var(cpue_wt) / n()) * mean(total_area_sq_nm)^2)) / biomass_t) -> mmb

# fit rema base model ----

## biomass input for rema
mmb %>%
  transmute(strata = "BBRKC", 
            year = akfin_survey_year,
            biomass = biomass_t,
            cv = biomass_t_cv) %>%
  as.data.frame() -> rema_in

## fit base rema model
prepare_rema_input(model_name = "BBRKC_REMA23.4",
                   start_year = 1975,
                   end_year = 2023,
                   biomass_dat = rema_in) %>%
  fit_rema(do.check = T) -> BBRKC_REMA23.4

# plot and save predicted mmb ----

# write csv
tidy_rema(BBRKC_REMA23.4)$biomass_by_strata %>%
  transmute(year, pred_t = pred, pred_l95 = pred_lci, pred_u95 = pred_uci,
            obs_t = obs, obs_l95 = obs_lci, obs_u95 = obs_uci) %>%
  write.csv("./BBRKC/bbrkc_23f/doc/figures/REMA/rema_fit.csv")

# plot mmb fit
p1 <- f_plot_rema_fit(list(BBRKC_REMA23.4))

ggsave("./BBRKC/bbrkc_23f/doc/figures/REMA/mmbfit.png", plot = p1, height = 4, width = 6, units = "in")

# estimate OFL and ABC using tier 4/5 ----
## mmb timeseries fit ----
tidy_rema(BBRKC_REMA23.4)$biomass_by_strata %>% 
  select(year, pred) -> predicted_mmb

# average B from 1984 to 2021
predicted_mmb %>% 
  filter(year >=1984 & year <= 2022) %>% 
  summarise(averageB = mean(pred)) %>% 
  as.numeric -> avg_B

# extract MMB
tidy_rema(BBRKC_REMA23.4)$biomass_by_strata %>%
  filter(year == max(year)) %>%
  pull(pred) %>% as.numeric -> MMB

# current status 
status <- MMB/avg_B

# natural mortality
M <- 0.18

# Fofl
Fofl <- (M*(status - 0.1))/(1-0.1) # where alpha = 0.1 Tier 4 OFL
  
# OFL - as adjusted by status
#OFL <- MMB * M  # not adjusted by status
OFL <- MMB*Fofl

# ABC 
ABC <- (1 - 0.20) * OFL


specs <- round(c(avg_B, MMB, status, M, Fofl, OFL, ABC), 2)
cnames <- c("avgB", "MMB", "B/Bmsy","M", "Fofl", "OFL", "ABC")
df <- data.frame(cnames, specs) 
df %>% 
  spread(cnames, specs) %>% 
  select(avgB, MMB, `B/Bmsy`, M, Fofl, OFL, ABC)-> df
write.csv(df, "./BBRKC/bbrkc_23f/doc/figures/REMA/specs_REMA.csv", row.names = FALSE)


