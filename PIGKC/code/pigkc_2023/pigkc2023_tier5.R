#  notes ----
## 2023 pigkc assessment, tier 5
## tyler jackson
## 12/4/2022

# load ---- 

library(tidyverse)
library(patchwork)

# data ----

# directed crab fisheries
dir_tot <- read_csv("../../data/directed_fishery_tot_catch_and_discard.csv")
dir_ret <- bind_rows(read_csv("../../data/retained_catch.csv"), read_csv("../../data/retained_catch_pre_1993.csv"))

# non directed crab fisheries
snow <- read_csv("../../data/snow_crab_fishery_bycatch.csv")
groovy <- read_csv("../../data/grooved_crab_fishery_bycatch.csv")

# groundfish fisheries
source("../gf_bycatch.R")

# data mgmt ----

# average of the estimated annual ratio of bycatch mortality to retained catch in the directed fishery during 2001–2010
dir_tot %>%
  ## correct discards of legal males
  mutate(discard_t = ifelse(discard_t < 0, 0, discard_t)) %>%
  group_by(opening_year) %>%
  summarise(discard_t = sum(discard_t, na.rm = T) * 0.2) %>%
  # join to retained catch
  full_join(dir_ret %>% dplyr::select(opening_year, retained_t), by = "opening_year") %>%
  # compute avg ratio from 2001 - 2010
  filter(opening_year %in% 2001:2010) -> R_tab
summarise(R_tab, R = mean(discard_t / retained_t, na.rm = T)) %>% pull %>% round(., 3) -> R

# average annual retained catch in the directed crab fishery during 1993–1998
dir_ret %>%
  filter(opening_year %in% 1993:1998) -> RET_tab
summarise(RET_tab, RET = mean(retained_t, na.rm = T)) %>% pull %>% round(., 2) -> RET

# estimated average annual bycatch mortality in non-directed crab fisheries during 1994–1998
## compute total mortality
snow %>%
  group_by(opening_year) %>%
  summarise(qo = sum(bycatch_t, na.rm = T)) %>%
  full_join(groovy %>%
              group_by(opening_year) %>%
              summarise(qg = sum(bycatch_t, na.rm = T))) %>%
  replace_na(list(qo = 0, qg = 0)) %>%
  mutate(mort = (qo + qg) * 0.5) %>%
  ## compute avg in time period
  filter(opening_year %in% 1994:1998) -> BMNC_tab
summarise(BMNC_tab, BMNC = mean(mort, na.rm = T)) %>% pull %>% round(., 2) -> BMNC

# the estimated average annual bycatch mortality in groundfish fisheries during 1992/93–1998/99.
gf_bycatch %>%
  filter(as.numeric(substring(year, 1, 4)) %in% 1992:1998) -> BMGF_tab
summarise(BMGF_tab, BMGF = mean(total_mortality, na.rm = T)) %>% pull %>% round(., 2) -> BMGF

# table of values ----
tibble(year = 1993:2010) %>%
  # RET
  left_join(RET_tab %>%
              transmute(year = opening_year,
                        RET = retained_t)) %>%
  # R
  left_join(R_tab %>%
              transmute(year = opening_year,
                        R = discard_t / retained_t)) %>%
  # BMNC
  left_join(BMNC_tab %>%
              transmute(year = opening_year,
                        BMNC = mort)) %>%
  # BMGF
  left_join(BMGF_tab %>%
              rename(season = year) %>%
              transmute(year = as.numeric(substring(season, 6, 9)),
                        season = season,
                        BMGF = total_mortality)) %>%
  mutate(season = paste0(year, "/", substring(year+1, 3, 4))) %>%
  dplyr::select(year, season, RET, R, BMNC, BMGF) -> tier5_input
  
# summary below main table
tibble(year = NA, 
       season = c("N", "Mean", "SE", "CV"),
       RET = c(nrow(RET_tab), RET, sqrt(var(tier5_input$RET, na.rm = T) / nrow(RET_tab)), sqrt(var(tier5_input$RET, na.rm = T) / nrow(RET_tab)) / RET),
       R = c(sum(!is.na(R_tab$discard_t)), R, sqrt(var(tier5_input$R, na.rm = T) / nrow(R_tab)), sqrt(var(tier5_input$R, na.rm = T) / nrow(R_tab)) / R),
       BMNC = c(nrow(BMNC_tab), BMNC, sqrt(var(tier5_input$BMNC, na.rm = T) / nrow(BMNC_tab)), sqrt(var(tier5_input$BMNC, na.rm = T) / nrow(BMNC_tab)) / BMNC),
       BMGF = c(nrow(BMGF_tab), BMGF, sqrt(var(tier5_input$BMGF, na.rm = T) / nrow(BMGF_tab)), sqrt(var(tier5_input$BMGF, na.rm = T) / nrow(BMGF_tab)) / BMGF))  -> tier5_input_sum


# compute ofl and abc----

## OFL
OFL <- (1 + R) * RET + BMNC + BMGF

## ABC, 25% buffer
ABC <- (1 - 0.25) * OFL

# bootstrap ofl calculation ----


# do bootstrap sample of ofl calculation
set.seed(1624)
bootOFL <- NULL
for(i in 1:1000) {
  # RET
  RETboot <- mean(sample(RET_tab$retained_t, 6, replace = T))
  # R
  Rboot <- mean(sample(R_tab[!is.na(R_tab$discard_t),]$discard_t / R_tab[!is.na(R_tab$discard_t),]$retained_t, 6, replace = T))
  # BMNC
  BMNCboot <- mean(sample(BMNC_tab$mort, 5, replace = T))
  # BMGF
  BMGFboot <- mean(sample(BMGF_tab$total_mortality, 7, replace = T))
  
  bootOFL[i] <- (1 + Rboot) * RETboot + BMNCboot + BMGFboot
  
}

boot_mean <- mean(bootOFL)
boot_cv <- sd(bootOFL) / mean(bootOFL)

## save plot
ggplot()+
  geom_histogram(aes(x = bootOFL), color = "black", fill = "grey70")+
  labs(y = "Frequency", x = "Alt. 1 OFL (t)")+
  theme_classic() -> hist

tibble(ofl = sort(bootOFL),
       cdf = 1:1000 / 1000) %>%
  ggplot()+
  geom_line(aes(x = ofl, y = cdf))+
  labs(y = "Fraction of Data", x = "Alt. 1 OFL (t)")+
  theme_classic() -> cdf

# sensitivity to different mortality rates ----

## double the mortality
# average of the estimated annual ratio of bycatch mortality to retained catch in the directed fishery during 2001–2010
dir_tot %>%
  ## correct discards of legal males
  mutate(discard_t = ifelse(discard_t < 0, 0, discard_t)) %>%
  group_by(opening_year) %>%
  summarise(discard_t = sum(discard_t, na.rm = T) * 0.4) %>%
  # join to retained catch
  full_join(dir_ret %>% dplyr::select(opening_year, retained_t), by = "opening_year") %>%
  # compute avg ratio from 2001 - 2010
  filter(opening_year %in% 2001:2010) %>%
  summarise(R = mean(discard_t / retained_t, na.rm = T)) %>% pull -> Rd

# estimated average annual bycatch mortality in non-directed crab fisheries during 1994–1998
## compute total mortality
snow %>%
  group_by(opening_year) %>%
  summarise(qo = sum(bycatch_t, na.rm = T)) %>%
  full_join(groovy %>%
              group_by(opening_year) %>%
              summarise(qg = sum(bycatch_t, na.rm = T))) %>%
  replace_na(list(qo = 0, qg = 0)) %>%
  mutate(mort = (qo + qg) * 1) %>%
  ## compute avg in time period
  filter(opening_year %in% 1994:1998) %>%
  summarise(BMNC = mean(mort, na.rm = T)) %>% pull -> BMNCd

# the estimated average annual bycatch mortality in groundfish fisheries during 1992/93–1998/99.
gf_bycatch %>%
  filter(as.numeric(substring(year, 1, 4)) %in% 1992:1998) %>%
  mutate(total_mortality = fixed * 1 + trawl * 1) %>%
  summarise(BMGF = mean(total_mortality, na.rm = T)) %>% pull -> BMGFd

double_mort_factor <- ((1 + Rd) * RET + BMNCd + BMGFd) / OFL


## halve the mortality
# average of the estimated annual ratio of bycatch mortality to retained catch in the directed fishery during 2001–2010
dir_tot %>%
  ## correct discards of legal males
  mutate(discard_t = ifelse(discard_t < 0, 0, discard_t)) %>%
  group_by(opening_year) %>%
  summarise(discard_t = sum(discard_t, na.rm = T) * 0.1) %>%
  # join to retained catch
  full_join(dir_ret %>% dplyr::select(opening_year, retained_t), by = "opening_year") %>%
  # compute avg ratio from 2001 - 2010
  filter(opening_year %in% 2001:2010) %>%
  summarise(R = mean(discard_t / retained_t, na.rm = T)) %>% pull -> Rh

# estimated average annual bycatch mortality in non-directed crab fisheries during 1994–1998
## compute total mortality
snow %>%
  group_by(opening_year) %>%
  summarise(qo = sum(bycatch_t, na.rm = T)) %>%
  full_join(groovy %>%
              group_by(opening_year) %>%
              summarise(qg = sum(bycatch_t, na.rm = T))) %>%
  replace_na(list(qo = 0, qg = 0)) %>%
  mutate(mort = (qo + qg) * 0.25) %>%
  ## compute avg in time period
  filter(opening_year %in% 1994:1998) %>%
  summarise(BMNC = mean(mort, na.rm = T)) %>% pull -> BMNCh

# the estimated average annual bycatch mortality in groundfish fisheries during 1992/93–1998/99.
gf_bycatch %>%
  filter(as.numeric(substring(year, 1, 4)) %in% 1992:1998) %>%
  mutate(total_mortality = fixed * 0.25 + trawl * 0.4) %>%
  summarise(BMGF = mean(total_mortality, na.rm = T)) %>% pull -> BMGFh

halve_mort_factor <- ((1 + Rh) * RET + BMNCh + BMGFh) / OFL
