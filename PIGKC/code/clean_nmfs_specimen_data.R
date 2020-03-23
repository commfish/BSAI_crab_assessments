# notes ----

## Clean NMFS Slope Survey GKC Specimen/Catch data for biomass estimate
## author: Tyler Jackson
## tyler.jackson@alaska.gov
## last updated: 2020/3/4

## object naming convention: abbrev_yyYY; yy - first year, YY - last year

# load ----

library(tidyverse)

# data ----

## raw data source: Jerry Hoff (AFSC), Ben Daly (ADF&G)
### 2004
spec_04 <- read_csv("./PIGKC/data/nmfs_slope_gkc_specimen_2004.csv")
### 2008 - 2012
spec_08 <- read_csv("./PIGKC/data/nmfs_slope_gkc_specimen_2008.csv")
spec_10 <- read_csv("./PIGKC/data/nmfs_slope_gkc_specimen_2010.csv")
spec_12 <- read_csv("./PIGKC/data/nmfs_slope_gkc_specimen_2012.csv")
### 2016 
spec_16 <- read_csv("./PIGKC/data/nmfs_slope_gkc_specimen_2016.csv")

# clean and join ----

## 2004
spec_04  %>%
  # remove area-swept, stratum, and subarea
  dplyr::select(-AREA_SWEPT_SQKM, -Stratum, -subarea) %>%
  # add survey_year, sex, add sample_factor
  mutate(survey_year = 2004,
         sex = case_when(Species == 11 ~ 1,
                         Species == 12 ~ 2),
         sampling_factor = 1) %>%
  # remove Species
  dplyr::select(survey_year, Haul, sex, Length, sampling_factor) %>%
  rename(haul = "Haul",
         length = "Length") -> spec_04


## 2008 - 2010
bind_rows(spec_08, spec_10, spec_12) %>%
  # select columns of interest
  dplyr::select(CRUISE, HAUL, SEX, LENGTH, CLUTCH_SIZE, WEIGHT, SAMPLING_FACTOR) %>%
  # coerce cruise to be year
  mutate(CRUISE = as.numeric(substring(CRUISE, 1, 4))) -> spec_0812

names(spec_0812) <- c("survey_year", "haul", "sex", "length", "clutch_size", "weight_g",
                      "sampling_factor")


## 2016
spec_16 %>%
  # select columns of interest
  dplyr::select(CRUISE, HAUL, SEX, LENGTH, CLUTCH_SIZE, WEIGHT, SAMPLING_FACTOR) %>%
  # coerce cruise to be year
  mutate(CRUISE = as.numeric(substring(CRUISE, 1, 4))) -> spec_16

names(spec_16) <- c("survey_year", "haul", "sex", "length", "clutch_size", "weight_g",
                    "sampling_factor")


## bind rows of specimen data
bind_rows(spec_04, spec_0812, spec_16) -> spec_0416
