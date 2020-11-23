# notes ----
## create survery random effects .dat file
## Tyler Jackson
## tyler.jackson@alaska.gov

# load ----
library(tidyverse)

YEAR = 2021

# data ----
survey_data <- read_csv("./PIGKC/output/survey_mmb_timeseries_surveyarea_pi_district.csv")


## export random effects model input data file
### extract data
#### model years
survey_data %>%
  pull(survey_year) %>%
  sort() -> yrs
#### starting year
start <- min(yrs)
#### ending year
end <- 2022 
#### number of estimates
n <- nrow(survey_data)
#### biomass estimates (in metric tons)
survey_data %>%
  pull(biomass) / 1000 -> biomass
#### cv of biomass estimates
survey_data %>%
  pull(cv_biomass) -> cv

### compile input file
rbind(c(start, "#Start year of model", rep("", n - 2)),
      c(end, "#End year of model", rep("", n - 2)),
      c(n, "#number of survey estimates", rep("", n - 2)),
      c("#Years of survey", rep("", n - 1)),
      c(yrs),
      c("#biomass estimates mature males", rep("", n - 1)),
      c(round(biomass, 2)),
      c("#Coefficients of variation for biomass estimates", rep("", n - 1)),
      c(round(cv, 2))) %>%
  write.table(., paste0("./PIGKC/model/", YEAR,"/20f/re.dat"), 
              quote = F, row.names = F, col.names = F)
