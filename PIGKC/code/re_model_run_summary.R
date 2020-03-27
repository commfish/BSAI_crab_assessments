# notes ----
## Run PIGKC Random Effects Model
## author: Tyler Jackson (model executable file -> Jim Ianelli)
## tyler.jackson@alaska.gov
## last updated: 3/23/2020

# load ----
library(tidyverse)
library(FNGr); theme_set(theme_sleek())

## source mature male biomass estimation if needed
#source("./PIGKC/code/nmfs_slope_biomass_est.R")

## global options
YEAR <- 2020
## version of input data to be run
subdir <- "mature_males_subarea_6"

# run model ----

## copy model executable file to annual directory if needed
if(!("re.exe" %in% list.files(paste0("./PIGKC/model/", YEAR, "/", subdir)))){
  file.copy(from = "./PIGKC/model/re.exe",
            to = paste0("./PIGKC/model/", YEAR, "/", subdir, "/re.exe"))
}

## run model
setwd(paste0("./PIGKC/model/", YEAR, "/", subdir))
system("./re.exe")
setwd("../../../..")


# read results ----

## read model output
### load file
output <- read.table(paste0("./PIGKC/model/", YEAR, "/", subdir, "/rwout.rep"), fill = T, sep = "\t")
### survey ests
tibble(yrs = na.omit(as.numeric(str_split(output[2,], pattern = " ", simplify = T))),
       survey_est = na.omit(as.numeric(str_split(output[4,], pattern = " ", simplify = T))),
       survey_sd = na.omit(as.numeric(str_split(output[6,], pattern = " ", simplify = T)))) -> tmp1
### model est
tibble(yrs = na.omit(as.numeric(str_split(output[8,], pattern = " ", simplify = T))),
       fit = na.omit(as.numeric(str_split(output[12,], pattern = " ", simplify = T))),
       l95 = na.omit(as.numeric(str_split(output[10,], pattern = " ", simplify = T))),
       u95 = na.omit(as.numeric(str_split(output[14,], pattern = " ", simplify = T))),
       l90 = na.omit(as.numeric(str_split(output[16,], pattern = " ", simplify = T))),
       u90 = na.omit(as.numeric(str_split(output[18,], pattern = " ", simplify = T))),
       fit_sd = na.omit(as.numeric(str_split(output[20,], pattern = " ", simplify = T))),
       fit_sd_sd = na.omit(as.numeric(str_split(output[22,], pattern = " ", simplify = T)))) -> tmp2
### join temporary objects results
full_join(tmp1, tmp2, by = "yrs") %>%
  arrange(yrs) -> model_est
### clear temporary objects 
rm(tmp1)
rm(tmp2)

### add columns for survey CIs
model_est %>%
  mutate(survey_u95 = survey_est * exp(2 * sqrt(log(1 + ((survey_est * survey_sd) / survey_est)^2))),
         survey_l95 = survey_est / exp(2 * sqrt(log(1 + ((survey_est * survey_sd) / survey_est)^2)))) %>%
  dplyr::select(1:3, 11:12, 4:10) -> model_est

# summarize results ----

## biomass plot (MMB, Subareas 2-4)
model_est %>%
  ggplot()+
  geom_ribbon(aes(x = yrs, ymin = l95, ymax = u95), fill = "grey80")+
  geom_line(aes(x = yrs, y = u95), linetype = 2)+
  geom_line(aes(x = yrs, y = l95), linetype = 2)+
  geom_line(aes(x = yrs, y = fit), size = 1)+
  geom_errorbar(aes(x = yrs, ymax = survey_u95, ymin = survey_l95), width = 0.3)+
  geom_point(aes(x = yrs, y = survey_est), shape = 22, fill = "white")+
  scale_x_continuous(breaks = tickr(model_est, yrs, 2)$breaks, 
                     labels = tickr(model_est, yrs, 2)$labels)+
  labs(x = NULL, y = "MMB (t)", title = "Subareas 2 - 4")+
  theme(plot.title = element_text(hjust = 0.5)) -> x
ggsave(paste0("./PIGKC/figures/", YEAR, "/mmb_subareas_2_4_fit.png"), plot = x, 
       height = 3, width = 6, units = "in")

## biomass plot (MMB, all subareas)
model_est %>%
  ggplot()+
  geom_ribbon(aes(x = yrs, ymin = l95, ymax = u95), fill = "grey80")+
  geom_line(aes(x = yrs, y = u95), linetype = 2)+
  geom_line(aes(x = yrs, y = l95), linetype = 2)+
  geom_line(aes(x = yrs, y = fit), size = 1)+
  geom_errorbar(aes(x = yrs, ymax = survey_u95, ymin = survey_l95), width = 0.3)+
  geom_point(aes(x = yrs, y = survey_est), shape = 22, fill = "white")+
  scale_x_continuous(breaks = tickr(model_est, yrs, 2)$breaks, 
                     labels = tickr(model_est, yrs, 2)$labels)+
  labs(x = NULL, y = "MMB (t)", title = "All Subareas")+
  theme(plot.title = element_text(hjust = 0.5)) -> x
ggsave(paste0("./PIGKC/figures/", YEAR, "/mmb_all_subareas_fit.png"), plot = x, 
       height = 3, width = 6, units = "in")

## biomass plot (MMB, subarea #)
model_est %>%
  ggplot()+
  geom_ribbon(aes(x = yrs, ymin = l95, ymax = u95), fill = "grey80")+
  geom_line(aes(x = yrs, y = u95), linetype = 2)+
  geom_line(aes(x = yrs, y = l95), linetype = 2)+
  geom_line(aes(x = yrs, y = fit), size = 1)+
  geom_errorbar(aes(x = yrs, ymax = survey_u95, ymin = survey_l95), width = 0.3)+
  geom_point(aes(x = yrs, y = survey_est), shape = 22, fill = "white")+
  scale_x_continuous(breaks = tickr(model_est, yrs, 2)$breaks, 
                     labels = tickr(model_est, yrs, 2)$labels)+
  labs(x = NULL, y = "MMB (t)", title = "Subarea 6")+
  theme(plot.title = element_text(hjust = 0.5)) -> x
ggsave(paste0("./PIGKC/figures/", YEAR, "/mmb_subarea_6_fit.png"), plot = x, 
       height = 3, width = 6, units = "in")




