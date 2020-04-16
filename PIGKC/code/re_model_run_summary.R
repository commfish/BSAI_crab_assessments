# notes ----
## Run PIGKC Random Effects Model
## author: Tyler Jackson (model executable file -> Jim Ianelli)
## tyler.jackson@alaska.gov
## last updated: 4/14/2020

# load ----
library(tidyverse)
library(FNGr); theme_set(theme_sleek())

## source mature male biomass estimation if needed
#source("./PIGKC/code/nmfs_slope_biomass_est.R")

## global options
YEAR <- 2020
## version of input data to be run
subdir <- "2020f"

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
### survey ests (sd is cv)
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

### add columns for survey normal 95% CIs
model_est %>%
  mutate(survey_u95 = survey_est + 1.96 * survey_sd * survey_est,
         survey_l95 = survey_est - 1.96 * survey_sd * survey_est) %>%
  dplyr::select(1:3, 11:12, 4:10) -> model_est


## read parameter estimate (process error variance)
### load file
par <- read.table(paste0("./PIGKC/model/", YEAR, "/", subdir, "/re.par"), fill = T, sep = "\t",
           comment.char = "", stringsAsFactors = F)
### print diagnostic stats
t(stringr::str_split(par[1,1], pattern = "  ", simplify = T))[2:3,]
### print process error standard deviation (i.e. parameter estimate)
exp(as.numeric(par[3,]))
### store process errors
proc_err <- na.omit(as.numeric(stringr::str_split(par[5,], pattern = " ", simplify = T)))


# summarize results ----

## biomass plot 
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
  scale_y_continuous(breaks = seq(0, 10000, 250))+
  labs(x = NULL, y = "MMB (t)", title = subdir)+
  theme(plot.title = element_text(hjust = 0.5)) -> x
ggsave(paste0("./PIGKC/figures/", YEAR, "/", subdir, ".png"), plot = x, 
       height = 2, width = 5, units = "in")


