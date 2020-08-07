# katie.palof@alaska.gov
# 8-6-2020

# Objective: Figures and summary of 2020 retrospective analysis, normal and with terminal survey year dropped.


# load --
source("./SMBKC/code/helper.R")

# data ---
mmb <- read.csv(paste0(here::here(), '/SMBKC/smbkc_20/retrospective_model_1/combined_data/ssb_2019.csv'))
sum_stats <- read.csv(paste0(here::here(), '/SMBKC/smbkc_20/retrospective_model_1/combined_data/summary.csv'))


# retro normal ---------
mmb %>% 
  ggplot(aes(year, ssb, group = Model)) +
    geom_line(aes(group = Model, colour = Model), lwd = 0.75) +
    theme_bw(base_size = 12, base_family = "")
