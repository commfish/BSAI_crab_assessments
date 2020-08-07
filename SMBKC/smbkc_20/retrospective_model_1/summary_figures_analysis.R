# katie.palof@alaska.gov
# 8-6-2020

# Objective: Figures and summary of 2020 retrospective analysis, normal and with terminal survey year dropped.


# load --
source("./SMBKC/code/helper.R")
.FIGS = c(paste0("./SMBKC/smbkc_20/retrospective_model_1/figures/"))

# data ---
mmb <- read.csv(paste0(here::here(), '/SMBKC/smbkc_20/retrospective_model_1/combined_data/ssb_2019.csv'))
sum_stats <- read.csv(paste0(here::here(), '/SMBKC/smbkc_20/retrospective_model_1/combined_data/summary.csv'))


# retro normal ---------
mmb %>% 
  ggplot(aes(year, ssb, group = Model)) +
    geom_line(aes(group = Model, colour = Model), lwd = 0.75) +
    theme_bw(base_size = 12, base_family = "") -> p1

ggsave(paste0(.FIGS, "ssb_time_series.png"), p1, dpi = 800, width = 1.5*6, height = 5)

#**FIX** need to add 2020 model here. 

# comparison btn retro and leave out terminal yr survey -----------------