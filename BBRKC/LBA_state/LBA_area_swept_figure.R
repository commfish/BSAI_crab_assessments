# k.palof
# Figure to compare area-swept from survey with model output from LBA model

# created 7-13-22/ 8-18-23

#READ ME:
# data is from fortran program
# survey data is constant from year to year and can be found in survey.dat file or "LBA file_breakdown_data_and_output_2023.xlsx"
# model output in found in "output23_vX" (model output) and rkXXfreq_vX" (bootstrap results)

# 2023 notes - see README in this folder
# 7-11-23 prelim runs - have # individuals and raw size comps
# males run once - no removals included
# females run a couple times -
      # v1/v2 are similar using raw survey data and NO manupulation of recruitment in initial year
      # V3 uses average (last 3 years) size comps instead of survey raw
      # v4 has manipulation of recruitment initial estimate towards the end of convergence
# versions 3 and 4 are plotted here

# load ----
library(tidyverse)
loadfonts(device="win")
windowsFonts(Times=windowsFont("TT Times New Roman"))

theme_set(theme_bw(base_size=12,base_family='Times New Roman')+ 
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank()))

## setup -------
folder <- "rk23"

# data -----
# read in area swept from lba data file "survey.dat" and surveyf.dat

#out <- read.table("./BBRKC/LBA/rk22/pop.out", header = FALSE, sep = "", nrows = 28)
                  #col.names = c(1972:2022))
#years <- as.character(c(1972:2022))
#out[nrow(out)+1, ] <- years

lba_out <- read.csv(paste0(here::here(), "/BBRKC/LBA_state/", folder, "/output23_v2.csv"))
#lba_out3 <- read.csv("C:/Users/kjpalof/Documents/BSAI_crab_assessments/BBRKC/LBA_state/rk23_prelim/rk23_avgF_SC.csv")
#lba_out4 <- read.csv("C:/Users/kjpalof/Documents/BSAI_crab_assessments/BBRKC/LBA_state/rk23_prelim/rk23_rawSC_manR.csv")
#lba_out5 <- read.csv("C:/Users/kjpalof/Documents/BSAI_crab_assessments/BBRKC/LBA_state/rk23_prelim/rk23_wo_large_tow.csv")

head(lba_out)
#lba_out <- read.csv("./BBRKC/LBA_state/rk22/rk22_r_input_2022edit.csv")

## male figure ------------
#v2
lba_out %>% 
  select(Year, survey.m, model.mm, matm_lower, matm_upper) %>% 
  gather(type, number, survey.m:model.mm) %>% 
  ggplot(aes(Year, number, group = type)) +
    geom_point(aes(shape = type), size = 3) +
    geom_line(aes(group = type, linetype = type), lwd = 1) +
  scale_shape_manual(name = "", values = c(32,19), 
                     labels = c("Model", "Survey")) + 
  scale_linetype_manual(name = "", values = c("solid", "blank"), 
                        labels = c("Model", "Survey")) +
  scale_x_continuous(breaks = seq(min(1972),max(max(lba_out$Year) + 1), by = 2)) +
  geom_ribbon(aes(x=Year, ymax = matm_upper, ymin = matm_lower), alpha = 0.2) +
  ggtitle("Mature males") + 
  ylab("Millions of crab") +
  xlab("Year") +
  theme(legend.position = c(0.8,0.8), 
        axis.text = element_text(size = 12), 
        axis.title=element_text(size=14,face="bold"), 
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.title = element_text(hjust =0.5)) -> males 
ggsave(paste0('C:/Users/kjpalof/Documents/BSAI_crab_assessments/BBRKC/LBA_state/', folder, '/mature_males_v2.png'), males, dpi = 800, width = 7.5, height = 5.5)
  

# female figure ----------
#v3
lba_out %>% 
  select(Year, survey.f, model.mf) %>% 
  gather(type, number, survey.f:model.mf) %>% 
  ggplot(aes(Year, number, group = type)) +
  geom_point(aes(shape = type), size = 3) +
  geom_line(aes(group = type, linetype = type), lwd = 1) +
  scale_shape_manual(name = "", values = c(32,19), 
                     labels = c("Model", "Survey")) + 
  scale_linetype_manual(name = "", values = c("solid", "blank"), 
                        labels = c("Model", "Survey")) +
  scale_x_continuous(breaks = seq(min(1972),max(max(lba_out$Year) + 1), by = 2)) +
  #geom_ribbon(aes(x=Year, ymax = matf_upper, ymin = matf_lower), alpha = 0.2) +
  ggtitle("Mature females") + 
  ylab("Millions of crab") +
  xlab("Year") +
  theme(legend.position = c(0.8,0.8), 
        axis.text = element_text(size = 12), 
        axis.title=element_text(size=14,face="bold"), 
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.title = element_text(hjust =0.5)) -> females 
  ggsave(paste0('C:/Users/kjpalof/Documents/BSAI_crab_assessments/BBRKC/LBA_state/', folder,'/mature_females_v2.png'), females, dpi = 800, width = 7.5, height = 5.5)

  
# female figure  with ribbons----------
#v2 -----
# recalc a confidence band on 2022---------
lba_out %>% 
    select(Year, model.mf, matf_lower, matf_upper) %>% 
    mutate(model.mf_t = model.mf, 
           upper_dif = matf_upper-model.mf_t, 
           lower_dif = matf_lower-model.mf_t) %>% 
    filter(Year >=2018 & Year <=2021) %>% 
    summarise(upper_dif_avg = mean(upper_dif), 
              lower_dif_avg = mean(lower_dif))

#upper_dif_avg lower_dif_avg
#  1       0.78221      -0.82454
# 2023 results for 2022
  #upper_dif_avg lower_dif_avg
#  1      0.70725        -0.998

lba_out %>% 
  filter(Year == 2022) %>% 
  select(Year, model.mm, model.mf, survey.m, survey.f, matm_lower, matm_upper, survey.f.CI) %>% 
  mutate(matf_lower = model.mf-(0.99), 
         matf_upper = model.mf+(0.71)) -> lba_out22

lba_out %>% 
  filter(Year != 2022) %>% 
  rbind(lba_out22) -> lba_out2_a
#write.csv(lba_out22a, "./BBRKC/LBA_state/rk22/rk22_r_input_2022edit.csv")

#lba_out2_a %>% 
#  mutate(survey.f_upper = survey.f + ((survey.f*survey.f.CV)*1.96),
#  survey.f_lower = survey.f - ((survey.f*survey.f.CV)*1.96))
  
#Figure with ribbons -------
lba_out2_a %>% 
    mutate(survey.f_upper = (survey.f + survey.f.CI), survey.f_lower = (survey.f - survey.f.CI)) %>% 
    select(Year, survey.f, model.mf, matf_lower, matf_upper, survey.f_upper, survey.f_lower) %>% 
    gather(type, number, survey.f:model.mf) %>% 
  filter(Year >= 2015) %>% 
   ggplot(aes(Year, number, group = type)) +
    geom_point(aes(shape = type), size = 3) +
    geom_line(aes(group = type, linetype = type), lwd = 1) +
    scale_shape_manual(name = "", values = c(32,19), 
                       labels = c("Model", "Survey")) + 
    scale_linetype_manual(name = "", values = c("solid", "blank"), 
                          labels = c("Model", "Survey")) +
    scale_x_continuous(breaks = seq(min(1972),max(max(lba_out$Year) + 1), by = 2)) +
    geom_ribbon(aes(x=Year, ymax = matf_upper, ymin = matf_lower), alpha = 0.2) +
    geom_errorbar(aes(x=Year, ymax = survey.f_upper, ymin = survey.f_lower), width = 0.3) +
    geom_hline(yintercept = 8.4, color = "red") +
    ggtitle("Mature females") + 
    ylab("Millions of crab") +
    xlab("Year") +
    theme(legend.position = c(0.8,0.8), 
          axis.text = element_text(size = 12), 
          axis.title=element_text(size=14,face="bold"), 
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(plot.title = element_text(hjust =0.5)) -> females 
  #ggsave(paste0('C:/Users/kjpalof/Documents/BSAI_crab_assessments/BBRKC/LBA_state/', folder,'/mature_females_ribbons_v2_error.png'), females, dpi = 800, width = 7.5, height = 5.5)
  ggsave(paste0('C:/Users/kjpalof/Documents/BSAI_crab_assessments/BBRKC/LBA_state/', folder,'/mature_females_recent_ribbons_v2_error.png'), females, dpi = 800, width = 7.5, height = 5.5)
  # to use "recent ribbons" just comment in the filter year line above
  

  
# recent figures done above by commented out lines -----------
### can run below or just un-comment above line and resave as "recent"
lba_out22a %>% 
  select(Year, survey.f, model.mf, matf_lower, matf_upper) %>% 
  gather(type, number, survey.f:model.mf) %>% 
  filter(Year >= 2015) %>% 
  ggplot(aes(Year, number/1000, group = type)) +
  geom_point(aes(shape = type), size = 3) +
  geom_line(aes(group = type, linetype = type), lwd = 1) +
  scale_shape_manual(name = "", values = c(32,19), 
                       labels = c("Model", "Survey")) + 
    scale_linetype_manual(name = "", values = c("solid", "blank"), 
                          labels = c("Model", "Survey")) +
    scale_x_continuous(breaks = seq(min(1972),max(max(lba_out$Year) + 1), by = 2)) +
    geom_ribbon(aes(x=Year, ymax = matf_upper, ymin = matf_lower), alpha = 0.2) +
    geom_hline(yintercept = 8.4, color = "red") +
    ggtitle("Mature females") + 
    ylab("Millions of crab") +
    xlab("Year") +
    theme(legend.position = c(0.8,0.8), 
          axis.text = element_text(size = 12), 
          axis.title=element_text(size=14,face="bold"), 
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(plot.title = element_text(hjust =0.5)) -> females 
  ggsave('./BBRKC/LBA_state/rk22/mature_females_ribbons_recent.png', females, dpi = 800, width = 7.5, height = 5.5)
  