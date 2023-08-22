# k.palof
# 8-16-22 / 8-21-23
# Figures and Tables needed for Sept 2022 BBRKC SAFE 
## Using Jie's notes and code - other file - 'figures_tables_create_SAFE.R - attempts to use gmr and SMBKC code

## combination of my and Jie's R scripts. 

# load -------
# done again when updates are made to the package or intially first time
#require(devtools)
#devtools::install_github("seacode/gmacs", subdir = "/gmr", ref = "develop") - only needs to be performed once.
# alternate gmr instructions:::
# if gmr is updated and above doesn't work try:
# go to Build above (menu option in R studio) - "configure build tools" - direct it (package directory option) 
#   to the gmr folder - press OK. 
# Over on right hand size in the tabs (between connections and git should be build) in the Build tab (upper right hand side) - 
# click "install and restart"
library(gmr) #require(gmr)
source("./SMBKC/code/functions.R") 
source("./SMBKC/code/helper.R") 
source("./SMBKC/code/packages.R")
source("./SMBKC/code/gmr_functions2020.R") 
source("./BBRKC/bbrkc_22f/Jie R code/read_rep.R")
.THEME    = list(theme_bw(base_size = 12, base_family = ""), scale_fill_manual(values=cbPalette), 
                 scale_colour_manual(values=cbPalette))
.FIGS     = c("./BBRKC/bbrkc_23f/doc/figures/")

## Jie's code ------
A <- read_rep("./BBRKC/bbrkc_23s/model_211b/gmacs.rep")
B <- read_rep("./BBRKC/bbrkc_23f/model_211b/gmacs.rep")
C <- read_rep("./BBRKC/bbrkc_23f/model_22/gmacs.rep")
D <- read_rep("./BBRKC/bbrkc_23f/model_23_0a/gmacs.rep")
# these need to be run before any code in 'Jie_cmn_files.R' is run

## FIGURES ----------------
# run code in data-range.R - with B as the letter - got 'data_range.png"
### !!Figure 1 ---- static in doc
### !!Figure 2 -----------
# data range
# run code in data-range.R - with B as the letter - got 'data_range.png"
### !!Figure 3 ------ 
#  see "RKC_SAFE21_kjp.xlsx' - catchbio tab **FIX update and put into R **
### !!Figure 4 
#  see "RKC_SAFE21_kjp.xlsx' - cpue tab **FIX update and put into R **
### !!Figure 5 ----------
# Survey abundance size comps, get Cody's code for these, size_bins_com_Kodiak_m or f_5mm.png
### !!Figure 6 --------
# CUT no VAST
### !!Figure 7 - static NO update
# !!Figure 8 -------
# selectivities - trawl selectivity - where are these? see 'Jie_cmn_files.R',sel-nmfs-one.cmn, sel-nmfs-one6.cmn, sel-bsfrf.cmn, sel-directedpot.cmn.
# !!Fiure 9 ------
# molting probability - see 'bbrkc_22f/Jie R code/Jie_cmn_files.R', Jie's script molt-prob.cmn, molt-prob2.cmn
# !!Figure 10 ------
# area swept trawl survey, BSFRF fit, created with gmr - see figures folder
# !!Figure 10c BSFRF selectivities, sel-bsfrf.cmn
## !!Figure 10e -----
# length comps for BSFRF, used gmr
# !!Figure 11 -----
#  Estimated MMB - from gmr output - "mod_scen_ssb....png'
# !!Figure 12a -------
# recruitment - Jie's code 'rec3.cmn'
# !!Figures 12b -----
# recruitment length distributions 'r-range.cmn'
# !!Figure 13 a, b, c -----
# Fishing mortality comparison by year - Jie's code 'rkf.cmn, rkf85.cmn'
### !!Figure 13d ------
# Natural mortality and directed pot fishing mortality , mortality.cmn
# !!Figure 14 -------
# recruit vs. mmb Jie's code 'rksr.cmn, rksrp.cmn'
# **Figure 15 -----
# clutchfullness and empty clutches ??? maybe see if Tyler can help with this?
# !! Figure 16a,b------
## catch predicted - gmr not working due to type =0 issues. Jie's code 'c-direct.cmn, c-discard.cmn'
### !!Figure 17 -----
## trawl biomass mod scen residuals.png - from gmr
# ** Figure 18 ------
# length comps - NMFS males
# ** FIgure 19-----
# length comps - NMFS females
# ** Figure 20  ------
# retained length frequencies
# ** Figure 21 -----
# total length frequencies - pot fishery
# ** Figure 22 -----
# female length frequencies - pot fishery
# ** Figure 23 a, b -----------
# groundfish trawl length frequencies - males and females
# ** Figure 24 a, b -----------
# groundfish FIXED gear length frequencies - males and females
# ** Figure 24c ------
# tanner crab length frequencies - males and females
# !!Figure 25 a, b -------------
# length frequencies residuals for model runs, males NMFS
# see bubbleplot-m.r (FIX move to master file for functions and figure calls eventually)
# !!Figure 26 a, b -------------
# length frequencies residuals for model runs, females NMFS 
#  see figure 25 
# Figure 27a, b
# retrospective of MMB --- look for my code for this????????
# Figure 28 a, b, c --------
# retrospective for recruitment, retrospective errors as function of recruitment, mean ratios of retrospective estimates
# Figure 29 ------
# base model old models NOT retro? code for this??? data for this?
# Figures 30 a, b, c, d, e, f, 
# MCMC output - see figures for which 
# hist-mmb.cmn, probability.cmn, proj-mmb.cmn, 

# Figures 31  to 33 ------
# projections of status for approaching overfishing 
# !!Figure 34 -----
# NMFS length frequncies for last 5 years of survey data - males and females
# size_bins_comp_Kodiak_f_5mm_LAST5.png
# size_bins_comp_Kodiak_m_5mm_LAST5.png
# Figure 35 -----
# comparison of BB vs red crab north of BB --- where is this????


## ssb ----

### TABLES ----------------
# Table 1 - see Excel file "RKC_SAFE22_kjp."
# Table 3a - look at Tyler's observer summary files and survey data/groundfish sample sizes
# Table 4 ------
# effective sample sizes and harmonic means, see im-ess.cmn
# now in Jie_cmn_files.R

# Table 7 ----
# see figures_tables_create_SAFE.R
# Table 8 & 9 ------
# gmacs-sum_model211b.csv - see Jie_cmn_files.R
# B$N_males - see which sizes classes to assign to legal, mature etc.
# B$N_females (>90mm mature)
# B$recruits
# effective spawning biomass (lba.cmn)
  # number of females * selectivity * Q * 

# model 21.1b mature females --------------
f_temp <- B$N_females_mature
head(f_temp)

f_temp %>% 
  as.data.frame() %>% 
  mutate(total_mature = rowSums(across(where(is.numeric)))/1000000, # this doesn't match what Jie has for mature females....
         totalGE90 = (V6+ V7 + V8 + V9 + V10 + V11 +V12 +V13 +V14 +V15 +V16)/1000000) # this doesn't match what Jie has for mature females....
  
# this is number greater than 90mm CL which is Jie's currency
f_temp2 <- B$N_females

f_temp2 %>%    
  as.data.frame() %>% 
  mutate(totalGE90 = (V6+ V7 + V8 + V9 + V10 + V11 +V12 +V13 +V14 +V15 +V16)/1000000) -> f_temp2_sum
tail(f_temp2_sum)
# mature females figure --------
f_temp2_sum %>% 
  select(totalGE90) %>% 
  mutate(year = c(1975:2022)) %>% 
  ggplot(aes(year, totalGE90)) +
  geom_line(lwd = 1) +
  scale_y_continuous(expand = c(0,0)) +
  ylim(0, max(f_temp2_sum$totalGE90) +10)+
  #scale_y_continuous(expand = c(0, 0))
  expand_limits(y=0) +
  #scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0))
  #geom_hline(data = Bmsy_options, aes(yintercept = Bmsy), color = c("blue", "red"), 
  #           lty = c("solid", "dashed"))+
  #geom_text(data = Bmsy_options, aes(x= 1980, y = Bmsy, label = label), 
  #          hjust = -0.45, vjust = 1.5, nudge_y = 0.05, size = 3.5) +
  ggtitle("Model estimated (21.1b) mature female abundance") +
  ylab("Mature female abundance (>90mm CL)") + xlab("Year") +
  .THEME #+ theme(legend.position = c(0.8, 0.85))

ggsave(paste0(.FIGS, "PRESENTATION_mature_female_abundance_211b.png"), width = 9, height =5)
ggsave(paste0(.FIGS, "PRESENTATION_mature_female_abundance_211b_v2.png"), width = 6, height =6.5)

# model 22.0 mature females --------------
f_temp <- C$N_females_mature
head(f_temp)
tail(f_temp)

f_temp %>% 
  as.data.frame() %>% 
  mutate(total_mature = rowSums(across(where(is.numeric)))/1000000, # this doesn't match what Jie has for mature females....
         totalGE90 = (V6+ V7 + V8 + V9 + V10 + V11 +V12 +V13 +V14 +V15 +V16)/1000000) # this doesn't match what Jie has for mature females....

f_temp2 <- C$N_females

f_temp2 %>%    
  as.data.frame() %>% 
  mutate(totalGE90 = (V6+ V7 + V8 + V9 + V10 + V11 +V12 +V13 +V14 +V15 +V16)/1000000) -> f_temp2_sum
tail(f_temp2_sum)

# model 22.0a mature females --------------
f_temp <- D$N_females_mature
head(f_temp)
tail(f_temp)

f_temp %>% 
  as.data.frame() %>% 
  mutate(total_mature = rowSums(across(where(is.numeric)))/1000000, # this doesn't match what Jie has for mature females....
         totalGE90 = (V6+ V7 + V8 + V9 + V10 + V11 +V12 +V13 +V14 +V15 +V16)/1000000) # this doesn't match what Jie has for mature females....

f_temp2 <- D$N_females

f_temp2 %>%    
  as.data.frame() %>% 
  mutate(totalGE90 = (V6+ V7 + V8 + V9 + V10 + V11 +V12 +V13 +V14 +V15 +V16)/1000000) -> f_temp2_sum
tail(f_temp2_sum)
# tables for parameters -------
# see Jie's cmn sum-table
# found in "Jie_cmn_files.R"
