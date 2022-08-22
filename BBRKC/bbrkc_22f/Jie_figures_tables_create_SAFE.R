# k.palof
# 8-16-22
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

## Jie's code ------
A <- read_rep("./BBRKC/bbrkc_22f/model_211b_spring/gmacs.rep")
B <- read_rep("./BBRKC/bbrkc_22f/model_211b/gmacs.rep")
C <- read_rep("./BBRKC/bbrkc_22f/model_22/gmacs.rep")
D <- read_rep("./BBRKC/bbrkc_22f/model_22a/gmacs.rep")

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
# molting probability - see 'Jie_cmn_files.R', Jie's script molt-prob.cmn, molt-prob2.cmn
# !!Figure 10 ------
# area swept trawl survey, BSFRF fit, created with gmr - see figures folder
## ** Figure 10e length comps for BSFRF
# !!Figure 11 --
#  Estimated MMB - from gmr output - "mod_scen_ssb....png'
# ** Figure 12a -------
# recruitment - Jie's code 'rec3.cmn'
# ** Figures 12b -----
# recruitment length distributions 'r-range.cmn'
# ** Figure 13 a, b, c -----
# Fishing mortality comparison by year - Jie's code 'rkf.cmn, rksr.cmn, rksrp.cmn, rkf85.cmn'
### !!Figure 13d ----
# Natural mortality and directed pot fishing mortality 
# ** Figure 14 -------
# recruit vs. mmb Jie's code 'rksr.cmn, rksrp.cmn'
# **Figure 15 -----
# clutchfullness and empty clutches ??? maybe see if Tyler can help with this?
# ** Figure 16a,b------
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
# Figure 25 a, b -------------
# length frequencies residuals for model runs, males NMFS
# Figure 26 a, b -------------
# length frequencies residuals for model runs, females NMFS
# Figure 27a, b
# retrospective of MMB --- look for my code for this????????
# Figure 28 a, b, c --------
# retrospective for recruitment, retrospective errors as function of recruitment, mean ratios of retrospective estimates
# Figure 29 ------
# base model old models NOT retro? code for this??? data for this?
# Figures 30 a, b, c, d, e, f, 
# MCMC output - see figures for which 
# Figures 31  to 33 ------
# projections of status for approaching overfishing 
# !!Figure 34 -----
# NMFS length frequncies for last 5 years of survey data - males and females
# size_bins_comp_Kodiak_f_5mm_LAST5.png
# size_bins_comp_Kodiak_m_5mm_LAST5.png
# Figure 35 -----
# comparison of BB vs red crab north of BB --- where is this????




## ssb ----

