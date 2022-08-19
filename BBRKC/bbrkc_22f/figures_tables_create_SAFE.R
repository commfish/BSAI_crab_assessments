# k.palof
# 8-16-22
# Figures and Tables needed for Sept 2022 BBRKC SAFE 


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
B <- read_rep("./BBRKC/bbrkc_22f/model_211b/gmacs.rep")

# run code in data-range.R - with B as the letter - got 'data_range.png"
### Figure 1 ---- static in doc
### Figure 2 ------ data range
# run code in data-range.R - with B as the letter - got 'data_range.png"
# Figure 3 ------ 
#  see "RKC_SAFE21_kjp.xlsx' - catchbio tab **FIX update and put into R **
# Figure 4 
#  see "RKC_SAFE21_kjp.xlsx' - cpue tab **FIX update and put into R **
# Figure 5 
# Survey abundance size comps, get Cody's code for these 
### Figure 6 - CUT no VAST
### Figure 7 - static NO update
# Figure 8 --
# selectivities - trawl selectivities - where are these?

# Fiure 9 --
# molting probability - Jie's script
# Figure 10 ---
# area swept trawl survey
# Figure 11 --
# BSFRF survey 


## ssb ----

