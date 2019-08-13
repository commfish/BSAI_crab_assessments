# k.palof katie.palof@alaska.gov
# date updated: 8-12-19

# Data manipulation for bycatch in Groundfish fisheries for SMBKC

# data obtained from AKFIN: provide instructions

# load -----
source("./SMBKC/code/packages.R")

# data -----
by_weight <- read.csv("C:/Users/kjpalof/Documents/SMBKC/DATA_SMBKC/EBSCrab_AB_Sizegroup.csv")

# clean-up data ---------
head(by_weight)
