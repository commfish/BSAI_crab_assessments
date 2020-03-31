# generic packages and graphic settings.
# Katie Palof
# katie.palof@alaska.gov
# 2018-11-26

# load packages -----
library(tidyverse)
library(reshape2)
library(scales)
library(extrafont)

library(grid)
library(gridExtra)
library(FNGr) #update this to fngr
library(scales)
library(cowplot)
library(readxl)
options(scipen=9999) # remove scientific notation

loadfonts(device="win")
windowsFonts(Times=windowsFont("TT Times New Roman"))
theme_set(theme_sleek())

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
