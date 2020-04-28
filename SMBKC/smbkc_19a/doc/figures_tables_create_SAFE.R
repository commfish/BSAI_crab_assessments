# k.palof
# 4-26-2020
# Figures and Tables needed for may 2020 SAFE

# Should be able to just run this file if model results are updated prior to creating
#   new .rmd for SAFE

# load -------
# done again when updates are made to the package or intially first time
#require(devtools)
#devtools::install_github("seacode/gmacs", subdir = "/gmr", ref = "develop") - only needs to be performed once.
# alternate gmr instructions:::
# if gmr is updated and above doesn't work try:
# go to Build above - direct it to the gmr folder - press OK. Over on right in the Build tab (upper right hand side) - 
# click "install and restart"
require(gmr)
source("./SMBKC/code/functions.R") 
source("./SMBKC/code/helper.R") 
source("./SMBKC/smbkc_19a/doc/gmr_functions2020.R") 

# All model plots  -------------------------
# first model is reference to previous year
cur_yr <- 2019 # update annually 
folder <- "smbkc_19a" # update annually 

# update model names and file locations
mod_names <- c("model 16.0", "model 16.0 (ref)", "model 19.1 (VAST)", "model 19.2 (add CV pot)", "model 19.3 (add CV both)", "model 19.4 (q time block pot)") 
.MODELDIR = c(paste0(here::here(), "/SMBKC/smbkc_19/model_1/"), 
              paste0(here::here(), "/SMBKC/smbkc_19a/model_1/"),
              paste0(here::here(), "/SMBKC/smbkc_19a/model_4/"), 
              paste0(here::here(), "/SMBKC/smbkc_19a/model_1a/"), 
              paste0(here::here(), "/SMBKC/smbkc_19a/model_1b/"),
              paste0(here::here(), "/SMBKC/smbkc_19a/model_3/")) #need to update these model options
.THEME    = theme_bw(base_size = 12, base_family = "")
.OVERLAY  = TRUE
.SEX      = c("Aggregate","Male")
.FLEET    = c("Pot","Trawl bycatch","Fixed bycatch","NMFS Trawl","ADF&G Pot", "ADF&G Pot2")
.TYPE     = c("Retained","Discarded", "Retained & Discarded")
.SHELL    = c("Aggregate")
.MATURITY = c("Aggregate")
.SEAS     = c("1","2","3","4","5")
.FIGS     = c("./SMBKC/smbkc_19a/doc/safe_figure/")
# Read report file and create gmacs report object (a list):
fn       <- paste0(.MODELDIR, "gmacs")
M        <- lapply(fn, read_admb)
names(M) <- mod_names

nmult_1 <- 1e+06
nmult_2 <- 0.0004535923 * 1e+6
#fn <- paste0(.MODELDIR[1], "gmacs")
#Mmatch <- lapply(fn, read_admb)
#names(Mmatch) <- c("SMBKC")

fn <- paste0(.MODELDIR[2], "gmacs")
Mbase <- lapply(fn, read_admb)
names(Mbase) <- c("SMBKC")

rinline <- function(code){
  html <- '<code  class="r">``` `CODE` ```</code>'
  sub("CODE", code, html)
}

#alt_mod <- 5 # alt reference time frame
ref_mod <- 1 # base
rec_mod <- 2 # base
mod_scen<- 2:6 #scenarios you want graphed together
mod_scen2 <- c(2, 6) # base model and q time block
mod_scen3 <- 2:5 # without q time block

ww <- 6
hh <- 5




