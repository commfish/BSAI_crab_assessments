# katie.palof@alaska.gov    8-21-19

# Figures for SAFE document for 2019 smbkc 
# goal would be for these to be created in the .Rmd document instead of here
#   but this file will be used to text out figures.

# load ------------
#require(devtools)
#devtools::install_github("seacode/gmacs", subdir = "/gmr", ref = "develop") - only needs to be performed once.
require(gmr)
#setwd("./smbkc_19/model_1")

# All model plots  -------------------------
# still reference 2018 models since I'm currently runing 2019 **FIX**
cur_yr <- 2019 # update annually 

mod_names <- c("model 18.0", "model 19.0 (reference)", "model 19.0a (alt regime)", "model 19.1 (survey fit)") 
.MODELDIR = c(paste0(here::here(), "/SMBKC/smbkc_18a/model_1/"), paste0(here::here(), "/SMBKC/smbkc_19/model_1/"), 
              paste0(here::here(), "/SMBKC/smbkc_19/model_1a/"), paste0(here::here(), "/SMBKC/smbkc_19/model_5/")) #need to update these model options
.THEME    = theme_bw(base_size = 12, base_family = "")
.OVERLAY  = TRUE
.SEX      = c("Aggregate","Male")
.FLEET    = c("Pot","Trawl bycatch","Fixed bycatch","NMFS Trawl","ADF&G Pot")
.TYPE     = c("Retained & Discarded","Retained","Discarded")
.SHELL    = c("Aggregate")
.MATURITY = c("Aggregate")
.SEAS     = c("1","2","3","4","5")

# Read report file and create gmacs report object (a list):
fn       <- paste0(.MODELDIR, "gmacs")
M        <- lapply(fn, read_admb)
names(M) <- mod_names
jj <- 1 # The position in the list that Jies model outputs sit

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

ref_mod <- 2 # base
rec_mod <- 2 # base
mod_scen<- 2:4 #scenarios you want graphed together

## data extent -----------
plot_datarange(M[rec_mod])

## fig 6/7 2018 safe - 2018 compared to reference model --------
plot_cpue(M[1:2], "NMFS Trawl", ylab = "Survey biomass (t)") 

plot_cpue(M[1:2], "ADF&G Pot", ylab = "Pot survey CPUE (crab/potlift)")

### Sensitivity of new data in 2018 on estimated recruitment ; 1978-2018
A <- M
for (i in c(1,2)) {
  ii <- which(A[[i]]$fit$names %in% "sd_log_recruits"); ii <- ii[length(ii)]
  A[[i]]$fit$est[ii] <- NA
  A[[i]]$fit$std[ii] <- NA
}
plot_recruitment(A[1:2])

## ssb -----------
#"Sensitivity of new data in 2019 on estimated mature male biomass (MMB); 1978-2019. \\label{fig:ssb1}"}
plot_ssb(M[1:2], ylab = "Mature male biomass (tons) on 15 February")

## this is NOT correct **FIX** since it doesn't include projected 2019 biomass


