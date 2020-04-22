# Troubleshooting gmr issues with udpate GMACS code in Jan 2020


# load ------------
#require(devtools)
#devtools::install_github("seacode/gmacs", subdir = "/gmr", ref = "develop") - only needs to be performed once.
require(gmr)
#setwd("./smbkc_19/model_1")

# All model plots  -------------------------
# still reference 2018 models since I'm currently runing 2019 **FIX**
cur_yr <- 2019 # update annually 

mod_names <- c("model 16.0", "model 16.0 (ref)", "model 19.1 (VAST))", "model 19.2 (add CV pot)", "model 19.3 (add CV both)") 
.MODELDIR = c(paste0(here::here(), "/SMBKC/smbkc_19/model_1/"), 
              paste0(here::here(), "/SMBKC/smbkc_19a/model_1/"),
              paste0(here::here(), "/SMBKC/smbkc_19a/model_4/"), 
              paste0(here::here(), "/SMBKC/smbkc_19a/model_1a/"), 
              paste0(here::here(), "/SMBKC/smbkc_19a/model_1b/")) #need to update these model options
.THEME    = theme_bw(base_size = 12, base_family = "")
.OVERLAY  = TRUE
.SEX      = c("Aggregate","Male")
.FLEET    = c("Pot","Trawl bycatch","Fixed bycatch","NMFS Trawl","ADF&G Pot")
.TYPE     = c("Retained","Discarded", "Retained & Discarded")
.SHELL    = c("Aggregate")
.MATURITY = c("Aggregate")
.SEAS     = c("1","2","3","4","5")
# Read report file and create gmacs report object (a list):
fn       <- paste0(.MODELDIR, "gmacs")
M        <- lapply(fn, read_admb)
names(M) <- mod_names
#jj <- 1 # The position in the list that Jies model outputs sit

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
mod_scen<- 2:4 #scenarios you want graphed together

ww <- 6
hh <- 5

## data extent -----------
plot_datarange(M[ref_mod])
plot_datarange(M[rec_mod]) # not working...output change?
test(M[rec_mod])

K <- M[rec_mod]
L <- M[ref_mod]

n <- length(K)
mdf <- NULL
A <- K[[1]]
repfile <- A$run_name
narepfile <- strsplit(scan(repfile,what="character",flush=TRUE,blank.lines.skip=FALSE,quiet=TRUE)[1:6],':')

narepfile2<- strsplit(scan(repfile2,what="character",flush=TRUE,blank.lines.skip=FALSE,quiet=TRUE)[1:4],':')


