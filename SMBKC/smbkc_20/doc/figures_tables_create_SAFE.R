# k.palof
# 4-26-2020 / 8-16-2020
# Figures and Tables needed for Sept 2020 SAFE SMBKC

# Should be able to just run this file if model results are updated prior to creating
#   new .rmd for SAFE

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
require(gmr)
source("./SMBKC/code/functions.R") 
source("./SMBKC/code/helper.R") 
source("./SMBKC/smbkc_20/doc/gmr_functions2020.R") 

# ALL Model setup  -------------------------
# first model is reference to previous year
cur_yr <- 2020 # update annually 
folder <- "smbkc_20" # update annually 

# update model names and file locations
mod_names <- c("model 16.0 (2019)", "model 16.0 (2020 base)", "model 20.1 (no pot)") 
.MODELDIR = c(paste0(here::here(), "/SMBKC/smbkc_19a/model_1/"),
              paste0(here::here(), "/SMBKC/smbkc_20/model_1/"), 
              paste0(here::here(), "/SMBKC/smbkc_20/model_2/")) #need to update these model options
.THEME    = theme_bw(base_size = 12, base_family = "")
.OVERLAY  = TRUE
.SEX      = c("Aggregate","Male")
.FLEET    = c("Pot","Trawl bycatch","Fixed bycatch","NMFS Trawl","ADF&G Pot", "ADF&G Pot2")
.TYPE     = c("Retained","Discarded", "Retained & Discarded")
.SHELL    = c("Aggregate")
.MATURITY = c("Aggregate")
.SEAS     = c("1","2","3","4","5")
.FIGS     = c("./SMBKC/smbkc_20/doc/safe_figure/")
.TABS     = c("./SMBKC/smbkc_20/doc/safe_tables/")
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
ref_mod <- 1 # base 2019
rec_mod <- 2 # base
mod_scen<- 2:3 #scenarios you want graphed together

ww <- 6
hh <- 5

### Executive summary stats ===========
# not currently saved here just viewed
.get_cpue_df(Mbase) %>% 
  filter(fleet==.FLEET[4]) %>% 
  mutate(x = round(100*pred/mean(pred),0)) %>% 
  select(x) %>% 
  tail(1) %>%
  .$x -> bio_lt_percent

# Tables 1 to 3 calcs -------
## table 1 ------
round(M[[ref_mod]]$spr_bmsy/1000 * 0.5, 2) -> msst_1819
round(M[[ref_mod]]$ssb[length(M[[ref_mod]]$ssb)]/1000, 2) -> mmb_1819
# use with actual 2020 data
round(M[[rec_mod]]$spr_bmsy/1000 * 0.5, 2) -> msst_1920
round(M[[rec_mod]]$ssb[length(M[[rec_mod]]$ssb)]/1000, 2) -> mmb_1920

round(M[[rec_mod]]$spr_bmsy*M[[rec_mod]]$spr_depl/1000, 2) -> mmb_2021
round(M[[rec_mod]]$spr_cofl/1000, 2) -> ofl_2021 # not estimating OFL correctly **FIX**
round(M[[rec_mod]]$spr_cofl/1000*0.8, 2) -> abc_2021

# table 2 ----------
round(M[[rec_mod]]$spr_bmsy* 0.5* 2204.62/1e6, 2) -> msst_1819_lb
round(M[[rec_mod]]$ssb[length(M[[rec_mod]]$ssb)]* 2204.62/1e6, 2) -> mmb_1819_lb
round(M[[rec_mod]]$spr_bmsy*M[[rec_mod]]$spr_depl* 2204.62/1e6, 2)-> mmb_1920_lb
round(M[[rec_mod]]$spr_cofl* 2204.62/1e6, 3) -> ofl_1920_lb
round(M[[rec_mod]]$spr_cofl* 2204.62/1e6*0.8, 2) -> abc_1920_lb

# ofl and abc basis -------- table 3
# ofl and abc basis -------- table 3
round(M[[rec_mod]]$spr_bmsy/1000, 2) -> bmsy_cur
round(M[[rec_mod]]$spr_depl, 2) -> ratio_bmsy
round(M[[rec_mod]]$sd_fofl[1], 3) -> fofl

## FIGURES ===================================
## data extent -----------
#plot_datarange(M[ref_mod]) # call in gmr not working - need to edit this on github
plot_datarangeSM(M[rec_mod]) # see gmr_functions2020.R
plot_datarangeSM(M[ref_mod]) # compare to 2019 base model 

# save the rec_mod one
# had to save manually because I can't get this call to work...**FIX

##!! fig 6/7 Last year's model compared to reference model --------
# models 1 and 2, 1 is 2019 model - smbkc_19a/model_1 
#     and 2 is smbkc_20/model_1 base model
plot_cpue(M[1:2], "NMFS Trawl", ylab = "Survey biomass (t)")
ggsave(paste0(.FIGS, "trawl_cpue_ref.png"), width = ww*1.08, height = hh*.9)

plot_cpue(M[1:2], "ADF&G Pot", ylab = "Pot survey CPUE (crab/potlift)")
ggsave(paste0(.FIGS, "pot_cpue_ref.png"), width = ww*1.08, height = hh)

plot_cpue(M[1:2])
ggsave(paste0(.FIGS, "cpue_ref_both.png"), width = ww*2.5, height = hh)


### Sensitivity of new data in 2018 on estimated recruitment ; 1978-2018
## !!recruit - ref to base ----------------------------
A <- M
for (i in c(2)) {
  ii <- which(A[[i]]$fit$names %in% "sd_log_recruits"); ii <- ii[length(ii)]
  A[[i]]$fit$est[ii] <- NA
  A[[i]]$fit$std[ii] <- NA
}
plot_recruitment(A[1:2]) # does not include recent recruitment - for comparison
plot_recruitment(M[1:2])
#plot_recruitment(M[1:2]) **FIX** determine which one of the above to use with new data?
ggsave(paste0(.FIGS, "recruit_ref.png"), width = ww*1.08, height = hh)

## !!fishing mortality ------
#plot_F(M[2]) **FIX** bring in this from model 1 for now.
#plot_F(M[mod_scen])
# *** FIX *** why are these not working?
plot_F(Mbase)
plot_F2(Mbase) # 
plot_F2(M[1:3])
ggsave(paste0(.FIGS, "fishing_mortality.png"), width = ww*1.5, height = hh)

