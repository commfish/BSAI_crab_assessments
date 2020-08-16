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
mod_names <- c("model 16.0 (2019)", "model 16.0 (2020)", "model 20.1 (no pot)") 
.MODELDIR = c(paste0(here::here(), "/SMBKC/smbkc_19a/model_1/"),
              paste0(here::here(), "/SMBKC/smbkc_20/model_1/"), 
              paste0(here::here(), "/SMBKC/smbkc_20/model_2/")) #need to update these model options
.THEME    = theme_bw(base_size = 12, base_family = "")
.OVERLAY  = TRUE
.SEX      = c("Aggregate","Male")
.FLEET    = c("Pot","Trawl bycatch","Fixed bycatch","NMFS Trawl","ADF&G Pot")
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
plot_F(Mbase)
plot_F2(M[2])
ggsave(paste0(.FIGS, "fishing_mortality.png"), width = ww*1.5, height = hh)

## ssb -----------
#"Sensitivity of new data in 2020 on estimated mature male biomass (MMB); 1978-2019. \\label{fig:ssb1}"}
plot_ssb(M[1:2], ylab = "Mature male biomass (tons) on 15 February")
# !!SSB lst yr / current yr base model-----------
ssb <- .get_ssb_df(M[1:2]) # ssb now does NOT include projection year so only up to 2018 crab year - 2019 projection (example)
head(ssb)
tail(ssb)

# Bmsy proxy and SHS level ------
SHS <- c(1978:2012)
ssb %>% 
  filter(Model == "model 16.0 (2020 base)") %>% 
  mutate(b_msy = mean(ssb)) %>% 
  mutate(SHS_proxy = mean(ssb[year %in% SHS])) %>% 
  group_by(Model) %>% 
  mutate(b_msy/SHS_proxy)

# ssb current year uncertainty --------
# need to run mcmc and projections here 

# !!ref_recruit ribbons -------------
rec <- .get_recruitment_df(M[1:2])
head(rec)

rec$rbar[1]
rec %>% 
  ggplot(aes(year, y = exp(log_rec)/1000000, group = Model, fill = Model)) +
  geom_line(aes(color = Model)) +
  geom_ribbon(aes(x=year, ymax = ub/1000000, ymin = lb/1000000), alpha = 0.15) +
  expand_limits(y=0) +
  ggtitle("Recruitment reference model") +
  ylab("Recruitment (millions of individuals)") + xlab("Year") +
  scale_colour_manual(name = "", values = c("red", "darkcyan"))+
  scale_fill_manual(name = "", values = c("red", "darkcyan")) +
  geom_hline(aes(yintercept = rbar[1]/1000000), color = "red") +
  geom_hline(aes(yintercept = rbar[80]/1000000), color = "darkcyan") +
  #geom_text(aes(x = 2000, y = rbar[1]/1000000, label = "R_bar"), 
  #          hjust = -0.45, vjust = -0.75, nudge_y = 0.05, size = 3.0) +
  .THEME +
  #geom_hline(data = avgR_options, aes(yintercept = meanR), color = c("blue", "red"), 
  #           lty = c("solid", "dashed"))+
  #geom_text(data = avgR_options, aes(x= 1980, y = meanR, label = years), 
  #          hjust = -2.45, vjust = 1.5, nudge_y = 0.05, size = 3.5) 
  ggsave(paste0(.FIGS, "recruitment_ref_ribbons.png"), width = 1.5*ww, height = hh)

# !!Current year (2020) ref recruit ribbon --------------
rec <- .get_recruitment_df(M[2])
head(rec)

rec$rbar[1]

# recruitment plot
rec %>% 
  ggplot(aes(year, y = exp(log_rec)/1000000, group = Model, fill = Model)) +
  geom_line(aes(color = Model)) +
  geom_ribbon(aes(x=year, ymax = ub/1000000, ymin = lb/1000000), alpha = 0.15) +
  expand_limits(y=0) +
  ggtitle("Recruitment reference (base) model") +
  ylab("Recruitment (millions of individuals)") + xlab("Year") +
  #scale_colour_manual(name = "", values = c("red", "darkcyan"))+
  #scale_fill_manual(name = "", values = c("red", "darkcyan")) +
  geom_hline(aes(yintercept = rbar[1]/1000000), color = "black") +
  #geom_hline(aes(yintercept = rbar[80]/1000000), color = "darkcyan") +
  #geom_text(aes(x = 2000, y = rbar[1]/1000000, label = "R_bar"), 
  #          hjust = -0.45, vjust = -0.75, nudge_y = 0.05, size = 3.0) +
  .THEME +
  #geom_hline(data = avgR_options, aes(yintercept = meanR), color = c("blue", "red"), 
  #           lty = c("solid", "dashed"))+
  #geom_text(data = avgR_options, aes(x= 1980, y = meanR, label = years), 
  #          hjust = -2.45, vjust = 1.5, nudge_y = 0.05, size = 3.5) 
  ggsave(paste0(.FIGS, "recruitment_ref_ribbons_", cur_yr, ".png"), width = 1.5*ww, height = hh)

## !!selectivity ----------
#"Comparisons of the estimated stage-1 and stage-2 selectivities for the different model scenarios (the stage-3 selectivities are all fixed at 1). Estimated selectivities are shown for the directed pot fishery, the trawl bycatch fishery, the fixed bycatch fishery, the NMFS trawl survey, and the ADF&G pot survey. Two selectivity periods are estimated in the directed pot fishery, from 1978-2008 and 2009-2017.\\label{fig:selectivity}", fig.height = 15}
plot_selectivity(M[2:3]) 
#plot_selectivity(M[2])
ggsave(paste0(.FIGS, "selectivity_mod_scen.png"), width = ww*1.50, height = 1.1*hh)
ggsave(paste0(.FIGS, "PRES_selectivity_mod_scen.png"), width = ww*1.20, height = 1.3*hh)



