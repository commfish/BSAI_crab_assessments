# k.palof
# 8-16-22  / 4-21-23
# Figures and Tables - May 2023 model runs exploration


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
source("./BBRKC/code/bbrkc_functions.R")
#source("./BBRKC/bbrkc_22f/Jie R code/read_rep.R")

# ALL Model setup  -------------------------
# first model is reference to previous year
cur_yr <- 2022 # update annually 
folder <- "bbrkc_23s" # update annually 
# The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#D55E00", "#0072B2", "#F0E442" , "#CC79A7")

# To use for fills, add
#scale_fill_manual(values=cbPalette)
# To use for line and point colors, add
#scale_colour_manual(values=cbPalette)

# update model names and file locations
mod_names <- c("21.1b (2022)", "21.1b (2022-update)", "23.0 M=0.257", "23.0a Mest", "23.0b M=0.31", 
               "23.1a inc CV_q", "22.0 1985")#
#mod_names <- c("16.0 (2019)", "16.0 (2020)", "16.0a (fix R ter)", "20.1 (no pot)")
.MODELDIR = c(paste0(here::here(), "/BBRKC/bbrkc_22f/model_211b/"),
              paste0(here::here(), "/BBRKC/bbrkc_23s/model_211b/"),#, 
              paste0(here::here(), "/BBRKC/bbrkc_23s/model_23_0/"),
              paste0(here::here(), "/BBRKC/bbrkc_23s/model_23_0a/"), 
              paste0(here::here(), "/BBRKC/bbrkc_23s/model_23_0b/"), 
              paste0(here::here(), "/BBRKC/bbrkc_23s/model_231a/"), 
              paste0(here::here(), "/BBRKC/bbrkc_23s/model_22/")) #need to update these model options
.THEME    = list(theme_bw(base_size = 12, base_family = ""), scale_fill_manual(values=cbPalette), 
                 scale_colour_manual(values=cbPalette), 
                 update_geom_defaults("line", list(size = 1.75)))
.OVERLAY  = TRUE
.SEX      = c("Aggregate","Male", "Female")
.FLEET    = c("Pot","Trawl bycatch", "Tanner bycatch", "Fixed bycatch","NMFS Trawl","BSFRF survey")
#.TYPE     = c("Retained","Discarded", "Total")
.TYPE     = c("Total", "Retained","Discarded", "Total")
.SHELL    = c("Aggregate","New", "Old")
.MATURITY = c("Aggregate")
.SEAS     = c("1","2","3","4","5")
.FIGS     = c("./BBRKC/bbrkc_23s/doc/figures/")
.TABS     = c("./BBRKC/bbrkc_23s/doc/tables/")
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
names(Mbase) <- c("BBRKC")

rinline <- function(code){
  html <- '<code  class="r">``` `CODE` ```</code>'
  sub("CODE", code, html)
}

#alt_mod <- 5 # alt reference time frame
ref_mod <- 1 # base 2020
rec_mod <- 2 # base
mod_scen<- 2:6 #scenarios you want graphed together

mod_q <- c(2,6)
mod_yr <- c(2,7)

ww <- 6
hh <- 5
update_geom_defaults("line", list(size = 1.75))
#raw_data <- data_out(mod_names[1:5], .MODELDIR[1:5]) # data pulled from .csv created from gmacsall.out - done manually


# FIGURES -------------------------------------
## data extent -----------
# USE jie's plot this isn't picking up all the fleets / indexes
#plot_datarange(M[ref_mod]) # call in gmr not working - need to edit this on github
#plot_datarangeSM(M[rec_mod]) # see gmr_functions2020.R
#plot_datarangeSM(M[ref_mod]) # compare to 2019 base model 

# save the rec_mod one
# had to save manually because I can't get this call to work...**FIX

##!! fig 6/7 Last year's model compared to reference model --------
# models 1 and 2, 1 is 2019 model - smbkc_19a/model_1 
#     and 2 is smbkc_20/model_1 base model
plot_cpue(M[1:2], "NMFS Trawl", ylab = "Survey biomass (t)")
ggsave(paste0(.FIGS, "trawl_cpue_ref.png"), width = ww*1.3, height = hh*.9)

plot_cpue(M[1:2], "BSFRF survey", ylab = "Survey biomass (t)")
ggsave(paste0(.FIGS, "BSFRF_survey_ref.png"), width = ww*1.3, height = hh)

plot_cpue(M[1:2])
ggsave(paste0(.FIGS, "cpue_ref_both.png"), width = ww*2.5, height = hh)

plot_recruitment(M[2])
#plot_recruitment(M[1])
ggsave(paste0(.FIGS, "recruit_ref.png"), width = ww*1.08, height = hh)

## !!fishing mortality ------
#plot_F(M[2]) **FIX** bring in this from model 1 for now.
plot_F(M[mod_scen])
ggsave(paste0(.FIGS, "fishing_mortality_mod_scen.png"), width = ww*1.25, height = hh)
plot_F(Mbase)
ggsave(paste0(.FIGS, "fishing_mortality.png"), width = ww*1.25, height = hh)

## !!selectivity ----------
#"Comparisons of the estimated stage-1 and stage-2 selectivities for the different model scenarios (the stage-3 selectivities are all fixed at 1). Estimated selectivities are shown for the directed pot fishery, the trawl bycatch fishery, the fixed bycatch fishery, the NMFS trawl survey, and the ADF&G pot survey. Two selectivity periods are estimated in the directed pot fishery, from 1978-2008 and 2009-2017.\\label{fig:selectivity}", fig.height = 15}
plot_selectivity(M[2]) 
# Can I change model names here to shorten them????
#plot_selectivity(M[2])
ggsave(paste0(.FIGS, "selectivity_211b.png"), width = ww*1.20, height = 1.1*hh)
#ggsave(paste0(.FIGS, "PRES_selectivity_mod_scen.png"), width = ww*1.50, height = 1.3*hh)
plot_selectivity(M[2:6])
ggsave(paste0(.FIGS, "selectivity_23s.png"), width = ww*1.20, height = 1.1*hh)
#?? may need to use Jie's code here to get just the trawl survey
plot_selectivity_kjp(M[2:6], "NMFS Trawl")
ggsave(paste0(.FIGS, "selectivity_23s_trawl.png"), width = ww*1.50, height = 0.85*hh)

plot_selectivity_kjp(M[2:6], "BSFRF survey")
ggsave(paste0(.FIGS, "selectivity_23s_BSFRF.png"), width = ww*1.50, height = 0.85*hh)

# !!!moliting probability -------
plot_molt_prob(M[2:6])
ggsave(paste0(.FIGS, "molt_prob_mod_scen.png"), width = ww*1.5, height = hh*1.5)

plot_molt_prob(M[mod_q])
ggsave(paste0(.FIGS, "molt_prob_mod_scen_q.png"), width = ww*1.5, height = hh*1.5)

plot_molt_prob(M[c(2,5)])
ggsave(paste0(.FIGS, "molt_prob_base_M0.31.png"), width = ww*1.5, height = hh*1.5)

#!! trawl survey -----------
#{r trawl_survey_biomass, fig.cap = "Comparisons of area-swept estimates of total (90+ mm CL) male survey biomass (tons) and model predictions for the model scenarios. The error bars are plus and minus 2 standard deviations.\\label{fig:trawl_survey_biomass}"} 

plot_cpue(M[c(mod_scen)], "NMFS Trawl", ylab = "NMFS survey biomass (t)")
ggsave(paste0(.FIGS, "trawl_biomass_mod_scen.png"), width = ww*1.90, height = hh)
ggsave(paste0(.FIGS, "trawl_biomass_mod_scen2.png"), width = ww*1.30, height = hh)

# see updated function in 'Jie_cmn_files.R'
plot_cpue_kjp(M[c(mod_scen)], subsetby = "NMFS Trawl", psex = "Male", ylab = "NMFS MALE survey biomass (t)")
ggsave(paste0(.FIGS, "trawl_biomass_MALES_mod_scen.png"), width = ww*1.30, height = hh)

plot_cpue_kjp(M[c(mod_scen)], subsetby = "NMFS Trawl", psex = "Female", ylab = "NMFS FEMALE survey biomass (t)")
ggsave(paste0(.FIGS, "trawl_biomass_FEMALES_mod_scen.png"), width = ww*1.30, height = hh)

# q changes ---
plot_cpue_kjp(M[mod_q], subsetby = "NMFS Trawl", psex = "Male", ylab = "NMFS MALE survey biomass (t)")
ggsave(paste0(.FIGS, "trawl_biomass_MALES_mod_scen_Q.png"), width = ww*1.30, height = hh)

plot_cpue_kjp(M[mod_q], subsetby = "NMFS Trawl", psex = "Female", ylab = "NMFS FEMALE survey biomass (t)")
ggsave(paste0(.FIGS, "trawl_biomass_FEMALES_mod_scen_Q.png"), width = ww*1.30, height = hh)


#!! BSFRF survey -------
#{r pot_survey_cpue, fig.cap = "Comparisons of total (90+ mm CL) male pot survey CPUEs and model predictions for the model scenarios. The error bars are plus and minus 2 standard deviations.\\label{fig:pot_survey_cpue}"}
# need extra error here 
plot_cpue(M[c(mod_scen)],  "BSFRF survey", ylab = "survey biomass (t)", ShowEstErr = TRUE)
ggsave(paste0(.FIGS, "BSFRF_mod_scen.png"), width = ww*1.10, height = hh)

# males 
plot_cpue_kjp(M[c(mod_scen)],  "BSFRF survey", psex = "Male", ylab = "MALE survey biomass (t)", ShowEstErr = TRUE)
ggsave(paste0(.FIGS, "BSFRF_mod_scen_MALES.png"), width = ww*1.10, height = hh)
# females 
plot_cpue_kjp(M[c(mod_scen)],  "BSFRF survey", psex = "Female", ylab = "FEMALE survey biomass (t)", ShowEstErr = TRUE)
ggsave(paste0(.FIGS, "BSFRF_mod_scen_FEMALES.png"), width = ww*1.10, height = hh)

## !!size comps ---------------
## !!!!!!!!!!!!! load my functions file here
#source("./SMBKC/code/functions.R") # moved to top
# CHECK to see which function is being used here **FIX** rename my function
#{r sc_pot_discarded, fig.cap = "Observed and model estimated size-frequencies of discarded male SMBKC by year in the NMFS trawl survey for the model scenarios. \\label{fig:sc_pot_discarded}"}
#plot_size_comps(M[mod_scen], 1, legend_loc = "right")#legend_loc=c(.87,.01))
#ggsave(paste0(.FIGS, "lf_1_retained_pot.png"), width = 8.5, height = 5, unit = "in")

## new function to sort these by season see bbrkc_functions.R -----------
plot_size_comps_kjp(M[mod_scen], 2, legend_loc = "right")
#plot_size_comps(M[mod_scen], 2, legend_loc = "right") # not working
ggsave(paste0(.FIGS, "lf_pot_total_M.png"), width = 12, height = 7.5, unit = "in")

# tanner crab fishery length comps seems to be combining with total obs for directed fishery...one is season 5 and one3
# tanner is season 5, fleet 1
# pot fishery season 3, fleet 1
#plot_size_comps(M[mod_scen], 3, legend_loc = "right")
plot_size_comps_kjp(M[mod_scen], 3, legend_loc = "right")
ggsave(paste0(.FIGS, "lf_tanner_males.png"), width = 12, height = 7.5, unit = "in")
plot_size_comps_kjp(M[mod_scen], 4, legend_loc = "right")
ggsave(paste0(.FIGS, "lf_pot_Females.png"), width = 12, height = 7.5, unit = "in")
plot_size_comps_kjp(M[mod_scen], 5, legend_loc = "right")
ggsave(paste0(.FIGS, "lf_tanner_females.png"), width = 12, height = 7.5, unit = "in")

plot_size_comps(M[mod_scen], 4, legend_loc = "right")
ggsave(paste0(.FIGS, "lf_trawl_bycatch_m.png"), width = 12, height = 7.5, unit = "in")
plot_size_comps(M[mod_scen], 5, legend_loc = "right")
ggsave(paste0(.FIGS, "lf_trawl_bycatch_F.png"), width = 12, height = 7.5, unit = "in")
plot_size_comps(M[mod_scen], 6, legend_loc = "right")
ggsave(paste0(.FIGS, "lf_fixed_bycatch_M.png"), width = 12, height = 7.5, unit = "in")
plot_size_comps(M[mod_scen], 7, legend_loc = "right")
ggsave(paste0(.FIGS, "lf_fixed_bycatch_F.png"), width = 12, height = 7.5, unit = "in")
plot_size_comps(M[mod_scen], 8, legend_loc = "right")
ggsave(paste0(.FIGS, "lf_NMFS_m.png"), width = 12, height = 7.5, unit = "in")
plot_size_comps(M[mod_scen], 9, legend_loc = "right")
ggsave(paste0(.FIGS, "lf_NMFS_f.png"), width = 12, height = 7.5, unit = "in")
plot_size_comps(M[mod_scen], 10, legend_loc = "right")
ggsave(paste0(.FIGS, "lf_BSFRF_m.png"), width = 12, height = 7.5, unit = "in")
plot_size_comps(M[mod_scen], 11, legend_loc = "right")
ggsave(paste0(.FIGS, "lf_BSFRF_f.png"), width = 12, height = 7.5, unit = "in")

#plot_size_comps(M[2:4], 3, legend_loc = "right") #legend_loc=c(.87,.2))
#ggsave(paste0(.FIGS, "lf_3.png"), width = 8.5, height = 5, unit = "in")

### !!!ssb mod scen ---------
ssb <- .get_ssb_dfKP(M[2:6])
ssb %>% 
  ggplot(aes(year, ssb, col = Model)) +
  geom_line() +
  #geom_ribbon(aes(x=year, ymax = ub, ymin = lb), alpha = 0.2) +
  expand_limits(y=0) +
  scale_y_continuous(expand = c(0,0)) +
  #geom_hline(data = Bmsy_options, aes(yintercept = Bmsy), color = c("blue", "red"), 
  #           lty = c("solid", "dashed"))+
  #geom_text(data = Bmsy_options, aes(x= 1980, y = Bmsy, label = label), 
  #          hjust = -0.45, vjust = 1.5, nudge_y = 0.05, size = 3.5) +
  ggtitle("Model scenarios") +
  ylab("Mature male biomass (t) on 15th February") + xlab("Year") +
  .THEME
ggsave(paste0(.FIGS, "mod_scen_ssb_wprojected_yr_tall.png"), width = ww*1.3, height = hh*1.25)
ggsave(paste0(.FIGS, "mod_scen_ssb_wprojected_yr.png"), width = ww*1.1, height = hh*1.1)

## ssb mod_scen limited years ------------
ssb <- .get_ssb_dfKP(M[2:7])
ssb %>% 
  filter(year >= 1985) %>% 
  ggplot(aes(year, ssb, col = Model)) +
  geom_line() +
  #geom_ribbon(aes(x=year, ymax = ub, ymin = lb), alpha = 0.2) +
  expand_limits(y=0) +
  scale_y_continuous(expand = c(0,0)) +
  #geom_hline(data = Bmsy_options, aes(yintercept = Bmsy), color = c("blue", "red"), 
  #           lty = c("solid", "dashed"))+
  #geom_text(data = Bmsy_options, aes(x= 1980, y = Bmsy, label = label), 
  #          hjust = -0.45, vjust = 1.5, nudge_y = 0.05, size = 3.5) +
  ggtitle("Model scenarios") +
  ylab("Mature male biomass (t) on 15th February") + xlab("Year") +
  .THEME
ggsave(paste0(.FIGS, "1985mod_scen_ssb_wprojected_yr.png"), width = ww*1.3, height = hh*.85)

### !! ssb mod q ---------
ssb <- .get_ssb_dfKP(M[c(2,6,7)])
ssb %>% 
  ggplot(aes(year, ssb, col = Model)) +
  geom_line() +
  #geom_ribbon(aes(x=year, ymax = ub, ymin = lb), alpha = 0.2) +
  expand_limits(y=0) +
  scale_y_continuous(expand = c(0,0)) +
  #geom_hline(data = Bmsy_options, aes(yintercept = Bmsy), color = c("blue", "red"), 
  #           lty = c("solid", "dashed"))+
  #geom_text(data = Bmsy_options, aes(x= 1980, y = Bmsy, label = label), 
  #          hjust = -0.45, vjust = 1.5, nudge_y = 0.05, size = 3.5) +
  ggtitle("Model scenarios") +
  ylab("Mature male biomass (t) on 15th February") + xlab("Year") +
  .THEME
ggsave(paste0(.FIGS, "mod_Q_ssb_wprojected_yr_tall.png"), width = ww*1.3, height = hh*1.25)
ggsave(paste0(.FIGS, "mod_Q_ssb_wprojected_yr.png"), width = ww*1.1, height = hh*1.1)

## !!recruitment -------------
plot_rec_bb_kjp(M[2:7]) # this is found in 'bbrkc_functions.R'
ggsave(paste0(.FIGS, "recruitment_mod_scen.png"), width = 1.18*ww, height = hh)

plot_rec_bb_kjp(M[c(2,5,7)]) # this is found in 'bbrkc_functions.R'
ggsave(paste0(.FIGS, "recruitment_base_highM.png"), width = 1.18*ww, height = hh)

plot_rec_bb_kjp(M[c(2,6,7)]) # this is found in 'bbrkc_functions.R'
ggsave(paste0(.FIGS, "recruitment_mod_Q.png"), width = 1.18*ww, height = hh)

##!!natural_mortality -------------
  #, fig.cap = "Time-varying natural mortality ($M_t$). Estimated pulse period occurs in 1998/99 (i.e. $M_{1998}$). \\label{fig:M_t}"}
  # updated with my own function due to issues with maturity in gmr
  #plot_natural_mortality(M[mod_scen], knots = NULL, slab = "Model")

plot_natural_mortality2(M[2:3])
plot_natural_mortality2(M[mod_scen], knots = NULL, slab = "Model")
ggsave(paste0(.FIGS, "mod_scen_M_t.png"), width = 1.20*ww, height = hh)
#plot_natural_mortality(M, knots = NULL, slab = "Model")
plot_natural_mortality2(M[c(2,6,7)])
ggsave(paste0(.FIGS, "mod_q_M_t.png"), width = 1.20*ww, height = hh)

plot_natural_mortality2(M[c(2:5)])
ggsave(paste0(.FIGS, "mod_nat_mort_M_t.png"), width = 1.20*ww, height = hh)

## TABLES ====================================

# Table 7 nat mort----
nat_mort <- .get_M_df_kjp(M[2:7]) # bbrkc_functions.R

nat_mort %>% 
  distinct(Model, Sex, M) -> natural_mort_all
write.csv(natural_mort_all, paste0(.TABS, "M_out.csv"))

##### !! STOP HERE may 2023 -----------------

#!! size comp residuals -------
plot_size_comps_res(M[rec_mod])
ggsave(paste0(.FIGS, "ref_mod_size_comp_residuals.png"), width = ww*1.20, height = 1.1*hh)

plot_size_comps_res(M[3])
ggsave(paste0(.FIGS, "model22_size_comp_residuals.png"), width = ww*1.20, height = 1.1*hh)

plot_size_comps_res(M[4])
ggsave(paste0(.FIGS, "model22a_size_comp_residuals.png"), width = ww*1.20, height = 1.1*hh)



##catch --------
plot_catch(M[2]) # not working
ggsave(paste0(.FIGS, "catch.png"), width = ww*1.02, height = hh*1.2)

plot_catch(M[mod_scen])
ggsave(paste0(.FIGS, "catch_mod_scen.png"), width = ww*1.35, height = hh*1.2)
#!! trawl_res --------
#{r bts_resid_nmfs, fig.cap = "Standardized residuals for area-swept estimates of total male survey biomass for the model scenarios. \\label{fig:bts_resid_nmfs}"}
#A <- M[mod_scen];
#plot_cpue_res(A, "NMFS Trawl")
plot_cpue_res(M[mod_scen], "NMFS Trawl")
ggsave(paste0(.FIGS, "trawl_biomass_mod_scen_residuals.png"), width = ww*1.30, height = 1.1*hh)

#!! bsfrf_res  --------
#{r bts_resid_adfg, fig.cap = "Standardized residuals for total male pot survey CPUEs for each of the Gmacs model scenarios.\\label{fig:bts_resid_adfg}"}
plot_cpue_res(M[mod_scen], "BSFRF survey")
ggsave(paste0(.FIGS, "BSFRF survey_mod_scen_residuals.png"), width = ww*1.20, height = 1.1*hh)

#plot_cpue_res(Mbase, "ADF&G Pot")
#ggsave(paste0(.FIGS, "pot_cpue_REF_residuals.png"), width = ww*1.20, height = 1.1*hh)

## ssb -----------
#SKIP "Sensitivity of new data in 2020 on estimated mature male biomass (MMB); 1978-2019. \\label{fig:ssb1}"}
ssb <- .get_ssb_dfKP(M[1:2])
ssb %>% 
  ggplot(aes(year, ssb, col = Model)) +
  geom_line() +
  #geom_ribbon(aes(x=year, ymax = ub, ymin = lb), alpha = 0.2) +
  expand_limits(y=0) +
  scale_y_continuous(expand = c(0,0)) +
  #geom_hline(data = Bmsy_options, aes(yintercept = Bmsy), color = c("blue", "red"), 
  #           lty = c("solid", "dashed"))+
  #geom_text(data = Bmsy_options, aes(x= 1980, y = Bmsy, label = label), 
  #          hjust = -0.45, vjust = 1.5, nudge_y = 0.05, size = 3.5) +
  ggtitle("Reference model (21.1b)") +
  ylab("Mature male biomass (t) on 15th February") + xlab("Year") +
  .THEME
ggsave(paste0(.FIGS, "reference_ssb_wprojected_yr.png"), width = ww, height = hh)



#plot_ssb(M[1:2], ylab = "Mature male biomass (tons) on 15 February")
# !!SSB lst yr / current yr base model-----------
#ssb <- .get_ssb_df(M[1:2]) # ssb now does NOT include projection year so only up to 2018 crab year - 2019 projection (example)
head(ssb)
tail(ssb)

#!!ssb current year uncertainty --------
### come back to this later ---------------------------
raw_data <- data_out(mod_names[1:5], .MODELDIR[1:5])

ssb1 <- get_ssb_out(mod_names[1:2], raw_data)
ssb_last <- get_ssb_last(M[1:2]) %>% select(-par, -sd)

ssb1 %>% 
  select(Model, ssb, year, lb, ub) %>% 
  rbind(ssb_last) -> ssb

# Figures -----
## reference with last year ------
ssb %>% 
  ggplot(aes(year, ssb, col = Model)) +
  geom_line() +
  geom_ribbon(aes(x=year, ymax = ub, ymin = lb, fill = Model, col = NULL), alpha = 0.2) +
  expand_limits(y=0) +
  ylim(0, max(ssb$ub)+ 100)+
  #ylab = "SSB (tonnes)" +
  #scale_y_continuous(expand = c(0,0)) +
  #geom_hline(data = Bmsy_options, aes(yintercept = Bmsy), color = c("blue", "red"), 
  #           lty = c("solid", "dashed"))+
  #geom_text(data = Bmsy_options, aes(x= 1980, y = Bmsy, label = label), 
  #          hjust = -0.45, vjust = 1.5, nudge_y = 0.05, size = 3.5) +
  #ggtitle("Base model - model 1 (Model 3 2018)") +
  ylab("Mature male biomass (tons) on 15 February") + xlab("Year") +
  .THEME + theme(legend.position = c(0.8, 0.85))
ggsave(paste0(.FIGS, "lastyr_reference_ssb_wprojected_yr.png"), width = ww*1.18, height = hh)
ggsave(paste0(.FIGS, "PRESENTATION_lastyr_reference_ssb_wprojected_yr.png"), width = ww*1.5, height = hh)

# !!SSB model scenarios-----------
#raw_data <- data_out(mod_names[2:4], .MODELDIR[2:4]) # see line 212
#  !! FIX!! need to pull this data from all models to create output file 'ssb_rec_out.csv'
ssb1 <- get_ssb_out(mod_names[2:5], raw_data)
ssb_last <- get_ssb_last(M[2:5]) %>% select(-par, -sd)

ssb1 %>% 
  select(Model, ssb, year, lb, ub) %>% 
  rbind(ssb_last) -> ssb

#ssb2 <- .get_ssb_dfKP(M[mod_scen]) # go to line 296
# SSB lst yr / current yr base model-----------

## ssb plot with current year 
ssb %>% 
  ggplot(aes(year, ssb, col = Model)) +
  geom_line() +
  expand_limits(y=0) +
  geom_ribbon(aes(x=year, ymax = ub, ymin = lb, fill = Model, col = NULL), alpha = 0.1) +
  #ylab = "SSB (tonnes)" +
  scale_y_continuous(expand = c(0,0)) +
  ylim(0, max(ssb$ub)+ 100)+
  #geom_hline(data = Bmsy_options, aes(yintercept = Bmsy), color = c("blue", "red"), 
  #           lty = c("solid", "dashed"))+
  #geom_text(data = Bmsy_options, aes(x= 1980, y = Bmsy, label = label), 
  #          hjust = -0.45, vjust = 1.5, nudge_y = 0.05, size = 3.5) +
  #ggtitle("Base model - model 1 (Model 3 2018)") +
  ylab("Mature male biomass (tons) on 15 February") + xlab("Year") +
  .THEME + theme(legend.position = c(0.8, 0.85))
ggsave(paste0(.FIGS, "mod_scen_ssb_wprojected_yr.png"), width = ww*1.18, height = hh)
ggsave(paste0(.FIGS, "PRESENTATION_mod_scen_ssb_wprojected_yr.png"), width = ww*1.5, height = hh)

# !!recruitment mod scen-------------
plot_recruitment(M[3]) # code issue doesn't work with mulitple models
rec <- .get_recruitment_df(M[3:4])
head(rec)

plot_rec_bb_kjp(M[4:5]) # this is found in 'bbrkc_functions.R'

ggsave(paste0(.FIGS, "recruitment_mod_scen.png"), width = 1.18*ww, height = hh)


# SKIP recruit ribbons -------------
rec <- .get_recruitment_df(M[mod_scen])
head(rec)

#rbar is estimated in model
# need to pull rbar from model output with different recruitment years
rec %>% 
  group_by(Model) %>% 
  summarise(meanR = mean(exp(log_rec)/1000000)) %>% 
  mutate(years = "1978-2018")-> avgR

avgR  -> avgR_options
#mutate(Bmsy50 = 0.5*Bmsy) -> Bmsy_options
avgR_options # see above is calculated average recruitment for each time series
rec$rbar[1]

# recruitment plot
rec %>% 
  ggplot(aes(year, y = exp(log_rec)/1000000, group = Model, fill = Model)) +
  geom_line(aes(color = Model)) +
  geom_ribbon(aes(x=year, ymax = ub/1000000, ymin = lb/1000000), alpha = 0.15) +
  expand_limits(y=0) +
  ggtitle("Recruitment model scenarios") +
  ylab("Recruitment (millions of individuals)") + xlab("Year") +
  #scale_colour_manual(name = "", values = c("red", "dark green", "blue"))+
  #scale_fill_manual(name = "", values = c("red", "dark green", "blue")) +
  #geom_hline(aes(yintercept = rbar[1]/1000000), color = "gray25") +
  #geom_text(aes(x = 2000, y = rbar[1]/1000000, label = "R_bar"), 
  #          hjust = -0.45, vjust = -0.75, nudge_y = 0.05, size = 3.0) +
  .THEME +
  #geom_hline(aes(yintercept = rbar, group = Model, fill = Model))
  geom_hline(aes(yintercept = rbar[1]/1000000), color = "#999999") +
  geom_hline(aes(yintercept = rbar[76]/1000000), color = "#E69F00") +
  geom_hline(aes(yintercept = rbar[121]/1000000), color = "#56B4E9")
#geom_hline(data = avgR_options, aes(yintercept = meanR, group = Model,
#                                    color = Model)) +
#geom_text(data = avgR_options, aes(x= 1980, y = meanR, label = years), 
#          hjust = -2.45, vjust = 1.5, nudge_y = 0.05, size = 3.5) 
ggsave(paste0(.FIGS, "recruitment_mod_scen_ribbons.png"), width = 1.18*ww, height = hh)
ggsave(paste0(.FIGS, "PRESENTATION_recruitment_mod_scen_ribbons.png"), width = 1.5*ww, height = hh)



#

# **FIX** dynamic Bzero ----------------------
#{r Dynamic_Bzero, fig.cap = "Comparisons of mature male biomass relative to the dynamic $B_0$ value, (15 February, 1978-2018) for  each of the model scenarios.\\label{fig:dynB0}"}
db0 <- get_Db0_out(mod_names[2:5], raw_data, M[2:5])

db0 %>% 
  ggplot(aes(x=year, y=ssb, col = Model)) +
  geom_line() +
  expand_limits(y=0) +
  geom_ribbon(aes(x=year, ymax = ub, ymin = lb, fill = Model, col = NULL), alpha = 0.1) +
  #ylab = "SSB (tonnes)" +
  scale_y_continuous(expand = c(0,0)) +
  ylim(0, max(db0$ub))+
  ylab("RSB (SSB/dB0)") + xlab("Year") +
  .THEME + theme(legend.position = c(0.3, 0.85))

#plot_dynB0(M[mod_scen]) #**FIX**
ggsave(paste0(.FIGS, "dyn_Bzero.png"), width = 8.5, height = 5, unit = "in")

# line 150 dynamic b0 base model 
dynb0 <- get_Db0_out(mod_names[2], raw_data, M[2])

dynb0 %>% transmute(round(100*ssb,0)) %>% slice(n())

get_Db0_out(mod_names[2], raw_data, M[2]) %>% transmute(round(100*ssb,0)) %>% slice(n())
# not currently being output in .rep file - made Jim aware of this I need to talk to him again about this.
#.get_dynB0_df(M)

## TABLES ====================================

# Tables 1 to 3 calcs -------
# need summary of models for may 2023
## table 1 -
round(M[[rec_mod]]$spr_bmsy/1000 * 0.5, 2) -> msst_2122
round(M[[rec_mod]]$ssb[length(M[[rec_mod]]$ssb)]/1000, 2) -> mmb_2122
round(M[[rec_mod]]$spr_bmsy*M[[rec_mod]]$spr_depl/1000, 2) -> mmb_2223
round(M[[rec_mod]]$spr_cofl/1000, 2) -> ofl_2223
round(M[[rec_mod]]$spr_cofl/1000*0.80, 2) -> abc_2223
table1specs_t <- c(msst_2122, mmb_2122, mmb_2223, ofl_2223, abc_2223)
table1specs_t
#rec_ofl <- read.csv(paste0(here::here(), "/SMBKC/smbkc_20/model_1/figure/ofl_calc.csv"))
#round(rec_ofl$OFL_2020/1000, 2) -> ofl_2021
#round(ofl_2021*0.8, 2) -> abc_2021

# table 2 --
round(M[[rec_mod]]$spr_bmsy* 0.5* 2204.62/1e6, 2) -> msst_2122_lb
round(M[[rec_mod]]$ssb[length(M[[rec_mod]]$ssb)]* 2204.62/1e6, 2) -> mmb_2122_lb
round(M[[rec_mod]]$spr_bmsy*M[[rec_mod]]$spr_depl* 2204.62/1e6, 2)-> mmb_2223_lb
round(M[[rec_mod]]$spr_cofl* 2204.62/1e6, 3) -> ofl_2223_lb
round(M[[rec_mod]]$spr_cofl* 2204.62/1e6*0.80, 2) -> abc_2223_lb
table1specs_lb <- c(msst_2122_lb, mmb_2122_lb, mmb_2223_lb, ofl_2223_lb, abc_2223_lb)
table1specs_lb
#round(rec_ofl$OFL_2020* 2204.62/1e6, 3) -> ofl_2021_lb
#round(ofl_2021_lb*0.8, 2) -> abc_2021_lb

# ofl and abc basis -------- table 3
# ofl and abc basis -------- table 3
round(M[[rec_mod]]$spr_bmsy/1000, 2) -> bmsy_cur
round(M[[rec_mod]]$spr_depl, 2) -> ratio_bmsy
round(M[[rec_mod]]$sd_fofl[1], 3) -> fofl
table3specs_t <- c(bmsy_cur, mmb_2223, ratio_bmsy, fofl)

table3specs_lb <- c(bmsy_cur*(2204.62/1e3), mmb_2223*(2204.62/1e3), ratio_bmsy, fofl)

# Table 7 nat mort----
nat_mort <- .get_M_df_kjp(M[mod_scen]) # bbrkc_functions.R

nat_mort %>% 
  distinct(Model, Sex, M) -> natural_mort_all
write.csv(natural_mort_all, paste0(.TABS, "M_out.csv"))

## !!table of all parameter output -------
#```{r est_pars_all, results = "asis"}
Parameter <- NULL
Estimate <- NULL
Model <- NULL
Mname <- c("last yr", "Ref","M0.21" ,"M0.26" ,"Mall")
#c("model 16.0 (2019)", "model 16.0 (2020)", "model 20.1 (no pot )") 
for (ii in 2:5)
{
  x <- M[[ii]]$fit
  i <- c(grep("m_dev", x$names)[1],
         grep("theta", x$names),
         grep("survey_q", x$names),
         grep("log_fbar", x$names),
         grep("log_slx_pars", x$names)) #,
  #grep("sd_fofl", x$names),
  #grep("sd_ofl", x$names) )
  Parameter <- c(Parameter, x$names[i])
  Estimate <- c(Estimate, x$est[i])
  Model <- c(Model, rep(Mname[ii], length(i)))
}
j <- grep("survey_q", Parameter)
Estimate[j] <- Estimate[j] * 1000
Parameter_ref <- c("Natural mortality deviation in 1998/99 ($\\delta^M_{1998})$",
                   "$\\log (\\bar{R})$","$\\log (n^0_1)$","$\\log (n^0_2)$","$\\log (n^0_3)$",
                   "$q_{pot}$", "$\\log (\\bar{F}^\\text{df})$","$\\log (\\bar{F}^\\text{tb})$","$\\log (\\bar{F}^\\text{fb})$",
                   "log Stage-1 directed pot selectivity 1978-2008","log Stage-2 directed pot selectivity 1978-2008",
                   "log Stage-1 directed pot selectivity 2009-2017","log Stage-2 directed pot selectivity 2009-2017",
                   "log Stage-1 NMFS trawl selectivity","log Stage-2 NMFS trawl selectivity",
                   "log Stage-1 ADF\\&G pot selectivity","log Stage-2 ADF\\&G pot selectivity")
#"$F_\\text{OFL}$","OFL")
#Parameter_no_M <- c(#"Natural mortality deviation in 1998/99 ($\\delta^M_{1998})$",
#                   "$\\log (\\bar{R})$","$\\log (n^0_1)$","$\\log (n^0_2)$","$\\log (n^0_3)$",
#                   "$q_{pot}$", "$\\log (\\bar{F}^\\text{df})$","$\\log (\\bar{F}^\\text{tb})$","$\\log (\\bar{F}^\\text{fb})$",
#                   "log Stage-1 directed pot selectivity 1978-2008","log Stage-2 directed pot selectivity 1978-2008",
#                   "log Stage-1 directed pot selectivity 2009-2017","log Stage-2 directed pot selectivity 2009-2017",
#                   "log Stage-1 NMFS trawl selectivity","log Stage-2 NMFS trawl selectivity",
#                   "log Stage-1 ADF\\&G pot selectivity","log Stage-2 ADF\\&G pot selectivity")
Parameter <- c(Parameter_ref, Parameter_ref, Parameter_ref, Parameter_ref) #, Parameter, Parameter, ParameterQ) 
df1 <- data.frame(Model, Parameter, Estimate)
#Mname <- c("last yr", "Ref","VAST","addCVpot", "addCVboth", "qBlock")
#ref_ofl <- read.csv(paste0(here::here(), "/SMBKC/smbkc_20/model_1/figure/ofl_calc.csv"))
#fixR_ofl <- read.csv(paste0(here::here(), "/SMBKC/smbkc_20/model_1_rfix_TPL/figure/ofl_calc.csv"))
#nopot_ofl <- read.csv(paste0(here::here(), "/SMBKC/smbkc_20/model_2/figure/ofl_calc.csv"))

df2 <- data.frame(Model = c("Ref", "Ref", "M0.21" ,"M0.21" ,"M0.26", "M0.26", "Mall", "Mall"),
                  Parameter = c("$F_\\text{OFL}$","OFL", "$F_\\text{OFL}$","OFL", 
                                "$F_\\text{OFL}$","OFL", "$F_\\text{OFL}$","OFL"), 
                  Estimate = c(M[[rec_mod]]$sd_fofl[1], M[[rec_mod]]$spr_cofl,
                               M[[3]]$sd_fofl[1], M[[3]]$spr_cofl, 
                               M[[4]]$sd_fofl[1], M[[4]]$spr_cofl, 
                               M[[5]]$sd_fofl[1], M[[5]]$spr_cofl))
df1 %>% 
  bind_rows(df2) -> df
df3 <- tidyr::spread(df, Model, Estimate) %>% 
  dplyr::select(Parameter, Ref, M0.21, M0.26, Mall)
# **FIX ** reorder these to match other tables - currently done manually
write.csv(df3, paste0(here::here(), '/SMBKC/', folder,'/doc/safe_tables/all_parms.csv'), 
          row.names = FALSE)
### see chunk in .rmd to bring this file in


## !!data weighting ---------------------
#```{r data_weighting, results = "asis"}
# updated to work for draft - need to figure out how to get Francis weightings and 
#   lamdas
# shorten names for tables
Mname2 <- c("Ref", "M_21","M_26", "Mall")

df <- NULL
for (ii in 2:5)
{
  x       <- M[[ii]]
  SDNR    <- c(x$sdnr_MAR_cpue[,1], 
               x$sdnr_MAR_lf[,1]); names(SDNR) <- c("SDNR NMFS trawl survey",
                                                    "SDNR ADF\\&G pot survey",
                                                    "SDNR directed pot LF",
                                                    "SDNR NMFS trawl survey LF",
                                                    "SDNR ADF\\&G pot survey LF")
  MAR     <- c(x$sdnr_MAR_cpue[,2], x$sdnr_MAR_lf[,2]); names(MAR) <- c("MAR NMFS trawl survey","MAR ADF\\&G pot survey","MAR directed pot LF","MAR NMFS trawl survey LF","MAR ADF\\&G pot survey LF")
  #Francis <- x$Francis_weights; names(Francis) <- c("Fancis weight for directed pot LF","Francis weight for NMFS trawl survey LF","Francis weight for ADF\\&G pot survey LF")
  wt_cpue <- c(ifelse(ii == 3, 1,1), ifelse(ii == 3, 1 ,1)); names(wt_cpue) <- c("NMFS trawl survey weight","ADF\\&G pot survey weight")
  wt_lf   <- c(1,1,1); names(wt_lf) <- c("Directed pot LF weight","NMFS trawl survey LF weight","ADF\\&G pot survey LF weight")
  v       <- c(wt_cpue, wt_lf, SDNR, MAR)
  df      <- cbind(df, v)
}
df_ref        <- data.frame(rownames(df), df, row.names = NULL)
names(df_ref) <- c("Component", "Ref", "M_21","M_26", "Mall") #mod_names[mod_scen])

#df <- NULL
#for (ii in 4)
#{
#  x       <- M[[ii]]
#  SDNR    <- c(x$sdnr_MAR_cpue[1], 
#               x$sdnr_MAR_lf[,1]); names(SDNR) <- c("SDNR NMFS trawl survey",
#                                                    "SDNR directed pot LF",
#                                                    "SDNR NMFS trawl survey LF")
#  MAR     <- c(x$sdnr_MAR_cpue[2], x$sdnr_MAR_lf[,2]); names(MAR) <- c("MAR NMFS trawl survey",
#                                                                       "MAR directed pot LF",
#                                                                       "MAR NMFS trawl survey LF")
#  #Francis <- x$Francis_weights; names(Francis) <- c("Fancis weight for directed pot LF","Francis weight for NMFS trawl survey LF","Francis weight for ADF\\&G pot survey LF")
#  wt_cpue <- c(ifelse(ii == 3, 1,1)); names(wt_cpue) <- 
#                                            c("NMFS trawl survey weight")
#  wt_lf   <- c(1,1); names(wt_lf) <- c("Directed pot LF weight","NMFS trawl survey LF weight")
#  v       <- c(wt_cpue, wt_lf, SDNR, MAR)
#  df      <- cbind(df, v)
#}
#df_nopot        <- data.frame(rownames(df), df, row.names = NULL)
#names(df_nopot) <- c("Component", "nopot") #mod_names[mod_scen])

#df_ref %>% 
#  left_join(df_nopot) -> df

write.csv(df_ref, paste0(here::here(), '/SMBKC/', folder, '/doc/safe_tables/data_weighting.csv'), 
          row.names = FALSE)

# !!Likelihood components -----------------
#```{r likelihood_components, results = "asis"}
Mname2 <- c("Ref", "M_21","M_26", "Mall")
df <- NULL
for (ii in mod_scen)
{
  x        <- M[[ii]]
  # Catch
  ll_catch <- x$nloglike[1,]
  dc       <- .get_catch_df(M[1])
  names(ll_catch) <- unique(paste0(dc$fleet, " ", dc$type, " Catch"))
  # Abundance indices
  ll_cpue  <- x$nloglike[2,1:2]
  names(ll_cpue) <- c("NMFS Trawl Survey","ADF\\&G Pot Survey CPUE")
  # Size compositions
  ll_lf    <- x$nloglike[3,1:3]
  names(ll_lf) <- c("Directed Pot LF","NMFS Trawl LF","ADF\\&G Pot LF")
  # Recruitment deviations
  ll_rec <- sum(x$nloglike[4,], na.rm = TRUE)
  names(ll_rec) <- "Recruitment deviations"
  # Penalties
  F_pen <- x$nlogPenalty[2]; names(F_pen) <- "F penalty"
  M_pen <- x$nlogPenalty[3]; names(M_pen) <- "M penalty"
  # Priors
  prior <- sum(x$priorDensity); names(prior) <- "Prior"
  v <- c(ll_catch, ll_cpue, ll_lf, ll_rec, F_pen, M_pen, prior)
  sv <- sum(v, na.rm = TRUE); names(sv) <- "Total"
  npar <- x$fit$nopar; names(npar) <- "Total estimated parameters"
  v <- c(v, sv, npar)
  df <- cbind(df, v)
}
df <- data.frame(rownames(df), df, row.names = NULL)
names(df) <- c("Component", Mname2) #mod_names[mod_scen])
write.csv(df, paste0(here::here(), '/SMBKC/', folder, '/doc/safe_tables/neg_log_like.csv'), 
          row.names = FALSE)

### !!population abundance last years model -----------------------
#```{r pop-abundance-2019, results = "asis"}
# need ssb or mmb and CV from last years model - here 1
#ssb1 <- get_ssb_out(mod_names[1], raw_data)
ssb_last <- get_ssb_last(M[1]) %>% select(-par)


A         <- M[[1]]
i         <- grep("sd_log_ssb", A$fit$names) #does not have proj value in here
SD        <- A$fit$std[i]
tl        <- length(A$mod_yrs)

# ssb current year uncertainty
#un_ssb2 <- read.csv(here::here("./SMBKC/smbkc_18a/model_1/projections/proj_1/d/uncertainty_ssb_2019.csv"))
#un_ssb2 <- read.csv(here::here("./SMBKC/smbkc_19/model_1/projections/proj_1/d/uncertainty_ssb_2019.csv"))
# USE ssb_last for now **FIX**

years = c(as.integer(A$mod_yrs[1:tl]), A$mod_yrs[tl]+1)
ssb = c(A$ssb, A$spr_bmsy*A$spr_depl)
ssb_cv = c((exp(SD[1:tl])-1), (ssb_last$sd/ssb_last$ssb))

df        <- data.frame(years, A$N_males[ ,1], A$N_males[ ,2], 
                        A$N_males[ ,3], ssb, ssb_cv)
names(df) <- c("Year","$n_1$","$n_2$","$n_3$","MMB","CV MMB")
write.csv(df, paste0(here::here(), '/SMBKC/', folder, '/doc/safe_tables/numbers_last_yrs.csv'), 
          row.names = FALSE)

### !!population abundance current year base model -----------------------
# **FIX**??  if I want this to be model with fixed recruitment in 2019
#```{r pop-abundance-2019, results = "asis"}
A         <- M[[rec_mod]]
i         <- grep("sd_log_ssb", A$fit$names) #does not have proj value in here
SD        <- A$fit$std[i]
tl        <- length(A$mod_yrs)

# ssb current year uncertainty
#un_ssb <- read.csv(here::here("./SMBKC/smbkc_20/model_1/projections/proj_1/d/uncertainty_ssb_2020.csv"))
ssb_last <- get_ssb_last(M[2]) %>% select(-par)

years = c(as.integer(A$mod_yrs[1:tl]), A$mod_yrs[tl]+1)
ssb = c(A$ssb, A$spr_bmsy*A$spr_depl)
ssb_cv = c((exp(SD[1:tl])-1), (ssb_last$sd/ssb_last$ssb))

df        <- data.frame(years, A$N_males[ ,1], A$N_males[ ,2], 
                        A$N_males[ ,3], ssb, ssb_cv)
names(df) <- c("Year","$n_1$","$n_2$","$n_3$","MMB","CV MMB")
write.csv(df, paste0(here::here(), '/SMBKC/', folder, '/doc/safe_tables/numbers_current_yrs.csv'), 
          row.names = FALSE)

## table 4 -------------------
df <- NULL

for (ii in mod_scen)
{
  x      <- M[[ii]]
  mmb    <- x$ssb[length(x$ssb)]; names(mmb) <- paste0("$\\text{MMB}_{", (x$mod_yrs[length(x$mod_yrs)]+ 1), "}$")
  fofl   <- x$sd_fofl[1]; names(fofl)          <- "$F_\\text{OFL}$"
  OFL    <- x$spr_cofl; names(OFL)           <- paste0("$\\text{OFL}_{", (x$mod_yrs[length(x$mod_yrs)]+ 1), "}$")
  Bmsy   <- x$spr_bmsy; names(Bmsy)          <- "$B_\\text{MSY}$"
  B_Bmsy <- x$spr_depl; names(B_Bmsy)          <- "$MMB/B_\\text{MSY}$"
  ABC    <- OFL * 0.75; names(ABC)            <- paste0("$\\text{ABC}_{", (x$mod_yrs[length(x$mod_yrs)]+ 1), "}$")
  v      <- c(mmb, Bmsy, fofl, OFL, ABC)
  df     <- cbind(df, v)
}


## archieved -----------------
# old ssb last uncertainty from projections ------

#ssb_pr <- .get_ssb_dfKP_2(M[1:2])

# need to run mcmc and projections here 
# last years ref model
un_ssb <- read.csv(here::here("./SMBKC/smbkc_19/model_1/projections/proj_1/d/uncertainty_ssb_2019.csv"))
# current years ref model
un_ssb2 <- read.csv(here::here("./SMBKC/smbkc_20/model_1/projections/proj_1/d/uncertainty_ssb_2020.csv"))

# ssb vector only includes model years - here crab year 1978 to 2019 does NOT include projection, need to add
#   projection year for graphical purposes
ssb_last <- data.frame("Model" = names(M[1:2]),
                       "year" = c(cur_yr-1, cur_yr),
                       #"year" = c(cur_yr-1, cur_yr), 
                       "ssb" = c(M[[1]]$spr_bmsy * M[[1]]$spr_depl,
                                 M[[2]]$spr_bmsy * M[[2]]$spr_depl),
                       "lb" = c(un_ssb$lci, un_ssb2$lci), # need to update these from .csv output
                       "ub" = c(un_ssb$uci, un_ssb2$uci)) 
# should be current crab year; update with lb and ub from projection file
# update with 95% credible interval

# SSB lst yr / current yr base model-----------
ssb <- .get_ssb_df(M[mod_scen]) # ssb now does NOT include projection year so only up to 2018 crab year - 2019 projection (example)
head(ssb)
tail(ssb)

# ssb current year uncertainty
un_ssb_ref <- read.csv(here::here("./SMBKC/smbkc_20/model_1/projections/proj_1/d/uncertainty_ssb_2020.csv"))#
un_ssb_nopot <- read.csv(here::here("./SMBKC/smbkc_20/model_2/projections/proj_1/d/uncertainty_ssb_2020.csv")) #need to run
un_ssb_fixR <- read.csv(here::here("./SMBKC/smbkc_20/model_1_rfix_TPL/projections/proj_1/d/uncertainty_ssb_2020.csv")) #need to run
#
#un_ssb_cv <- read.csv(here::here("./SMBKC/smbkc_19a/model_1a/projections/proj_1/d/uncertainty_ssb_2019.csv")) #need to run
#un_ssb_cv2 <- read.csv(here::here("./SMBKC/smbkc_19a/model_1b/projections/proj_1/d/uncertainty_ssb_2019.csv")) #need to run
#un_ssb_q <- read.csv(here::here("./SMBKC/smbkc_19a/model_3/projections/proj_1/d/uncertainty_ssb_2019.csv")) #need to run

#un_ssb2 <- read.csv(here::here("./SMBKC/smbkc_19/model_1/projections/proj_5/d/uncertainty_ssb_2019.csv")) #
# temporary way to estimate uncertainty in proj year
#ssb %>% filter(Model == "model 16.0 (ref)") %>% filter(year == 2018) -> temp1
#temp1 %>% 
#  mutate(lper = lb/ssb, uper = ub/ssb) -> temp2

# ssb vector only includes model years - here crab year 1978 to 2019 does NOT include projection, need to add
#   projection year for graphical purposes
ssb_last <- data.frame("Model" = names(M[mod_scen]),
                       "year" = c(cur_yr, cur_yr, cur_yr), 
                       "ssb" = c(M[[2]]$spr_bmsy * M[[2]]$spr_depl,
                                 M[[3]]$spr_bmsy * M[[3]]$spr_depl, 
                                 M[[4]]$spr_bmsy * M[[4]]$spr_depl),
                       "lb" = c(un_ssb_ref$lci, un_ssb_fixR$lci, un_ssb_nopot$lci), # need to update these from .csv output
                       "ub" = c(un_ssb_ref$uci, un_ssb_fixR$uci, un_ssb_nopot$uci))
#"ub" = c(M[[2]]$spr_bmsy * M[[2]]$spr_depl*temp2$uper,
#M[[3]]$spr_bmsy * M[[3]]$spr_depl*temp2$uper))
#"lb" = c(un_ssb_ref$lci, un_ssb_fit$lci, un_ssb_cv$lci), # need to update these from .csv output
#"ub" = c(un_ssb_ref$uci, un_ssb_fit$uci, un_ssb_cv$uci)) 
# should be current crab year; update with lb and ub from projection file
# update with 95% credible interval#
ssb %>% 
  bind_rows(ssb_last) -> ssb2