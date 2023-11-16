# may 2023 model explorations 
# BBRKC, katie.palof@alaska.gov 
# 4-22-23

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
               "23.1a inc CV_q", "22.0 1985", "23.3 Mest_incCV")#
#mod_names <- c("16.0 (2019)", "16.0 (2020)", "16.0a (fix R ter)", "20.1 (no pot)")
.MODELDIR = c(paste0(here::here(), "/BBRKC/bbrkc_22f/model_211b/"),
              paste0(here::here(), "/BBRKC/bbrkc_23s/model_211b/"),#, 
              paste0(here::here(), "/BBRKC/bbrkc_23s/model_23_0/"),
              paste0(here::here(), "/BBRKC/bbrkc_23s/model_23_0a/"), 
              paste0(here::here(), "/BBRKC/bbrkc_23s/model_23_0b/"), 
              paste0(here::here(), "/BBRKC/bbrkc_23s/model_231a/"), 
              paste0(here::here(), "/BBRKC/bbrkc_23s/model_22/"), 
              paste0(here::here(), "/BBRKC/bbrkc_23s/model_23_3/")) #need to update these model options
.THEME    = list(theme_bw(base_size = 12, base_family = ""), scale_fill_manual(values=cbPalette), 
                 scale_colour_manual(values=cbPalette))
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

mod_q <- c(2,6,8)
mod_yr <- c(2,7)

ww <- 6
hh <- 5
update_geom_defaults("line", list(size = 1.75))


### figures from gmr ---------------
plot_datarangeSM(M[2])
# USE jie's plot this isn't picking up all the fleets / indexes

# ref cpues ------------
plot_cpue(M[1:2], "NMFS Trawl", ylab = "Survey biomass (t)")
ggsave(paste0(.FIGS, "trawl_cpue_ref.png"), width = ww*1.3, height = hh*.9)

plot_cpue(M[1:2], "BSFRF survey", ylab = "Survey biomass (t)")
ggsave(paste0(.FIGS, "BSFRF_survey_ref.png"), width = ww*1.3, height = hh)

plot_cpue(M[1:2])
ggsave(paste0(.FIGS, "cpue_ref_both.png"), width = ww*2.5, height = hh)

plot_recruitment(M[2]) # this isn't working with output from sept 2022 models, works with udpated GMACS and gmr
ggsave(paste0(.FIGS, "recruit_ref.png"), width = ww*1.08, height = hh)

### !!!ssb ref_new GMACS ---------
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
  ggtitle("Model scenarios") +
  ylab("Mature male biomass (t) on 15th February") + xlab("Year") +
  .THEME
ggsave(paste0(.FIGS, "ref_ssb_wprojected_yr_tall.png"), width = ww*1.3, height = hh*1.25)
ggsave(paste0(.FIGS, "ref_ssb_wprojected_yr.png"), width = ww*1.1, height = hh*1.1)

ssb <- .get_ssb_dfKP(M[1:2])
ssb2 <- .get_ssb_dfKP_2(M[2]) # gets ssb with sd_last_ssb from std file



## !!fishing mortality ------
#plot_F(M[2]) **FIX** bring in this from model 1 for now.
plot_F(M[mod_scen])
ggsave(paste0(.FIGS, "fishing_mortality_mod_scen.png"), width = ww*1.25, height = hh)
plot_F(Mbase)
ggsave(paste0(.FIGS, "fishing_mortality.png"), width = ww*1.25, height = hh)

## !!selectivity ----------

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

# selectivity mod -q
plot_selectivity_kjp(M[mod_q], "NMFS Trawl")
ggsave(paste0(.FIGS, "selectivity_23s_trawl_MOD_Q.png"), width = ww*1.50, height = 0.85*hh)

plot_selectivity_kjp(M[mod_q], "BSFRF survey")
ggsave(paste0(.FIGS, "selectivity_23s_BSFRF_MOD_Q.png"), width = ww*1.50, height = 0.85*hh)

# selectivity mod_yr
plot_selectivity_kjp(M[mod_yr], "NMFS Trawl")
ggsave(paste0(.FIGS, "selectivity_23s_trawl_MOD_year.png"), width = ww*1.50, height = 0.85*hh)


# experiment 
plot_selectivity_kjp(M[2:3], "NMFS Trawl", ctype = "Capture", tlab = "Model")
mdf <- .get_selectivity_df(M[2:3])

mdf <- subset(mdf, fleet == "NMFS Trawl")
mdf <- subset(mdf, type == "Capture")

# !!!moliting probability -------
#plot_molt_prob_sex(M[2:6], "Male") # didn't quite do what i wanted
plot_molt_prob(M[1:2])
ggsave(paste0(.FIGS, "molt_prob_ref.png"), width = ww*1.5, height = hh*1.5)

plot_molt_prob(M[2:6])
ggsave(paste0(.FIGS, "molt_prob_mod_scen.png"), width = ww*1.5, height = hh*1.5)

plot_molt_prob(M[mod_q])
ggsave(paste0(.FIGS, "molt_prob_mod_scen_q.png"), width = ww*1.5, height = hh*1.5)

plot_molt_prob(M[c(2,5)])
ggsave(paste0(.FIGS, "molt_prob_base_M0.31.png"), width = ww*1.5, height = hh*1.5)







# mod_scen cpues----------
plot_cpue(M[mod_scen], "NMFS Trawl", ylab = "Survey biomass (t)")
ggsave(paste0(.FIGS, "trawl_cpue_ref.png"), width = ww*1.3, height = hh*.9)

plot_cpue(M[1:2], "BSFRF survey", ylab = "Survey biomass (t)")
ggsave(paste0(.FIGS, "BSFRF_survey_ref.png"), width = ww*1.3, height = hh)

plot_cpue(M[1:2])
ggsave(paste0(.FIGS, "cpue_ref_both.png"), width = ww*2.5, height = hh)

plot_recruitment(M[2]) # this isn't working with output from sept 2022 models



## ssb -----------
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

### ssb mod scen ---------
ssb <- .get_ssb_dfKP(M[2:4])
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

