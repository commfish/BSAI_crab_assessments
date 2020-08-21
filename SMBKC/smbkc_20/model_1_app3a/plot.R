# K.Palof  11-20-19/ 04-01-20/ 7-28-20 / 8-20-2020

# Code for plotting output of GMACS models for SMBKC
# Taken from Jim Ianellii https://github.com/seacode/gmacs/tree/develop/examples/smbkc_18a/model_1 but updated 

# Model or Model(s) plotted here: 
# Stock: SMBKC
# Year and timing: 2020 - models for sept 2020
# Model: model_1_app3a - model with fake 2020 survey data with large CV

# load ------------
#require(devtools)
#devtools::install_github("seacode/gmacs", subdir = "/gmr", ref = "develop") #, INSTALL_opts="--no-staged-install") #- only needs to be performed once, but needs to be 
# done again when updates are made to the package
# if gmr is updated and above doesn't work try:
# go to Build above - "configure build tools" - direct it to the gmr folder - press OK. 
# Over on right in the Build tab (upper right hand side) - 
# click "install and restart"

require(gmr)
source("./SMBKC/code/functions.R") 
source("./SMBKC/code/helper.R") 
source("./SMBKC/smbkc_19a/doc/gmr_functions2020.R") 

# Model 1 plots -------------------------
cur_yr <- 2020 # update annually 

mod_names <- c("2020 base - App 3a")
.MODELDIR = c("./SMBKC/smbkc_20/model_1_app3a/") # directory where the model results are
.THEME    = theme_bw(base_size = 12, base_family = "")
.OVERLAY  = TRUE
.SEX      = c("Aggregate","Male")
.FLEET    = c("Pot","Trawl bycatch","Fixed bycatch","NMFS Trawl","ADFG Pot")
.TYPE     = c("Retained","Discarded", "Retained & Discarded")
.SHELL    = c("Aggregate","Aggregate")
.MATURITY = c("Aggregate")
.SEAS     = c("Annual")
.FIGS     = c("./SMBKC/smbkc_20/model_1_app3a/figure/")
.FILES    = c("./SMBKC/smbkc_20/retrospective_model_1/combined_data/")

fn       <- paste0(.MODELDIR, "gmacs")
M        <- lapply(fn, read_admb) #need .prj file to run gmacs and need .rep file here
names(M) <- mod_names

ww <- 6
hh <- 5

# look at trawl survey cpue -----

plot_cpue(M, ShowEstErr = TRUE)
ggsave(paste0(.FIGS, "cpue.png"), width = ww*2.5, height = hh)
dev.off()

plot_cpue(M, ShowEstErr = TRUE, "NMFS Trawl", ylab = "Survey biomass (t)")
ggsave(paste0(.FIGS, "cpue_trawl.png"), width = ww, height = hh)
dev.off()

# obtain predicted 2020 value --------


### cpue ---------------
cpue <- .get_cpue_df(M)

cpue %>% 
  filter(fleet == "NMFS Trawl") ->cpueT

cpueT %>% 
  filter(year == 2020)

# pred = 1657.498   
# take this back to plot.R file in Model 1 and estimate 25% and 75% percentiles 

## trawl survey
cpue %>% 
  filter(fleet == "NMFS Trawl") %>% 
  ggplot(aes(year, cpue)) +
  expand_limits(y = 0) +
  geom_pointrange(aes(year, cpue, ymax = ub, ymin = lb), col = "black") +
  #geom_pointrange(aes(year, cpue, ymax = ube, ymin = lbe), color = "red", 
  #                shape = 1, linetype = "dotted", position = position_dodge(width = 1)) +
  geom_line(aes(year, pred), linetype = "solid", col = "red") +
  labs(x = "Year", y = "CPUE") +
  .THEME

tail(cpue)
