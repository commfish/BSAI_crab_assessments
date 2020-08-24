# K.Palof  / 8-20-2020

# Code for plotting output of GMACS models for SMBKC
# Taken from Jim Ianellii https://github.com/seacode/gmacs/tree/develop/examples/smbkc_18a/model_1 but updated 

# Model or Model(s) plotted here: 
# Stock: SMBKC
# Year and timing: 2020 - models for sept 2020
# Model: model_1_app3_low - model with fake 2020 survey data based on fit app3a and multiplicative quantiles 
#- see plot.R in model 1 folder

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

mod_names <- c("2020 base - App 3 low")
.MODELDIR = c("./SMBKC/smbkc_20/model_1_app3_low/") # directory where the model results are
.THEME    = theme_bw(base_size = 12, base_family = "")
.OVERLAY  = TRUE
.SEX      = c("Aggregate","Male")
.FLEET    = c("Pot","Trawl bycatch","Fixed bycatch","NMFS Trawl","ADFG Pot")
.TYPE     = c("Retained","Discarded", "Retained & Discarded")
.SHELL    = c("Aggregate","Aggregate")
.MATURITY = c("Aggregate")
.SEAS     = c("Annual")
.FIGS     = c("./SMBKC/smbkc_20/model_1_app3_low/figure/")
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

plot_ssb(M) #doesn't show projection year, which is current year
ggsave(paste0(.FIGS, "ssb.png"), width = ww, height = hh)
dev.off()

plot_recruitment(M)
ggsave(paste0(.FIGS, "recruitment.png"), width = ww, height = hh)
dev.off()


## saved output for App 3 comparison -------------
ssb <- .get_ssb_df(M) # ssb now does NOT include projection year so only up to 2018 crab year - 2019 projection (example)
head(ssb)
# ssb vector only includes model years - here crab year 1978 to 2019 does NOT include projection, need to add
#   projection year for graphical purposes
# ssb current year uncertainty
ssb_last <- data.frame("Model" = mod_names, "year" = cur_yr, "ssb" = M[[1]]$spr_bmsy * M[[1]]$spr_depl, 
                       "lb" = M[[1]]$spr_bmsy * M[[1]]$spr_depl, 
                       "ub" = M[[1]]$spr_bmsy * M[[1]]$spr_depl) 
ssb %>% 
  bind_rows(ssb_last) -> ssb
write.csv(ssb, paste0(.FIGS, paste0("ssb_", cur_yr, "_app3_low.csv")), row.names = FALSE)
#write.csv(ssb, paste0(.FILES, "ssb_.csv"), row.names = FALSE)


rec <- .get_recruitment_df(M)
head(rec)
rec %>% 
  mutate(recruit = exp(log_rec)) %>% 
  select(year, sex, recruit, lb, ub) %>% 
  mutate(recruit_tons = recruit*0.000748427) %>% 
  write.csv(paste0(.FIGS, paste0("recruitment_output_", cur_yr, "_app3_low.csv"))) 

temp <- data.frame(year = cur_yr, 
                   avgr = rec$rbar[1], 
                   bmsy = M[[1]]$spr_bmsy,
                   mmb_terminal = M[[1]]$spr_bmsy * M[[1]]$spr_depl, 
                   status = M[[1]]$spr_depl, 
                   f_ofl = M[[1]]$sd_fofl[1],
                   OFL = M[[1]]$spr_cofl/1000,
                   type = "App3_L")
temp
write.csv(temp, paste0(.FIGS, "summary_app3_low.csv"), row.names = FALSE)

rec$rbar[1]
M[[1]]$spr_bmsy
M[[1]]$spr_bmsy * M[[1]]$spr_depl
M[[1]]$spr_depl
M[[1]]$sd_fofl[1]
M[[1]]$spr_cofl/1000


## summary of this data compiled in smbkc_20/retrospective_model_1/summary_figures_analysis.R

### OFL --------
M[[1]]$sd_fofl[1]
M[[1]]$spr_cofl

# the OFL calculations do not appear correct, Jie helped me to do them "by hand" attempt here to do so in R.

# 2020 numbers
.get_numbers_df(M) -> numbers
numbers[124:129, ] -> numbers
numbers %>% 
  mutate(Year = c(2019, 2019, 2019, 2020, 2020, 2020)) %>% 
  select(Model, variable = mp, Year, N) %>% 
  spread(Year, N) -> numbers20
numbers20
# selectivity 2020
.get_selectivity_df(M) -> selectivity

selectivity %>% 
  filter(year == 2009) %>% 
  spread(fleet, value) %>% 
  filter(type == "Capture") %>% 
  select(Model, variable, Pot, `Trawl bycatch`) ->sel20a

selectivity %>% 
  filter(year == 2009) %>% 
  spread(fleet, value) %>% 
  filter(type == "Retained") %>% 
  select(Model, variable, Pot_retain = Pot) -> sel20b

sel20a %>% 
  left_join(sel20b) -> sel20
sel20
# weight - at - length
M[[1]]$mean_wt[43,]
M[[1]]$mid_points

weight <- data.frame(variable = M[[1]]$mid_points, weight = M[[1]]$mean_wt[43,])

# selectivity by fleet

s_fleet <- data.frame(M[[1]]$slx_capture)
s_fleet %>% 
  rename(Year = V1, male = V2, fleet = V3, '97.5' = V4, '112.5' = V5, '127.5' = V6) -> s_fleet

s_fleet %>% 
  filter(Year == 2018) -> s_fleet_cur

# combine 
sel20 %>% 
  left_join(numbers20) %>% 
  left_join(weight) -> ofl_data
ofl_data

M[[1]]$sd_fofl[1] -> fofl

# calculate OFL step 1 --------
ofl_data %>% 
  mutate(step1 = `2020`*weight*(1-exp(-fofl))*Pot*(Pot_retain+(1-Pot_retain)*.20), 
         step2 = (`2019`- step1/weight)*exp(-0.44)*`Trawl bycatch`*(1-exp(-0.0016))*weight*0.5) -> ofl_data2

ofl_data2 %>% 
  group_by(Model) %>% 
  summarise(st1 = sum(step1), st2 = sum(step2)) %>% 
  mutate(OFL_2020 = st1 + st2) %>% 
  as.data.frame() %>% 
  write.csv(paste0(.FIGS, "ofl_calc.csv"), row.names = FALSE) 
