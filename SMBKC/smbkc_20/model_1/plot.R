# K.Palof  11-20-19/ 04-01-20/ 7-28-20

# Code for plotting output of GMACS models for SMBKC
# Taken from Jim Ianellii https://github.com/seacode/gmacs/tree/develop/examples/smbkc_18a/model_1 but updated 

# Model or Model(s) plotted here: 
# Stock: SMBKC
# Year and timing: 2020 - models for sept 2020
# Model: model_1

# load ------------
#require(devtools)
#devtools::install_github("seacode/gmacs", subdir = "/gmr", ref = "develop") #, INSTALL_opts="--no-staged-install") #- only needs to be performed once, but needs to be 
# done again when updates are made to the package
# if gmr is updated and above doesn't work try:
# go to Build above - "configure build tools" - direct it to the gmr folder - press OK. Over on right in the Build tab (upper right hand side) - 
# click "install and restart"

require(gmr)
source("./SMBKC/code/functions.R") 
source("./SMBKC/code/helper.R") 
source("./SMBKC/smbkc_19a/doc/gmr_functions2020.R") 

# Model 1 plots -------------------------
cur_yr <- 2020 # update annually 

mod_names <- c("model 16.0 (ref)")
.MODELDIR = c("./SMBKC/smbkc_20/model_1/") # directory where the model results are
.THEME    = theme_bw(base_size = 12, base_family = "")
.OVERLAY  = TRUE
.SEX      = c("Aggregate","Male")
.FLEET    = c("Pot","Trawl bycatch","Fixed bycatch","NMFS Trawl","ADFG Pot")
.TYPE     = c("Retained","Discarded", "Retained & Discarded")
.SHELL    = c("Aggregate","Aggregate")
.MATURITY = c("Aggregate")
.SEAS     = c("Annual")
.FIGS     = c("./SMBKC/smbkc_20/model_1/figure/")

fn       <- paste0(.MODELDIR, "gmacs")
M        <- lapply(fn, read_admb) #need .prj file to run gmacs and need .rep file here
names(M) <- mod_names

ww <- 6
hh <- 5

# Plots from Jim Ianelli's -------------------------------
plot_recruitment_size(M)
ggsave(paste0(.FIGS, "rec_size.png"), width = ww*2.5, height = hh*1.5)
dev.off()

plot_catch(M) # adjusted .TYPE to reflect what it should be check this in the future
ggsave(paste0(.FIGS, "catch.png"), width = ww*1.2, height = hh*1.2)
dev.off()

plot_cpue(M, ShowEstErr = TRUE)
ggsave(paste0(.FIGS, "cpue.png"), width = ww*2.5, height = hh)
dev.off()

plot_cpue(M, ShowEstErr = TRUE, "NMFS Trawl", ylab = "Survey biomass (t)")
ggsave(paste0(.FIGS, "cpue_trawl.png"), width = ww, height = hh)
dev.off()

plot_cpue(M, ShowEstErr = TRUE, "ADFG Pot", ylab = "Pot survey CPUE (crab/potlift)")
ggsave(paste0(.FIGS, "cpue_pot.png"), width = ww, height = hh)
dev.off()

# smbkc edited function - see gmr_functions2020.R
plot_natural_mortality2(M, plt_knots = FALSE)
ggsave(paste0(.FIGS, "M_t.png"), width = ww, height = hh)
dev.off()

plot_ssb(M) #doesn't show projection year, which is current year
ggsave(paste0(.FIGS, "ssb.png"), width = ww, height = hh)
dev.off()

plot_recruitment(M)
ggsave(paste0(.FIGS, "recruitment.png"), width = ww, height = hh)
dev.off()

plot_selectivity(M) # **FIX** not displaying well.  working.
ggsave(paste0(.FIGS, "selectivity.png"), width = ww*1.5, height = hh*1.5)
dev.off()

# plot_growth_transition(M)
# ggsave(paste0(.FIGS, "growth_transition.png"), width = ww*1.5, height = hh*1.5)
# dev.off()
# 
# plot_molt_prob(M)
# ggsave(paste0(.FIGS, "molt_prob.png"), width = ww*1.5, height = hh*1.5)
# dev.off()
# 
# plot_size_transition(M)
# ggsave(paste0(.FIGS, "size_transition.png"), width = ww*1.5, height = hh*1.5)
# # dev.off()
# 
# plot_growth_inc(M)
# ggsave(paste0(.FIGS, "gi.png"), width = ww, height = hh)
# dev.off()
# 
# plot_length_weight(M)
# ggsave(paste0(.FIGS, "length_weight.png"), width = ww, height = hh)
# dev.off()
# 
plot_numbers(M) # not updating for 2019 **FIX**
ggsave(paste0(.FIGS, "numbers.png"), width = ww*2, height = hh*1.5)
dev.off()
# 
# plot_numbers(M, subsetby = c("1975","2014"))
# ggsave(paste0(.FIGS, "numbers.png"), width = ww*1.2, height = hh)
# dev.off()
# 

# updated .tpl to include output. Not sure why but this was taken out by Jie? ask him
plot_size_comps(M, 1)
ggsave(paste0(.FIGS, "lf_1.png"), width = ww*2, height = hh*1.5)
dev.off()
 
plot_size_comps(M, 2)
ggsave(paste0(.FIGS, "lf_2.png"), width = ww*2, height = hh*1.5)
dev.off()
 
plot_size_comps(M, 3)
ggsave(paste0(.FIGS, "lf_3.png"), width = ww*2, height = hh*1.5)
dev.off()
 
#.get_sizeComps_df
# plot_size_comps(M, 4)
# ggsave(paste0(.FIGS, "lf_4.png"), width = ww*2, height = hh*1.5)
# dev.off()
# 
# plot_size_comps(M, 5)
# ggsave(paste0(.FIGS, "lf_5.png"), width = ww*2, height = hh*1.5)
# dev.off()
# 
# plot_size_comps(M, 6)
# ggsave(paste0(.FIGS, "lf_6.png"), width = ww*2, height = hh*1.5)
# # # dev.off()

plot_cpue_res(M, "NMFS Trawl")
ggsave(paste0(.FIGS, "cpue_trawl_residuals.png"), width = ww*2.5, height = hh)
dev.off()

plot_cpue_res(M, "ADFG Pot")
ggsave(paste0(.FIGS, "cpue_pot_residuals.png"), width = ww*2.5, height = hh)
dev.off()

# fishing mortality ----
plot_F(M)
plot_F2(M)
ggsave(paste0(.FIGS, "fishing_mortality.png"), width = ww*1.2, height = hh*1.2)

# SMBKC plots new  -------------
# SSB -----------
# ** FIX ** use correct variablity for this figure by running projections. 
ssb <- .get_ssb_df(M) # ssb now does NOT include projection year so only up to 2018 crab year - 2019 projection (example)
head(ssb)
# ssb vector only includes model years - here crab year 1978 to 2019 does NOT include projection, need to add
#   projection year for graphical purposes
# ssb current year uncertainty
un_ssb <- read.csv(here::here("./SMBKC/smbkc_19/model_1/projections/proj_1/d/uncertainty_ssb_2019.csv"))
ssb_last <- data.frame("year" = cur_yr, "ssb" = M[[1]]$spr_bmsy * M[[1]]$spr_depl, 
                       "lb" = M[[1]]$spr_bmsy * M[[1]]$spr_depl, 
                       "ub" = M[[1]]$spr_bmsy * M[[1]]$spr_depl) 
#ssb_last <- data.frame("year" = cur_yr, "ssb" = M[[1]]$spr_bmsy * M[[1]]$spr_depl, 
#                       "lb" = un_ssb$lci, 
#                       "ub" = un_ssb$uci)

# should be current crab year; update with lb and ub from projection file
# update with 95% credible interval
ssb %>% 
  bind_rows(ssb_last) -> ssb

ssb %>% 
  ggplot(aes(year, ssb)) +
    geom_line() +
    geom_ribbon(aes(x=year, ymax = ub, ymin = lb), alpha = 0.2) +
    expand_limits(y=0) +
    scale_y_continuous(expand = c(0,0)) +
    #geom_hline(data = Bmsy_options, aes(yintercept = Bmsy), color = c("blue", "red"), 
    #           lty = c("solid", "dashed"))+
    #geom_text(data = Bmsy_options, aes(x= 1980, y = Bmsy, label = label), 
    #          hjust = -0.45, vjust = 1.5, nudge_y = 0.05, size = 3.5) +
    ggtitle("Reference model (16.0)") +
    ylab("Mature male biomass (t) on 15th February") + xlab("Year") +
    .THEME
ggsave(paste0(.FIGS, "ssb20_wprojected_yr.png"), width = ww, height = hh)
dev.off()

# Bmsy proxy table --------
# need ssb from above
ssb %>% 
  filter(year <= cur_yr-1) %>% 
  summarise(Bmsy = mean(ssb)) %>% 
  mutate(years = "1978-2018", label = "1978-2018 B_MSY" )-> Bmsy
ssb %>% 
  filter(year <= cur_yr-1) %>% 
  filter(year >= 1996) %>% 
  summarise(Bmsy = mean (ssb)) %>% 
  mutate(years = "1996-2018", label = "1996-2018 B_MSY")->Bmsy2

Bmsy %>% 
  bind_rows(Bmsy2) %>% 
  mutate(Bmsy50 = 0.5*Bmsy) -> Bmsy_options

Bmsy_options %>% 
  mutate(reduction = (Bmsy-Bmsy[1])/ Bmsy[1])


Bmsy = M[[1]]$spr_bmsy
MMB = M[[1]]$spr_bmsy * M[[1]]$spr_depl
B_Bmsy = M[[1]]$spr_depl
Fofl = M[[1]]$sd_fofl[1] # Fofl for current year
years = as.character(M[[1]]$spr_syr)
as.character(M[[1]]$spr_nyr)


ofl_df <- data.frame(Bmsy, MMB, B_Bmsy, Fofl, years)
write_csv(ofl_df, paste0(.MODELDIR, '/ofl_table_', mod_names, '.csv'))

ssb %>% 
  ggplot(aes(year, ssb)) +
  geom_line() +
  geom_ribbon(aes(x=year, ymax = ub, ymin = lb), alpha = 0.2) +
  expand_limits(y=0) +
  scale_y_continuous(expand = c(0,0)) +
  scale_y_continuous(limits = c(0,max(ssb$ub, na.rm = TRUE)),
                     breaks= seq(min(0), max(max(ssb$ub, 
                                                 na.rm = TRUE)), by = 2000)) +
  geom_hline(data = Bmsy_options, aes(yintercept = Bmsy), color = c("blue", "red"), 
             lty = c("solid", "dashed"))+
  geom_text(data = Bmsy_options, aes(x= 1980, y = Bmsy, label = label), 
            hjust = -1.25, vjust = 1.5, nudge_y = 0.05, size = 3.5) +
  ggtitle("Reference model (19.0)") +
  ylab("Mature male biomass (t) on 15th February") + xlab("Year") +
  .THEME + 
  FNGr::theme_sleek()
ggsave(paste0(.FIGS, "ssb19_Bmsy_wprojected_yr.png"), width = ww, height = hh)
dev.off()

### cpue ---------------
cpue <- .get_cpue_df(M)

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

# pot survey
cpue %>% 
  filter(fleet == "ADFG Pot") %>% 
  ggplot(aes(year, cpue)) +
  expand_limits(y = 0) +
  geom_pointrange(aes(year, cpue, ymax = ub, ymin = lb), col = "black") +
  geom_pointrange(aes(year, cpue, ymax = ube, ymin = lbe), color = "red", 
                  shape = 1, linetype = "dotted", position = position_dodge(width = 1)) +
  geom_line(aes(year, pred)) +
  labs(x = "Year", y = "CPUE") +
  .THEME

## predicted cpue trawl for executive summary - line 89
cpue %>% 
  filter(fleet==.FLEET[4]) %>% 
  transmute(x = round(100*pred/mean(pred),0)) %>% 
  tail(1) %>%
  .$x


# recruitment -------------
rec <- .get_recruitment_df(M)
head(rec)

#rbar is estimated in model
# need to pull rbar from model output with different recruitment years
rec %>% 
  summarise(meanR = mean(exp(log_rec)/1000000)) %>% 
  mutate(years = "1978-2018")-> avgR

#mutate(Bmsy50 = 0.5*Bmsy) -> Bmsy_options
avgR # see above is calculated average recruitment for each time series
rec$rbar[1]

# recruitment plot ----------
rec %>% 
  ggplot(aes(year, y = exp(log_rec)/1000000)) +
  geom_line() +
  geom_ribbon(aes(x=year, ymax = ub/1000000, ymin = lb/1000000), alpha = 0.25) +
  expand_limits(y=0) +
  ggtitle("base model 2019") +
  ylab("Recruitment (millions of individuals)") + xlab("Year") +
  geom_hline(aes(yintercept = rbar[1]/1000000), color = "gray25") +
  geom_text(aes(x = 2000, y = rbar[1]/1000000, label = "R_bar"), 
            hjust = -0.45, vjust = -0.75, nudge_y = 0.05, size = 3.0) +
  .THEME +
  #geom_hline(data = avgR_options, aes(yintercept = meanR), color = c("blue", "red"), 
  #           lty = c("solid", "dashed"))+
  #geom_text(data = avgR_options, aes(x= 1980, y = meanR, label = years), 
  #          hjust = -2.45, vjust = 1.5, nudge_y = 0.05, size = 3.5) 
ggsave(paste0(.FIGS, "recruitment_line_with years.png"), width = ww, height = hh)
dev.off()
         
## recruitment est for ESP ------
# recruitment output for ecosystem indicators
rec %>% 
  mutate(recruit = exp(log_rec)) %>% 
  select(year, sex, recruit, lb, ub) %>% 
  mutate(recruit_tons = recruit*0.000748427) %>% 
  write.csv(paste0(.FIGS, "recruitment_output.csv")) 

#this is in number of individuals
# weight for first size bin is 0.000748427 (from line 31 of .ctl file)
# 0.0007, 0.0012, 0.0019  I think these weights are already in tons???? check with Jie
# 524010.0422*0.0019 + 158547.8651*0.0012 seems about corret for ssb for 2018 


### need option with new average recruitment    

# abundance -----
num <- .get_numbers_df(M)

lw1 <- .get_length_weight_df(M) #not working



### OFL --------
M[[base_model_1]]$spr_cofl

## Dynamic B0 ----
#.get_dynB0_df(M)  # not currently in output, can I add this?

plot_dynB0(M) 
ggsave(paste0(.FIGS, "dynamicB0.png"), width = ww*2.5, height = hh)
dev.off()
# currently working! see updates to .tpl file 
# From Jim I: Also, I think a commented out line needs to be reinstated.  
#             So comment out lines 1786 and 1787 and UNcomment lines 1800 and 1801, 
#             then call it directly: plot_dynB0(model_object) .
## plot selectivity -----------

