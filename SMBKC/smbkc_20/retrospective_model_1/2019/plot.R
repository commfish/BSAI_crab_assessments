# K.Palof  8-2-2020

# Code for plotting output of GMACS models for SMBKC
# Taken from Jim Ianellii https://github.com/seacode/gmacs/tree/develop/examples/smbkc_18a/model_1 but updated 

## file set up for examining retrospective output. Focus on SSB time series.

# Model or Model(s) plotted here: 
# Stock: SMBKC
# Year and timing: 2020 retrospecive_model_1 
# Model: model_1_19

# load ------------
#require(devtools)
#devtools::install_github("seacode/gmacs", subdir = "/gmr", ref = "develop") #, INSTALL_opts="--no-staged-install") #- only needs to be performed once, but needs to be 
# done again when updates are made to the package
# if gmr is updated and above doesn't work try:
# go to Build above - direct it to the gmr folder - press OK. Over on right in the Build tab (upper right hand side) - 
# click "install and restart"

require(gmr)
source("./SMBKC/code/functions.R") 
source("./SMBKC/code/helper.R") 
source("./SMBKC/smbkc_19a/doc/gmr_functions2020.R") 

# Model 1 plots -------------------------
cur_yr <- 2019 # update annually 

mod_names <- c("retro_19")
.MODELDIR = c("./SMBKC/smbkc_20/retrospective_model_1/2019/") # directory where the model results are
.THEME    = theme_bw(base_size = 12, base_family = "")
.OVERLAY  = TRUE
.SEX      = c("Aggregate","Male")
.FLEET    = c("Pot","Trawl bycatch","Fixed bycatch","NMFS Trawl","ADFG Pot")
.TYPE     = c("Retained","Discarded", "Retained & Discarded")
.SHELL    = c("Aggregate","Aggregate")
.MATURITY = c("Aggregate")
.SEAS     = c("Annual")
.FIGS     = c("./SMBKC/smbkc_20/retrospective_model_1/2019/output/")
.FILES    = c("./SMBKC/smbkc_20/retrospective_model_1/combined_data/")

fn       <- paste0(.MODELDIR, "gmacs")
M        <- lapply(fn, read_admb) #need .prj file to run gmacs and need .rep file here
names(M) <- mod_names

ww <- 6
hh <- 5

# Plots from Jim Ianelli's -------------------------------
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

## saved output for retrospective -------------
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
write.csv(ssb, paste0(.FILES, paste0("ssb_", cur_yr, ".csv")), row.names = FALSE)


rec <- .get_recruitment_df(M)
head(rec)
rec %>% 
  mutate(recruit = exp(log_rec)) %>% 
  select(year, sex, recruit, lb, ub) %>% 
  mutate(recruit_tons = recruit*0.000748427) %>% 
  write.csv(paste0(.FILES, paste0("recruitment_output_", cur_yr, ".csv"))) 

temp <- data.frame(year = cur_yr, 
                   avgr = rec$rbar[1], 
                   bmsy = M[[1]]$spr_bmsy,
                   mmb_terminal = M[[1]]$spr_bmsy * M[[1]]$spr_depl, 
                   status = M[[1]]$spr_depl, 
                   OFL = M[[1]]$sd_fofl[1], 
                   type = "retro")
write.csv(temp, paste0(.FILES, "summary.csv"))
          
rec$rbar[1]
M[[1]]$spr_bmsy
M[[1]]$spr_bmsy * M[[1]]$spr_depl
M[[1]]$spr_depl
M[[1]]$sd_fofl[1]

# SMBKC plots new  -------------
# SSB -----------
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
ggsave(paste0(.FIGS, "ssb19_wprojected_yr.png"), width = ww, height = hh)
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
         


