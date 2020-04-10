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

mod_names <- c("model 18.0", "model 19.0 (ref)", "model 19.1 (fit survey)", "model 19.2 (add CV pot)", "model 19.0a (alt regime)") 
.MODELDIR = c(paste0(here::here(), "/SMBKC/smbkc_18a/model_1/"), 
              paste0(here::here(), "/SMBKC/smbkc_19/model_1/"),
              paste0(here::here(), "/SMBKC/smbkc_19/model_5/"), 
              paste0(here::here(), "/SMBKC/smbkc_19/model_1b/"), 
              paste0(here::here(), "/SMBKC/smbkc_19/model_1a/")) #need to update these model options
.THEME    = theme_bw(base_size = 12, base_family = "") 
.OVERLAY  = TRUE
.SEX      = c("Aggregate","Male")
.FLEET    = c("Pot","Trawl bycatch","Fixed bycatch","NMFS Trawl","ADF&G Pot")
.TYPE     = c("Retained","Discarded", "Retained & Discarded")
.SHELL    = c("Aggregate")
.MATURITY = c("Aggregate")
.SEAS     = c("1","2","3","4","5")
.FIGS     = c("./SMBKC/smbkc_19/doc/safe_figure/")

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

alt_mod <- 5 # alt reference time frame
ref_mod <- 2 # base
rec_mod <- 2 # base
mod_scen<- 2:4 #scenarios you want graphed together

ww <- 6
hh <- 5

# executive summary -----------
.get_cpue_df(Mbase) %>% 
  filter(fleet==.FLEET[4]) %>% 
  mutate(x = round(100*pred/mean(pred),0)) %>% 
  select(x) %>% 
  tail(1) %>%
  .$x -> bio_lt_percent

## data extent -----------
plot_datarange(M[rec_mod])
ggsave(p1, paste0(.FIGS, "data_extent.png"), width = ww, height = hh)

## fig 6/7 2018 safe - 2018 compared to reference model --------
plot_cpue(M[1:2], "NMFS Trawl", ylab = "Survey biomass (t)") 
ggsave(paste0(.FIGS, "trawl_cpue_ref.png"), width = ww*1.25, height = hh*.9)

plot_cpue(M[1:2], "ADF&G Pot", ylab = "Pot survey CPUE (crab/potlift)")
ggsave(paste0(.FIGS, "pot_cpue_ref.png"), width = ww*1.5, height = hh)

plot_cpue(M[1:2])
ggsave(paste0(.FIGS, "cpue_ref_both.png"), width = ww*2.5, height = hh)

### Sensitivity of new data in 2018 on estimated recruitment ; 1978-2018
A <- M
for (i in c(1,2)) {
  ii <- which(A[[i]]$fit$names %in% "sd_log_recruits"); ii <- ii[length(ii)]
  A[[i]]$fit$est[ii] <- NA
  A[[i]]$fit$std[ii] <- NA
}
plot_recruitment(A[1:2])
plot_recruitment(M[1:2])
ggsave(paste0(.FIGS, "recruit_ref.png"), width = ww*1.5, height = hh)


## !!fishing mortality ------
#plot_F(M[2]) 
plot_F2(M[2])
ggsave(paste0(.FIGS, "fishing_mortality.png"), width = ww*1.5, height = hh)

plot_F2 <- function (M, scales = "free_y", xlab = "Year", ylab = "F", 
          mlab = "Model") 
{
  xlab <- paste0("\n", xlab)
  ylab <- paste0(ylab, "\n")
  m <- .get_F_df(M)
  mdf <- m$F
  mdf$season <- paste0("Season: ", mdf$season)
  fbar <- m$fbar
  p <- ggplot(data = mdf) + geom_hline(data = fbar, aes(yintercept = fbar, 
                                                        color = Model), linetype = "dashed", alpha = 0.5) + 
    geom_line(aes(year, F, col = model, group = 1)) + facet_grid(fleet ~ 
                                                                   season, scales = scales) + labs(x = xlab, y = ylab)
  print(p + .THEME + theme(legend.position = c(0.2, 0.9)))
}


## ssb -----------
#"Sensitivity of new data in 2019 on estimated mature male biomass (MMB); 1978-2019. \\label{fig:ssb1}"}
plot_ssb(M[1:2], ylab = "Mature male biomass (tons) on 15 February")
# !!SSB lst yr / current yr base model-----------
ssb <- .get_ssb_df(M[1:2]) # ssb now does NOT include projection year so only up to 2018 crab year - 2019 projection (example)
head(ssb)
tail(ssb)

# Bmsy proxy and SHS level ------
SHS <- c(1978:2012)
ssb %>% 
  filter(Model == "model 19.0 (ref)") %>% 
  mutate(b_msy = mean(ssb)) %>% 
  mutate(SHS_proxy = mean(ssb[year %in% SHS])) %>% 
  group_by(Model) %>% 
  mutate(b_msy/SHS_proxy)


# ssb current year uncertainty
un_ssb <- read.csv(here::here("./SMBKC/smbkc_19/model_1/projections/proj_1/d/uncertainty_ssb_2019.csv"))
un_ssb2 <- read.csv(here::here("./SMBKC/smbkc_18a/model_1/projections/proj_1/d/uncertainty_ssb_2019.csv"))

# ssb vector only includes model years - here crab year 1978 to 2019 does NOT include projection, need to add
#   projection year for graphical purposes
ssb_last <- data.frame("Model" = names(M[1:2]),
                       "year" = c(cur_yr-1, cur_yr), 
                       "ssb" = c(M[[1]]$spr_bmsy * M[[1]]$spr_depl,
                                 M[[2]]$spr_bmsy * M[[2]]$spr_depl),
                       "lb" = c(un_ssb2$lci, un_ssb$lci), # need to update these from .csv output
                       "ub" = c(un_ssb2$uci, un_ssb$uci)) 
# should be current crab year; update with lb and ub from projection file
# update with 95% credible interval
ssb %>% 
  bind_rows(ssb_last) -> ssb


## ssb plot with current year 
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
  .THEME
ggsave(paste0(.FIGS, "lastyr_reference_ssb_wprojected_yr.png"), width = ww*1.25, height = hh)
ggsave(paste0(.FIGS, "PRESENTATION_lastyr_reference_ssb_wprojected_yr.png"), width = ww*1.5, height = hh)


# no need for 2018 figures 10 and 11 because no VAST scenario

# !!ref_recruit ribbons -------------
rec <- .get_recruitment_df(M[1:2])
head(rec)

rec$rbar[1]

# recruitment plot
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
dev.off()

# !!2019 ref recruit ribbon --------------
rec <- .get_recruitment_df(M[2])
head(rec)

rec$rbar[1]

# recruitment plot
rec %>% 
  ggplot(aes(year, y = exp(log_rec)/1000000, group = Model, fill = Model)) +
  geom_line(aes(color = Model)) +
  geom_ribbon(aes(x=year, ymax = ub/1000000, ymin = lb/1000000), alpha = 0.15) +
  expand_limits(y=0) +
  ggtitle("Recruitment reference model") +
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
  ggsave(paste0(.FIGS, "recruitment_ref19_ribbons.png"), width = 1.5*ww, height = hh)
dev.off()



## selectivity ----------
#"Comparisons of the estimated stage-1 and stage-2 selectivities for the different model scenarios (the stage-3 selectivities are all fixed at 1). Estimated selectivities are shown for the directed pot fishery, the trawl bycatch fishery, the fixed bycatch fishery, the NMFS trawl survey, and the ADF&G pot survey. Two selectivity periods are estimated in the directed pot fishery, from 1978-2008 and 2009-2017.\\label{fig:selectivity}", fig.height = 15}
plot_selectivity(M[mod_scen]) 
ggsave(paste0(.FIGS, "selectivity_mod_scen.png"), width = ww*1.5, height = hh)
## ** FIX ** not currently being output in .rep file.  Need to read this from Gmacsall.out 

## recruitment mod scen ----------------
# "Estimated recruitment 1979-2018 comparing model alternatives. The solid horizontal lines in the background represent the estimate of the average recruitment parameter ($\\bar{R}$) in each model scenario.\\label{fig:recruitment}"}
#A <- M
#for (i in mod_scen) {
#  ii <- which(A[[i]]$fit$names %in% "sd_log_recruits"); ii <- ii[length(ii)]
#  A[[i]]$fit$est[ii] <- NA
#  A[[i]]$fit$std[ii] <- NA
#}
#plot_recruitment(A[mod_scen])
plot_recruitment(M[mod_scen])
ggsave(paste0(.FIGS, "recruit_mod_scen.png"), width = ww*1.5, height = hh)

# recruit ribbons -------------
rec <- .get_recruitment_df(M[mod_scen])
head(rec)

#rbar is estimated in model
# need to pull rbar from model output with different recruitment years
rec %>% 
  group_by(Model) %>% 
  summarise(meanR = mean(exp(log_rec)/1000000)) %>% 
  mutate(years = "1978-2018")-> avgR

rec %>% 
  filter(year >= 1996) %>% 
  group_by(Model) %>% 
  summarise(meanR = mean (exp(log_rec)/1000000)) %>% 
  mutate(years = "1996-2018")-> avgR2

avgR %>% 
  bind_rows(avgR2) -> avgR_options
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
  scale_colour_manual(name = "", values = c("red", "dark green", "blue"))+
  scale_fill_manual(name = "", values = c("red", "dark green", "blue")) +
  #geom_hline(aes(yintercept = rbar[1]/1000000), color = "gray25") +
  #geom_text(aes(x = 2000, y = rbar[1]/1000000, label = "R_bar"), 
  #          hjust = -0.45, vjust = -0.75, nudge_y = 0.05, size = 3.0) +
  .THEME +
  #geom_hline(data = avgR_options, aes(yintercept = meanR), color = c("blue", "red"), 
  #           lty = c("solid", "dashed"))+
  #geom_text(data = avgR_options, aes(x= 1980, y = meanR, label = years), 
  #          hjust = -2.45, vjust = 1.5, nudge_y = 0.05, size = 3.5) 
ggsave(paste0(.FIGS, "recruitment_mod_scen_ribbons.png"), width = 1.5*ww, height = hh)
dev.off()



# ssb mod scen ---------------
  
#"Comparisons of estimated mature male biomass (MMB) time series on 15 February during 1978-2018 for each of the model scenarios.\\label{fig:mmb}"}
plot_ssb(M[mod_scen], ylab = "Mature male biomass (tons) on 15 February")
## **FIX** to include 2019 projected value and associate error

# !!SSB model scenarios-----------
# SSB lst yr / current yr base model-----------
ssb <- .get_ssb_df(M[mod_scen]) # ssb now does NOT include projection year so only up to 2018 crab year - 2019 projection (example)
head(ssb)
tail(ssb)

# ssb current year uncertainty
un_ssb_ref <- read.csv(here::here("./SMBKC/smbkc_19/model_1/projections/proj_1/d/uncertainty_ssb_2019.csv"))#
un_ssb_fit <- read.csv(here::here("./SMBKC/smbkc_19/model_5/projections/proj_1/d/uncertainty_ssb_2019.csv")) #need to run
un_ssb_cv <- read.csv(here::here("./SMBKC/smbkc_19/model_1b/projections/proj_1/d/uncertainty_ssb_2019.csv")) #need to run
#un_ssb2 <- read.csv(here::here("./SMBKC/smbkc_19/model_1/projections/proj_5/d/uncertainty_ssb_2019.csv")) #

# ssb vector only includes model years - here crab year 1978 to 2019 does NOT include projection, need to add
#   projection year for graphical purposes
ssb_last <- data.frame("Model" = names(M[mod_scen]),
                       "year" = c(cur_yr, cur_yr, cur_yr), 
                       "ssb" = c(M[[2]]$spr_bmsy * M[[2]]$spr_depl,
                                 M[[3]]$spr_bmsy * M[[3]]$spr_depl,
                                 M[[4]]$spr_bmsy * M[[4]]$spr_depl),
                       "lb" = c(un_ssb_ref$lci, un_ssb_fit$lci, un_ssb_cv$lci), # need to update these from .csv output
                       "ub" = c(un_ssb_ref$uci, un_ssb_fit$uci, un_ssb_cv$uci)) 
# should be current crab year; update with lb and ub from projection file
# update with 95% credible interval
ssb %>% 
  bind_rows(ssb_last) -> ssb


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
  .THEME
ggsave(paste0(.FIGS, "mod_scen_ssb_wprojected_yr.png"), width = ww*1.25, height = hh)
ggsave(paste0(.FIGS, "PRESENTATION_mod_scen_ssb_wprojected_yr.png"), width = ww*1.5, height = hh)



#  ```{r natural_mortality, fig.cap = "Time-varying natural mortality ($M_t$). Estimated pulse period occurs in 1998/99 (i.e. $M_{1998}$). \\label{fig:M_t}"}
plot_natural_mortality(M, knots = NULL, slab = "Model")

#{r trawl_survey_biomass, fig.cap = "Comparisons of area-swept estimates of total (90+ mm CL) male survey biomass (tons) and model predictions for the model scenarios. The error bars are plus and minus 2 standard deviations.\\label{fig:trawl_survey_biomass}"} 
plot_cpue(M[c(mod_scen)],  "NMFS Trawl", ylab = "NMFS survey biomass (t)")
ggsave(paste0(.FIGS, "trawl_biomass_mod_scen.png"), width = ww*1.5, height = hh)
#p.df <- .get_cpue_df(M[mod_scen])
#filter(p.df,fleet=="NMFS Trawl") %>% 
#  ggplot(aes(x=(year),y=cpue,col=Model)) + geom_point(position=position_dodge(0.9) ) + .THEME + xlab("Year") +
#  geom_errorbar(aes(ymin=lbe, ymax=ube), width=.2,position=position_dodge(0.9) ) + geom_line(aes(x=year,y=pred)) + ylab("NMFS survey biomass (t)")
#plot_cpue(M[mod_scen], "NMFS Trawl", ylab = "Survey biomass (tons)") 

#{r pot_survey_cpue, fig.cap = "Comparisons of total (90+ mm CL) male pot survey CPUEs and model predictions for the model scenarios. The error bars are plus and minus 2 standard deviations.\\label{fig:pot_survey_cpue}"}
plot_cpue(M[c(mod_scen)],  "ADF&G Pot", ylab = "Pot survey CPUE (crab/potlift)")
ggsave(paste0(.FIGS, "pot_cpue_mod_scen.png"), width = ww*1.5, height = hh)
# add cv on pot survey -----------
plot_cpue(M[4],  ShowEstErr = TRUE,"ADF&G Pot", ylab = "Pot survey CPUE (crab/potlift)")
ggsave(paste0(.FIGS, "pot_cpue_addcv.png"), width = ww*1.5, height = hh)

plot_cpue(M[c(mod_scen)])
ggsave(paste0(.FIGS, "cpue_mod_scen_both.png"), width = ww*2.5, height = 1.25*hh)

##catch --------
plot_catch(M[2])
ggsave(paste0(.FIGS, "catch.png"), width = ww*1.2, height = hh*1.2)
dev.off()

  A <- M[[2]]
  df <- data.frame(Model = names(M)[2], A$dCatchData_out)
  colnames(df) <- c("model", "year", "seas", 
                    "fleet", "sex", "obs", "cv", 
                    "type", "units", "mult", "effort", 
                    "discard.mortality")
  df$observed <- na.omit(as.vector(t(A$obs_catch_out)))
  df$predicted <- na.omit(as.vector(t(A$pre_catch_out)))
  df$residuals <- as.vector(t(A$res_catch_out))
  df$sex <- .SEX[df$sex + 1]
  df$fleet <- .FLEET[df$fleet]
  df$type <- .TYPE[df$type]
  df$sd <- sqrt(log(1 + df$cv^2))
  df$lb <- exp(log(df$obs) - 1.96 * df$sd)
  df$ub <- exp(log(df$obs) + 1.96 * df$sd)
  mdf <- rbind(mdf, df)
  if (!all(df$observed == df$obs)) {
    stop("Error: observed catch data is buggered.")
  }
}
mdf$year <- as.integer(mdf$year)
mdf$sex <- factor(mdf$sex, levels = .SEX)
mdf$type <- as.factor(mdf$type)
mdf$fleet <- factor(mdf$fleet, levels = .FLEET)
return(mdf)





#{r bts_resid_nmfs, fig.cap = "Standardized residuals for area-swept estimates of total male survey biomass for the model scenarios. \\label{fig:bts_resid_nmfs}"}
A <- M[mod_scen];
plot_cpue_res(A, "NMFS Trawl")


#{r bts_resid_adfg, fig.cap = "Standardized residuals for total male pot survey CPUEs for each of the Gmacs model scenarios.\\label{fig:bts_resid_adfg}"}
A <- M[mod_scen];
plot_cpue_res(A, "ADF&G Pot")

## !!size comps ---------------
## !!!!!!!!!!!!! load my functions file here
source("./SMBKC/code/functions.R") 
#{r sc_pot, fig.cap = "Observed and model estimated size-frequencies of SMBKC by year retained in the directed pot fishery for the model scenarios. \\label{fig:sc_pot}"}
plot_size_comps(M[mod_scen], 1, legend_loc=c(.87,.1))
ggsave(paste0(.FIGS, "lf_1.png"), width = 8.5, height = 5, unit = "in")

#{r sc_pot_discarded, fig.cap = "Observed and model estimated size-frequencies of discarded male SMBKC by year in the NMFS trawl survey for the model scenarios. \\label{fig:sc_pot_discarded}"}
plot_size_comps(M[mod_scen], 2, legend_loc = "bottom")
ggsave(paste0(.FIGS, "lf_2.png"), width = 12, height = 7.5, unit = "in")

#{r sc_trawl_discarded, fig.cap = "Observed and model estimated size-frequencies of discarded SMBKC by year in the ADF&G pot survey for the model scenarios.\\label{fig:sc_trawl_discarded}"}
plot_size_comps(M[mod_scen], 3, legend_loc=c(.87,.2))
ggsave(paste0(.FIGS, "lf_3.png"), width = 8.5, height = 5, unit = "in")

#{r sc_pot_res_selex, fig.cap = "Bubble plots of residuals by stage and year for the directed pot fishery size composition data for SMBKC in the reference model.\\label{fig:sc_res_ref}"}
plot_size_comps_res(M[ref_mod])

#{r sc_trawl_discarded_res, fig.cap = "Bubble plots of residuals by stage and year for the ADF&G pot survey size composition data for SMBKC in the **fit surveys** model.\\label{fig:sc_res_fit_survey}"}
plot_size_comps_res(M[3])

#{r fit_to_catch, fig.cap = "Comparison of observed and model predicted retained catch and bycatches in each of the Gmacs models. Note that difference in units between each of the panels, some panels are expressed in numbers of crab, some as biomass (tons).\\label{fig:fit_to_catch}", fig.height = 12}
plot_catch(M[rec_mod]) # Note this should be rec_mod or all models

# !!dynamic Bzero ----------------------
#{r Dynamic_Bzero, fig.cap = "Comparisons of mature male biomass relative to the dynamic $B_0$ value, (15 February, 1978-2018) for  each of the model scenarios.\\label{fig:dynB0}"}
plot_dynB0(M[mod_scen])
ggsave(paste0(.FIGS, "dyn_Bzero.png"), width = 8.5, height = 5, unit = "in")
# not currently being output in .rep file - made Jim aware of this I need to talk to him again about this.
#.get_dynB0_df(M)


# survey fit residuals -----------
A <- M[mod_scen];
plot_cpue_res(A, "NMFS Trawl")
plot_cpue_res(A)
ggsave(paste0(.FIGS, "survey_residuals.png"), width = 1.5*ww, height = hh)


## create table for model parameter estimates-----------

x <- M[[ref_mod]]$fit
i <- c(grep("m_dev", x$names)[1],
       grep("theta", x$names),
       grep("survey_q", x$names),
       grep("log_fbar", x$names),
       grep("log_slx_pars", x$names)
       #grep("sd_fofl", x$names),
       #grep("spr_cofl", x$names)
)
Parameter <- x$names[i]
Estimate <- x$est[i]
SD <- x$std[i]
j <- grep("survey_q", Parameter)
Estimate[j] <- Estimate[j] * 1000
SD[j] <- SD[j] * 1000
Parameter <- c("Natural mortality deviation in 1998/99 ($\\delta^M_{1998})$",
               "$\\log (\\bar{R})$","$\\log (n^0_1)$","$\\log (n^0_2)$","$\\log (n^0_3)$",
               "$q_{pot}$", "$\\log (\\bar{F}^\\text{df})$","$\\log (\\bar{F}^\\text{tb})$","$\\log (\\bar{F}^\\text{fb})$",
               "log Stage-1 directed pot selectivity 1978-2008","log Stage-2 directed pot selectivity 1978-2008",
               "log Stage-1 directed pot selectivity 2009-2017","log Stage-2 directed pot selectivity 2009-2017",
               "log Stage-1 NMFS trawl selectivity","log Stage-2 NMFS trawl selectivity",
               "log Stage-1 ADF\\&G pot selectivity","log Stage-2 ADF\\&G pot selectivity")
               #"$F_\\text{OFL}$","OFL")
df1 <- data.frame(Parameter, Estimate, SD)
df2 <- data.frame(Parameter = c("$F_\\text{OFL}$","OFL"), 
                  Estimate = c(M[[ref_mod]]$sd_fofl[1], M[[ref_mod]]$spr_cofl))
df1 %>% 
  bind_rows(df2) -> df

## !!table of all parameter output -------
#```{r est_pars_all, results = "asis"}
Parameter <- NULL
Estimate <- NULL
Model <- NULL
Mname <- c("2018", "Ref","FitSurvey","addCVpot", "altregime")
for (ii in 2:5)
{
  x <- M[[ii]]$fit
  i <- c(grep("m_dev", x$names)[1],
         grep("theta", x$names),
         grep("survey_q", x$names),
         grep("log_fbar", x$names),
         grep("log_slx_pars", x$names))
         #grep("sd_fofl", x$names),
         #grep("sd_ofl", x$names) )
  Parameter <- c(Parameter, x$names[i])
  Estimate <- c(Estimate, x$est[i])
  Model <- c(Model, rep(Mname[ii], length(i)))
}
j <- grep("survey_q", Parameter)
Estimate[j] <- Estimate[j] * 1000
Parameter <- c("Natural mortality deviation in 1998/99 ($\\delta^M_{1998})$",
               "$\\log (\\bar{R})$","$\\log (n^0_1)$","$\\log (n^0_2)$","$\\log (n^0_3)$",
               "$q_{pot}$", "$\\log (\\bar{F}^\\text{df})$","$\\log (\\bar{F}^\\text{tb})$","$\\log (\\bar{F}^\\text{fb})$",
               "log Stage-1 directed pot selectivity 1978-2008","log Stage-2 directed pot selectivity 1978-2008",
               "log Stage-1 directed pot selectivity 2009-2017","log Stage-2 directed pot selectivity 2009-2017",
               "log Stage-1 NMFS trawl selectivity","log Stage-2 NMFS trawl selectivity",
               "log Stage-1 ADF\\&G pot selectivity","log Stage-2 ADF\\&G pot selectivity")
               #"$F_\\text{OFL}$","OFL")
Parameter <- c(Parameter, Parameter, Parameter, Parameter) 
df1 <- data.frame(Model, Parameter, Estimate)

df2 <- data.frame(Model = c("Ref", "Ref", "FitSurvey", "FitSurvey", "addCVpot", "addCVpot", "altregime", "altregime"),
                  Parameter = c("$F_\\text{OFL}$","OFL", "$F_\\text{OFL}$","OFL", "$F_\\text{OFL}$","OFL", "$F_\\text{OFL}$","OFL"), 
                  Estimate = c(M[[ref_mod]]$sd_fofl[1], M[[ref_mod]]$spr_cofl,
                                M[[3]]$sd_fofl[1], M[[3]]$spr_cofl, 
                                M[[4]]$sd_fofl[1], M[[4]]$spr_cofl, 
                                M[[5]]$sd_fofl[1], M[[5]]$spr_cofl))
df1 %>% 
  bind_rows(df2) -> df
df <- tidyr::spread(df, Model, Estimate) %>% dplyr::select(Parameter, Ref, FitSurvey, addCVpot, altregime)
write.csv(df, paste0(here::here(), '/SMBKC/smbkc_19/doc/safe_tables/all_parms.csv'), 
          row.names = FALSE)
### see chunk in .rmd to bring this file in

## data weighting ---------------------
#```{r data_weighting, results = "asis"}
# updated to work for draft - need to figure out how to get Francis weightings and 
#   lamdas
df <- NULL
for (ii in mod_scen)
{
  x       <- M[[ii]]
  SDNR    <- c(x$sdnr_MAR_cpue[,1], x$sdnr_MAR_lf[,1]); names(SDNR) <- c("SDNR NMFS trawl survey","SDNR ADF\\&G pot survey","SDNR directed pot LF","SDNR NMFS trawl survey LF","SDNR ADF\\&G pot survey LF")
  MAR     <- c(x$sdnr_MAR_cpue[,2], x$sdnr_MAR_lf[,2]); names(MAR) <- c("MAR NMFS trawl survey","MAR ADF\\&G pot survey","MAR directed pot LF","MAR NMFS trawl survey LF","MAR ADF\\&G pot survey LF")
  #Francis <- x$Francis_weights; names(Francis) <- c("Fancis weight for directed pot LF","Francis weight for NMFS trawl survey LF","Francis weight for ADF\\&G pot survey LF")
  wt_cpue <- c(ifelse(ii == 3, 1.5,1), ifelse(ii == 3, 2 ,1)); names(wt_cpue) <- c("NMFS trawl survey weight","ADF\\&G pot survey weight")
  wt_lf   <- c(1,1,1); names(wt_lf) <- c("Directed pot LF weight","NMFS trawl survey LF weight","ADF\\&G pot survey LF weight")
  v       <- c(wt_cpue, wt_lf, SDNR, MAR)
  df      <- cbind(df, v)
}
df        <- data.frame(rownames(df), df, row.names = NULL)
names(df) <- c("Component",mod_names[mod_scen])
write.csv(df, paste0(here::here(), '/SMBKC/smbkc_19/doc/safe_tables/data_weighting.csv'), 
          row.names = FALSE)


# !!Likelihood components -----------------
#```{r likelihood_components, results = "asis"}
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
  sv <- sum(v); names(sv) <- "Total"
  npar <- x$fit$nopar; names(npar) <- "Total estimated parameters"
  v <- c(v, sv, npar)
  df <- cbind(df, v)
}
df <- data.frame(rownames(df), df, row.names = NULL)
names(df) <- c("Component",mod_names[mod_scen])
write.csv(df, paste0(here::here(), '/SMBKC/smbkc_19/doc/safe_tables/neg_log_like.csv'), 
          row.names = FALSE)

### !!population abundance 2018 model -----------------------
#```{r pop-abundance-2018, results = "asis"}

A         <- M[[1]]
i         <- grep("sd_log_ssb", A$fit$names) #does not have proj value in here
SD        <- A$fit$std[i]
tl        <- length(A$mod_yrs)

# ssb current year uncertainty
un_ssb2 <- read.csv(here::here("./SMBKC/smbkc_18a/model_1/projections/proj_1/d/uncertainty_ssb_2019.csv"))


years = c(as.integer(A$mod_yrs[1:tl]), A$mod_yrs[tl]+1)
ssb = c(A$ssb, A$spr_bmsy*A$spr_depl)
ssb_cv = c((exp(SD[1:tl])-1), (exp(un_ssb2$CV_ssb)-1))

df        <- data.frame(years, A$N_males[ ,1], A$N_males[ ,2], 
                        A$N_males[ ,3], ssb, ssb_cv)
names(df) <- c("Year","$n_1$","$n_2$","$n_3$","MMB","CV MMB")
write.csv(df, paste0(here::here(), '/SMBKC/smbkc_19/doc/safe_tables/numbers_last_yrs.csv'), 
          row.names = FALSE)

### !!population abundance 2019 base model -----------------------
#```{r pop-abundance-2019, results = "asis"}
A         <- M[[ref_mod]]
i         <- grep("sd_log_ssb", A$fit$names) #does not have proj value in here
SD        <- A$fit$std[i]
tl        <- length(A$mod_yrs)

# ssb current year uncertainty
un_ssb <- read.csv(here::here("./SMBKC/smbkc_19/model_1/projections/proj_1/d/uncertainty_ssb_2019.csv"))


years = c(as.integer(A$mod_yrs[1:tl]), A$mod_yrs[tl]+1)
ssb = c(A$ssb, A$spr_bmsy*A$spr_depl)
ssb_cv = c((exp(SD[1:tl])-1), (exp(un_ssb$CV_ssb)-1))

df        <- data.frame(years, A$N_males[ ,1], A$N_males[ ,2], 
                        A$N_males[ ,3], ssb, ssb_cv)
names(df) <- c("Year","$n_1$","$n_2$","$n_3$","MMB","CV MMB")
write.csv(df, paste0(here::here(), '/SMBKC/smbkc_19/doc/safe_tables/numbers_current_yrs.csv'), 
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
  ABC    <- OFL * 0.8; names(ABC)            <- paste0("$\\text{ABC}_{", (x$mod_yrs[length(x$mod_yrs)]+ 1), "}$")
  v      <- c(mmb, Bmsy, fofl, OFL, ABC)
  df     <- cbind(df, v)
}





