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

mod_names <- c("model 18.0", "model 19.0 (reference)", "model 19.1 (survey fit)", "model 19.0a (current regime)") 
.MODELDIR = c(paste0(here::here(), "/SMBKC/smbkc_18a/model_1/"), paste0(here::here(), "/SMBKC/smbkc_19/model_1/initial_run/"),
              paste0(here::here(), "/SMBKC/smbkc_19/model_5/"), paste0(here::here(), "/SMBKC/smbkc_19/model_1a/")) #need to update these model options
.THEME    = theme_bw(base_size = 12, base_family = "")
.OVERLAY  = TRUE
.SEX      = c("Aggregate","Male")
.FLEET    = c("Pot","Trawl bycatch","Fixed bycatch","NMFS Trawl","ADF&G Pot")
.TYPE     = c("Retained & Discarded","Retained","Discarded")
.SHELL    = c("Aggregate")
.MATURITY = c("Aggregate")
.SEAS     = c("1","2","3","4","5")

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

ref_mod <- 2 # base
rec_mod <- 2 # base
mod_scen<- 2:3 #scenarios you want graphed together

# executive summary -----------
.get_cpue_df(Mbase) %>% 
  filter(fleet==.FLEET[4]) %>% 
  mutate(x = round(100*pred/mean(pred),0)) %>% 
  select(x) %>% 
  tail(1) %>%
  .$x -> bio_lt_percent

## data extent -----------
plot_datarange(M[rec_mod])

## fig 6/7 2018 safe - 2018 compared to reference model --------
plot_cpue(M[1:2], "NMFS Trawl", ylab = "Survey biomass (t)") 

plot_cpue(M[1:2], "ADF&G Pot", ylab = "Pot survey CPUE (crab/potlift)")

### Sensitivity of new data in 2018 on estimated recruitment ; 1978-2018
A <- M
for (i in c(1,2)) {
  ii <- which(A[[i]]$fit$names %in% "sd_log_recruits"); ii <- ii[length(ii)]
  A[[i]]$fit$est[ii] <- NA
  A[[i]]$fit$std[ii] <- NA
}
plot_recruitment(A[1:2])

## ssb -----------
#"Sensitivity of new data in 2019 on estimated mature male biomass (MMB); 1978-2019. \\label{fig:ssb1}"}
plot_ssb(M[1:2], ylab = "Mature male biomass (tons) on 15 February")

## this is NOT correct **FIX** since it doesn't include projected 2019 biomass

# no need for 2018 figures 10 and 11 because no VAST scenario

## selectivity ----------
#"Comparisons of the estimated stage-1 and stage-2 selectivities for the different model scenarios (the stage-3 selectivities are all fixed at 1). Estimated selectivities are shown for the directed pot fishery, the trawl bycatch fishery, the fixed bycatch fishery, the NMFS trawl survey, and the ADF&G pot survey. Two selectivity periods are estimated in the directed pot fishery, from 1978-2008 and 2009-2017.\\label{fig:selectivity}", fig.height = 15}
plot_selectivity(M[mod_scen]) 
## ** FIX ** not currently being output in .rep file.  Need to read this from Gmacsall.out 

## recruitment mod scen ----------------
# "Estimated recruitment 1979-2018 comparing model alternatives. The solid horizontal lines in the background represent the estimate of the average recruitment parameter ($\\bar{R}$) in each model scenario.\\label{fig:recruitment}"}
A <- M
for (i in mod_scen) {
  ii <- which(A[[i]]$fit$names %in% "sd_log_recruits"); ii <- ii[length(ii)]
  A[[i]]$fit$est[ii] <- NA
  A[[i]]$fit$std[ii] <- NA
}
plot_recruitment(A[mod_scen])

# ssb mod scen ---------------
  
#"Comparisons of estimated mature male biomass (MMB) time series on 15 February during 1978-2018 for each of the model scenarios.\\label{fig:mmb}"}
plot_ssb(M[mod_scen], ylab = "Mature male biomass (tons) on 15 February")
## **FIX** to include 2019 projected value and associate error

#  ```{r natural_mortality, fig.cap = "Time-varying natural mortality ($M_t$). Estimated pulse period occurs in 1998/99 (i.e. $M_{1998}$). \\label{fig:M_t}"}
plot_natural_mortality(M, knots = NULL, slab = "Model")

#{r trawl_survey_biomass, fig.cap = "Comparisons of area-swept estimates of total (90+ mm CL) male survey biomass (tons) and model predictions for the model scenarios. The error bars are plus and minus 2 standard deviations.\\label{fig:trawl_survey_biomass}"} 
plot_cpue(M[c(mod_scen)],  "NMFS Trawl", ylab = "NMFS survey biomass (t)")
#p.df <- .get_cpue_df(M[mod_scen])
#filter(p.df,fleet=="NMFS Trawl") %>% 
#  ggplot(aes(x=(year),y=cpue,col=Model)) + geom_point(position=position_dodge(0.9) ) + .THEME + xlab("Year") +
#  geom_errorbar(aes(ymin=lbe, ymax=ube), width=.2,position=position_dodge(0.9) ) + geom_line(aes(x=year,y=pred)) + ylab("NMFS survey biomass (t)")
#plot_cpue(M[mod_scen], "NMFS Trawl", ylab = "Survey biomass (tons)") 

#{r pot_survey_cpue, fig.cap = "Comparisons of total (90+ mm CL) male pot survey CPUEs and model predictions for the model scenarios. The error bars are plus and minus 2 standard deviations.\\label{fig:pot_survey_cpue}"}
plot_cpue(M[c(mod_scen)],  "ADF&G Pot", ylab = "Pot survey CPUE (crab/potlift)")

#{r bts_resid_nmfs, fig.cap = "Standardized residuals for area-swept estimates of total male survey biomass for the model scenarios. \\label{fig:bts_resid_nmfs}"}
A <- M[mod_scen];
plot_cpue_res(A, "NMFS Trawl")


#{r bts_resid_adfg, fig.cap = "Standardized residuals for total male pot survey CPUEs for each of the Gmacs model scenarios.\\label{fig:bts_resid_adfg}"}
A <- M[mod_scen];
plot_cpue_res(A, "ADF&G Pot")

#{r sc_pot, fig.cap = "Observed and model estimated size-frequencies of SMBKC by year retained in the directed pot fishery for the model scenarios. \\label{fig:sc_pot}"}
plot_size_comps(M[mod_scen], 1)


#{r sc_pot_discarded, fig.cap = "Observed and model estimated size-frequencies of discarded male SMBKC by year in the NMFS trawl survey for the model scenarios. \\label{fig:sc_pot_discarded}"}
plot_size_comps(M[mod_scen], 2)

#{r sc_trawl_discarded, fig.cap = "Observed and model estimated size-frequencies of discarded SMBKC by year in the ADF&G pot survey for the model scenarios.\\label{fig:sc_trawl_discarded}"}
plot_size_comps(M[mod_scen], 3)

#{r sc_pot_res_selex, fig.cap = "Bubble plots of residuals by stage and year for the directed pot fishery size composition data for SMBKC in the reference model.\\label{fig:sc_res_ref}"}
plot_size_comps_res(M[ref_mod])

#{r sc_trawl_discarded_res, fig.cap = "Bubble plots of residuals by stage and year for the ADF&G pot survey size composition data for SMBKC in the **fit surveys** model.\\label{fig:sc_res_fit_survey}"}
plot_size_comps_res(M[3])

#{r fit_to_catch, fig.cap = "Comparison of observed and model predicted retained catch and bycatches in each of the Gmacs models. Note that difference in units between each of the panels, some panels are expressed in numbers of crab, some as biomass (tons).\\label{fig:fit_to_catch}", fig.height = 12}
plot_catch(M[rec_mod]) # Note this should be rec_mod or all models

# dynamic Bzero ----------------------
#{r Dynamic_Bzero, fig.cap = "Comparisons of mature male biomass relative to the dynamic $B_0$ value, (15 February, 1978-2018) for  each of the model scenarios.\\label{fig:dynB0}"}
#plot_dynB0(M[mod_scen])
# **FIX ** not currently being output in .rep file - made Jim aware of this I need to talk to him again about this.

