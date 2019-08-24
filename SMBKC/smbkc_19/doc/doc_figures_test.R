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
.MODELDIR = c(paste0(here::here(), "/SMBKC/smbkc_18a/model_1/"), paste0(here::here(), "/SMBKC/smbkc_19/model_1/initial_run/"),
              paste0(here::here(), "/SMBKC/smbkc_19/model_5/"), paste0(here::here(), "/SMBKC/smbkc_19/model_1b/"), 
              paste0(here::here(), "/SMBKC/smbkc_19/model_1a/")) #need to update these model options
.THEME    = theme_bw(base_size = 12, base_family = "")
.OVERLAY  = TRUE
.SEX      = c("Aggregate","Male")
.FLEET    = c("Pot","Trawl bycatch","Fixed bycatch","NMFS Trawl","ADF&G Pot")
.TYPE     = c("Retained & Discarded","Retained","Discarded")
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
#ggsave(paste0(.FIGS, "data_extent.png"), width = ww, height = hh)

## fig 6/7 2018 safe - 2018 compared to reference model --------
plot_cpue(M[1:2], "NMFS Trawl", ylab = "Survey biomass (t)") 
ggsave(paste0(.FIGS, "trawl_cpue_ref.png"), width = ww*1.25, height = hh*.9)

plot_cpue(M[1:2], "ADF&G Pot", ylab = "Pot survey CPUE (crab/potlift)")
ggsave(paste0(.FIGS, "pot_cpue_ref.png"), width = ww*1.5, height = hh)

### Sensitivity of new data in 2018 on estimated recruitment ; 1978-2018
A <- M
for (i in c(1,2)) {
  ii <- which(A[[i]]$fit$names %in% "sd_log_recruits"); ii <- ii[length(ii)]
  A[[i]]$fit$est[ii] <- NA
  A[[i]]$fit$std[ii] <- NA
}
plot_recruitment(A[1:2])
plot_recruitment(M[1:2])

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
#A <- M
#for (i in mod_scen) {
#  ii <- which(A[[i]]$fit$names %in% "sd_log_recruits"); ii <- ii[length(ii)]
#  A[[i]]$fit$est[ii] <- NA
#  A[[i]]$fit$std[ii] <- NA
#}
#plot_recruitment(A[mod_scen])
plot_recruitment(M[mod_scen])

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

# add cv on pot survey -----------
plot_cpue(M[4],  ShowEstErr = TRUE,"ADF&G Pot", ylab = "Pot survey CPUE (crab/potlift)")

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
Mname <- c("2018", "Ref","FitSurvey","addCVpot")
for (ii in 2:4)
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
Parameter <- c(Parameter, Parameter, Parameter) 
df1 <- data.frame(Model, Parameter, Estimate)

df2 <- data.frame(Model = c("Ref", "Ref", "FitSurvey", "FitSurvey", "addCVpot", "addCVpot"),
                  Parameter = c("$F_\\text{OFL}$","OFL", "$F_\\text{OFL}$","OFL", "$F_\\text{OFL}$","OFL"), 
                  Estimate = c(M[[ref_mod]]$sd_fofl[1], M[[ref_mod]]$spr_cofl,
                                M[[3]]$sd_fofl[1], M[[3]]$spr_cofl, 
                                M[[4]]$sd_fofl[1], M[[4]]$spr_cofl))
df1 %>% 
  bind_rows(df2) -> df
df <- tidyr::spread(df, Model, Estimate) %>% dplyr::select(Parameter, Ref, FitSurvey, addCVpot)
write.csv(df, paste0(here::here(), '/SMBKC/smbkc_19/doc/safe_tables/all_parms.csv'), 
          row.names = FALSE)
### see chunk in .rmd to bring this file in

## data weighting ---------------------
#```{r data_weighting, results = "asis"}
df <- NULL
for (ii in mod_scen)
{
  x       <- M[[ii]]
  SDNR    <- c(x$sdnr_MAR_cpue[,1], x$sdnr_MAR_lf[,1]); names(SDNR) <- c("SDNR NMFS trawl survey","SDNR ADF\\&G pot survey","SDNR directed pot LF","SDNR NMFS trawl survey LF","SDNR ADF\\&G pot survey LF")
  MAR     <- c(x$sdnr_MAR_cpue[,2], x$sdnr_MAR_lf[,2]); names(MAR) <- c("MAR NMFS trawl survey","MAR ADF\\&G pot survey","MAR directed pot LF","MAR NMFS trawl survey LF","MAR ADF\\&G pot survey LF")
  Francis <- x$Francis_weights; names(Francis) <- c("Fancis weight for directed pot LF","Francis weight for NMFS trawl survey LF","Francis weight for ADF\\&G pot survey LF")
  wt_cpue <- x$cpue_lambda; names(wt_cpue) <- c("NMFS trawl survey weight","ADF\\&G pot survey weight")
  wt_lf   <- x$lf_lambda; names(wt_lf) <- c("Directed pot LF weight","NMFS trawl survey LF weight","ADF\\&G pot survey LF weight")
  v       <- c(wt_cpue, wt_lf, Francis, SDNR, MAR)
  df      <- cbind(df, v)
}
df        <- data.frame(rownames(df), df, row.names = NULL)
names(df) <- c("Component",mod_names[mod_scen])
tab       <- xtable(df, caption = "Comparisons of data weights, Francis LF weights (i.e. the new weights that should be applied to the LFs), SDNR and MAR (standard deviation of normalized residuals and median absolute residual) values for the model scenarios.", label = "tab:data_weighting")
print(tab, caption.placement = "top", include.rownames = FALSE, sanitize.text.function = function(x){x}, hline.after = c(-1,0,5,8,13,nrow(tab)))


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

years = c(as.integer(A$mod_yrs[1:tl]), A$mod_yrs[tl]+1)
ssb = c(A$ssb, A$spr_bmsy*A$spr_depl)
ssb_cv = c((exp(SD[1:tl])-1), 999)

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

years = c(as.integer(A$mod_yrs[1:tl]), A$mod_yrs[tl]+1)
ssb = c(A$ssb, A$spr_bmsy*A$spr_depl)
ssb_cv = c((exp(SD[1:tl])-1), 999)

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
