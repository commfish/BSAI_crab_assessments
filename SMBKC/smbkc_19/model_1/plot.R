# K.Palof  8-16-19

# Code for plotting output of GMACS models for SMBKC
# Taken from Jim Ianellii https://github.com/seacode/gmacs/tree/develop/examples/smbkc_18a/model_1 but updated 

# load ------------
#require(devtools)
#devtools::install_github("seacode/gmacs", subdir = "/gmr", ref = "develop") - only needs to be performed once.
require(gmr)
#setwd("./smbkc_19/model_1")

# Model 1 plots -------------------------
# ploting for model 1 under smbkc19 folder - using gmr and Jim's code 

mod_names <- c("model_1")
.MODELDIR = c("./SMBKC/smbkc_19/model_1/initial_run/")
.THEME    = theme_bw(base_size = 12, base_family = "")
.OVERLAY  = TRUE
.SEX      = c("Aggregate","Male")
.FLEET    = c("Pot","Trawl bycatch","Fixed bycatch","NMFS Trawl","ADFG Pot")
.TYPE     = c("Retained & Discarded","Retained","Discarded")
.SHELL    = c("Aggregate","Aggregate")
.MATURITY = c("Aggregate")
.SEAS     = c("Annual")
.FIGS     = c("./SMBKC/smbkc_19/model_1/figure/")

fn       <- paste0(.MODELDIR, "gmacs")
M        <- lapply(fn, read_admb) #need .prj file to run gmacs and need .rep file here
names(M) <- mod_names

ww <- 6
hh <- 5

# Jim's plots -------------------------------
plot_recruitment_size(M)
ggsave(paste0(.FIGS, "rec_size.png"), width = ww*2.5, height = hh*1.5)
dev.off()

plot_catch(M)
ggsave(paste0(.FIGS, "catch.png"), width = ww*1.2, height = hh*1.2)
dev.off()

plot_cpue(M, ShowEstErr = TRUE)
ggsave(paste0(.FIGS, "cpue.png"), width = ww*2.5, height = hh)
dev.off()

plot_cpue(M, ShowEstErr = TRUE, "NMFS Trawl", ylab = "Survey biomass (t)")
ggsave(paste0(.FIGS, "cpue_trawl.png"), width = ww, height = hh)
dev.off()

plot_cpue(M, ShowEstErr = TRUE, "ADFG Pot", ylab = "Survey biomass (t)")
ggsave(paste0(.FIGS, "cpue_pot.png"), width = ww, height = hh)
dev.off()

plot_natural_mortality(M, plt_knots = FALSE)
ggsave(paste0(.FIGS, "M_t.png"), width = ww, height = hh)
dev.off()

plot_ssb(M)
ggsave(paste0(.FIGS, "ssb.png"), width = ww, height = hh)
dev.off()

plot_recruitment(M)
ggsave(paste0(.FIGS, "recruitment.png"), width = ww, height = hh)
dev.off()

plot_selectivity(M, ncol = 5) # **FIX** not working
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



# SMBKC plots new  -------------
# SSB -----------
ssb <- .get_ssb_df(M) # ssb now does NOT include projection year so only up to 2018 crab year - 2019 projection (example)
head(ssb)


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
    ggtitle("Base model - model 1 (Model 3 2018)") +
    ylab("MMB (t)") + xlab("Year") +
    .THEME
ggsave(paste0(.FIGS, "ssb_Bmsy.png"), width = ww, height = hh)
dev.off()

# Bmsy proxy table --------
# need ssb from above
ssb %>% 
  summarise(Bmsy = mean(ssb)) %>% 
  mutate(years = "1978-2018", label = "1978-2018 B_MSY" )-> Bmsy
ssb %>% 
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
write_csv(ofl_df, paste0('./SMBKC/smbkc_19/model_1/ofl_table_', mod_names, '.csv'))

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

rec %>% 
  filter(year >= 1996) %>% 
  summarise(meanR = mean (exp(log_rec)/1000000)) %>% 
  mutate(years = "1996-2018")-> avgR2

avgR %>% 
  bind_rows(avgR2) -> avgR_options
#mutate(Bmsy50 = 0.5*Bmsy) -> Bmsy_options
avgR_options # see above is calculated average recruitment for each time series
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
  geom_hline(data = avgR_options, aes(yintercept = meanR), color = c("blue", "red"), 
             lty = c("solid", "dashed"))+
  geom_text(data = avgR_options, aes(x= 1980, y = meanR, label = years), 
            hjust = -2.45, vjust = 1.5, nudge_y = 0.05, size = 3.5) 
ggsave(paste0(.FIGS, "recruitment_line_with years.png"), width = ww, height = hh)
dev.off()
         


### need option with new average recruitment    



### OFL --------
M[[base_model_1]]$spr_cofl

