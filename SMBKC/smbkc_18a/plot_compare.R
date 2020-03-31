#require(devtools)
#devtools::install_github("seacode/gmacs", subdir = "/gmr", ref = "develop")
# ploting for model 1 under smbkc18a folder - using gmr and Jim's code 
require(gmr)
#setwd("./smbkc_18a/model_1")

mod_names <- c("base_model_1", "VAST_model", "survey_fit_model")
.MODELDIR = c("./SMBKC/smbkc_18a/model_1/", "./SMBKC/smbkc_18a/model_4/", "./SMBKC/smbkc_18a/model_5/")
.THEME    = theme_bw(base_size = 12, base_family = "")
.OVERLAY  = TRUE
.SEX      = c("Aggregate","Male")
.FLEET    = c("Pot","Trawl bycatch","Fixed bycatch","NMFS Trawl","ADFG Pot")
.TYPE     = c("Retained & Discarded","Retained","Discarded")
.SHELL    = c("Aggregate","Aggregate")
.MATURITY = c("Aggregate")
.SEAS     = c("Annual")
.FIGS     = c("./SMBKC/smbkc_18a/figure/")

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

plot_cpue(M, ShowEstErr = FALSE)
ggsave(paste0(.FIGS, "cpue.png"), width = ww*2.5, height = hh)
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

plot_selectivity(M, ncol = 5) # FIX not working
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
plot_numbers(M)
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






# my plots -------------
# SSB -----------
ssb <- .get_ssb_df(M)
head(ssb)


ssb %>% 
  ggplot(aes(year, ssb)) +
    geom_line() +
    geom_ribbon(aes(x=year, ymax = ub, ymin = lb), alpha = 0.1) +
    expand_limits(y=0) +
    geom_hline(data = Bmsy_options, aes(yintercept = Bmsy50), color = c("blue", "red"), 
               lty = c("solid", "dashed"))+
    geom_text(data = Bmsy_options, aes(x= 1980, y = Bmsy50, label = years), 
              hjust = -0.45, vjust = 1.5, nudge_y = 0.05, size = 3.5) +
    ggtitle("Model 1 2018a")
    
ssb %>% 
  summarise(Bmsy = mean(ssb)) %>% 
  mutate(years = "1978-2017")-> Bmsy
ssb %>% 
  filter(year >= 1996) %>% 
  summarise(Bmsy = mean (ssb)) %>% 
  mutate(years = "1996-2017")->Bmsy2

# recruitment -------------
rec <- .get_recruitment_df(M)
head(rec)

#rbar is estimated in model

rec %>% 
  ggplot(aes(year, y = exp(log_rec)/1000000)) +
  geom_line() +
  geom_ribbon(aes(x=year, ymax = ub/1000000, ymin = lb/1000000), alpha = 0.1) +
  expand_limits(y=0) +
  ggtitle("Model 1 2018a") +
  geom_hline(aes(yintercept = rbar[1]/1000000), color = "blue") 
             
             color = c("blue", "red"), 
             lty = c("solid", "dashed"))+
  geom_text(data = avgR_options, aes(x= 1980, y = meanR, label = years), 
            hjust = -0.45, vjust = 1.5, nudge_y = 0.05, size = 3.5) +
 

# need to pull rbar from model output with different recruitment years


#rec %>% 
#  summarise(meanR = mean(exp(log_rec)/1000000)) %>% 
#  mutate(years = "1978-2017")-> avgR
#rec %>% 
#  filter(year >= 1996) %>% 
#  summarise(meanR = mean (exp(log_rec)/1000000)) %>% 
#  mutate(years = "1996-2017")-> avgR2

avgR %>% 
  bind_rows(avgR2) -> avgR_options
  #mutate(Bmsy50 = 0.5*Bmsy) -> Bmsy_options

rec %>% 
  mutate(rec = exp(log_rec)/1000000) %>% 
  summarise(meanR = mean(rec))
