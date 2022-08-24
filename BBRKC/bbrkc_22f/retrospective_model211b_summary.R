#k.palof, 8-23-2022
# Script for compiling retrospective runs - mainly ssb and recruitment

#
# load ------------
#require(devtools)
#devtools::install_github("seacode/gmacs", subdir = "/gmr", ref = "develop") #, INSTALL_opts="--no-staged-install") #- only needs to be performed once, but needs to be 
# done again when updates are made to the package
# if gmr is updated and above doesn't work try:
# go to Build above - direct it to the gmr folder - press OK. Over on right in the Build tab (upper right hand side) - 
# click "install and restart"

library(gmr) #require(gmr)
source("./SMBKC/code/functions.R") 
source("./SMBKC/code/helper.R") 
source("./SMBKC/code/packages.R")
source("./SMBKC/code/gmr_functions2020.R") 
source("./BBRKC/code/bbrkc_functions.R")
library(icesAdvice)
# Model 1 plots -------------------------
# ALL retrospective model setup  -------------------------
# first model is reference to previous year
cur_yr <- 2022 # update annually 
folder <- "bbrkc_22f/model_211b/retrospective" # update annually 
# The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# To use for fills, add
#scale_fill_manual(values=cbPalette)
# To use for line and point colors, add
#scale_colour_manual(values=cbPalette)

# update model names and file locations
mod_names <- c("21.1b (2022)", "2021", "2020", "2019", "2018", "2017", "2016", "2015", "2014", "2013", "2012")
#mod_names <- c("16.0 (2019)", "16.0 (2020)", "16.0a (fix R ter)", "20.1 (no pot)")
.MODELDIR = c(paste0(here::here(), "/BBRKC/bbrkc_22f/model_211b/"),
              paste0(here::here(), "/BBRKC/bbrkc_22f/model_211b/retrospective/retro1/"),
              paste0(here::here(), "/BBRKC/bbrkc_22f/model_211b/retrospective/retro2/"),
              paste0(here::here(), "/BBRKC/bbrkc_22f/model_211b/retrospective/retro3/"),
              paste0(here::here(), "/BBRKC/bbrkc_22f/model_211b/retrospective/retro4/"),
              paste0(here::here(), "/BBRKC/bbrkc_22f/model_211b/retrospective/retro5/"),
              paste0(here::here(), "/BBRKC/bbrkc_22f/model_211b/retrospective/retro6/"),
              paste0(here::here(), "/BBRKC/bbrkc_22f/model_211b/retrospective/retro7/"),
              paste0(here::here(), "/BBRKC/bbrkc_22f/model_211b/retrospective/retro8/"),
              paste0(here::here(), "/BBRKC/bbrkc_22f/model_211b/retrospective/retro9/"),
              paste0(here::here(), "/BBRKC/bbrkc_22f/model_211b/retrospective/retro10/")) #need to update these model options
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
.FIGS     = c("./BBRKC/bbrkc_22f/figures/")
.TABS     = c("./BBRKC/bbrkc_22f/doc/safe_tables/")
.FILES     = c("./BBRKC/bbrkc_22f/model_211b/retrospective/")
# Read report file and create gmacs report object (a list):
fn       <- paste0(.MODELDIR, "gmacs")
M        <- lapply(fn, read_admb)
names(M) <- mod_names

nmult_1 <- 1e+06
nmult_2 <- 0.0004535923 * 1e+6


rinline <- function(code){
  html <- '<code  class="r">``` `CODE` ```</code>'
  sub("CODE", code, html)
}

#alt_mod <- 5 # alt reference time frame
ref_mod <- 1 # base 2022

retro_scen<- 2:11 #scenarios you want graphed together

ww <- 6
hh <- 5
#####----------

ssb <- .get_ssb_dfKP(M[1:11])
# retro runs in GMACS do not output projected ssb...which is frustrating. need to adjust function for this see 'gmr
ssb2 <- .get_ssb_dfKP_retro(M[1:11])

# plot ssb retro -----
ssb2 %>% 
  ggplot(aes(year, ssb/1000, group = Model)) +
  geom_line(aes(group = Model, colour = Model), lwd = 0.75) +
  ylab("Mature male biomass (tons) on Feb 15th") +
  xlab("Year (Feb. 15, year+1)") +
  #ylim(c(0,11500)) +
  theme_bw(base_size = 12, base_family = "") +
  scale_colour_discrete(name  ="Model end year")

ggsave(paste0(.FIGS, "ssb_retrospective_model211b.png"), width = 1.25*6, height = 5)

## Mohn's rho ssb------
ssb2 %>% 
  mutate(ssb = ssb/1000) %>% 
  mutate(model.end.yr = ifelse(Model == '21.1b (2022)', '2021', as.numeric(Model)-1)) %>% 
  select(model.end.yr, year, ssb) %>% 
  spread(model.end.yr, ssb) %>% 
  #needs to be in descending order for code to work
  select(year, `2021`, `2020`, `2019`, `2018`, `2017`, `2016`, `2015`, `2014`, `2013`, `2012`, `2011`) -> out3
# issue because year model names as column names don't match estimates

mohn(out3)
mohn(out3, peels = 5, details = FALSE, plot = TRUE)
mohn(out3, peels = 10, details = TRUE, plot = TRUE)
mohn(out3, peels = 1, details = TRUE, plot = TRUE)

# output to calculate in Excel like Jie
ssb2 %>% 
  mutate(ssb = ssb/1000) %>% 
  spread(year, ssb) -> retro_out

write.csv(retro_out, paste0(.FILES, paste0("retro_out_", cur_yr, ".csv")), row.names = FALSE)



## rec -----
#
rec1 <- data.frame(Model = names(M)[1], 
                   recruit_m = M[[1]]$recruits[1, ], 
                   recruit_f = M[[1]]$recruits[2, ])
rec1$year <- c(M[[1]]$mod_yrs)
#
rec2 <- data.frame(Model = names(M)[2], 
                  recruit_m = M[[2]]$recruits[1, ], 
                   recruit_f = M[[2]]$recruits[2, ])
rec2$year <- c(M[[2]]$mod_yrs)
#
rec3 <- data.frame(Model = names(M)[3], 
                   recruit_m = M[[3]]$recruits[1, ], 
                   recruit_f = M[[3]]$recruits[2, ])
rec3$year <- c(M[[3]]$mod_yrs)
#
rec4 <- data.frame(Model = names(M)[4], 
                   recruit_m = M[[4]]$recruits[1, ], 
                   recruit_f = M[[4]]$recruits[2, ])
rec4$year <- c(M[[4]]$mod_yrs)
#
rec5 <- data.frame(Model = names(M)[5], 
                   recruit_m = M[[5]]$recruits[1, ], 
                   recruit_f = M[[5]]$recruits[2, ])
rec5$year <- c(M[[5]]$mod_yrs)
#
rec6 <- data.frame(Model = names(M)[6], 
                   recruit_m = M[[6]]$recruits[1, ], 
                   recruit_f = M[[6]]$recruits[2, ])
rec6$year <- c(M[[6]]$mod_yrs)


rec1 %>% 
  rbind(rec2) -> recruits_all
#### ------------------

#write.csv(ssb, paste0(.FILES, paste0("ssb_", cur_yr, ".csv")), row.names = FALSE)

#write.table(ssb, file = paste0(.FILES, "ssb_all.csv"), sep = ",",
#            append = TRUE, col.names = FALSE, row.names = FALSE)