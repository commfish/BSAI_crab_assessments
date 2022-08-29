# k. palof, k.palof@alaska.gov
# 8-26-22

# Summary of retrospective runs done in GMACS for SMBKC

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
folder <- "/SMBKC/smbkc_22f/retrospective_model_1" # update annually retrospective_model_1
# The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# To use for fills, add
#scale_fill_manual(values=cbPalette)
# To use for line and point colors, add
#scale_colour_manual(values=cbPalette)

# update model names and file locations
mod_names <- c("16.0 (2022)", "2021", "2020", "2019", "2018", "2017", "2016", "2015", "2014", "2013", "2012")
#mod_names <- c("16.0 (2019)", "16.0 (2020)", "16.0a (fix R ter)", "20.1 (no pot)")
.MODELDIR = c(paste0(here::here(), "/SMBKC/smbkc_22f/model_1_22f/"),
              paste0(here::here(), folder, "/retro1/"),
              paste0(here::here(), folder, "/retro2/"),
              paste0(here::here(), folder, "/retro3/"),
              paste0(here::here(), folder, "/retro4/"),
              paste0(here::here(), folder, "/retro5/"),
              paste0(here::here(), folder, "/retro6/"),
              paste0(here::here(), folder, "/retro7/"),
              paste0(here::here(), folder, "/retro8/"),
              paste0(here::here(), folder, "/retro9/"),
              paste0(here::here(), folder, "/retro10/")) #need to update these model options
.THEME    = list(theme_bw(base_size = 12, base_family = ""), scale_fill_manual(values=cbPalette), 
                 scale_colour_manual(values=cbPalette))
.SEX      = c("Aggregate","Male")
.FLEET    = c("Pot","Trawl bycatch","Fixed bycatch","NMFS Trawl","ADF&G Pot")
.TYPE     = c("Retained","Discarded", "Retained & Discarded")
.SHELL    = c("Aggregate")
.MATURITY = c("Aggregate")
.SEAS     = c("1","2","3","4","5")
.FIGS     = c("./SMBKC/smbkc_22f/doc/safe_figure/")
.TABS     = c("./SMBKC/smbkc_22f/doc/safe_tables/")
.FILES     = c("./SMBKC/smbkc_22f/retrospective_model_1/")
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
  ggplot(aes(year, ssb, group = Model)) +
  geom_line(aes(group = Model, colour = Model), lwd = 0.75) +
  ylab("Mature male biomass (tons) on Feb 15th") +
  xlab("Year (Feb. 15)") +
  #ylim(c(0,11500)) +
  theme_bw(base_size = 12, base_family = "") +
  scale_colour_discrete(name  ="Model end year") +
  scale_x_continuous(breaks = seq(min(1975),max(max(ssb2$year) + 1), by = 4)) +
  theme(legend.position = c(0.85, 0.7), 
        text = element_text(size = 13), 
        axis.text = element_text(size = 13), 
        axis.title = element_text(size = 13)) +
  geom_text(x = 1995, y = 9000, label = "Mohn's rho: -0.183", size = 6) # add in Mohn's rho - currently calculated in excel - see retro_out_2022

ggsave(paste0(.FIGS, "ssb_retrospective_model_1.png"), width = 1.35*6, height = 9)

## Mohn's rho ssb------
ssb2 %>% 
  mutate(ssb = ssb) %>% 
  mutate(model.end.yr = ifelse(Model == '16.0 (2022)', '2021', as.numeric(Model)-1)) %>% 
  select(model.end.yr, year, ssb) %>% 
  spread(model.end.yr, ssb) %>% 
  mutate(year = as.integer(year)) %>% 
  #needs to be in descending order for code to work
  select(year, `2021`, `2020`, `2019`, `2018`, `2017`, `2016`, `2015`, `2014`, `2013`, `2012`, `2011`) -> out3
# issue because year model names as column names don't match estimates

mohn(out3)
mohn(out3, peels = 5, details = TRUE, plot = TRUE)
mohn(out3, peels = 10, details = TRUE, plot = TRUE) # copy this result up to the above figure text
mohn(out3, peels = 1, details = TRUE, plot = TRUE)


