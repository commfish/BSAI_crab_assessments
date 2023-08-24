#k.palof, 8-23-2022 / 8-24-23
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
cur_yr <- 2023 # update annually 
folder <- "bbrkc_23f/model_211b/retrospective" # update annually 
# The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# To use for fills, add
#scale_fill_manual(values=cbPalette)
# To use for line and point colors, add
#scale_colour_manual(values=cbPalette)

# update model names and file locations
mod_names <- c("21.1b (2023)", "2022", "2021", "2020", "2019", "2018", "2017", "2016", "2015", "2014", "2013")
#mod_names <- c("16.0 (2019)", "16.0 (2020)", "16.0a (fix R ter)", "20.1 (no pot)")
.MODELDIR = c(paste0(here::here(), "/BBRKC/bbrkc_23f/model_211b/"),
              paste0(here::here(), "/BBRKC/", folder, "/retro1/"),
              paste0(here::here(), "/BBRKC/", folder, "/retro2/"),
              paste0(here::here(), "/BBRKC/", folder, "/retro3/"),
              paste0(here::here(), "/BBRKC/", folder, "/retro4/"),
              paste0(here::here(), "/BBRKC/", folder, "/retro5/"),
              paste0(here::here(), "/BBRKC/", folder, "/retro6/"),
              paste0(here::here(), "/BBRKC/", folder, "/retro7/"),
              paste0(here::here(), "/BBRKC/", folder, "/retro8/"),
              paste0(here::here(), "/BBRKC/", folder, "/retro9/"),
              paste0(here::here(), "/BBRKC/", folder, "/retro10/")) #need to update these model options
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
.FIGS     = c("./BBRKC/bbrkc_23f/doc/figures/")
.TABS     = c("./BBRKC/bbrkc_23f/doc/tables/")
.FILES     = c("./BBRKC/bbrkc_23f/model_211b/retrospective/")
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
ref_mod <- 1 # base cur yr

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
  scale_colour_discrete(name  ="Model end year") +
  scale_x_continuous(breaks = seq(min(1975),max(max(ssb2$year) + 1), by = 5)) +
  theme(legend.position = c(0.8, 0.7), 
        text = element_text(size = 13), 
        axis.text = element_text(size = 13), 
        axis.title = element_text(size = 13)) +
  geom_text(x = 1993, y = 100, label = "Mohn's rho: 0.382", size = 6) # add in Mohn's rho - 
# currently calculated in excel - see retro_out_2022

ggsave(paste0(.FIGS, "ssb_retrospective_model211b.png"), width = 1.35*6, height = 9)

## Mohn's rho ssb------
ssb2 %>% 
  mutate(ssb = ssb/1000) %>% 
  mutate(model.end.yr = ifelse(Model == '21.1b (2023)', 2022, as.integer(Model)-1)) %>% 
  select(model.end.yr, year, ssb) %>% 
  spread(model.end.yr, ssb) %>% 
  mutate(year = as.integer(year)) %>% 
  #needs to be in descending order for code to work
  select(`2022`,`2021`, `2020`, `2019`, `2018`, `2017`, `2016`, `2015`, `2014`, `2013`, `2012`) -> out3
# issue because year model names as column names don't match estimates

row.names(out3) <- 1975:2022 # only works if rownames are years and retrospective estimates are in columns
# need to extend this 1 year

mohn(out3)
mohn(out3, peels = 5, details = FALSE, plot = TRUE)
mohn(out3, peels = 10, details = TRUE, plot = TRUE) # this matches excel calc - put into figure
mohn(out3, peels = 1, details = TRUE, plot = TRUE)

# output to calculate in Excel like Jie
ssb2 %>% 
  mutate(ssb = ssb/1000) %>% 
  spread(year, ssb) -> retro_out

write.csv(retro_out, paste0(.FILES, paste0("retro_out_", cur_yr, ".csv")), row.names = FALSE)



## rec -----
# calcs function not workin -------
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
#
rec7 <- data.frame(Model = names(M)[7], 
                   recruit_m = M[[7]]$recruits[1, ], 
                   recruit_f = M[[7]]$recruits[2, ])
rec7$year <- c(M[[7]]$mod_yrs)
#
rec8 <- data.frame(Model = names(M)[8], 
                   recruit_m = M[[8]]$recruits[1, ], 
                   recruit_f = M[[8]]$recruits[2, ])
rec8$year <- c(M[[8]]$mod_yrs)
#
rec9 <- data.frame(Model = names(M)[9], 
                   recruit_m = M[[9]]$recruits[1, ], 
                   recruit_f = M[[9]]$recruits[2, ])
rec9$year <- c(M[[9]]$mod_yrs)
#
rec10 <- data.frame(Model = names(M)[10], 
                   recruit_m = M[[10]]$recruits[1, ], 
                   recruit_f = M[[10]]$recruits[2, ])
rec10$year <- c(M[[10]]$mod_yrs)
#
rec11 <- data.frame(Model = names(M)[11], 
                    recruit_m = M[[11]]$recruits[1, ], 
                    recruit_f = M[[11]]$recruits[2, ])
rec11$year <- c(M[[11]]$mod_yrs)

rec1 %>% 
  rbind(rec2, rec3, rec4, rec5, rec6, rec7, rec8, rec9, rec10, rec11) -> recruits_all

recruits_all %>% 
  mutate(recruit_m = recruit_m/1000000, recruit_f = recruit_f/1000000, 
         recruits_total = (recruit_m + recruit_f)) %>% 
  mutate(year = year+1) -> recruits_all2 # according to Jie's notes recruitment estimates start in 1976 

# output to calculate in Excel like Jie-----
recruits_all2 %>%
  select(Model, year, recruits_total) %>% 
  spread(year, recruits_total) -> retro_R_out

write.csv(retro_R_out, paste0(.FILES, paste0("recruitment_retro_out_", cur_yr, ".csv")), row.names = FALSE)

# plot recruitment retro -----
recruits_all2 %>% 
  ggplot(aes(year, recruits_total, group = Model)) +
  geom_line(aes(group = Model, colour = Model), lwd = 0.75) +
  ylab("Total recruitment (million)") +
  xlab("Year") +
  #ylim(c(0,11500)) +
  theme_bw(base_size = 12, base_family = "") +
  scale_colour_discrete(name  ="Model") +
  scale_x_continuous(breaks = seq(min(1975),max(max(recruits_all2$year) + 1), by = 5)) +
  theme(legend.position = c(0.8, 0.6), 
        text = element_text(size = 13), 
        axis.text = element_text(size = 13), 
        axis.title = element_text(size = 13)) +
  geom_text(x = 1995, y = 150, label = "Mohn's rho: 1.18", size = 7) # add in Mohn's rho - currently calculated in excel - see retro_out_2022
# Mohn's rho in excel currently - need to fix this.
ggsave(paste0(.FIGS, "recruitment_retrospective_model211b.png"), width = 1.35*6, height = 9)

## ratio of recruitment to this this years model ---------
# need to create this input...currently in excel but can I just do it hear?
#head(retro_R_out)
#retro_R_out %>% 
#  select(Model, `2012`:`2022`) %>% 
#  gather("year", "rec", 2:12)

# Figure 28b
ratioR <- read_excel(paste0(here::here(), "/BBRKC/bbrkc_23f/model_211b/retrospective/recruitment_retro_out_2023.xlsx"), sheet = "rec_rinput")

ratioR %>% 
  pivot_longer(-num.yrs.est) %>% 
  ggplot(aes(num.yrs.est, value, group = name)) +
  geom_line(aes(group = name, colour = name), lwd = 1.2) +
  xlab("Number of years estimated in the model") +
  ylab("Ratios of estimated retro recruits  \n to terminal estimates in 2022") +
  theme_bw(base_size = 14, base_family = "") +
  scale_colour_discrete(name  = "Recruitment year") +
  scale_x_continuous(breaks = seq(1, 11, 1)) +
  theme(legend.position = c(0.8, 0.59), 
        text = element_text(size = 13), 
        axis.text = element_text(size = 13), 
        axis.title = element_text(size = 13))
ggsave(paste0(.FIGS, "ratio_to_terminal_yr_model211b.png"), width = 1.1*6, height = 4.2)

## Figure 28c ------------
ratioR %>% 
  pivot_longer(-num.yrs.est) %>% 
  group_by(num.yrs.est) %>% 
  summarise(avg = mean(value, na.rm = TRUE), 
            sd = sd(value, na.rm = TRUE)) %>% 
  ggplot() +
     geom_bar(aes(x = num.yrs.est, y = avg), stat = "identity", fill = "darkturquoise", width = 0.5) +
     geom_line(aes(x = num.yrs.est, y = sd), stat = "identity", lwd = 1.3, color = "red") +
  expand_limits(y=0) +
  theme_bw(base_size = 14, base_family = "") +
  scale_x_continuous(breaks = seq(1, 11, 1)) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 2.75)) +
  #ylim(0, 2.5) +
  xlab("Number of years estimated in the model") +
  ylab("Mean ratios and standard deviation of ratios")
ggsave(paste0(.FIGS, "Recruitment_Mean_sd_ratio_model211b.png"), width = 1.1*6, height = 4.0)
#### ------------------

#write.csv(ssb, paste0(.FILES, paste0("ssb_", cur_yr, ".csv")), row.names = FALSE)

#write.table(ssb, file = paste0(.FILES, "ssb_all.csv"), sep = ",",
#            append = TRUE, col.names = FALSE, row.names = FALSE)