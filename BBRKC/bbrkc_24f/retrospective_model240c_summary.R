#k.palof, 8-23-2022 / 8-24-23 / 8-18-24
# Script for compiling retrospective runs - mainly ssb and recruitment

# Currently use a new script for each model - need to make this figure a function!

# load ------------
#require(devtools)
#devtools::install_github("seacode/gmacs", subdir = "/gmr", ref = "develop") #, INSTALL_opts="--no-staged-install") #- only needs to be performed once, but needs to be 
# done again when updates are made to the package
# if gmr is updated and above doesn't work try:
# go to Build above - direct it to the gmr folder - press OK. Over on right in the Build tab (upper right hand side) - 
# click "install and restart"

#library(gmr) #require(gmr)
#source("./SMBKC/code/functions.R") 
#source("./SMBKC/code/helper.R") 
#source("./SMBKC/code/packages.R")
#source("./SMBKC/code/gmr_functions2020.R") 
#source("./BBRKC/code/bbrkc_functions.R")
library(icesAdvice)
# Model 1 plots -------------------------
# ALL retrospective model setup  -------------------------
# first model is reference to previous year
cur_yr <- 2024 # update annually 
#folder <- "bbrkc_24f/model_24_0c/retrospective" # update annually 
# The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

source("./gmacsr/gmacsr.R")
# **************************************************************************************************
cur_yr <- 2024 # update annually 
folder <- "bbrkc_24f" # update annually 
model <- "m24c"
ww <- 6
hh <- 5

plot_save <- paste0(here::here(), "/BBRKC/", folder, "/doc/figures/retro24c")

m24c <- gmacs_read_allout(file = "./BBRKC/bbrkc_24f/model_24_0c/Gmacsall.out", model_name = "m24c", version = "2.20.14")

m2023 <- gmacs_read_allout(file = paste0("./BBRKC/", folder, "/model_24_0c/retrospective/retro1/Gmacsall.out"), model_name = "2023", version = "2.20.14")
m2022 <- gmacs_read_allout(file = paste0("./BBRKC/", folder, "/model_24_0c/retrospective/retro2/Gmacsall.out"), model_name = "2022", version = "2.20.14")

m2021 <- gmacs_read_allout(file = paste0("./BBRKC/", folder, "/model_24_0c/retrospective/retro3/Gmacsall.out"), model_name = "2021", version = "2.20.14")
m2020 <- gmacs_read_allout(file = paste0("./BBRKC/", folder, "/model_24_0c/retrospective/retro4/Gmacsall.out"), model_name = "2020", version = "2.20.14")
m2019 <- gmacs_read_allout(file = paste0("./BBRKC/", folder, "/model_24_0c/retrospective/retro5/Gmacsall.out"), model_name = "2019", version = "2.20.14")
m2018 <- gmacs_read_allout(file = paste0("./BBRKC/", folder, "/model_24_0c/retrospective/retro6/Gmacsall.out"), model_name = "2018", version = "2.20.14")
m2017 <- gmacs_read_allout(file = paste0("./BBRKC/", folder, "/model_24_0c/retrospective/retro7/Gmacsall.out"), model_name = "2017", version = "2.20.14")
m2016 <- gmacs_read_allout(file = paste0("./BBRKC/", folder, "/model_24_0c/retrospective/retro8/Gmacsall.out"), model_name = "2016", version = "2.20.14")
m2015 <- gmacs_read_allout(file = paste0("./BBRKC/", folder, "/model_24_0c/retrospective/retro9/Gmacsall.out"), model_name = "2015", version = "2.20.14")
m2014 <- gmacs_read_allout(file = paste0("./BBRKC/", folder, "/model_24_0c/retrospective/retro10/Gmacsall.out"), model_name = "2014", version = "2.20.14")

retro_models <- list(m24c, m2023, m2022, m2021, m2020, m2019, m2018, m2017, m2016, m2015, m2014)

#####----------

gmacs_plot_mmb(all_out = retro_models, save_plot = T, plot_dir = plot_save, plot_proj = F, plot_ci = F ) #std_list = base_std)


#ggsave(paste0(.FIGS, "ssb_retrospective_model_23_0a.png"), width = 1.35*6, height = 8)


temp1 <- gmacs_get_derived_quantity_summary(all_out = retro_models)

temp1 %>% 
  as.data.frame() %>% 
  select(model, year, ssb) %>% 
  mutate(ssb = ssb/1000) %>% 
  mutate(model.end.yr = ifelse(model == 'm24c', 2023, as.integer(model)-1)) %>% 
  select(model.end.yr, year, ssb) %>% 
  spread(model.end.yr, ssb) %>% 
  mutate(year = as.integer(year)) %>% 
  #needs to be in descending order for code to work
  select(`2023`, `2022`,`2021`, `2020`, `2019`, `2018`, `2017`, `2016`, `2015`, `2014`, `2013`) -> out4
#out4 <- data.frame(out4)
# issue because year model names as column names don't match estimates

row.names(out4) <- 1975:2023 # only works if rownames are years and retrospective estimates are in columns


mohn(out4) # 0.0762; default is 5 
mohn(out4, peels = 5, details = FALSE, plot = TRUE)
mohn(out4, peels = 10, details = TRUE, plot = TRUE) # 0.2334216
mohn(out4, peels = 1, details = TRUE, plot = TRUE)

# output to calculate in Excel like Jie
temp1 %>% 
  as.data.frame() %>% 
  select(model, year, ssb) %>%  
  mutate(ssb = ssb/1000) %>% 
  spread(year, ssb) -> retro_out
folder <- "bbrkc_24f/model_24_0c/retrospective" # update annually 
write.csv(retro_out, paste0("./BBRKC/", folder,"/retro_out_", cur_yr, ".csv"), row.names = FALSE)



## rec -----

temp1 %>% 
  select(model, year, recruit_male, recruit_female) %>% 
  mutate(recruit_total = (recruit_male + recruit_female)/1000000, 
         year = year+1) -> recruits_all2 # according to Jie's notes recruitment estimates start in 1976 

# output to calculate in Excel like Jie-----
recruits_all2 %>%
  select(model, year, recruit_total) %>% 
  spread(year, recruit_total) -> retro_R_out

write.csv(retro_R_out, paste0("./BBRKC/", folder,"/recruitment_retro_out_", cur_yr, ".csv"), row.names = FALSE)

# plot recruitment retro -----
recruits_all2 %>% 
  ggplot(aes(year, recruit_total, group = model)) +
  geom_line(aes(group = model, colour = model), lwd = 0.75) +
  ylab("Total recruitment (million)") +
  xlab("Year") +
  #ylim(c(0,11500)) +
  theme_bw(base_size = 12, base_family = "") +
  scale_colour_discrete(name  ="model") +
  scale_x_continuous(breaks = seq(min(1975),max(max(recruits_all2$year) + 1), by = 5)) +
  theme(legend.position = c(0.8, 0.6), 
        text = element_text(size = 13), 
        axis.text = element_text(size = 13), 
        axis.title = element_text(size = 13)) #+
#geom_text(x = 1995, y = 150, label = "Mohn's rho: 1.02", size = 7) # add in Mohn's rho - currently calculated in excel - see retro_out_2022

ggsave(paste0(plot_save, "/recruitment_retrospective_model_24c.png"), width = 1.35*6, height = 9)

## ratio of recruitment to this this years model ---------
# try to manipulate here instead of excel
head(retro_R_out)

recruits_all2 %>% 
  select(model, year, recruit_total) %>% 
  filter(year >= 2014) %>% 
  spread(model, recruit_total) %>% 
  group_by(year) %>% 
  mutate(yr1 =  `2014`/`m23.0a (2024)`, 
         yr2 =  `2015`/`m23.0a (2024)`, 
         yr3 =  `2016`/`m23.0a (2024)`, 
         yr4 =  `2017`/`m23.0a (2024)`, 
         yr5 =  `2018`/`m23.0a (2024)`, 
         yr6 =  `2019`/`m23.0a (2024)`, 
         yr7 =  `2020`/`m23.0a (2024)`, 
         yr8 =  `2021`/`m23.0a (2024)`, 
         yr9 =  `2022`/`m23.0a (2024)`, 
         yr10 =  `2023`/`m23.0a (2024)`) %>% 
  select(year, yr1, yr2, yr3, yr4, yr5, yr6, yr7, yr8, yr9, yr10) %>% 
  mutate(year = year -6) -> rec_temp

rec_temp[, -1] %>% 
  t() %>% as.data.frame() %>% 
  setNames(c("2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018")) %>% 
  mutate(num.yrs.est = c(10:1)) -> ratioR
## **FIX** this isn't correct still but it's closer - bring in excel sheet for now

# Figure 28b
folder <- "bbrkc_24f/model_24_0c/retrospective" # update annually 
ratioR <- read_excel(paste0(here::here(), "/BBRKC/", folder, "/recruitment_retro_out_2024.xlsx"), sheet = "rec_input")

ratioR %>% 
  pivot_longer(-num.yrs.est) %>% 
  ggplot(aes(num.yrs.est, value, group = name)) +
  geom_line(aes(group = name, colour = name), lwd = 1.2) +
  xlab("Number of years estimated in the model") +
  ylab("Ratios of estimated retro recruits  \n to terminal estimates in 2024") +
  theme_bw(base_size = 14, base_family = "") +
  scale_colour_discrete(name  = "Recruitment year") +
  scale_x_continuous(breaks = seq(1, 11, 1)) +
  ylim(0, 7)+
  theme(legend.position = c(0.8, 0.59), 
        text = element_text(size = 13), 
        axis.text = element_text(size = 13), 
        axis.title = element_text(size = 13))
ggsave(paste0(plot_save, "/ratio_to_terminal_yr_model_24c.png"), width = 1.1*6, height = 4.2)

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
  scale_y_continuous(expand = c(0,0), limits = c(0, 2.5)) +
  #ylim(0, 2.5) +
  xlab("Number of years estimated in the model") +
  ylab("Mean ratios (bar) and \n standard deviation of ratios (red line)")
ggsave(paste0(plot_save, "/Recruitment_Mean_sd_ratio_model_24c.png"), width = 1.1*6, height = 4.0)
#### ------------------

#write.csv(ssb, paste0(.FILES, paste0("ssb_", cur_yr, ".csv")), row.names = FALSE)

#write.table(ssb, file = paste0(.FILES, "ssb_all.csv"), sep = ",",
#            append = TRUE, col.names = FALSE, row.names = FALSE)