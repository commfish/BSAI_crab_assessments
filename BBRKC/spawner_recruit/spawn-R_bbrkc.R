# Date: 6-26-2024
# Katie Palof, katie.palof@alaska.gov
# RE: spawner-recruitment relationship similar to that from 1995 analysis for BBRKC harvest strategy decisions
#       - Obj: perform S-R analysis on data up to 2023
#                 - determine if female threshold is still appropriate


# load ----
library(fishmethods)
library(tidyverse)
source("./gmacsr/gmacsr.R")
# **************************************************************************************************
# The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#D55E00", "#0072B2", "#CC79A7", "#F0E442")


plot_dirt <- paste0(here::here(), "/BBRKC/spawner_recruit/figures")

# data -------
# output from GMACS assessment model - use run C:\Users\kjpalof\Documents\BSAI_crab_assessments\BBRKC\bbrkc_24f\5_30_24\23.0a_test
# this should be similar to 23.0a_p7 from May 2024 but with most recent version of GMACS

# read in gmacsall.out
m230a <- gmacs_read_allout(file = "./BBRKC/bbrkc_24f/5_30_24/23.0a_test/Gmacsall.out", model_name = "m23.0a", 
                           version = "2.20.14")

sum <- data.frame(m230a$derived_quant_summary)


# N(total) Numbers at size 1 to 20 size bins by year but this is total
# N(males)
# N(females)

N_mat <- data.frame(m230a$n_matrix)

# male reproductive potential ----
# mature male abundance (defined by CL > 119mm) * maximum number of females with which a male of a particular length can mate (Table 1 - ref f95-120.pdf)
N_mat %>% 
  select(year, size, males) %>% 
  mutate(size = as.numeric(size)) %>% 
  filter(size >= 120) %>% # just mature size classes exclude less than 120
  group_by(year) %>% 
  summarise(mat_total = sum(males)/1000000) -> MMA # should this match SSA ?? it's higher. use this for now since it's same as females

#Table 1 
mate_tab <- read.csv(paste0(here::here(), "/BBRKC/spawner_recruit/table1_male_per_female.csv"))
#mature male abundance (> 120mm CL)

N_mat %>% 
  select(year, size, males) %>% 
  mutate(size = as.numeric(size)) %>% 
  filter(size >= 120) %>% # just mature size classes exclude less than 120
  merge(mate_tab) %>% 
  mutate(mrp = males*fem_mate) %>% # male reproductive potential is the number of females each male can mate with
  #mutate(mrp_biomass = mrp) %>% # how to do this calc? this is number of females but I don't have size comps?
  group_by(year) %>% 
  summarise(total_mrp = sum(mrp)/1000000) -> MRP
#            total_mrp_biomass = sum(mrp_biomass)) -> MRP


# mature female abundance -------
# mature females > 95mm (exclude first 5 size classes)
N_mat %>% 
  select(year, size, females) %>% 
  mutate(size = as.numeric(size)) %>% 
  filter(size >= 92.5, size < 147.5) %>% 
  mutate(wt.kg_calc = (0.02286*size^(2.234))/1000) %>% 
  mutate(fem_biomass = females*wt.kg_calc) %>% # just mature size classes 6 to 16 exclude first 5
  group_by(year) %>% 
  summarise(fmat_total = sum(females)/1000000, 
            total_f_biomass = sum(fem_biomass))  -> MFA

### reproductive potential -----
# if MFA < male reproductive potential then use MFA, else female spawning abundnace set == male reproductive potential

# weight-length relationship
# MFA converted to biomass
# W = 0.02286*L^(2.234) ; unit: W in grams here, L in mm

# female spawning abundance ------------
MFA %>% 
  merge(MRP, all = TRUE) %>% 
  mutate(scale_for_mrp_b = total_f_biomass/fmat_total) %>% # biomass in kg / crabs in millions
  mutate(total_mrp_biomass = scale_for_mrp_b*total_mrp) %>% 
  group_by(year) %>% 
  mutate(fsa = ifelse(fmat_total < total_mrp, fmat_total, total_mrp), 
         esb = ifelse(fmat_total < total_mrp, total_f_biomass, total_mrp_biomass)/1000/1000) -> FSA

# Recruitment -------
# GMACS model defines recruitment as what's coming into the model; males first 7 size classes, females first 5
# should I use this? 
sum %>% 
  select(year, recruit_male, recruit_female) %>% 
  group_by(year) %>% 
  mutate(recruit_total = sum(recruit_male + recruit_female) / 1000000) %>% # millions of recruits
  select(year, recruit_total) -> recruits# in millions of crab

# data summmary -------
# bring all together for graphing analysis etc
# esb = effective spawning biomass (see above for calc); units 1000t
# recruits = millions of individuals
FSA %>% 
  merge(recruits, all = TRUE) %>% 
  mutate(logR = log(recruit_total)) -> all_data

# QUESTION: how do I get the mrp biomass when mrp is less than females? i used the male weights but not sure that's right
# paper read: "when mrp is less than fsa the esb is set equal to the biomass of hypothesized maximum number of female crabs that can be fertilized by the males available"
# see above calc where I used the scale in the same year to convert - under FSA summary

# Lagged data ----
all_data %>% 
  select(year, fsa, esb, recruit_total, logR) %>% 
  mutate(lag7_logR = dplyr::lead(logR, n=7)) -> all_data2

all_data2 %>% 
  ggplot() +
  geom_text(aes(x = esb, y = lag7_logR, label = substring(year, 3, 4)), size = 3.5)+
  #geom_point(aes(x = esb, y= lag7_logR))
  labs(x = "effective spanwing biomass (1000t)", y = "log total recruitment")

# spawner recruit fit linear model -----------------
all_data2 %>% filter(year < 2016) %>% 
  mutate(lag7_recruits = exp(lag7_logR), 
         lnRS = log(lag7_recruits/esb)) -> all_data3

# linear model based on paper????
#all_data3 %>% mutate(log_esb = log(esb)) -> all_data3
model.m1 <- lm(lnRS~esb, data = all_data3)
summary(model.m1)$coefficients[1, 1] -> intercept #extract intercept from model
summary(model.m1)$coefficients[2, 1] -> slope #extract slope from model

replacement_line = seq(from = 2, to = 100, by = 1)
as.data.frame(replacement_line) %>%
  mutate(pred = exp(intercept + slope*replacement_line)*replacement_line) %>% # predicted curve based on regression model
  ggplot(aes(x = replacement_line, y = log(pred))) +
  geom_line(color ="black") + 
  #geom_line(aes(x = replacement_line, y = replacement_line), color ="grey50", lty = 2) +
 # geom_point(aes(x = esb, y = lag7_logR), data = all_data3, color ="grey50", size=3) + 
  geom_text(aes(x = esb, y = lag7_logR, label = substring(year, 3, 4)), data = all_data3, size = 3.5)+
  #scale_x_continuous(labels = comma,breaks = seq(0, 300000, 50000), limits = c(0, 300000)) +
  #scale_y_continuous(labels = comma,breaks = seq(0, 300000, 50000), limits = c(0, 300000)) +
  labs(y = "log R (lag 7)", x =  "esb") +
  expand_limits(x = 0,y = 0) +
  theme_bw() + theme(panel.grid.major = element_blank(),plot.title = element_text(hjust = 0.5),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
ggsave(paste0(here::here(), "/BBRKC/spawner_recruit/autoRS_fig1.png"), dpi = 500, height = 4.5, width = 6, units = "in")

# IGNORE linear model but not what I want......-------
model.m1 <- lm(lag7_logR~esb, data = all_data3)
summary(model.m1)$coefficients[1, 1] -> intercept #extract intercept from model
summary(model.m1)$coefficients[2, 1] -> slope #extract slope from model

replacement_line = seq(from = 0, to = 120, by = 10)
as.data.frame(replacement_line) %>%
  mutate(pred = intercept + slope*replacement_line) %>% # predicted curve based on regression model
  ggplot(aes(x = replacement_line, y = pred)) +
  geom_line(color ="black") + 
  #geom_line(aes(x = replacement_line, y = replacement_line), color ="grey50", lty = 2) +
  geom_point(aes(x = esb, y = lag7_logR), data = all_data3, color ="grey50", size=3) + 
  #scale_x_continuous(labels = comma,breaks = seq(0, 300000, 50000), limits = c(0, 300000)) +
  #scale_y_continuous(labels = comma,breaks = seq(0, 300000, 50000), limits = c(0, 300000)) +
  labs(y = "log R", x =  "esb") +
  theme_bw() + theme(panel.grid.major = element_blank(),plot.title = element_text(hjust = 0.5),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

# idea from http://derekogle.com/fishR/examples/oldFishRVignettes/StockRecruit.pdf ------
library(FSA)
library(FSAdata)
library(nlstools)
library(plotrix)

all_data2 %>% filter(year < 2016) %>% 
  mutate(lag7_recruits = exp(lag7_logR)) -> all_data3

r3 <- srFuns(type = "Ricker", param =3)
r3s <- srStarts(lag7_recruits~esb, data = all_data3, type = "Ricker", param = 3)
unlist(r3s)

r3fit <- nls(lag7_logR~log(r3(esb, a, Rp)), data = all_data3, start = r3s, algorithm = "port", lower = c(0,0))
r0 <- lag7_logR~log(a*esb)
r0s <- r3s[1]
r0fit <- nls(r0, data = all_data3, start = r0s, algorithm = "port", lower = c(0,0))
anova(r0fit, r3fit)
AIC(r0fit, r3fit)
# Botha“small”p-value (p=0.002)andthe smaller AIC value suggest that the Ricker model with the 
#density-dependent parameter providea“better”fit to the data then the simple density-independence model.

plot(lag7_recruits~esb, data = all_data3, pch = 19)
curve(r3(x,coef(r3fit)[1],coef(r3fit)[2]),from=0,to=100,col="red",lwd=2,add=TRUE)
curve(coef(r0fit)[1]*x,from=0,to=130,col="blue",lwd=2,add=TRUE) 
legend("topright",legend=c("densityindependent","densitydependent"),col=c("blue","red"), lwd=2,cex=0.6)

overview(r3fit)
