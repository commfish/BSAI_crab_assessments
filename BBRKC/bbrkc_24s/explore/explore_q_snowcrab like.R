# k.palof
# 4-16-2024 / 4-22-25 
# expanded to include data from Buck
# q comparison similar to snow crab 

#
library(tidyverse)
library(gam)
library(ggeffects)
library(mgcViz)

# data ------
biomass_q <- read.csv(paste0(here::here(), '/BBRKC/bbrkc_24s/explore/biomass_ q analysis.csv'))
size_comp <- read.csv(paste0(here::here(), '/BBRKC/bbrkc_24s/explore/size_comps for q analysis.csv'))

# 2025 data from Buck
bsfrf_haul <- read.csv(paste0(here::here(), '/BBRKC/data/bsfrf/BBRKC_SBS.csv'))


### 2025 -----
# total count for each survey for each year 
bsfrf_haul %>% 
  group_by(year) %>% 
  summarise(tot.bsfrf = sum(N.BSFRF), tot.nmfs = sum(N.NMFS)) -> tot.all
# no sex data for this data set. Need to summarize by year and bin
bsfrf_haul %>% 
  group_by(year, bin5) %>% 
  summarise(n.bsfrf = sum(N.BSFRF), n.nmfs = sum(N.NMFS), n.tot = sum(N.tot)) %>% 
  merge(tot.all) %>% 
  mutate(prop.BSFRF = n.bsfrf/tot.bsfrf, prop.NMFS = n.nmfs/tot.nmfs) %>% # name proportions survey name for data rearrangement later
  select(year, bin5, prop.BSFRF, prop.NMFS) -> bsfrf_haul2
  
  #mutate(selectivity = n.nmfs/n.bsfrf) -> bsfrf_haul2
# need to have proportion of each size bins scaled to total in that year 

biomass_combo %>% # see code under 2024 for this data frame
  spread(survey, biomass) -> biomass_combo2
  
bsfrf_haul2 %>% 
  merge(biomass_combo2) %>% 
  mutate(bio.bsfrf = prop.BSFRF*BSFRF, bio.nmfs = prop.NMFS*NMFS, 
         selectivity = (bio.nmfs/bio.bsfrf)) %>% 
  mutate(selectivity =ifelse(!is.infinite(selectivity) == FALSE, NA, selectivity )) %>% 
  filter(selectivity <= 2) -> bsfrf_haul3

bsfrf_haul3 %>%  # this uses side by side haul data
  filter(selectivity <= 2) %>% 
  ggplot() +
  geom_point(aes(bin5, selectivity, color = as.character(year))) +
  #geom_line(data = predict_gam1_df, mapping = aes(x = x, y = predicted), col = "blue") +
  #geom_ribbon(data = predict_gam1_df, mapping = aes(x = x, ymin = clow, ymax = chigh), alpha =0.2) +
  labs(color = "year") + xlab("size (5 mm bins)") + ylab("estimated selectivity") +
  theme_bw() 

## 2025 fit gam ---
# fitting a GAM model -------------
mod_gam2 = gam(selectivity ~ s(bin5), weights = BSFRF, data = bsfrf_haul3)
summary(mod_gam2)
#Family: gaussian 
#Link function: identity 

#Formula:
#  selectivity ~ s(bin5)

#Parametric coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  0.9006     0.1217     7.4 4.85e-11 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Approximate significance of smooth terms:
#  edf Ref.df    F p-value    
#s(bin5) 7.623  8.523 1.881   0.101
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#R-sq.(adj) =  0.0946   Deviance explained =   16%
#GCV = 1.6204e+05  Scale est. = 1.4886e+05  n = 106
#plot(mod_gam1)


plot(ggeffects::ggpredict(mod_gam2), facets = TRUE)
#gratia::draw(mod_gam1)

model_p <- ggpredict(mod_gam2)
predict_gam2 <- model_p$bin5

predict_gam2_df <- data.frame(x = predict_gam2$x, predicted = predict_gam2$predicted, 
                              clow = predict_gam2$conf.low, chigh = predict_gam2$conf.high)

bsfrf_haul3 %>%  
  ggplot() +
  geom_point(aes(bin5, selectivity, color = as.character(year))) +
  geom_line(data = predict_gam2_df, mapping = aes(x = x, y = predicted), col = "blue") +
  geom_ribbon(data = predict_gam2_df, mapping = aes(x = x, ymin = clow, ymax = chigh), alpha =0.2) +
  labs(color = "year") + xlab("size (5 mm bins)") + ylab("estimated selectivity") +
  theme_bw() 
# save this ASAP!
ggsave(paste0(here::here(), "/BBRKC/bbrkc_24s/explore/by_haul_SBS_inferred selectivity_explore.png"),
       width = 5, height = 3)#width = 1.3, height = 1.25)

# input in model: 
# need mean and std error for line fit for each interval
input_gam2 <- data.frame(size_bin = predict_gam2$x, 
                         mean = predict_gam2$predicted, stdv = predict_gam2$std.error)
write.csv(input_gam2, paste0(here::here(), "/BBRKC/bbrkc_24s/explore/input_gam2.csv"), row.names = FALSE) 
# input for prior parameters 

### 2024 ------
# notes:
# need to expanded biomass by size bin but size bins add to 1 for males and females combined but biomass is separate.
# need to normalize proportions to 1 for each sex prior to expanding
head(biomass_q)
biomass_q %>% 
  select(survey, year, sex, biomass) -> biomass_merge

biomass_q %>% 
  group_by(survey, year) %>% 
  summarise(biomass = sum(biomass)) -> biomass_combo

size_comp %>% 
  rowwise() %>% 
  mutate(total = sum(c_across(X65:X160), na.rm = TRUE)) %>% 
  group_by(survey, year, sex, total) %>% 
  select(survey, year, sex, total) -> totals_sc

size_comp %>% 
  rowwise() %>% 
  mutate(total = sum(c_across(X65:X160), na.rm = TRUE)) %>% 
  group_by(survey, year, sex, total) %>% 
  gather(size, proportion, X65:X160) %>% 
  mutate(rescale_prop = proportion/total) %>% 
  #group_by(survey, year, sex) %>% 
  #summarise(sum(rescale_prop, na.rm = T)) %>% 
  #print( n = 50)
# need to merge appropriate biomass here 
  left_join(biomass_merge) %>% # get the biomass for each size bin for each survey
  mutate(prop_biomass = rescale_prop*biomass) -> temp2
# so this gets values to compare in teh prop_biomass column. For BSFRF this is 100% of crab there - since q assumed to be 1
# so to get a relative selectivity need to 
temp2 %>% 
  select(year, sex, size, survey, prop_biomass) -> temp3

head(temp3) 
temp3 %>% 
  ungroup() %>% 
  select(year, sex, size, survey, prop_biomass) %>%
  spread(survey, prop_biomass) %>% 
  mutate(size_bin = as.numeric(gsub("\\D", "", size))) %>% 
  mutate(selectivity = NMFS/BSFRF) -> temp4
# need to remove N/A and Inf values here
  
temp4 %>%  
  #filter(sex == 1) %>% 
ggplot(aes(size_bin, selectivity, color = sex) ) +
    geom_point()
# this is by sex but need sexes combined.

## combine sexes ------------
size_comp %>% 
  group_by(survey, year, sex) %>%
  gather(size, proportion, X65:X160) %>% 
  group_by(survey, year, size) %>% 
  summarise(comb_prop = sum(proportion, na.rm = T)) %>% 
  #spread(size, comb_prop) %>% 
  #rowwise() %>% 
  #mutate(total = sum(c_across(X100:X95), na.rm = TRUE)) -> btemp
  ## these confirmed that they all add to 1 when rounded.
  ## no need to rescale since they all add to 1
  # need to merge appropriate biomass here 
  left_join(biomass_combo) %>% 
  mutate(prop_biomass = comb_prop*biomass) -> Btemp2

Btemp2 %>% 
  select(year, size, survey, prop_biomass)  %>% 
  ungroup() %>% 
  select(year, size, survey, prop_biomass) %>%
  spread(survey, prop_biomass) %>% 
  mutate(size_bin = as.numeric(gsub("\\D", "", size))) %>% 
  mutate(selectivity = NMFS/BSFRF) -> Btemp4

Btemp4 %>%  
  ggplot(aes(size_bin, selectivity, group = year) ) +
  geom_point(aes(color = year))

# fitting a GAM model -------------
mod_gam1 = gam(selectivity ~ s(size_bin), weights = BSFRF, data = Btemp4)
summary(mod_gam1)
#Family: gaussian 
#Link function: identity 

#Formula:
#  selectivity ~ s(size_bin)

#Parametric coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  0.77528    0.02512   30.86   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Approximate significance of smooth terms:
#  edf Ref.df    F p-value    
#s(size_bin) 2.727  3.411 19.3  <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#R-sq.(adj) =  0.362   Deviance explained = 37.6%
#GCV = 335.87  Scale est. = 325.35    n = 119
#plot(mod_gam1)


plot(ggeffects::ggpredict(mod_gam1), facets = TRUE)
#gratia::draw(mod_gam1)

model_p <- ggpredict(mod_gam1)
predict_gam1 <- model_p$size_bin

predict_gam1_df <- data.frame(x = predict_gam1$x, predicted = predict_gam1$predicted, 
                              clow = predict_gam1$conf.low, chigh = predict_gam1$conf.high)

Btemp4 %>%  
  ggplot() +
  geom_point(aes(size_bin, selectivity, color = as.character(year))) +
  geom_line(data = predict_gam1_df, mapping = aes(x = x, y = predicted), col = "blue") +
  geom_ribbon(data = predict_gam1_df, mapping = aes(x = x, ymin = clow, ymax = chigh), alpha =0.2) +
  labs(color = "year") + xlab("size (5 mm bins)") + ylab("estimated selectivity") +
  theme_bw() 
# save this ASAP!
  ggsave(paste0(here::here(), "/BBRKC/bbrkc_24s/explore/inferred selectivity_explore.png"),
         width = 5, height = 3)#width = 1.3, height = 1.25)
  
# input in model: 
# need mean and std error for line fit for each interval
input_gam1 <- data.frame(size_bin = predict_gam1$x, 
                         mean = predict_gam1$predicted, stdv = predict_gam1$std.error)
write.csv(input_gam1, paste0(here::here(), "/BBRKC/bbrkc_24s/explore/input_gam.csv"), row.names = FALSE) 


### not working like i'd like ----------------
b <- getViz(mod_gam1)
#We extract the first smooth component using the sm function and we plot it. 
#The resulting o object contains, among other things, a ggplot object. 
#This allows us to add several visual layers.

fig1 <- plot( sm(b, 1) )
fig1 + l_fitLine(colour = "red") + #l_rug(mapping = aes(x=x, y=y), alpha = 0.8) +
  l_ciLine(mul = 5, colour = "blue", linetype = 2) + 
  l_points(shape = 19, size = 1, alpha = 0.1) + theme_classic()
