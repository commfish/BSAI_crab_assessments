## K Palof
## 4-09-19

# change the model and version with each projection output - should make this a function....
#model <- "proj1"
#version <- "b"

# load -------
source("./SMBKC/code/helper.R")
source("./SMBKC/code/functions.R") # load function for summarising output from projection

# for all of these below
#model_yr <- "smbkc_19"  # folder year for smbkc
#proj <- "proj_1" # projection set up - involve recruitment years # projection type - see readme
#version <- "d" # projection version - see readme
#label <- "19.0 (ref)" # model label - used for SAFE
#model <- "model_1" # model name in files, folder direction

# current year ssb variability from mcmc ------------
un_cur_yr("smbkc_18a", "proj_1", "d", "model_1", "18.0")
un_cur_yr("smbkc_19", "proj_1", "d", "model_1", "19.0 (ref)")
un_cur_yr("smbkc_19", "proj_5", "d", "model_1", "19.0a (alt regime)") 
un_cur_yr("smbkc_19", "proj_1", "d", "model_5", "19.1 (fit survey)") # need to run projection
un_cur_yr("smbkc_19", "proj_1", "d", "model_1b", "19.2 (add CV pot)") # need to run projection


### rebuilding probability for 1 year above Bmsy -------------
write_rec_prob1(1, "smbkc_19", "proj_1", "d", "19.0 (ref)", "model_1")
write_rec_prob1(1, "smbkc_19", "proj_1", "aa", "19.0 (ref)", "model_1")
write_rec_prob1(1, "smbkc_19", "proj_5", "d", "19.0 (ref)", "model_1")
write_rec_prob1(1, "smbkc_19", "proj_5", "aa", "19.0 (ref)", "model_1")

# add in projections from CPT sept meeting
write_rec_prob1(1, "smbkc_19", "proj_4", "d", "19.0 (ref)", "model_1") # mix match of ref pt and recruitment time frame
write_rec_prob1(1, "smbkc_19", "proj_2", "d", "19.0 (ref)", "model_1") # ricker recruitment
write_rec_prob1(1, "smbkc_19", "proj_2", "aa", "19.0 (ref)", "model_1") # ricker recruitment
write_rec_prob1(1, "smbkc_19", "proj_2", "b", "19.0 (ref)", "model_1") # ricker recruitment, F=M
write_rec_prob1(1, "smbkc_19", "proj_2", "abc", "19.0 (ref)", "model_1") # ricker recruitment, F=M*0.75 or ABC
write_rec_prob1(1, "smbkc_19", "projSHP", "d", "19.0 (ref)", "model_1") # ricker recruitment, 
# updates to SHpolicy to reflect implementing when MMA is 75% of long term average
# ricker s-r F =0 and SHR

# updated with chages from Andre to .tpl and projection runs - prow and eps - should be the same as 2b
write_rec_prob1(1, "smbkc_19a", "proj2", "b", "19.0 (ref)", "model_1") # ricker recruitment, 


# creates .csv and .png files for each - old function

write_rec_prob(2, "proj1", "a")
write_rec_prob(2, "proj1", "b")
write_rec_prob(2, "proj1", "c")
write_rec_prob(2, "proj1", "d")


write_rec_prob(2, "proj5", "a")
write_rec_prob(2, "proj5", "b")
write_rec_prob(2, "proj5", "c")
write_rec_prob(2, "proj5", "d")

write_rec_prob(2, "proj4", "a")
write_rec_prob(2, "proj4", "b")
write_rec_prob(2, "proj4", "c")
write_rec_prob(2, "proj4", "d")
write_rec_prob(2, "proj4", "e")
write_rec_prob(2, "proj4", "e_50yrs2")
write_rec_prob(2, "proj4", "f")


write_rec_prob(2, "proj3", "a")
write_rec_prob(2, "proj3", "b")
write_rec_prob(2, "proj3", "c")
write_rec_prob(2, "proj3", "d")

write_rec_prob(2, "proj2", "a")
write_rec_prob(2, "proj2", "b")
write_rec_prob(2, "proj2", "c")
write_rec_prob(2, "proj2", "d")

write_rec_prob(2, "proj7", "a")
write_rec_prob(2, "proj7", "b")
write_rec_prob(2, "proj7", "c")
write_rec_prob(2, "proj7", "d")


# bycatch comparison figure -----------------
# use projection 1 version a and b to show no difference
no_byc <- read.csv("./projections/proj1/a/rec_prob_out_proj1a.csv")
byc <- read.csv("./projections/proj1/b/rec_prob_out_proj1b.csv")

byc %>% 
  mutate(bycatch = "avg") -> byc

no_byc %>% 
  mutate(bycatch = 'none') %>% 
  bind_rows(byc) -> byc_all

byc_all %>% 
  filter(year == 1) -> year1

year1 %>% 
  mutate(year2 = year) -> year1

byc_all %>% 
  mutate(year2 = year + 1) %>% 
  bind_rows(year1) %>% 
  ggplot(aes(year2, recovery, group = V3)) + 
  geom_line(aes(group = V3, color = bycatch), lwd = 1.25) +
  #geom_point(aes(group = V3, shape = FishMort), size = 1.5) +
  #scale_shape_manual(name = "", values = c(16, 22)) +
  scale_color_manual(name = "", values = c("darkgoldenrod3","blue3")) +
  geom_hline(yintercept = 50, color = "red", lty = "dashed", lwd = 1.5) +
  geom_vline(xintercept = 10, color = "blue", lty = 2, lwd = 1.5) +
  ggtitle("Bycatch influence - projection 1 example") +
  ylab("Probability of recovery") +
  xlab("Year") +
  theme(plot.title = element_text(hjust = 0.5)) -> plotA
ggsave(('./projections/figures/bycatch_mort_influence.png'), plotA, dpi = 800,
       width = 7.5, height = 3.75)

# weighted combinations ---------------
# bring in output csv here
# proj2 ricker and proj4 recent recruitment - all using 1978-2017 Bmsy 
p2d <- read.csv("./projections/proj2/d/rec_prob_out_proj2d.csv")
p4d <- read.csv("./projections/proj4/d/rec_prob_out_proj4d.csv")

p2d %>% 
  mutate(proj = 2, weight = 0.5) ->p2d

p4d %>% 
  mutate(proj = 4, weight = 0.5) %>% 
  bind_rows(p2d) %>% 
  mutate(w_rec = recovery*weight) %>% 
  group_by(year, V3, FishMort) %>% 
  summarise(weighted_all = sum(w_rec)) %>% 
ggplot(aes(year, weighted_all, group = V3)) + 
  geom_line() +
  geom_point(aes(group = V3, shape = FishMort), size = 2) +
  scale_shape_manual(name = "", values = c(16, 22)) +
  geom_hline(yintercept = 50, color = "red", lty = "dashed", lwd = 1.5) +
  geom_vline(xintercept = 10, color = "blue", lty = 2, lwd = 1.5) +
  ggtitle("Weighted combination of proj2d and 4d - equal weighting") +
  ylab("Probability of recovery") +
  xlab("Year") +
  ylim(0,100) +
  theme(plot.title = element_text(hjust = 0.5)) 
ggsave('./projections/figures/weighted_2d_4d_combo.png', dpi = 800,
       width = 7.5, height = 3.75)


# old code from Andre -------
TheD <- read.table(paste0("./projections/", model, "/", version, "/mcoutPROJ.rep"))[,-c(4,5,6,7,8)]
Nyear <- length(TheD[1,])-4
Nline <- length(TheD[,1])
print(Nyear)
print(Nline)
n_prob_yr <- 2

# prob of recovery 
for (Iline in 1:Nline)
  TheD[Iline,5:(4 + Nyear)] <- TheD[Iline, 5:(4 + Nyear)] / TheD[Iline, 4] * 100 


TheD %>% 
  #mutate(ratio = f_sum(., V10, V59, V9)) %>% 
  mutate_at(vars(-V1, -V2, -V3, -V9), f_test) %>% 
  mutate(id = 1:n()) %>% 
  gather(year, value, -V1, -V2, -V3, -V9, -id) %>% 
  group_by(id) %>% 
  mutate(test = as.numeric(lead(value) + value ==2),
         year = as.numeric(as.factor(year))) %>% 
  group_by(year, V3) %>% 
  summarise(recovery = sum(test) / n() * 100)  %>% 
  mutate(FishMort = ifelse(V3 == 1, "F = 0", "F = 0.18")) %>% 
  ggplot(aes(year, recovery, group = V3)) + 
  geom_line() +
  geom_point(aes(group = V3, shape = FishMort), size = 2) +
  scale_shape_manual(name = "", values = c(16, 22)) +
  geom_hline(yintercept = 50, color = "red", lty = "dashed", lwd = 1.5) +
  geom_vline(xintercept = 10, color = "blue", lty = 2, lwd = 1.5) +
  ggtitle(paste0(model, version)) +
  ylab("Probability of recovery") +
  xlab("Year") +
  theme(plot.title = element_text(hjust = 0.5))
