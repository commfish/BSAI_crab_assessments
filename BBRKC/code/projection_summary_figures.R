# k.palof - 8-27-22/ 8-27-23 / 8-23-2024

# Objective: code to summarize projection output from GMACS for
#       1) ssb projected
#       2) histograms

## load ------
library(ggplot2)
source("./SMBKC/code/helper.R") 
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
.THEME    = list(theme_bw(base_size = 12, base_family = ""), scale_fill_manual(values=cbPalette), 
                 scale_colour_manual(values=cbPalette))
#.FIGS     = c("./BBRKC/bbrkc_24f/doc/figures/")
.FIGS     = c("./BBRKC/bbrkc_24f/doc/figures/state/")

folder = "bbrkc_24f"
#model = "m23.0a"
#folder2 = "model_23_0a_ph7_24_mcmc"

model = "m24.0c"
#folder2 = "model_24_0c_mcmc"
folder2 = "model_24_0c_mcmc_statePrj"

###### data m 21.1b ------

#Bproj <- read.table("C:/Users/kjpalof/Documents/Current projects/statewide shellfish/bbrkc/rk22s/mcoutPROJ211b.rep", 
#                    header = TRUE)
# these have more F values for SHS purposes 
#Bproj <- read.table(paste0(here::here(), "/BBRKC/", folder, "/model_211b-mcmc/mcoutPROJ.rep"), header = T)
#B_ref <- read.table(paste0(here::here(), "/BBRKC/", folder, "/model_211b-mcmc/mcoutREF.rep"), header = T)

# original figures for SAFE 
Bproj <-read.table(paste0(here::here(), "/BBRKC/", folder, "/", folder2, "/mcoutPROJSSB.rep"), header = T)
B_ref <- read.table(paste0(here::here(), "/BBRKC/", folder, "/", folder2, "/mcoutREF.rep"), header = T)
# make sure to change back figure name - line !!

## ssb proj data summary -------------
Bproj %>% 
  select(F_val, f_for_fleet_1, BMSY, SSB_2024:SSB_2034) %>% #needs to be updated with correct years
  group_by(F_val) %>% 
  summarise(across(everything(), mean))

Bproj%>% 
  select(F_val, f_for_fleet_1, BMSY, SSB_2024:SSB_2034) %>% #needs to be updated with correct years
  group_by(F_val) %>% 
  #filter(F_val == 1) %>% 
  gather(xvar, value, BMSY:SSB_2034) %>% 
  group_by(F_val, xvar) %>% 
  summarise(mean.x = quantile(value, probs = 0.50), 
            lower.x = quantile(value, probs = 0.05),
            upper.x = quantile(value, probs = 0.95)) -> sum1
sum1 %>% 
  filter(xvar == "BMSY") %>% 
  select(F_val, mean.x) %>% 
  spread(F_val, mean.x) -> B_BMSY

  

# Figure for all F values together ------
## FIGURE 32 all together ------

# need a column of years 
sum1 %>% 
  filter(xvar != "BMSY") %>% 
  mutate(year = gsub("[^0-9]", "", xvar), 
         F_val = as.character(F_val)) %>% 
  select(-xvar) %>% 
  #filter(F_val <= 4) %>% 
  ggplot(aes(year, mean.x, group = F_val, fill = F_val))+
    geom_line(aes(color = F_val)) +
    geom_ribbon(aes(x=year, ymax = upper.x, ymin = lower.x), alpha = 0.15) +
    #scale_fill_manual(name = "", labels = c("F=0", "F=0.083", "F=0.167", "F=0.25")) +
    #labs(fill = "Fishing mortality") +
    ylab(bquote(MMB[yr[t+1]])) +
    xlab("Year") +
    ggtitle(paste0("Model ", model)) +
    geom_hline(aes(yintercept = (B_BMSY$`1`/2)), color = "#999999", lty = "dashed") +
  geom_hline(aes(yintercept = (B_BMSY$`1`)), color = "#999999") +
  geom_text(aes(x = 1.2, y = B_BMSY$`1`, label = "B[MSY]"), 
           hjust = -0.45, vjust = -0.75, nudge_y = 0.05, size = 4.0, parse = T) +
  geom_text(aes(x = 0.1, y = B_BMSY$`1`/2, label = "50% Bmsy"), 
            hjust = -0.45, vjust = -0.75, nudge_y = 0.05, size = 4.0) +
  scale_fill_discrete(labels = c("F=0", "F=0.07", "F=0.15", "F=0.22"))+
  #scale_fill_discrete(labels = c("F=0", "F=0.083", "F=0.167", "F=0.25"))+
  #scale_fill_discrete(labels = c("F=0", "F=0.038", "F=0.071", "F=0.107","F=0.143", "F=0.179", "F=0.214", "F=0.25"))+
  labs(fill = "Fishing mortality") +
  guides(color = "none") 

ggsave(paste0(.FIGS, "proj_ssb_model_", model, ".png"), width = 7, height = 6) # version 3 uncomment line 61 
#ggsave(paste0(.FIGS, "proj_ssb_model_211b.png"), width = 7, height = 6)
 # .THEME

### seperate out F values -------------
Bproj%>% 
  select(F_val, f_for_fleet_1, BMSY, SSB_2023:SSB_2033) %>% # update years
  group_by(F_val) %>% 
  filter(F_val == 2) -> Bproj_F08

Bproj%>% 
  select(F_val, f_for_fleet_1, BMSY, SSB_2023:SSB_2033) %>% 
  group_by(F_val) %>% 
  filter(F_val == 3) -> Bproj_F16

Bproj%>% 
  select(F_val, f_for_fleet_1, BMSY, SSB_2023:SSB_2033) %>% 
  group_by(F_val) %>% 
  filter(F_val == 4) -> Bproj_F25


# need a column of years 
sum1 %>% 
  filter(xvar != "BMSY") %>% 
  mutate(year = gsub("[^0-9]", "", xvar), 
         F_val = as.character(F_val)) %>% 
  select(-xvar) %>% 
  filter(F_val <= 2) %>% 
  ggplot(aes(year, mean.x, group = F_val, fill = F_val))+
  geom_line(aes(color = F_val)) +
  geom_ribbon(aes(x=year, ymax = upper.x, ymin = lower.x), alpha = 0.15) +
  #scale_fill_manual(name = "", labels = c("F=0", "F=0.083", "F=0.167", "F=0.25")) +
  #labs(fill = "Fishing mortality") +
  ylab(bquote(MMB[yr[t+1]])) +
  xlab("Year") +
  ggtitle(paste0("Model ", model)) +
  geom_hline(aes(yintercept = (B_BMSY$`1`/2)), color = "#999999", lty = "dashed") +
  geom_hline(aes(yintercept = (B_BMSY$`1`)), color = "#999999") +
  geom_text(aes(x = 1.2, y = B_BMSY$`1`, label = "B[MSY]"), 
            hjust = -0.45, vjust = -0.75, nudge_y = 0.05, size = 4.0, parse = T) +
  geom_text(aes(x = 0.1, y = B_BMSY$`1`/2, label = "50% Bmsy"), 
            hjust = -0.45, vjust = -0.75, nudge_y = 0.05, size = 4.0) +
  scale_fill_discrete(labels = c("F=0", "F=0.083", "F=0.167", "F=0.25"))+
  #scale_fill_discrete(labels = c("F=0", "F=0.038", "F=0.071", "F=0.107","F=0.143", "F=0.179", "F=0.214", "F=0.25"))+
  labs(fill = "Fishing mortality") +
  guides(color = "none") 
ggsave(paste0(.FIGS, "proj_ssb_STATE_v1_", model, ".png"), width = 7, height = 6)



# histograms --------------
head(B_ref)

# mmb histo ------
B_ref %>% 
  mutate(MMB = BMSY*BMSY.B0) %>% 
  select(Draw, OFL, MMB) %>% 
  #filter(MMB <= 21500) %>% 
  ggplot(aes(MMB/1000)) +
   geom_histogram(color = "black", fill = "grey70", bins = 15) +
   xlab("MMB on 2/15 (1000 t)")+
   ylab("Count") +
   ggtitle(paste0("Model ", model)) +
   scale_x_continuous(breaks = 11:21) 
  #xlim(12, 22)

ggsave(paste0(.FIGS, "proj_MMB_histogram_", model, ".png"), width = 7, height = 3.5)
## MMB density ----
B_ref %>% 
  mutate(MMB = BMSY*BMSY.B0) %>% 
  select(Draw, OFL, MMB) %>% 
  #filter(MMB <= 21500) %>% 
  ggplot(aes(MMB/1000)) +
  geom_density(fill = "lightblue")+
  geom_vline(aes(xintercept = m230a_24$mmb_curr/1000), ### change this with 23.0a #m230a_24 m24c
             color="black", linetype="dashed", size=1) +
  xlab("MMB on 2/15 (1000 t)")+
  ylab("Count") +
  ggtitle(paste0("Model ", model)) +
  scale_x_continuous(breaks = 11:21) 
ggsave(paste0(.FIGS, "proj_MMB_density_plot_model_", model, ".png"), width = 7, height = 3.5)



## OFL histo ------------
B_ref %>% 
  mutate(MMB = BMSY*BMSY.B0) %>% 
  select(Draw, OFL, MMB) %>% 
  #filter(OFL <= 4500) %>% 
  ggplot(aes(OFL/1000)) +
  geom_histogram(color = "black", fill = "grey70", bins = 15) +
  #geom_density()+
  xlab("OFL (1000 t)")+
  ylab("Count") +
  ggtitle(paste0("Model ", model)) +
  scale_x_continuous(breaks = 1:5) 
#xlim(12, 22)

ggsave(paste0(.FIGS, "proj_OFL_histogram_model_", model, ".png"), width = 7, height = 3.5)

## OFL density ----
B_ref %>% 
  select(Draw, OFL) %>% 
  ggplot(aes(x = OFL/1000))+
  geom_density(fill = "lightblue")+
  geom_vline(aes(xintercept = m230a_24$reference_points %>% filter(parameter_name == "OFL_tot") %>% pull(estimate)/1000), #m230a_24 m24c
             color="black", linetype="dashed", size=1) +
  theme_classic() +
  geom_text(aes(x = 5.7, y = 0.70, label = paste0("Model ", model, " OFL")), size = 3.5)+
  ylab("Density") +
  xlab("OFL (1000 t)")
ggsave(paste0(.FIGS, "proj_OFL_density_plot_model_", model, ".png"), width = 7, height = 3.5)



## cumulative probability MMB current year------------
head(B_ref)

B_ref %>% 
  select(BMSY.B0) -> temp1
  

ggplot(temp1, aes(BMSY.B0))+
  stat_ecdf(geom = "step")+
  xlab(expression(MMB[2024]/MMB[35~percent]))+
  ylab("Cumulative probability")+
  scale_x_continuous(breaks = seq(0.4, 1.00, by = 0.1)) +
  geom_vline(xintercept = 0.5, color = "red", lty = "dashed") +
  xlab(expression(MMB[2024]/MMB[35~percent]))+
  ggtitle(paste0("Model ", model)) +
  .THEME
ggsave(paste0(.FIGS, "proj_CDF_MMB_model_", model, ".png"), width = 6, height = 7.5)
  
#CDF <- ecdf(temp1$BMSY.B0)
#plot(CDF)

## cumulative probability in each year ------------
head(Bproj)

Bproj %>% 
  select(F_val, BMSY, SSB_2024:SSB_2027) %>% # need to change years here
  mutate(year24 = SSB_2024/BMSY,
         year25 = SSB_2025/BMSY,
         year26 = SSB_2026/BMSY,
         year27 = SSB_2027/BMSY,
         F_valu = ifelse(F_val == 1, "F=0", 
                         ifelse(F_val == 2, "F=0.083", 
                                ifelse(F_val ==3, "F=0.167", "F=0.25")))) %>% #"F=0", "F=0.083", "F=0.167", "F=0.25"
  select(F_valu, year24:year27) %>% 
  gather(year, value, year24:year27) -> temp2

yearlabel <- c("Feb.15th,2025", "Feb.15th,2026", "Feb.15th,2027", "Feb.15th,2028")
names(yearlabel) <- c("year24", "year25", "year26", "year27")

ggplot(temp2, aes(value, group = year))+
  stat_ecdf(geom = "step")+
  #xlab(expression(MMB[2022]/MMB[35~percent]))+
  ylab("Cumulative probability")+
  facet_wrap(~year + F_valu, 
             labeller = labeller(year = yearlabel)) +
  xlim(0.25, 1.1) +
  geom_hline(yintercept = 0.5, color ="red", lty = "dashed") +
  geom_vline(xintercept = 0.5, color = "red", lty = "dashed") +
  theme_bw()+
  theme(strip.text.x = element_text(margin = margin(2, 0, 2, 0)), 
        #panel.grid.major = element_blank(), 
        plot.background = element_blank())
        #panel.grid.minor = element_blank()) 
ggsave(paste0(.FIGS, "proj_CDF_MMB_by_year_model_", model, ".png"), width = 6.5, height = 7.5)


# pdf of the OFL -------------
head(B_ref)

pdf <- dnorm(B_ref$OFL, mean(B_ref$OFL), sd(B_ref$OFL))
plot(B_ref$OFL, pdf)


## instead of using below just recreate above with the other model name????
## 2024 stop here -----------------------------------
# data m 23.0a ------

#Bproj <- read.table("C:/Users/kjpalof/Documents/Current projects/statewide shellfish/bbrkc/rk22s/mcoutPROJ211b.rep", 
#                    header = TRUE)
model_folder = "model_230a"
Model = "Model 23.0a"
Bproj <- read.table(paste0(here::here(), "/BBRKC/", folder, "/", model_folder, "-mcmc/mcoutPROJ.rep"), header = T)
B_ref <- read.table(paste0(here::here(), "/BBRKC/", folder, "/", model_folder, "-mcmc/mcoutREF.rep"), header = T)

## ssb proj data summary -------------
Bproj %>% 
  select(F_val, f_for_fleet_1, BMSY, SSB_2023:SSB_2033) %>% #needs to be updated with correct years
  group_by(F_val) %>% 
  summarise(across(everything(), mean))

Bproj%>% 
  select(F_val, f_for_fleet_1, BMSY, SSB_2023:SSB_2033) %>% #needs to be updated with correct years
  group_by(F_val) %>% 
  #filter(F_val == 1) %>% 
  gather(xvar, value, BMSY:SSB_2032) %>% 
  group_by(F_val, xvar) %>% 
  summarise(mean.x = quantile(value, probs = 0.50), 
            lower.x = quantile(value, probs = 0.05),
            upper.x = quantile(value, probs = 0.95)) -> sum1
sum1 %>% 
  filter(xvar == "BMSY") %>% 
  select(F_val, mean.x) %>% 
  spread(F_val, mean.x) -> B_BMSY



# Figure for all F values together ------
## FIGURE 32 all together ------

# need a column of years 
sum1 %>% 
  filter(xvar != "BMSY") %>% 
  mutate(year = gsub("[^0-9]", "", xvar), 
         F_val = as.character(F_val)) %>% 
  select(-xvar) %>% 
  ggplot(aes(year, mean.x, group = F_val, fill = F_val))+
  geom_line(aes(color = F_val)) +
  geom_ribbon(aes(x=year, ymax = upper.x, ymin = lower.x), alpha = 0.15) +
  #scale_fill_manual(name = "", labels = c("F=0", "F=0.083", "F=0.167", "F=0.25")) +
  #labs(fill = "Fishing mortality") +
  ylab(bquote(MMB[yr[t+1]])) +
  xlab("Year") +
  ggtitle(paste0(Model)) +
  geom_hline(aes(yintercept = (B_BMSY$`1`/2)), color = "#999999", lty = "dashed") +
  geom_hline(aes(yintercept = (B_BMSY$`1`)), color = "#999999") +
  geom_text(aes(x = 1.2, y = B_BMSY$`1`, label = "B[MSY]"), 
            hjust = -0.45, vjust = -0.75, nudge_y = 0.05, size = 4.0, parse = T) +
  geom_text(aes(x = 0.1, y = B_BMSY$`1`/2, label = "50% Bmsy"), 
            hjust = -0.45, vjust = -0.75, nudge_y = 0.05, size = 4.0) +
  scale_fill_discrete(labels = c("F=0", "F=0.083", "F=0.167", "F=0.25"))+
  labs(fill = "Fishing mortality") +
  guides(color = "none") 

ggsave(paste0(.FIGS, paste0("proj_ssb_", model_folder, ".png")), width = 7, height = 6)
# .THEME

### seperate out F values -------------
Bproj%>% 
  select(F_val, f_for_fleet_1, BMSY, SSB_2023:SSB_2033) %>% # update years
  group_by(F_val) %>% 
  filter(F_val == 2) -> Bproj_F08

Bproj%>% 
  select(F_val, f_for_fleet_1, BMSY, SSB_2023:SSB_2033) %>% 
  group_by(F_val) %>% 
  filter(F_val == 3) -> Bproj_F16

Bproj%>% 
  select(F_val, f_for_fleet_1, BMSY, SSB_2023:SSB_2033) %>% 
  group_by(F_val) %>% 
  filter(F_val == 4) -> Bproj_F25

# histograms --------------
head(B_ref)

# mmb histo ------
B_ref %>% 
  mutate(MMB = BMSY*BMSY.B0) %>% 
  select(Draw, OFL, MMB) %>% 
  filter(MMB <= 21500) %>% 
  ggplot(aes(MMB/1000)) +
  geom_histogram(color = "black", fill = "grey70", bins = 15) +
  xlab("MMB on 2/15 (1000 t)")+
  ggtitle(Model) +
  scale_x_continuous(breaks = 11:21) 
#xlim(12, 22)

ggsave(paste0(.FIGS, "proj_MMB_histogram_", model_folder, ".png"), width = 7, height = 3.5)


## OFL histo ------------
B_ref %>% 
  mutate(MMB = BMSY*BMSY.B0) %>% 
  select(Draw, OFL, MMB) %>% 
  #filter(OFL <= 4500) %>% 
  ggplot(aes(OFL/1000)) +
  geom_histogram(color = "black", fill = "grey70", bins = 15) +
  xlab("OFL (1000 t)")+
  ggtitle(Model) +
  scale_x_continuous(breaks = 1:7) 
#xlim(12, 22)

ggsave(paste0(.FIGS, "proj_OFL_histogram_", model_folder, ".png"), width = 7, height = 3.5)


## cumulative probability MMB current year------------
head(B_ref)

B_ref %>% 
  select(BMSY.B0) -> temp1


ggplot(temp1, aes(BMSY.B0))+
  stat_ecdf(geom = "step")+
  xlab(expression(MMB[2022]/MMB[35~percent]))+
  ylab("Cumulative probability")+
  scale_x_continuous(breaks = seq(0.4, 1.00, by = 0.1)) +
  geom_vline(xintercept = 0.5, color = "red", lty = "dashed") +
  xlab(expression(MMB[2022]/MMB[35~percent]))+
  .THEME
ggsave(paste0(.FIGS, "proj_CDF_", model_folder, ".png"), width = 6, height = 7.5)

#CDF <- ecdf(temp1$BMSY.B0)
#plot(CDF)

## cumulative probability in each year ------------
head(Bproj)

Bproj %>% 
  select(F_val, BMSY, SSB_2023:SSB_2026) %>% # need to change years here
  mutate(year23 = SSB_2023/BMSY, 
         year24 = SSB_2024/BMSY,
         year25 = SSB_2025/BMSY,
         year26 = SSB_2026/BMSY, 
         F_valu = ifelse(F_val == 1, "F=0", 
                         ifelse(F_val == 2, "F=0.083", 
                                ifelse(F_val ==3, "F=0.167", "F=0.25")))) %>% #"F=0", "F=0.083", "F=0.167", "F=0.25"
  select(F_valu, year23:year26) %>% 
  gather(year, value, year23:year26) -> temp2

yearlabel <- c("Feb.15th,2024", "Feb.15th,2025", "Feb.15th,2026", "Feb.15th,2027")
names(yearlabel) <- c("year23", "year24", "year25", "year26")

ggplot(temp2, aes(value, group = year))+
  stat_ecdf(geom = "step")+
  #xlab(expression(MMB[2022]/MMB[35~percent]))+
  ylab("Cumulative probability")+
  facet_wrap(~year + F_valu, 
             labeller = labeller(year = yearlabel)) +
  xlim(0.25, 0.9) +
  geom_hline(yintercept = 0.5, color ="red", lty = "dashed") +
  geom_vline(xintercept = 0.5, color = "red", lty = "dashed") +
  theme_bw()+
  theme(strip.text.x = element_text(margin = margin(2, 0, 2, 0)), 
        #panel.grid.major = element_blank(), 
        plot.background = element_blank())
#panel.grid.minor = element_blank()) 
ggsave(paste0(.FIGS, "proj_CDF_MMB_by_year_", model_folder, ".png"), width = 6.5, height = 7.5)


# TAC setting 21.1b----------------

nums<-expand.grid(c("r1","r2"),c("m1","m2"))
r_m<-apply(nums, 1, function(x) paste(x[!is.na(x) & x != "No"], collapse = "_"))
f_scene<-c("0_f","1_0.083","2_0.167","3_0.25")
all_dir<-expand.grid(r_m,f_scene)
#fin_dir<-apply(all_dir, 1, function(x) paste(x[!is.na(x) & x != "No"], collapse = "_"))
fin_dir <- "10year_projections_recent_recruit"
name_in<-expand.grid(c("Rec = 2013-2022"),c("M = 2022"))
rootname<-"C:/Users/kjpalof/Documents/BSAI_crab_assessments/BBRKC/bbrkc_23f/model_211b-mcmc/"


#C:\Users\kjpalof\Documents\BSAI_crab_assessments\BBRKC\bbrkc_23f\model_211b-mcmc\10year_projections_recent_recruit
draw_dirs_tac<-rep(NA,length(fin_dir))
all_summ<-NULL

for(x in 1:length(fin_dir))
{
  draw_dirs_tac[x]<-paste(rootname, fin_dir[x],"/mcoutDIAG.rep",sep='')
  tac_file<-  repfile<-scan( draw_dirs_tac[x],skip=4,what='list')
  ofl_pt<-grep("OFL",tac_file)
  chk_pt<-grep("Decision",tac_file)
  abc_pt <- grep("Retained", tac_file)
  bmsy_pt<-grep("BMSY",tac_file)
  
  removalsA<-as.numeric(tac_file[chk_pt+2]) # this grabs the "stateTAC" but not sure where that's coming from since the harvest strategy is not coded into GMACS?
  removalsB <- as.numeric(tac_file[abc_pt+2])
  year<-as.numeric(tac_file[ofl_pt-1])
  treatment<-as.numeric(tac_file[chk_pt+5])
  replic<-as.numeric(rep(tac_file[bmsy_pt+2],each=16))
  
  df<-data.frame(removalsS=(removalsA),removalsR=(removalsB),year=(year),treat=treatment,rep=replic)
  #casted<-dcast(df,treat+rep~year,value.var='removals')
  
  #==filter for what we want
# just for 0.083
  tmp<-filter(df,treat == 0.0833333000)
  tmp<- filter(tmp, year <= 2024) # Use 2024 since 2023 is set in the file?
#NOTE: 2023 values are all the same because those are dictated by the projection
  # built into GMACS. Not sure this actual gives the correct F value? maybe 2024 onward it would?
  # ALSO: "decision" seems low - not sure where that's coming from vs. retained ABC
  # need to dig into this projection code to see what's actually going on.
  # 2023 gave Ben upper limit of "decision" since it seems in ballpark
  
  # mean 
tmp %>% 
  group_by(year) %>% 
  summarise(mean.xS = quantile(removalsS, probs = 0.50), 
            mean.xR = quantile(removalsR, probs = 0.50), 
            lower.xS = quantile(removalsS, probs = 0.05),
            upper.xS = quantile(removalsS, probs = 0.95), 
            lbs_mean = mean.xS*2204.62/1e6, 
            upper.lbs = upper.xS*2204.62/1e6)
            #lbs_meanR = mean.xR*2204.62/1e6) #conversion to million pounds

# just for 0.25
tmp<-filter(df,treat == 0.1666670000)
tmp<- filter(tmp, year <= 2024) # Use 2024 since 2023 is set in the file?

# mean 
tmp %>% 
  group_by(year) %>% 
  summarise(mean.xS = quantile(removalsS, probs = 0.50), 
            mean.xR = quantile(removalsR, probs = 0.50), 
            lower.xS = quantile(removalsS, probs = 0.05),
            upper.xS = quantile(removalsS, probs = 0.95), 
            lbs_mean = mean.xS*2204.62/1e6, 
            upper.lbs = upper.xS*2204.62/1e6)
#lbs_meanR = mean.xR*2204.62/1e6) #conversion to million pounds

#   make_rib<-filter(casted,treat!=1e-10)[,3:ncol(casted)]
#   # if(length(grep('bycatch',fin_dir[x]))>0)
#   #   make_rib<-filter(casted,treat==1e-10)[,3:ncol(casted)]
#   sorted<-apply(make_rib,2,sort)
#   up_rib<-sorted[0.05*nrow(sorted),]
#   dn_rib<-sorted[0.95*nrow(sorted),]
#   md_rib<-apply(sorted,2,median)
#   
#   df2<-data.frame(year=colnames(sorted),removals=md_rib,upper=up_rib,lower=dn_rib)
#   #==names
#   df2$"Recruitment"<-"Rec = 1982-2017"
#   if(length(grep('r2',fin_dir[x]))>0)
#     df2$"Recruitment"<-"Rec = 2005-2019"
#   df2$"Mortality"<-"M = 1982-2017"
#   if(length(grep('m2',fin_dir[x]))>0)
#     df2$"Mortality"<-"M = 2005-2019"  
#   
#   tmp<-unlist(strsplit(fin_dir[x],split='_'))
#   df2$fmort<-paste(tmp[4:length(tmp)],sep="",collapse="")
#   
#   all_summ<-rbind(all_summ,df2)
# }
# 
# 
# use_it<-filter(all_summ,fmort!='f')
write.csv(all_summ,"TAC_projections.csv")

# TAC setting 23.0a----------------

nums<-expand.grid(c("r1","r2"),c("m1","m2"))
r_m<-apply(nums, 1, function(x) paste(x[!is.na(x) & x != "No"], collapse = "_"))
f_scene<-c("0_f","1_0.083","2_0.167","3_0.25")
all_dir<-expand.grid(r_m,f_scene)
#fin_dir<-apply(all_dir, 1, function(x) paste(x[!is.na(x) & x != "No"], collapse = "_"))
#fin_dir <- "10year_projections_recent_recruit"
name_in<-expand.grid(c("Rec = 2013-2022"),c("M = 2022"))
rootname<-"C:/Users/kjpalof/Documents/BSAI_crab_assessments/BBRKC/bbrkc_23f/model_230a-mcmc/"


#C:\Users\kjpalof\Documents\BSAI_crab_assessments\BBRKC\bbrkc_23f\model_230a-mcmc
draw_dirs_tac<-rep(NA,length(fin_dir))
all_summ<-NULL

for(x in 1:length(fin_dir))
{
  draw_dirs_tac[x]<-paste(rootname, "/mcoutDIAG.rep",sep='')
  tac_file<-  repfile<-scan( draw_dirs_tac[x],skip=4,what='list')
  ofl_pt<-grep("OFL",tac_file)
  chk_pt<-grep("Decision",tac_file)
  abc_pt <- grep("Retained", tac_file)
  bmsy_pt<-grep("BMSY",tac_file)
  
  removalsA<-as.numeric(tac_file[chk_pt+2]) # this grabs the "stateTAC" but not sure where that's coming from since the harvest strategy is not coded into GMACS?
  removalsB <- as.numeric(tac_file[abc_pt+2])
  year<-as.numeric(tac_file[ofl_pt-1])
  treatment<-as.numeric(tac_file[chk_pt+5])
  replic<-as.numeric(rep(tac_file[bmsy_pt+2],each=11))
  
  df<-data.frame(removalsS=(removalsA),removalsR=(removalsB),year=(year),treat=treatment,rep=replic)
  #casted<-dcast(df,treat+rep~year,value.var='removals')
  
  #==filter for what we want
  # just for 0.083
  tmp<-filter(df,treat == 0.0833333000)
  tmp<- filter(tmp, year <= 2024) # Use 2024 since 2023 is set in the file?
  #NOTE: 2023 values are all the same because those are dictated by the projection
  # built into GMACS. Not sure this actual gives the correct F value? maybe 2024 onward it would?
  # ALSO: "decision" seems low - not sure where that's coming from vs. retained ABC
  # need to dig into this projection code to see what's actually going on.
  # 2023 gave Ben upper limit of "decision" since it seems in ballpark
  
  # mean 
  tmp %>% 
    group_by(year) %>% 
    summarise(mean.xS = quantile(removalsS, probs = 0.50), 
              mean.xR = quantile(removalsR, probs = 0.50), 
              lower.xS = quantile(removalsS, probs = 0.05),
              upper.xS = quantile(removalsS, probs = 0.95), 
              lbs_mean = mean.xS*2204.62/1e6, 
              upper.lbs = upper.xS*2204.62/1e6)
  #lbs_meanR = mean.xR*2204.62/1e6) #conversion to million pounds
  
  # just for 0.25
  tmp<-filter(df,treat == 0.1666670000)
  tmp<- filter(tmp, year <= 2024) # Use 2024 since 2023 is set in the file?
  
  # mean 
  tmp %>% 
    group_by(year) %>% 
    summarise(mean.xS = quantile(removalsS, probs = 0.50), 
              mean.xR = quantile(removalsR, probs = 0.50), 
              lower.xS = quantile(removalsS, probs = 0.05),
              upper.xS = quantile(removalsS, probs = 0.95), 
              lbs_mean = mean.xS*2204.62/1e6, 
              upper.lbs = upper.xS*2204.62/1e6)
  #lbs_meanR = mean.xR*2204.62/1e6) #conversion to million pounds
  
  #   make_rib<-filter(casted,treat!=1e-10)[,3:ncol(casted)]
  #   # if(length(grep('bycatch',fin_dir[x]))>0)
  #   #   make_rib<-filter(casted,treat==1e-10)[,3:ncol(casted)]
  #   sorted<-apply(make_rib,2,sort)
  #   up_rib<-sorted[0.05*nrow(sorted),]
  #   dn_rib<-sorted[0.95*nrow(sorted),]
  #   md_rib<-apply(sorted,2,median)
  #   
  #   df2<-data.frame(year=colnames(sorted),removals=md_rib,upper=up_rib,lower=dn_rib)
  #   #==names
  #   df2$"Recruitment"<-"Rec = 1982-2017"
  #   if(length(grep('r2',fin_dir[x]))>0)
  #     df2$"Recruitment"<-"Rec = 2005-2019"
  #   df2$"Mortality"<-"M = 1982-2017"
  #   if(length(grep('m2',fin_dir[x]))>0)
  #     df2$"Mortality"<-"M = 2005-2019"  
  #   
  #   tmp<-unlist(strsplit(fin_dir[x],split='_'))
  #   df2$fmort<-paste(tmp[4:length(tmp)],sep="",collapse="")
  #   
  #   all_summ<-rbind(all_summ,df2)
  # }
  # 
  # 
  # use_it<-filter(all_summ,fmort!='f')
  write.csv(all_summ,"TAC_projections.csv")
  
### archived NOT used -----------
## proj-mmb.cnm jie's code ----------------
H<-read.table("mcoutPROJ193g.rep")
c1<-H[c(1:1000),c(11:21)]
c2<-H[c(1:1000),c(11:21)]
c3<-H[c(1:1000),c(11:21)]
c4<-H[c(1:1000),c(11:21)]
for (i in 1:2)
{
  for (j in 1:500)
  {
    if (i == 1) n2 = j
    else n2 = 500+j
    for (k in 1:4)
    {
      if (k == 1) c1<-H[n2,c(11:21)]/1000.0
      if (k == 2) c2<-H[n2,c(11:21)]/1000.0
      if (k == 3) c3<-H[n2,c(11:21)]/1000.0
      if (k == 4) c4<-H[n2,c(11:21)]/1000.0
    }
  }
}   
c5<-sort(c1[c(1:1000),1])
c6<-(c5[500,c(1:11)]+c5[501,c(1:11)])/2.0
c7<-c5[50,c(1:11)]
c8<-c5[951,c(1:11)]
par(oma=c(3.50,4.0,1.0,1.0),tck=-0.01,xaxs="i",yaxs="i",font=1,lwd=0.75)
par(mgp=c(1.2,1.0,0),mar=c(0,0,0,0),mfcol=c(n1,n2),xpd=FALSE)
xat<-c(2021,2022,2023,2024,2025,2026,2027,2028,2029,2030,2031)
yat<-c(0,5,10,15,20,25,30)
xm<-2020
xx<-2032
ym<-0
up<-0.87
year<-c(2021:2031)

yx<-32.0
plot(year,c6,axes=FALSE,ylim=c(ym,yx),xlim=c(xm,xx),type="n",lwd=1)
lines(year,c6,lty=1,lwd=2.5,col=1)
lines(year,c7,lty=2,lwd=2.5,col=2)
lines(year,c8,lty=2,lwd=2.5,col=2)
legend("topright",inset=0.02,c("F=0","5% Limit","95% Limit"),lwd=c(2.5,2.5,2.5),lty=c(1,2,3),col=c(1,2,3),cex=c(0.8,0.8,0.8))
axis(2,at=yat,labels=yat,outer=T,cex=1.5)
par(mgp=c(3.0,0.75,0))
axis(1,at=xat,labels=xat,outer=T,cex=1.5)
box()
mtext("MMB(1000 t)",2,2.8,outer=T,cex=1.0)
mtext('Year',1,2.10,outer=T,cex=1.0)
par(mfrow=c(1,1))

