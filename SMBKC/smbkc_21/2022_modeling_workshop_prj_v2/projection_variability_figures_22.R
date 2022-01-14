# projection figures for SMBKC rebuilding --------------------
# 2-11-2020, 9-7-2020 / modeling workshop projection work.
# katie.palof@alaska.gov
# for rebuilding action plan document

# load -------
source("./SMBKC/code/helper.R")
source("./SMBKC/code/functions.R") # load function for summarising output from projection

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
.THEME    = list(theme_bw(base_size = 12, base_family = ""), scale_fill_manual(values=cbPalette), 
                 scale_colour_manual(values=cbPalette))
ww <- 6
hh <- 5

# just use for the projection used in the rebulding doc (#2) needed here.

# projection typical ------- 
# same reference and projection parameters, uses mean recruitment for entire time series
#  old code from Andre -------
#TheD <- read.table(paste0("./projections/", model, "/", version, "/mcoutPROJ.rep"))[,-c(4,5,6,7,8)]
#proj1d <- read.table(paste0(here::here(), "./SMBKC/smbkc_20/model_1/projections/proj_1/d/mcoutPROJ.rep"))[,-c(4,5,6,7,8)]
#proj_t <- read.table(paste0(here::here(), "/SMBKC/smbkc_21/2022_modeling_workshop_prj/projection_typical/mcoutPROJ.rep"))[,-c(4,5,6,7,8)]
proj_t <- read.table(paste0(here::here(), "/SMBKC/smbkc_21/2022_modeling_workshop_prj_v2/smbkc_base/mcoutPROJ.rep"), 
                     header = TRUE)[,-c(4,5,6,7,8)]

Nyear <- length(proj_t[1,])-4
Nline <- length(proj_t[,1])
print(Nyear)
print(Nline)

# !!raw with variablity attempts !! used in EA --------------
raw <- proj_t

raw %>% 
  #filter(V3 == 1) %>% 
  #mutate(id = 1:n()) %>% 
  gather(year, mmb, -V1, -V2, -V3, -V9) %>% 
  mutate(year = as.numeric(as.factor(year)), 
         FishMort = ifelse(V3 == 1, "F = 0", "F = 0.18")) -> raw_all 
         #FishMort = ifelse(V3 == 1, "F = 0.18", "F = 0")) -> raw_all 
raw_all %>% 
  summarise(Bmsy = mean(V9)) -> Bmsy


raw_all %>% 
  group_by(year, FishMort) %>% 
  summarise(q0.05 = quantile(mmb, prob = 0.05), 
            q0.25 = quantile(mmb, prob = 0.25),
            q0.50 = quantile(mmb, prob = 0.50),
            q0.75 = quantile(mmb, prob = 0.75),
            q0.95 = quantile(mmb, prob = 0.95), 
            Bmsy = mean(V9)) -> typical_var
  write.csv(typical_var,paste0(here::here(), '/SMBKC/smbkc_21/2022_modeling_workshop_prj/projection_typical/proj_variability.csv'))
# 95 % of the distribution in gray. Average MMB in year projection in solid line
raw_all %>% 
  #filter(year <= 10) %>% 
  group_by(year, FishMort) %>% 
  summarise(q0.05 = quantile(mmb, prob = 0.05), 
            q0.25 = quantile(mmb, prob = 0.25),
            q0.50 = quantile(mmb, prob = 0.50),
            q0.75 = quantile(mmb, prob = 0.75),
            q0.95 = quantile(mmb, prob = 0.95), 
            Bmsy = mean(V9)) %>% 
  ggplot(aes(year, q0.50, colour = FishMort)) +
  geom_line(lwd = 1) +
  scale_color_manual(name = "", values = c(cbPalette[4], cbPalette[1]))+
  geom_ribbon(aes(ymin = q0.05, ymax = q0.95, x = year, fill = FishMort), alpha = 0.17) +
  scale_fill_manual(name = "", values = c(cbPalette[4], cbPalette[1]))+
  geom_hline(yintercept = Bmsy[1,], lwd = 0.75, color = "darkgoldenrod4", linetype = "dashed") +
  geom_text(aes(0, 3300, label = "Bmsy proxy",
                vjust = -1, hjust = 0.05)) +
  ylab ("MMB (tons)") +
  xlab ("Projection Year") +
  ggtitle("Mean recruitment (1978 - 2019), average bycatch levels") -> plotA

ggsave(paste0(here::here(), '/SMBKC/smbkc_21/2022_modeling_workshop_prj/typical_all.png'), plotA, dpi = 800,
       width = 7.5, height = 3.75)

## high M new projections -------------
proj_M <- read.table(paste0(here::here(), "/SMBKC/smbkc_21/2022_modeling_workshop_prj/proj_highM/mcoutPROJ.rep"))[,-c(4,5,6,7,8)]
Nyear <- length(proj_M[1,])-4
Nline <- length(proj_M[,1])
print(Nyear)
print(Nline)

# !!raw with variablity attempts !! used in EA
raw <- proj_M

raw %>% 
  #filter(V3 == 1) %>% 
  #mutate(id = 1:n()) %>% 
  gather(year, mmb, -V1, -V2, -V3, -V9) %>% 
  mutate(year = as.numeric(as.factor(year)), 
         FishMort = ifelse(V3 == 1, "F = 0", "F = 0.18")) -> raw_all 
#FishMort = ifelse(V3 == 1, "F = 0.18", "F = 0")) -> raw_all 
raw_all %>% 
  summarise(Bmsy = mean(V9)) -> Bmsy


raw_all %>% 
  group_by(year, FishMort) %>% 
  summarise(q0.05 = quantile(mmb, prob = 0.05), 
            q0.25 = quantile(mmb, prob = 0.25),
            q0.50 = quantile(mmb, prob = 0.50),
            q0.75 = quantile(mmb, prob = 0.75),
            q0.95 = quantile(mmb, prob = 0.95), 
            Bmsy = mean(V9)) -> typical_var
#write.csv(typical_var,paste0(here::here(), '/SMBKC/smbkc_21/2022_modeling_workshop_prj/projection_typical/proj_variability.csv'))
# 95 % of the distribution in gray. Average MMB in year projection in solid line
raw_all %>% 
  #filter(year <= 10) %>% 
  group_by(year, FishMort) %>% 
  summarise(q0.05 = quantile(mmb, prob = 0.05), 
            q0.25 = quantile(mmb, prob = 0.25),
            q0.50 = quantile(mmb, prob = 0.50),
            q0.75 = quantile(mmb, prob = 0.75),
            q0.95 = quantile(mmb, prob = 0.95), 
            Bmsy = mean(V9)) %>% 
  ggplot(aes(year, q0.50, colour = FishMort)) +
  geom_line(lwd = 1) +
  scale_color_manual(name = "", values = c(cbPalette[4], cbPalette[1]))+
  geom_ribbon(aes(ymin = q0.05, ymax = q0.95, x = year, fill = FishMort), alpha = 0.17) +
  scale_fill_manual(name = "", values = c(cbPalette[4], cbPalette[1]))+
  geom_hline(yintercept = Bmsy[1,], lwd = 0.75, color = "darkgoldenrod4", linetype = "dashed") +
  geom_text(aes(0, 3300, label = "Bmsy proxy",
                vjust = -1, hjust = 0.05)) +
  ylab ("MMB (tons)") +
  xlab ("Projection Year") +
  ggtitle("Mean recruitment (1978 - 2019), average bycatch levels") -> plotA

ggsave(paste0(here::here(), '/SMBKC/smbkc_21/2022_modeling_workshop_prj/highM.png'), plotA, dpi = 800,
       width = 7.5, height = 3.75)


## recent recruitment  -----
proj_2 <- read.table(paste0(here::here(), "/SMBKC/smbkc_21/2022_modeling_workshop_prj/proj_recent_recruit/mcoutPROJ.rep"))[,-c(4,5,6,7,8)]
Nyear <- length(proj_2[1,])-4
Nline <- length(proj_2[,1])
print(Nyear)
print(Nline)

# !!raw with variablity attempts !! used in EA
raw <- proj_2

raw %>% 
  #filter(V3 == 1) %>% 
  #mutate(id = 1:n()) %>% 
  gather(year, mmb, -V1, -V2, -V3, -V9) %>% 
  mutate(year = as.numeric(as.factor(year)), 
         FishMort = ifelse(V3 == 1, "F = 0", "F = 0.18")) -> raw_all 
#FishMort = ifelse(V3 == 1, "F = 0.18", "F = 0")) -> raw_all 
raw_all %>% 
  summarise(Bmsy = mean(V9)) -> Bmsy


raw_all %>% 
  group_by(year, FishMort) %>% 
  summarise(q0.05 = quantile(mmb, prob = 0.05), 
            q0.25 = quantile(mmb, prob = 0.25),
            q0.50 = quantile(mmb, prob = 0.50),
            q0.75 = quantile(mmb, prob = 0.75),
            q0.95 = quantile(mmb, prob = 0.95), 
            Bmsy = mean(V9)) -> typical_var
#write.csv(typical_var,paste0(here::here(), '/SMBKC/smbkc_21/2022_modeling_workshop_prj/projection_typical/proj_variability.csv'))
# 95 % of the distribution in gray. Average MMB in year projection in solid line
raw_all %>% 
  #filter(year <= 10) %>% 
  group_by(year, FishMort) %>% 
  summarise(q0.05 = quantile(mmb, prob = 0.05), 
            q0.25 = quantile(mmb, prob = 0.25),
            q0.50 = quantile(mmb, prob = 0.50),
            q0.75 = quantile(mmb, prob = 0.75),
            q0.95 = quantile(mmb, prob = 0.95), 
            Bmsy = mean(V9)) %>% 
  ggplot(aes(year, q0.50, colour = FishMort)) +
  geom_line(lwd = 1) +
  scale_color_manual(name = "", values = c(cbPalette[4], cbPalette[1]))+
  geom_ribbon(aes(ymin = q0.05, ymax = q0.95, x = year, fill = FishMort), alpha = 0.17) +
  scale_fill_manual(name = "", values = c(cbPalette[4], cbPalette[1]))+
  geom_hline(yintercept = Bmsy[1,], lwd = 0.75, color = "darkgoldenrod4", linetype = "dashed") +
  geom_text(aes(0, 3300, label = "Bmsy proxy",
                vjust = -1, hjust = 0.05)) +
  ylab ("MMB (tons)") +
  xlab ("Projection Year") +
  ggtitle("Mean recruitment (2008 - 2019), average bycatch levels") -> plotA

ggsave(paste0(here::here(), '/SMBKC/smbkc_21/2022_modeling_workshop_prj/recent_recruit.png'), plotA, dpi = 800,
       width = 7.5, height = 3.75)

# stock recruit  -----
proj_3 <- read.table(paste0(here::here(), "/SMBKC/smbkc_21/2022_modeling_workshop_prj/proj_stock_recruit/mcoutPROJ.rep"))[,-c(4,5,6,7,8)]
Nyear <- length(proj_3[1,])-4
Nline <- length(proj_3[,1])
print(Nyear)
print(Nline)

# !!raw with variablity attempts !! used in EA
raw <- proj_3

raw %>% 
  #filter(V3 == 1) %>% 
  #mutate(id = 1:n()) %>% 
  gather(year, mmb, -V1, -V2, -V3, -V9) %>% 
  mutate(year = as.numeric(as.factor(year)), 
         FishMort = ifelse(V3 == 1, "F = 0", "F = 0.18")) -> raw_all 
#FishMort = ifelse(V3 == 1, "F = 0.18", "F = 0")) -> raw_all 
raw_all %>% 
  summarise(Bmsy = mean(V9)) -> Bmsy


raw_all %>% 
  group_by(year, FishMort) %>% 
  summarise(q0.05 = quantile(mmb, prob = 0.05), 
            q0.25 = quantile(mmb, prob = 0.25),
            q0.50 = quantile(mmb, prob = 0.50),
            q0.75 = quantile(mmb, prob = 0.75),
            q0.95 = quantile(mmb, prob = 0.95), 
            Bmsy = mean(V9)) -> typical_var
#write.csv(typical_var,paste0(here::here(), '/SMBKC/smbkc_21/2022_modeling_workshop_prj/projection_typical/proj_variability.csv'))
# 95 % of the distribution in gray. Average MMB in year projection in solid line
raw_all %>% 
  #filter(year <= 10) %>% 
  group_by(year, FishMort) %>% 
  summarise(q0.05 = quantile(mmb, prob = 0.05), 
            q0.25 = quantile(mmb, prob = 0.25),
            q0.50 = quantile(mmb, prob = 0.50),
            q0.75 = quantile(mmb, prob = 0.75),
            q0.95 = quantile(mmb, prob = 0.95), 
            Bmsy = mean(V9)) %>% 
  ggplot(aes(year, q0.50, colour = FishMort)) +
  geom_line(lwd = 1) +
  scale_color_manual(name = "", values = c(cbPalette[4], cbPalette[1]))+
  geom_ribbon(aes(ymin = q0.05, ymax = q0.95, x = year, fill = FishMort), alpha = 0.17) +
  scale_fill_manual(name = "", values = c(cbPalette[4], cbPalette[1]))+
  geom_hline(yintercept = Bmsy[1,], lwd = 0.75, color = "darkgoldenrod4", linetype = "dashed") +
  geom_text(aes(0, 3300, label = "Bmsy proxy",
                vjust = -1, hjust = 0.05)) +
  ylab ("MMB (tons)") +
  xlab ("Projection Year") +
  ggtitle("stock recruit ricker, average bycatch levels") -> plotA

ggsave(paste0(here::here(), '/SMBKC/smbkc_21/2022_modeling_workshop_prj/stock_recruit_ricker.png'), plotA, dpi = 800,
       width = 7.5, height = 3.75)



# attempt trials -----
proj_trial <- read.table(paste0(here::here(), "/SMBKC/smbkc_21/2022_modeling_workshop_prj/proj_trial/mcoutPROJ.rep"))[,-c(4,5,6,7,8)]
Nyear <- length(proj_trial[1,])-4
Nline <- length(proj_trial[,1])
print(Nyear)
print(Nline)

# !!raw with variablity attempts !! used in EA
raw <- proj_trial

raw %>% 
  #filter(V3 == 1) %>% 
  #mutate(id = 1:n()) %>% 
  gather(year, mmb, -V1, -V2, -V3, -V9) %>% 
  mutate(year = as.numeric(as.factor(year)), 
         FishMort = ifelse(V3 == 1, "F = 0", "F = 0.18")) -> raw_all 
#FishMort = ifelse(V3 == 1, "F = 0.18", "F = 0")) -> raw_all 
raw_all %>% 
  summarise(Bmsy = mean(V9)) -> Bmsy


raw_all %>% 
  group_by(year, FishMort) %>% 
  summarise(q0.05 = quantile(mmb, prob = 0.05), 
            q0.25 = quantile(mmb, prob = 0.25),
            q0.50 = quantile(mmb, prob = 0.50),
            q0.75 = quantile(mmb, prob = 0.75),
            q0.95 = quantile(mmb, prob = 0.95), 
            Bmsy = mean(V9)) -> typical_var
#write.csv(typical_var,paste0(here::here(), '/SMBKC/smbkc_21/2022_modeling_workshop_prj/projection_typical/proj_variability.csv'))
# 95 % of the distribution in gray. Average MMB in year projection in solid line
raw_all %>% 
  #filter(year <= 10) %>% 
  group_by(year, FishMort) %>% 
  summarise(q0.05 = quantile(mmb, prob = 0.05), 
            q0.25 = quantile(mmb, prob = 0.25),
            q0.50 = quantile(mmb, prob = 0.50),
            q0.75 = quantile(mmb, prob = 0.75),
            q0.95 = quantile(mmb, prob = 0.95), 
            Bmsy = mean(V9)) %>% 
  ggplot(aes(year, q0.50, colour = FishMort)) +
  geom_line(lwd = 1) +
  scale_color_manual(name = "", values = c(cbPalette[4], cbPalette[1]))+
  geom_ribbon(aes(ymin = q0.05, ymax = q0.95, x = year, fill = FishMort), alpha = 0.17) +
  scale_fill_manual(name = "", values = c(cbPalette[4], cbPalette[1]))+
  geom_hline(yintercept = Bmsy[1,], lwd = 0.75, color = "darkgoldenrod4", linetype = "dashed") +
  geom_text(aes(0, 3300, label = "Bmsy proxy",
                vjust = -1, hjust = 0.05)) +
  ylab ("MMB (tons)") +
  xlab ("Projection Year") +
  ggtitle("Mean recruitment (1978 - 2019), average bycatch levels") -> plotA

ggsave(paste0(here::here(), '/SMBKC/smbkc_21/2022_modeling_workshop_prj/trial_line2.png'), plotA, dpi = 800,
       width = 7.5, height = 3.75)






#### STOP ----------------------------

### EA figure with shading for difference ----------
# must run code above in !!raw with variability attempts first 
raw_all %>% 
  group_by(year, FishMort) %>% 
  summarise(q0.05 = quantile(mmb, prob = 0.05), 
            q0.25 = quantile(mmb, prob = 0.25),
            q0.50 = quantile(mmb, prob = 0.50),
            q0.75 = quantile(mmb, prob = 0.75),
            q0.95 = quantile(mmb, prob = 0.95), 
            Bmsy = mean(V9)) %>% 
  mutate(alt = ifelse(FishMort == "F = 0", "none", "SHS")) %>% 
      as.data.frame() -> data2 
data2 %>% 
  subset(6 <= year & year <=26) %>% 
  select(year, alt , q0.50) %>% 
  spread(alt, q0.50) -> data3

ggplot(data2, aes(year, q0.50, colour = FishMort)) +
  geom_line(lwd = 1) +
  scale_color_manual(name = "", values = c(cbPalette[4], cbPalette[1])) +
  geom_ribbon(aes(ymin = q0.05, ymax = q0.95, x = year, fill = FishMort), alpha = 0.17) +
  scale_fill_manual(name = "", values = c(cbPalette[4], cbPalette[1]))+
  geom_hline(yintercept = Bmsy[1,], lwd = 0.75, color = "darkgoldenrod4", linetype = "dashed") +
  geom_text(aes(0, 3300, label = "Bmsy proxy",
                vjust = -1, hjust = 0.05)) +
  ylab ("MMB (tons)") +
  xlab ("Projection Year") +
  ggtitle("Ricker S-R recruitment, average bycatch levels") +
  geom_ribbon(data = subset(data2, 6 <= year & year <=26), 
              aes(ymin = filter(data2, q0.50(alt == "SHS"), 
                  ymax = q0.50(alt == "none")), fill = "blue", alpha = "0.5"))



# for Fishing Mortality F = 0, that's what v3 = 1 stands for -----------
raw %>% 
  filter(V3 == 1) %>% 
  mutate(id = 1:n()) %>% 
  gather(year, mmb, -V1, -V2, -V3, -V9, -id) %>% 
  mutate(year = as.numeric(as.factor(year))) -> raw_0

raw_0 %>% 
  summarise(Bmsy = mean(V9)) -> Bmsy

# 95 % of the distribution in gray. Average MMB in year projection in solid line
raw_0 %>% 
  group_by(year) %>% 
  summarise(q0.05 = quantile(mmb, prob = 0.05), 
            q0.25 = quantile(mmb, prob = 0.25),
            q0.50 = quantile(mmb, prob = 0.50),
            q0.75 = quantile(mmb, prob = 0.75),
            q0.95 = quantile(mmb, prob = 0.95), 
            Bmsy = mean(V9)) %>% 
  ggplot(aes(year, q0.50)) +
  geom_line(lwd = 1) +
  geom_ribbon(aes(ymin = q0.05, ymax = q0.95, x = year), alpha = 0.17)+
  #geom_line(aes(year, q0.95), lwd = 0.5, color = "blue") +
  #geom_line(aes(year, q0.05), lwd = 0.5, color = "blue") +
  geom_hline(yintercept = Bmsy[1,], lwd = 0.75, color = "darkgoldenrod4", linetype = "dashed") +
  ylab ("MMB (units)") +
  xlab ("Projection year")
# label this figure with projection type 

# fishing mortality F = SHS ----------
# for Fishing Mortality F = 0, that's what v3 = 1 stands for
raw %>% 
  filter(V3 == 2) %>% 
  mutate(id = 1:n()) %>% 
  gather(year, mmb, -V1, -V2, -V3, -V9, -id) %>% 
  mutate(year = as.numeric(as.factor(year))) -> raw_SHS

# 95 % of the distribution in gray. Average MMB in year projection in solid line
raw_SHS %>% 
  group_by(year) %>% 
  summarise(q0.05 = quantile(mmb, prob = 0.05), 
            q0.25 = quantile(mmb, prob = 0.25),
            q0.50 = quantile(mmb, prob = 0.50),
            q0.75 = quantile(mmb, prob = 0.75),
            q0.95 = quantile(mmb, prob = 0.95), 
            Bmsy = mean(V9)) %>% 
  ggplot(aes(year, q0.50)) +
  geom_line(lwd = 1) +
  geom_ribbon(aes(ymin = q0.05, ymax = q0.95, x = year), alpha = 0.17)+
  #geom_line(aes(year, q0.95), lwd = 0.5, color = "blue") +
  #geom_line(aes(year, q0.05), lwd = 0.5, color = "blue") +
  geom_hline(yintercept = Bmsy[1,], lwd = 0.75, color = "darkgoldenrod4", linetype = "dashed") +
  ylab ("MMB (units)") +
  xlab ("Projection year")
# label this figure with projection type 



# projection 2d ------- 
#  old code from Andre -------
#TheD <- read.table(paste0("./projections/", model, "/", version, "/mcoutPROJ.rep"))[,-c(4,5,6,7,8)]
#proj2d <- read.table(paste0(here::here(), "./SMBKC/smbkc_20/model_1/projections/proj_2/d/mcoutPROJ.rep"))[,-c(4,5,6,7,8)]
proj2d <- read.table(paste0(here::here(), "./SMBKC/smbkc_20/model_proj/mcoutPROJ.rep"))[,-c(4,5,6,7,8)]

Nyear <- length(proj2d[1,])-4
Nline <- length(proj2d[,1])
print(Nyear)
print(Nline)

# !!raw with variablity attempts !! used in EA --------------
raw <- proj2d

raw %>% 
  #filter(V3 == 1) %>% 
  #mutate(id = 1:n()) %>% 
  gather(year, mmb, -V1, -V2, -V3, -V9) %>% 
  mutate(year = as.numeric(as.factor(year)), 
         FishMort = ifelse(V3 == 1, "F = 0", "F = SHS")) -> raw_all 

raw_all %>% 
  summarise(Bmsy = mean(V9)) -> Bmsy


raw_all %>% 
  group_by(year, FishMort) %>% 
  summarise(q0.05 = quantile(mmb, prob = 0.05), 
            q0.25 = quantile(mmb, prob = 0.25),
            q0.50 = quantile(mmb, prob = 0.50),
            q0.75 = quantile(mmb, prob = 0.75),
            q0.95 = quantile(mmb, prob = 0.95), 
            Bmsy = mean(V9)) -> ricker_var
#write.csv(ricker_var,paste0(here::here(), '/SMBKC/smbkc_20/model_1/projections/proj2d_variability.csv'))
# 95 % of the distribution in gray. Average MMB in year projection in solid line
raw_all %>% 
  #filter(year <= 10) %>% 
  group_by(year, FishMort) %>% 
  summarise(q0.05 = quantile(mmb, prob = 0.05), 
            q0.25 = quantile(mmb, prob = 0.25),
            q0.50 = quantile(mmb, prob = 0.50),
            q0.75 = quantile(mmb, prob = 0.75),
            q0.95 = quantile(mmb, prob = 0.95), 
            Bmsy = mean(V9)) %>% 
  ggplot(aes(year, q0.50, colour = FishMort)) +
  geom_line(lwd = 1) +
  scale_color_manual(name = "", values = c(cbPalette[4], cbPalette[1]))+
  geom_ribbon(aes(ymin = q0.05, ymax = q0.95, x = year, fill = FishMort), alpha = 0.17) +
  scale_fill_manual(name = "", values = c(cbPalette[4], cbPalette[1]))+
  geom_hline(yintercept = Bmsy[1,], lwd = 0.75, color = "darkgoldenrod4", linetype = "dashed") +
  geom_text(aes(0, 3300, label = "Bmsy proxy",
                vjust = -1, hjust = 0.05)) +
  ylab ("MMB (tons)") +
  xlab ("Projection Year") +
  ggtitle("Mean recruitment (1978 - 2019), average bycatch levels") #-> plotA

ggsave(paste0(here::here(), '/SMBKC/smbkc_20/projections/proj_2d.png'), plotA, dpi = 800,
       width = 7.5, height = 3.75)



# testing projections -------------
#  old code from Andre -------
#TheD <- read.table(paste0("./projections/", model, "/", version, "/mcoutPROJ.rep"))[,-c(4,5,6,7,8)]
proj2020 <- read.table(paste0(here::here(), "./SMBKC/smbkc_20/model_1/projections/2020_safe/mcoutPROJ.rep"))[,-c(4,5,6,7,8)]
Nyear <- length(proj2020[1,])-4
Nline <- length(proj2020[,1])
print(Nyear)
print(Nline)

# !!raw with variablity attempts !! used in EA --------------
raw <- proj2020

raw %>% 
  #filter(V3 == 1) %>% 
  #mutate(id = 1:n()) %>% 
  gather(year, mmb, -V1, -V2, -V3, -V9) %>% 
  mutate(year = as.numeric(as.factor(year)), 
         FishMort = ifelse(V3 == 1, "F = 0", "F = SHS")) -> raw_all 

raw_all %>% 
  summarise(Bmsy = mean(V9)) -> Bmsy


raw_all %>% 
  group_by(year, FishMort) %>% 
  summarise(q0.05 = quantile(mmb, prob = 0.05), 
            q0.25 = quantile(mmb, prob = 0.25),
            q0.50 = quantile(mmb, prob = 0.50),
            q0.75 = quantile(mmb, prob = 0.75),
            q0.95 = quantile(mmb, prob = 0.95), 
            Bmsy = mean(V9)) -> ricker_var
#write.csv(ricker_var,paste0(here::here(), '/SMBKC/smbkc_20/model_1/projections/proj1d_variability.csv'))
# 95 % of the distribution in gray. Average MMB in year projection in solid line
raw_all %>% 
  #filter(year <= 10) %>% 
  group_by(year, FishMort) %>% 
  summarise(q0.05 = quantile(mmb, prob = 0.05), 
            q0.15 = quantile(mmb, prob = 0.15),
            q0.25 = quantile(mmb, prob = 0.25),
            q0.50 = quantile(mmb, prob = 0.50),
            q0.75 = quantile(mmb, prob = 0.75),
            q0.85 = quantile(mmb, prob = 0.75),
            q0.95 = quantile(mmb, prob = 0.95), 
            Bmsy = mean(V9)) %>% 
  mutate(Year = ifelse(year == 1, 2021, ifelse(year == 2, 2022,
                                               ifelse(year == 3, 2023, 
                                                      ifelse(year == 4, 2024, 
                                                             ifelse(year == 5, 2025, 
                                                                    ifelse(year == 6, 2026, 2027))))))) %>% 
  ggplot(aes(Year, q0.50, colour = FishMort)) +
  geom_line(lwd = 1) +
  scale_color_manual(name = "", values = c(cbPalette[4], cbPalette[1]))+
  geom_ribbon(aes(ymin = q0.15, ymax = q0.85, x = Year, fill = FishMort), alpha = 0.17) +
  scale_fill_manual(name = "", values = c(cbPalette[4], cbPalette[1]))+
  geom_hline(yintercept = Bmsy[1,], lwd = 0.75, color = "darkgoldenrod4", linetype = "dashed") +
  geom_text(aes(2020, 3300, label = "Bmsy proxy",
                vjust = -1, hjust = 0.05)) +
  #expand_limits(y = 0) +
  ylim(0, 4000) +
  ylab ("MMB (tons)") +
  xlab ("Projection Year") +
  ggtitle("MMB projections for the near future \nmean recruitment (1996 - 2019), average bycatch levels") +
  .THEME #-> plotA

ggsave(paste0(here::here(), '/SMBKC/smbkc_20/model_1/projections/safe_proj20.png'), width = 1.18*ww, height = hh)

