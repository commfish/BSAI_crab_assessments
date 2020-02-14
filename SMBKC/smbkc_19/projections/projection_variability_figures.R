# projection figures for SMBKC rebuilding --------------------
# 2-11-2020, katie.palof@alaska.gov
# for rebuilding action plan document

# load -------
source("./SMBKC/code/helper.R")
source("./SMBKC/code/functions.R") # load function for summarising output from projection


# just use for the projection used in the rebulding doc (#2) needed here.

# projection 2d ------- 
#  old code from Andre -------
#TheD <- read.table(paste0("./projections/", model, "/", version, "/mcoutPROJ.rep"))[,-c(4,5,6,7,8)]
proj2d <- read.table(paste0(here::here(), "./SMBKC/smbkc_19/model_1/projections/proj_2/d/mcoutPROJ.rep"))[,-c(4,5,6,7,8)]
Nyear <- length(proj2d[1,])-4
Nline <- length(proj2d[,1])
print(Nyear)
print(Nline)

# raw with variablity attempts --------------
raw <- proj2d

raw %>% 
  #filter(V3 == 1) %>% 
  #mutate(id = 1:n()) %>% 
  gather(year, mmb, -V1, -V2, -V3, -V9) %>% 
  mutate(year = as.numeric(as.factor(year)), 
         FishMort = ifelse(V3 == 1, "F = 0", "F = SHS")) -> raw_all 

raw_all %>% 
  summarise(Bmsy = mean(V9)) -> Bmsy

# 95 % of the distribution in gray. Average MMB in year projection in solid line
raw_all %>% 
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
  ggtitle("Ricker S-R recruitment, average bycatch levels") -> plotA

ggsave(paste0(here::here(), '/SMBKC/smbkc_19/doc/rebuilding_2019/proj2d_variability.png'), plotA, dpi = 800,
       width = 7.5, height = 3.75)

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