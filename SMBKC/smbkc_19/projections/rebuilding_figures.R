# katie.palof@alaska.gov      8-30-19
# Rebuilding projections for SMBKC figures

# results from the projections can be found in the /projections/proj_x/x folders for each model type

# here look at those from smbkc_19/model_1/projections

# load -----------
# load -------
source("./SMBKC/code/helper.R")
source("./SMBKC/code/functions.R") # load function for summarising output from projection

# data ---------
proj1d <- read.csv(here::here("SMBKC/smbkc_19/model_1/projections/proj_1/d/rec_1yr_prob_out_proj_1d.csv"))
proj1aa <- read.csv(here::here("SMBKC/smbkc_19/model_1/projections/proj_1/aa/rec_1yr_prob_out_proj_1aa.csv"))

proj5d <- read.csv(here::here("SMBKC/smbkc_19/model_1/projections/proj_5/d/rec_1yr_prob_out_proj_5d.csv"))
proj5aa <- read.csv(here::here("SMBKC/smbkc_19/model_1/projections/proj_5/aa/rec_1yr_prob_out_proj_5aa.csv"))

proj4d <- read.csv(here::here("SMBKC/smbkc_19/model_1/projections/proj_4/d/rec_1yr_prob_out_proj_4d.csv"))


## projection 1 --------
# the label for F =0.18 needs to be SHR or state harvest rate 
proj1d %>% 
  mutate(projection = "avg recent bycatch") %>% 
  select(-FishMort) %>% 
  mutate(FishMort = ifelse(V3 == 1, "F = 0", "F = SHR")) -> proj1d

  
proj1aa %>% 
  mutate(projection = "max bycatch") %>% 
  select(-FishMort) %>% 
  mutate(FishMort = ifelse(V3 == 1, "F = 0", "F = SHR")) -> proj1aa

proj1d %>% 
  bind_rows(proj1aa) -> proj1

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

proj1 %>% 
ggplot(aes(year, recovery, shape = FishMort, colour = projection)) + 
  geom_point(size = 2)+
  scale_shape_manual(name = "", values = c(16, 22)) +
  scale_color_manual(name = "", values = cbPalette[2:3])+
  geom_line() +
  geom_hline(yintercept = 50, color = "red", lty = "dashed", lwd = 1.5) +
  geom_vline(xintercept = 10, color = "blue", lty = 2, lwd = 1.5) +
  ggtitle("Recruitment drawn from 1978 - 2018") +
  ylab("Probability of recovery") +
  xlab("Year") +
  ylim(0,100) +
  theme(plot.title = element_text(hjust = 0.5)) -> plotA
ggsave(paste0(here::here(), '/SMBKC/smbkc_19/doc/safe_figure/proj1ALL_rec_1yr_prob.png'), plotA, dpi = 800,
       width = 7.5, height = 3.75)

## projection 5 --------
# the label for F =0.18 needs to be SHR or state harvest rate 
proj5d %>% 
  mutate(projection = "avg recent bycatch") %>% 
  select(-FishMort) %>% 
  mutate(FishMort = ifelse(V3 == 1, "F = 0", "F = SHR")) -> proj5d


proj5aa %>% 
  mutate(projection = "max bycatch") %>% 
  select(-FishMort) %>% 
  mutate(FishMort = ifelse(V3 == 1, "F = 0", "F = SHR")) -> proj5aa

proj5d %>% 
  bind_rows(proj5aa) -> proj5

proj5 %>% 
  ggplot(aes(year, recovery, shape = FishMort, colour = projection)) + 
  geom_point(size = 2)+
  scale_shape_manual(name = "", values = c(16, 22)) +
  scale_color_manual(name = "", values = cbPalette[2:3])+
  geom_line() +
  geom_hline(yintercept = 50, color = "red", lty = "dashed", lwd = 1.5) +
  geom_vline(xintercept = 10, color = "blue", lty = 2, lwd = 1.5) +
  ggtitle("Recruitment drawn from 1996 - 2018") +
  ylab("Probability of recovery") +
  xlab("Year") +
  ylim(0,100) +
  theme(plot.title = element_text(hjust = 0.5)) -> plotA
ggsave(paste0(here::here(), '/SMBKC/smbkc_19/doc/safe_figure/proj5ALL_rec_1yr_prob.png'), plotA, dpi = 800,
       width = 7.5, height = 3.75)

# Figures with only avg bycatch ---------
proj1d %>% 
  mutate(projection = "avg recent bycatch") %>% 
  select(-FishMort) %>% 
  mutate(FishMort = ifelse(V3 == 1, "F = 0", "F = SHR")) -> proj1d

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

proj1d %>% 
  ggplot(aes(year, recovery, shape = FishMort, colour = projection)) + 
  geom_point(size = 2)+
  scale_shape_manual(name = "", values = c(16, 22)) +
  scale_color_manual(name = "", values = cbPalette[2])+
  geom_line() +
  geom_hline(yintercept = 50, color = "red", lty = "dashed", lwd = 1.5) +
  geom_vline(xintercept = 10, color = "blue", lty = 2, lwd = 1.5) +
  ggtitle("Recruitment drawn from 1978 - 2018, average recent bycatch levels") +
  ylab("Probability of recovery") +
  xlab("Year") +
  ylim(0,100) +
  theme(plot.title = element_text(hjust = 0.5)) -> plotA
ggsave(paste0(here::here(), '/SMBKC/smbkc_19/doc/safe_figure/proj1d_rec_1yr_prob.png'), plotA, dpi = 800,
       width = 7.5, height = 3.75)
  
# projection 5 avg bycatch-----
proj5d %>% 
  mutate(projection = "avg recent bycatch") %>% 
  select(-FishMort) %>% 
  mutate(FishMort = ifelse(V3 == 1, "F = 0", "F = SHR")) -> proj5d

proj5d %>% 
  ggplot(aes(year, recovery, shape = FishMort, colour = projection)) + 
  geom_point(size = 2)+
  scale_shape_manual(name = "", values = c(16, 22)) +
  scale_color_manual(name = "", values = cbPalette[2])+
  geom_line() +
  geom_hline(yintercept = 50, color = "red", lty = "dashed", lwd = 1.5) +
  geom_vline(xintercept = 10, color = "blue", lty = 2, lwd = 1.5) +
  ggtitle("Recruitment drawn from 1996 - 2018, average recent bycatch levels") +
  ylab("Probability of recovery") +
  xlab("Year") +
  ylim(0,100) +
  theme(plot.title = element_text(hjust = 0.5)) -> plotA
ggsave(paste0(here::here(), '/SMBKC/smbkc_19/doc/safe_figure/proj5d_rec_1yr_prob.png'), plotA, dpi = 800,
       width = 7.5, height = 3.75)
  
  

### CPT recommendations figures ---------
## projection 4
proj4d %>% 
  mutate(projection = "avg recent bycatch") %>% 
  select(-FishMort) %>% 
  mutate(FishMort = ifelse(V3 == 1, "F = 0", "F = SHR")) -> proj4d

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

proj4d %>% 
  ggplot(aes(year, recovery, shape = FishMort, colour = projection)) + 
  geom_point(size = 2)+
  scale_shape_manual(name = "", values = c(16, 22)) +
  scale_color_manual(name = "", values = cbPalette[2])+
  geom_line() +
  geom_hline(yintercept = 50, color = "red", lty = "dashed", lwd = 1.5) +
  geom_vline(xintercept = 10, color = "blue", lty = 2, lwd = 1.5) +
  ggtitle("Recruitment drawn from 1996 - 2018 (Bmsy proxy 1978 - 2018)") +
  ylab("Probability of recovery") +
  xlab("Year") +
  ylim(0,100) +
  theme(plot.title = element_text(hjust = 0.5)) -> plotA
ggsave(paste0(here::here(), '/SMBKC/smbkc_19/doc/safe_figure/proj4d_rec_1yr_prob.png'), plotA, dpi = 800,
       width = 7.5, height = 3.75)


## projection 2
proj2d %>% 
  mutate(projection = "avg recent bycatch") %>% 
  select(-FishMort) %>% 
  mutate(FishMort = ifelse(V3 == 1, "F = 0", "F = SHR")) -> proj2d

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

proj2d %>% 
  ggplot(aes(year, recovery, shape = FishMort, colour = projection)) + 
  geom_point(size = 2)+
  scale_shape_manual(name = "", values = c(16, 22)) +
  scale_color_manual(name = "", values = cbPalette[2])+
  geom_line() +
  geom_hline(yintercept = 50, color = "red", lty = "dashed", lwd = 1.5) +
  geom_vline(xintercept = 10, color = "blue", lty = 2, lwd = 1.5) +
  ggtitle("Recruitment drawn from 1996 - 2018 (Bmsy proxy 1978 - 2018)") +
  ylab("Probability of recovery") +
  xlab("Year") +
  ylim(0,100) +
  theme(plot.title = element_text(hjust = 0.5)) -> plotA
ggsave(paste0(here::here(), '/SMBKC/smbkc_19/doc/safe_figure/proj2d_rec_1yr_prob.png'), plotA, dpi = 800,
       width = 7.5, height = 3.75)
