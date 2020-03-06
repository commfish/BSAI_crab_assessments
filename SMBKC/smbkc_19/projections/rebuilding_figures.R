# katie.palof@alaska.gov      8-30-19/ 11-11-2019
# Rebuilding projections for SMBKC figures

# results from the projections can be found in the /projections/proj_x/x folders for each model type

# here look at those from smbkc_19/model_1/projections

# load -----------
source("./SMBKC/code/helper.R")
source("./SMBKC/code/functions.R") # load function for summarising output from projection

# data ---------
proj1d <- read.csv(here::here("SMBKC/smbkc_19/model_1/projections/proj_1/d/rec_1yr_prob_out_proj_1d.csv"))
proj1aa <- read.csv(here::here("SMBKC/smbkc_19/model_1/projections/proj_1/aa/rec_1yr_prob_out_proj_1aa.csv"))

proj5d <- read.csv(here::here("SMBKC/smbkc_19/model_1/projections/proj_5/d/rec_1yr_prob_out_proj_5d.csv"))
proj5aa <- read.csv(here::here("SMBKC/smbkc_19/model_1/projections/proj_5/aa/rec_1yr_prob_out_proj_5aa.csv"))

proj4d <- read.csv(here::here("SMBKC/smbkc_19/model_1/projections/proj_4/d/rec_1yr_prob_out_proj_4d.csv"))

proj2a <- read.csv(here::here("SMBKC/smbkc_19/model_1/projections/proj_2/d/rec_1yr_prob_out_proj_2d.csv"))
# didn't run a since it was the same as d previous, so bring in d here to stand in for a
proj2d <- read.csv(here::here("SMBKC/smbkc_19/model_1/projections/proj_2/d/rec_1yr_prob_out_proj_2d.csv"))
proj2aa <- read.csv(here::here("SMBKC/smbkc_19/model_1/projections/proj_2/aa/rec_1yr_prob_out_proj_2aa.csv"))
proj2b <- read.csv(here::here("SMBKC/smbkc_19/model_1/projections/proj_2/b/rec_1yr_prob_out_proj_2b.csv"))
proj2abc <- read.csv(here::here("SMBKC/smbkc_19/model_1/projections/proj_2/abc/rec_1yr_prob_out_proj_2abc.csv"))

projSHP <- read.csv(here::here("SMBKC/smbkc_19/model_1/projections/projSHP/d/rec_1yr_prob_out_projSHPd.csv"))

proj2b_changes <- read.csv(here::here("SMBKC/smbkc_19a/model_1/projections/proj2/b/rec_1yr_prob_out_proj2b.csv"))


## projection 1 --------
# the label for F =0.18 needs to be SHR or state harvest rate 
proj1d %>% 
  mutate(projection = "avg recent bycatch") %>% 
  select(-FishMort) %>% 
  mutate(FishMort = ifelse(V3 == 1, "F = 0", "F = SHS")) -> proj1d

  
proj1aa %>% 
  mutate(projection = "max bycatch") %>% 
  select(-FishMort) %>% 
  mutate(FishMort = ifelse(V3 == 1, "F = 0", "F = SHS")) -> proj1aa

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
ggsave(paste0(here::here(), '/SMBKC/smbkc_19/doc/rebuilding_2019/proj1ALL_rec_1yr_prob.png'), plotA, dpi = 800,
       width = 7.5, height = 3.75)

## avg only =====
proj1d %>% 
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
  theme(plot.title = element_text(hjust = 0.5)) -> plot1d
ggsave(paste0(here::here(), '/SMBKC/smbkc_19/doc/rebuilding_2019/proj1_d_rec_1yr_prob.png'), plot1d, dpi = 800,
       width = 7.5, height = 3.75)

## projection 5 --------
# the label for F =0.18 needs to be SHR or state harvest rate 
proj5d %>% 
  mutate(projection = "avg recent bycatch") %>% 
  select(-FishMort) %>% 
  mutate(FishMort = ifelse(V3 == 1, "F = 0", "F = SHS")) -> proj5d


proj5aa %>% 
  mutate(projection = "max bycatch") %>% 
  select(-FishMort) %>% 
  mutate(FishMort = ifelse(V3 == 1, "F = 0", "F = SHS")) -> proj5aa

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
  ggtitle("BMSY and Recruitment drawn from 1996 - 2018") +
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
  mutate(FishMort = ifelse(V3 == 1, "F = 0", "F = SHS")) -> proj5d

proj5d %>% 
  ggplot(aes(year, recovery, shape = FishMort, colour = projection)) + 
  geom_point(size = 2)+
  scale_shape_manual(name = "", values = c(16, 22)) +
  scale_color_manual(name = "", values = cbPalette[2])+
  geom_line() +
  geom_hline(yintercept = 50, color = "red", lty = "dashed", lwd = 1.5) +
  geom_vline(xintercept = 10, color = "blue", lty = 2, lwd = 1.5) +
  ggtitle("Bmsy and Recruitment drawn from 1996 - 2018") +
  ylab("Probability of recovery") +
  xlab("Year") +
  ylim(0,100) +
  theme(plot.title = element_text(hjust = 0.5)) -> plotA
ggsave(paste0(here::here(), '/SMBKC/smbkc_19/doc/rebuilding_2019/proj5d_rec_1yr_prob.png'), plotA, dpi = 800,
       width = 7.5, height = 3.75)
  
  

### CPT recommendations figures ---------
## projection 4
proj4d %>% 
  mutate(projection = "avg recent bycatch") %>% 
  select(-FishMort) %>% 
  mutate(FishMort = ifelse(V3 == 1, "F = 0", "F = SHS")) -> proj4d

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

## proj4 avg only =====
proj4d %>% 
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
  theme(plot.title = element_text(hjust = 0.5)) -> plot4d
ggsave(paste0(here::here(), '/SMBKC/smbkc_19/doc/rebuilding_2019/proj4_d_rec_1yr_prob.png'), plot4d, dpi = 800,
       width = 7.5, height = 3.75)

## proj 4 max bycatch
#proj4aa %>% 
#  mutate(projection = "max bycatch") %>% 
#  select(-FishMort) %>% 
#  mutate(FishMort = ifelse(V3 == 1, "F = 0", "F = SHR")) -> proj4aa

#proj4d %>% 
#  bind_rows(proj4aa) -> proj4

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#proj4 %>% 
#  ggplot(aes(year, recovery, shape = FishMort, colour = projection)) + 
#  geom_point(size = 2)+
#  scale_shape_manual(name = "", values = c(16, 22)) +
#  scale_color_manual(name = "", values = cbPalette[2:3])+
#  geom_line() +
#  geom_hline(yintercept = 50, color = "red", lty = "dashed", lwd = 1.5) +
#  geom_vline(xintercept = 10, color = "blue", lty = 2, lwd = 1.5) +
#  ggtitle("Recruitment drawn from 1996 - 2018 (Bmsy proxy 1978 - 2018)") +
#  ylab("Probability of recovery") +
#  xlab("Year") +
#  ylim(0,100) +
#  theme(plot.title = element_text(hjust = 0.5)) -> plotA
#ggsave(paste0(here::here(), '/SMBKC/smbkc_19/doc/safe_figure/proj4ALL_rec_1yr_prob.png'), plotA, dpi = 800,
#       width = 7.5, height = 3.75)


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
  ggtitle("Ricker stock-recruit relationship (Bmsy proxy 1978 - 2018)") +
  ylab("Probability of recovery") +
  xlab("Year") +
  ylim(0,100) +
  theme(plot.title = element_text(hjust = 0.5)) -> plotA
ggsave(paste0(here::here(), '/SMBKC/smbkc_19/doc/safe_figure/proj2d_rec_1yr_prob.png'), plotA, dpi = 800,
       width = 7.5, height = 3.75)

## proj 2 max bycatch
proj2aa %>% 
  mutate(projection = "max bycatch") %>% 
  select(-FishMort) %>% 
  mutate(FishMort = ifelse(V3 == 1, "F = 0", "F = SHR")) -> proj2aa

proj2d %>% 
  bind_rows(proj2aa) -> proj2

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

proj2 %>% 
  ggplot(aes(year, recovery, shape = FishMort, colour = projection)) + 
  geom_point(size = 2)+
  scale_shape_manual(name = "", values = c(16, 22)) +
  scale_color_manual(name = "", values = cbPalette[2:3])+
  geom_line() +
  geom_hline(yintercept = 50, color = "red", lty = "dashed", lwd = 1.5) +
  geom_vline(xintercept = 10, color = "blue", lty = 2, lwd = 1.5) +
  ggtitle(expression(paste("Ricker stock-recruit relationship (", B[MSY]," proxy 1978 - 2018)"))) +
  ylab("Probability of recovery") +
  xlab("Year") +
  ylim(0,100) +
  theme(plot.title = element_text(hjust = 0.5)) -> plotA
ggsave(paste0(here::here(), '/SMBKC/smbkc_19/doc/safe_figure/proj2ALL_rec_1yr_prob.png'), plotA, dpi = 800,
       width = 7.5, height = 3.75)

## proj 2 avg by, max by, and F=M, for rebuilding document -------------
proj2d %>% 
  mutate(projection = "avg recent bycatch") %>% 
  select(-FishMort) %>% 
  mutate(FishMort = ifelse(V3 == 1, "F = 0", "F = SHS")) -> proj2d

proj2aa %>% 
  mutate(projection = "max bycatch") %>% 
  select(-FishMort) %>% 
  mutate(FishMort = ifelse(V3 == 1, "F = 0", "F = SHS")) -> proj2aa

proj2b %>% 
  mutate(projection = "alternative 1") %>% 
  filter(V3 == 2) %>% 
  select(year, V3, recovery, projection, FishMort) -> proj2b
  #elect(-FishMort) %>% 
  #mutate(FishMort = ifelse(V3 == 1, "F = 0", "F = SHR")) -> proj2b

proj2d %>% 
  bind_rows(proj2aa) %>% 
  bind_rows(proj2b) -> proj2

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

proj2 %>% 
  ggplot(aes(year, recovery, shape = FishMort, colour = projection)) + 
  geom_point(size = 2)+
  scale_shape_manual(name = "", values = c(16, 17, 22)) +
  scale_color_manual(name = "", values = cbPalette[1:3])+
  geom_line() +
  geom_hline(yintercept = 50, color = "red", lty = "dashed", lwd = 1.5) +
  geom_vline(xintercept = 10, color = "blue", lty = 2, lwd = 1.5) +
  ggtitle(expression(paste("Ricker stock-recruit relationship (", B[MSY]," proxy 1978 - 2018)"))) +
  ylab("Probability of recovery") +
  xlab("Year") +
  ylim(0,100) +
  theme(plot.title = element_text(hjust = 0.5)) -> plotA
ggsave(paste0(here::here(), '/SMBKC/smbkc_19/doc/rebuilding_2019/proj2_rec_1yr_prob_REBUILD.png'), plotA, dpi = 800,
       width = 7.5, height = 3.75)


### proj 2 - ABC level ----------------
proj2d %>% 
  mutate(projection = "avg recent bycatch") %>% 
  select(-FishMort) %>% 
  mutate(FishMort = ifelse(V3 == 1, "F = 0", "F = SHS")) -> proj2d

proj2aa %>% 
  mutate(projection = "max bycatch") %>% 
  select(-FishMort) %>% 
  mutate(FishMort = ifelse(V3 == 1, "F = 0", "F = SHS")) -> proj2aa

proj2abc %>% 
  mutate(projection = "avg recent bycatch w/ABC") %>% 
  filter(V3 == 2) %>% 
  mutate(FishMort = "F = ABC") %>% 
  select(year, V3, recovery, projection, FishMort) -> proj2abc
#elect(-FishMort) %>% 
#mutate(FishMort = ifelse(V3 == 1, "F = 0", "F = SHR")) -> proj2b

proj2d %>% 
  bind_rows(proj2aa) %>% 
  bind_rows(proj2abc) -> proj2

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

proj2 %>% 
  ggplot(aes(year, recovery, shape = FishMort, colour = projection)) + 
  geom_point(size = 2)+
  scale_shape_manual(name = "", values = c(16, 17, 22)) +
  scale_color_manual(name = "", values = c(cbPalette[2], cbPalette[1], cbPalette[3]))+
  geom_line() +
  geom_hline(yintercept = 50, color = "red", lty = "dashed", lwd = 1.5) +
  geom_vline(xintercept = 10, color = "blue", lty = 2, lwd = 1.5) +
  ggtitle(expression(paste("Ricker stock-recruit relationship (", B[MSY]," proxy 1978 - 2018)"))) +
  ylab("Probability of recovery") +
  xlab("Year") +
  ylim(0,100) +
  theme(plot.title = element_text(hjust = 0.5)) -> plotA
ggsave(paste0(here::here(), '/SMBKC/smbkc_19/doc/rebuilding_2019/proj2_rec_1yr_prob_REBUILD_ABC.png'), plotA, dpi = 800,
       width = 7.5, height = 3.75)

## proj 2 avg bycatch only ---------
## avg only =====
proj2d %>% 
  ggplot(aes(year, recovery, shape = FishMort, colour = projection)) + 
  geom_point(size = 2)+
  scale_shape_manual(name = "", values = c(16, 22)) +
  scale_color_manual(name = "", values = cbPalette[2:3])+
  geom_line() +
  geom_hline(yintercept = 50, color = "red", lty = "dashed", lwd = 1.5) +
  geom_vline(xintercept = 10, color = "blue", lty = 2, lwd = 1.5) +
  ggtitle("Ricker stock-recruit relationship") +
  ylab("Probability of recovery") +
  xlab("Year") +
  ylim(0,100) +
  theme(plot.title = element_text(hjust = 0.5)) -> plot2d
ggsave(paste0(here::here(), '/SMBKC/smbkc_19/doc/rebuilding_2019/proj2_d_rec_1yr_prob.png'), plot2d, dpi = 800,
       width = 7.5, height = 3.75)

## bycatch: none, avg & max only =====
proj2a %>% 
  mutate(projection = "avg recent bycatch or no bycatch") %>% 
  select(-FishMort) %>% 
  mutate(FishMort = ifelse(V3 == 1, "F = 0", "F = SHS")) -> proj2a

#proj2d %>% 
#  mutate(projection = "avg recent bycatch") %>% 
#  select(-FishMort) %>% 
#  mutate(FishMort = ifelse(V3 == 1, "F = 0", "F = SHS")) -> proj2d


proj2aa %>% 
  mutate(projection = "max bycatch") %>% 
  select(-FishMort) %>% 
  mutate(FishMort = ifelse(V3 == 1, "F = 0", "F = SHS")) -> proj2aa

proj2a %>% 
  bind_rows(proj2aa) -> proj2

#  bind_rows(proj2a) -> proj2

proj2 %>% 
  ggplot(aes(year, recovery, shape = FishMort, colour = projection)) + 
  geom_point(size = 2)+
  scale_shape_manual(name = "", values = c(16, 22)) +
  scale_color_manual(name = "", values = c(cbPalette[2:3])) +
  geom_line() +
  geom_hline(yintercept = 50, color = "red", lty = "dashed", lwd = 1.5) +
  geom_vline(xintercept = 10, color = "blue", lty = 2, lwd = 1.5) +
  ggtitle("Ricker stock-recruit relationship") +
  ylab("Probability of recovery") +
  xlab("Year") +
  ylim(0,100) +
  theme(plot.title = element_text(hjust = 0.5)) -> plotB
ggsave(paste0(here::here(), '/SMBKC/smbkc_19/doc/rebuilding_2019/proj2_BYCATCH_a_aa_rec_1yr_prob.png'), plotB, dpi = 800,
       width = 7.5, height = 3.75)

### proj 2 - changes to SHP level ----------------
## see rebuilding readme
proj2d %>% 
  mutate(projection = "avg recent bycatch") %>% 
  select(-FishMort) %>% 
  mutate(FishMort = ifelse(V3 == 1, "F = 0", "F = SHR")) -> proj2d

proj2aa %>% 
  mutate(projection = "max bycatch") %>% 
  select(-FishMort) %>% 
  mutate(FishMort = ifelse(V3 == 1, "F = 0", "F = SHR")) -> proj2aa

proj2abc %>% 
  mutate(projection = "alternative 1") %>% 
  filter(V3 == 2) %>% 
  mutate(FishMort = "F = ABC") %>% 
  select(year, V3, recovery, projection, FishMort) -> proj2abc
#elect(-FishMort) %>% 
#mutate(FishMort = ifelse(V3 == 1, "F = 0", "F = SHR")) -> proj2b

projSHP %>% 
  mutate(projection = "alternative 2") %>% 
  filter(V3 == 2) %>%
  select(-FishMort) %>% 
  mutate(FishMort = ifelse(V3 == 1, "F = 0", "F = adj_SHR")) -> projSHP

proj2d %>% 
  bind_rows(projSHP) %>% 
  bind_rows(proj2abc) -> proj2

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

proj2 %>% 
  ggplot(aes(year, recovery, shape = FishMort, colour = projection)) + 
  geom_point(size = 2)+
  scale_shape_manual(name = "", values = c(16, 17, 25, 22)) +
  scale_color_manual(name = "", values = c(cbPalette[1], cbPalette[4], cbPalette[2])) +
  geom_line() +
  geom_hline(yintercept = 50, color = "red", lty = "dashed", lwd = 1.5) +
  geom_vline(xintercept = 10, color = "blue", lty = 2, lwd = 1.5) +
  ggtitle(expression(paste("Ricker stock-recruit relationship (", B[MSY]," proxy 1978 - 2018)"))) +
  ylab("Probability of recovery") +
  xlab("Year") +
  ylim(0,100) +
  theme(plot.title = element_text(hjust = 0.5)) -> plotA
ggsave(paste0(here::here(), '/SMBKC/smbkc_19/doc/rebuilding_2019/proj2_rec_1yr_prob_SHPchanges.png'), plotA, dpi = 800,
       width = 7.5, height = 3.75)

# compare Andre changes -----------
# this projection should be the same as proj 2d just with new parameters in ricker relationship set to 0
proj2b %>% 
  mutate(projection = "avg recent bycatch") %>% 
  select(-FishMort) %>% 
  mutate(FishMort = ifelse(V3 == 1, "F = 0", "F = 0.18"))  -> proj2b

proj2b_changes %>% 
  mutate(projection = "d with Andre changes") %>% 
  select(-FishMort) %>% 
  mutate(FishMort = ifelse(V3 == 1, "F = 0", "F = 0.18")) -> proj2b_changes

proj2b %>% 
  bind_rows(proj2b_changes) -> proj2

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

proj2 %>% 
  ggplot(aes(year, recovery, shape = FishMort, colour = projection)) + 
  geom_point(size = 2)+
  scale_shape_manual(name = "", values = c(16, 22)) +
  scale_color_manual(name = "", values = cbPalette[2:3])+
  geom_line() +
  geom_hline(yintercept = 50, color = "red", lty = "dashed", lwd = 1.5) +
  geom_vline(xintercept = 10, color = "blue", lty = 2, lwd = 1.5) +
  ggtitle(expression(paste("Ricker stock-recruit relationship (", B[MSY]," proxy 1978 - 2018)"))) +
  ylab("Probability of recovery") +
  xlab("Year") +
  ylim(0,100) +
  theme(plot.title = element_text(hjust = 0.5)) -> plotA
ggsave(paste0(here::here(), '/SMBKC/smbkc_19a/model_1/projections/proj2dchanges_rec_1yr_prob.png'), plotA, dpi = 800,
       width = 7.5, height = 3.75)


