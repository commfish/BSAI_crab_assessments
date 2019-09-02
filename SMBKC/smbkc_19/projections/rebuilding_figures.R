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

proj1 %>% 
ggplot(aes(year, recovery, shape = FishMort, colour = projection)) + 
  geom_point(size = 2)+
  scale_shape_manual(name = "", values = c(16, 22)) +
  geom_line() +
  geom_hline(yintercept = 50, color = "red", lty = "dashed", lwd = 1.5) +
  geom_vline(xintercept = 10, color = "blue", lty = 2, lwd = 1.5) +
  ggtitle("Recruitment drawn from 1978 - 2018") +
  ylab("Probability of recovery") +
  xlab("Year") +
  ylim(0,100) +
  theme(plot.title = element_text(hjust = 0.5)) -> plotA
ggsave(paste0(here::here(), '/SMBKC/', model_yr, '/', model, '/projections/', proj, '/', version, '/_rec_1yr_prob.png'), plotA, dpi = 800,
       width = 7.5, height = 3.75)



  
  
  
  
  