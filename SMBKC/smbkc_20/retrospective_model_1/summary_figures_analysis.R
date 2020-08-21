# katie.palof@alaska.gov
# 8-6-2020

# SMBKC model output
# model 1 from Sept. 2020

# Objective: Figures and summary of 2020 retrospective analysis, normal and with terminal survey year dropped.


# load --
source("./SMBKC/code/helper.R")
.FIGS = c(paste0("./SMBKC/smbkc_20/retrospective_model_1/figures/"))

# data ---
mmb <- read.csv(paste0(here::here(), '/SMBKC/smbkc_20/retrospective_model_1/combined_data/ssb_all.csv'))
sum_stats <- read.csv(paste0(here::here(), '/SMBKC/smbkc_20/retrospective_model_1/combined_data/summary.csv'))

mmb_cur <- read.csv(paste0(here::here(), '/SMBKC/smbkc_20/retrospective_model_1/combined_data/ssb_2020.csv'))
# data clean-up ---------
mmb_cur %>% 
  mutate(model = "2020 base") %>% 
  select(Model = model, par, log_ssb, log_sd, year, ssb, lb, ub) %>% 
  mutate(type = "retro") -> mmb_cur2

mmb %>% 
  mutate(type = ifelse(str_detect(Model, "woTS"), "woTS", "retro")) %>% 
  rbind(mmb_cur2) -> mmb2


# retro mmb all ---------
mmb2 %>% 
  ggplot(aes(year, ssb, group = Model)) +
    geom_line(aes(group = Model, colour = Model), lwd = 0.75) +
    ylab("Mature male biomass (tons) on Feb 15th") +
  xlab("Year") +
    theme_bw(base_size = 12, base_family = "")

ggsave(paste0(.FIGS, "ssb_time_series_all.png"), width = 1.5*6, height = 5)

# retro mmb normal ----------
mmb2 %>% 
  filter(type == "retro") %>% 
  ggplot(aes(year, ssb, group = Model)) +
  geom_line(aes(group = Model, colour = Model), lwd = 0.75) +
  ylab("Mature male biomass (tons) on Feb 15th") +
  xlab("Year") +
  theme_bw(base_size = 12, base_family = "")

ggsave(paste0(.FIGS, "ssb_time_series_normal.png"), width = 1.5*6, height = 5)
#**FIX** need to add 2020 model here. 

# comparison btn retro and leave out terminal yr survey -----------------
head(sum_stats)



# bar graphs for visual ------------
sum_stats %>% 
  select(year, avgr, type) %>% 
  ggplot(aes(year, avgr, fill = type)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  ggtitle("average recruitment")


sum_stats %>% 
  select(year, bmsy, type) %>% 
  ggplot(aes(year, bmsy, fill = type)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  ggtitle("bmsy")

sum_stats %>% 
  select(year, mmb_terminal, type) %>% 
  ggplot(aes(year, mmb_terminal, fill = type)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  ggtitle("mmb_terminal")

sum_stats %>% 
  select(year, status, type) %>% 
  ggplot(aes(year, status, fill = type)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  ggtitle("status")

sum_stats %>% 
  select(year, OFL, type) %>% 
  ggplot(aes(year, OFL, fill = type)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  ggtitle("OFL")

sum_stats %>% 
  select(year, f_ofl, type) %>% 
  ggplot(aes(year, f_ofl, fill = type)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  ggtitle("f_ofl")
