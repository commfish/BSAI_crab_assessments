# katie.palof@alaska.gov
# 8-6-2020

# SMBKC model output
# model 1 from Sept. 2020

# Objective: Figures and summary of 2020 retrospective analysis, normal and with terminal survey year dropped.


# load --
source("./SMBKC/code/helper.R")
.FIGS = c(paste0("./SMBKC/smbkc_20/retrospective_model_1/figures/"))
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
.THEME    = list(theme_bw(base_size = 12, base_family = ""), scale_fill_manual(values=cbPalette), 
                 scale_colour_manual(values=cbPalette))
# data ---
mmb <- read.csv(paste0(here::here(), '/SMBKC/smbkc_20/retrospective_model_1/combined_data/ssb_all.csv'))
sum_stats <- read.csv(paste0(here::here(), '/SMBKC/smbkc_20/retrospective_model_1/combined_data/summary.csv'))

mmb_cur <- read.csv(paste0(here::here(), '/SMBKC/smbkc_20/retrospective_model_1/combined_data/ssb_2020.csv'))
# bring in 2020 model results here. - current year

# data clean-up ---------
mmb_cur %>% 
  mutate(model = "2020 base") %>% 
  select(Model = model, par, log_ssb, log_sd, year, ssb, lb, ub) %>% 
  mutate(Model.end.yr = "2020", type = "retro") -> mmb_cur2

mmb %>% 
  mutate(type = ifelse(str_detect(Model, "woTS"), "woTS", "retro")) %>% 
  rbind(mmb_cur2) -> mmb2

sum_stats %>% 
  mutate(year = as.factor(year)) -> sum_stats

# retro mmb all ---------
mmb2 %>% 
  ggplot(aes(year, ssb, group = Model)) +
    geom_line(aes(group = Model, colour = Model), lwd = 0.75) +
    ylab("Mature male biomass (tons) on Feb 15th") +
  xlab("Year") +
  ylim(c(0,11500)) +
    theme_bw(base_size = 12, base_family = "")

ggsave(paste0(.FIGS, "ssb_time_series_all.png"), width = 1.25*6, height = 5)


# retro mmb compare last 5 years ----------
mmb2 %>% 
  filter(Model.end.yr > 2015) %>% 
  ggplot(aes(year, ssb, group = Model)) +
  geom_line(aes(group = Model, colour = Model.end.yr, linetype = type), lwd = 0.75) +
  ylab("Mature male biomass (tons) on Feb 15th") +
  xlab("Year") +
  ylim(c(0,11500)) +
  theme_bw(base_size = 12, base_family = "") +
  scale_colour_discrete(name  ="Model end year")

ggsave(paste0(.FIGS, "ssb_time_series_last5_compare.png"), width = 1.25*6, height = 5)

# retro mmb compare last 5 years recent time period----------
mmb2 %>% 
  filter(Model.end.yr > 2015) %>% 
  filter(year > 2000) %>% 
  ggplot(aes(year, ssb, group = Model)) +
  geom_line(aes(group = Model, colour = Model.end.yr, linetype = type), lwd = 0.75) +
  ylab("Mature male biomass (tons) on Feb 15th") +
  xlab("Year") +
  ylim(c(0,3000)) +
  theme_bw(base_size = 12, base_family = "") +
  scale_colour_discrete(name  ="Model end year")

ggsave(paste0(.FIGS, "ssb_time_series_last5_compare_recent.png"), width = 1.25*6, height = 5)

# retro mmb normal ----------
mmb2 %>% 
  filter(type == "retro") %>% 
  ggplot(aes(year, ssb, group = Model.end.yr)) +
  geom_line(aes(group = Model, colour = Model.end.yr), lwd = 0.75) +
  ylab("Mature male biomass (tons) on Feb 15th") +
  xlab("Year") +
  ylim(c(0,11500)) +
  theme_bw(base_size = 12, base_family = "") +
  scale_colour_discrete(name  ="Model end year")

ggsave(paste0(.FIGS, "ssb_time_series_normal.png"), width = 1.25*6, height = 5)

# retro mmb normal current years ----------
mmb2 %>% 
  filter(type == "retro") %>%
  filter(year > 2000) %>% 
  ggplot(aes(year, ssb, group = Model.end.yr)) +
  geom_line(aes(group = Model, colour = Model.end.yr), lwd = 0.75) +
  ylab("Mature male biomass (tons) on Feb 15th") +
  xlab("Year") +
  ylim(c(0,7500)) +
  theme_bw(base_size = 12, base_family = "") +
  scale_colour_discrete(name  ="Model end year")

ggsave(paste0(.FIGS, "ssb_time_series_normal_current.png"), width = 1.25*6, height = 5)
# comparison btn retro and leave out terminal yr survey -----------------
head(sum_stats)

sum_stats %>% 
  select(year, avgr, type) %>% 
  spread(type, avgr) %>% 
  mutate(avgR = (wo_term_sur_retro-retro)/retro*100) %>% 
  select(year, avgR) -> avgR

sum_stats %>% 
  select(year, bmsy, type) %>% 
  spread(type, bmsy) %>% 
  mutate(Bmsy = (wo_term_sur_retro-retro)/retro*100) %>% 
  select(year, Bmsy) -> Bmsy

sum_stats %>% 
  select(year, mmb_terminal, type) %>% 
  spread(type, mmb_terminal) %>% 
  mutate(Terminal_mmb = (wo_term_sur_retro-retro)/retro*100) %>% 
  select(year, Terminal_mmb) -> Terminal_mmb

sum_stats %>% 
  select(year, status, type) %>% 
  spread(type, status) %>% 
  mutate(Status = (wo_term_sur_retro-retro)/retro*100) %>% 
  select(year, Status) -> Status

sum_stats %>% 
  select(year, f_ofl , type) %>% 
  spread(type, f_ofl ) %>% 
  mutate(F_ofl = (wo_term_sur_retro-retro)/retro*100) %>% 
  select(year, F_ofl) -> F_ofl


avgR %>% 
  left_join(Bmsy) %>% 
  left_join(Terminal_mmb) %>% 
  left_join(Status) %>% 
  left_join(F_ofl) -> stats_compare

stats_compare
write.csv(stats_compare, paste0(.FIGS, "stats_summary_compare_table.csv"), row.names = FALSE)


# bar graphs for visual ------------
sum_stats %>% 
  select(year, avgr, type) %>% 
  ggplot(aes(year, avgr, fill = type)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  ggtitle("Average recruitment [1978 - (terminal year - 1)]") +
  ylab("Average recruitment (millions)") +
  xlab("Year") +
  scale_fill_discrete(name = "Type", labels = c("Retrospective", "MissingSurvey")) +
  theme_bw(base_size = 12, base_family = "")
ggsave(paste0(.FIGS, "avgR_compare.png"), width = 1.15*6, height = 5)


sum_stats %>% 
  select(year, bmsy, type) %>% 
  ggplot(aes(year, bmsy, fill = type)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  ggtitle("B msy") +
  ylab("Bmsy (t)") +
  xlab("Assessment Year") +
  scale_fill_discrete(name = "Type", labels = c("Retrospective", "MissingSurvey")) +
  theme_bw(base_size = 12, base_family = "") 
ggsave(paste0(.FIGS, "Bmsy_compare.png"), width = 1.15*6, height = 5)


sum_stats %>% 
  select(year, mmb_terminal, type) %>% 
  ggplot(aes(year, mmb_terminal, fill = type)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  ggtitle("Terminal MMB") +
ylab("Terminal MMB (t)") +
  xlab("Assessment Year") +
  scale_fill_discrete(name = "Type", labels = c("Retrospective", "MissingSurvey")) +
  theme_bw(base_size = 12, base_family = "")
ggsave(paste0(.FIGS, "Terminal_mmb_compare.png"), width = 1.15*6, height = 5)

sum_stats %>% 
  select(year, status, type) %>% 
  ggplot(aes(year, status, fill = type)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  ggtitle(expression(paste("Status (", B[prj], "/", B[MSY],")"))) +
  ylab("status (projected MMB/Bmsy") +
  xlab("Assessment Year") +
  scale_fill_discrete(name = "Type", labels = c("Retrospective", "MissingSurvey")) +
  theme_bw(base_size = 12, base_family = "")+
  geom_hline(yintercept = 1.0, linetype = "longdash", color = "blue") +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "red")
ggsave(paste0(.FIGS, "Status_compare.png"), width = 1.15*6, height = 5)


sum_stats %>% 
  select(year, OFL, type) %>% 
  ggplot(aes(year, OFL, fill = type)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  ggtitle("OFL") +
  ylab("OFL (t)") +
  xlab("Assessment Year") +
  scale_fill_discrete(name = "Type", labels = c("Retrospective", "MissingSurvey")) +
  theme_bw(base_size = 12, base_family = "")
ggsave(paste0(.FIGS, "OFL_compare.png"), width = 1.15*6, height = 5)

sum_stats %>% 
  select(year, f_ofl, type) %>% 
  ggplot(aes(year, f_ofl, fill = type)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  ggtitle(expression(paste("", F[OFL], ""))) +
  ylab(expression(paste("", F[OFL], ""))) +
  xlab("Assessment Year") +
  scale_fill_discrete(name = "Type", labels = c("Retrospective", "MissingSurvey")) +
  theme_bw(base_size = 12, base_family = "")
ggsave(paste0(.FIGS, "fofl_compare.png"), width = 1.15*6, height = 5)


### Approach 3 ------------
# highs and lows ----
 
# load ---------
summary_2020 <- read.csv(paste0(here::here(), '/SMBKC/smbkc_20/retrospective_model_1/combined_data/summary_2020_model_1.csv'))

low_mmb <- read.csv(paste0(here::here(), '/SMBKC/smbkc_20/model_1_app3_low/figure/ssb_2020_app3_low.csv'))
low_summary <- read.csv(paste0(here::here(), '/SMBKC/smbkc_20/model_1_app3_low/figure/summary_app3_low.csv'))

high_mmb <- read.csv(paste0(here::here(), '/SMBKC/smbkc_20/model_1_app3_high/figure/ssb_2020_app3_high.csv'))
high_summary <- read.csv(paste0(here::here(), '/SMBKC/smbkc_20/model_1_app3_high/figure/summary_app3_high.csv'))

# data clean up ---------
# combine low, high, and cur year - 2020
head(mmb_cur)
tail(low_mmb)
tail(high_mmb)

mmb_cur %>% 
  mutate(Bmsy = mean(ssb[year %in% c(1978:2019)])) -> mmb_cur
low_mmb %>% 
  mutate(Bmsy = mean(ssb[year %in% c(1978:2019)])) -> low_mmb
high_mmb %>% 
  mutate(Bmsy = mean(ssb[year %in% c(1978:2019)])) -> high_mmb
mmb_cur %>% 
  rbind(low_mmb) %>% 
  rbind(high_mmb) -> app3_ssb


# mmb for all 2020 model, low, and high --------------
app3_ssb %>% 
  ggplot(aes(year, ssb, group = Model)) +
  geom_line(aes(group = Model, colour = Model), lwd = 0.75) +
  ylab("Mature male biomass (tons) on Feb 15th") +
  xlab("Year") +
  ylim(c(0,11000)) +
  .THEME + #theme_bw(base_size = 12, base_family = "") +
  geom_line(aes(year, Bmsy, group = Model, colour = Model))
ggsave(paste0(.FIGS, "app3_ssb_all_yrs.png"), width = 1.5*6, height = 5)

app3_ssb %>% 
  filter(year > 2010) %>% 
  ggplot(aes(year, ssb, group = Model)) +
  geom_line(aes(group = Model, colour = Model), lwd = 0.75) +
  ylab("Mature male biomass (tons) on Feb 15th") +
  xlab("Year") +
  #ylim(c(0, 3500)) +
  .THEME  #theme_bw(base_size = 12, base_family = "") +
  #geom_line(aes(year, Bmsy, group = Model, colour = Model))
ggsave(paste0(.FIGS, "app3_last_10yrs_ssb.png"), width = 1.5*6, height = 5)

# summary stats app3 -----------
low_summary
high_summary
summary_2020

summary_2020 %>% 
  rbind(low_summary) %>% 
  rbind(high_summary) -> sum_stats2


## bring in OFL calcs from manual file -----------
ref_ofl <- read.csv(paste0(here::here(), "/SMBKC/smbkc_20/model_1/figure/ofl_calc.csv"))
low_ofl <- read.csv(paste0(here::here(), "/SMBKC/smbkc_20/model_1_app3_low/figure/ofl_calc.csv"))
high_ofl <- read.csv(paste0(here::here(), "/SMBKC/smbkc_20/model_1_app3_high/figure/ofl_calc.csv"))

sum_stats2 %>% 
  mutate(OFL = c(ref_ofl$OFL_2020, low_ofl$OFL_2020, high_ofl$OFL_2020)) -> sum_stats2
write.csv(sum_stats2, paste0(here::here(), "/SMBKC/smbkc_20/doc/safe_tables/app3_sum_stats.csv"), row.names = FALSE)
# save output for table here -----

# bar graphs for summary -------
sum_stats2 %>% 
  select(-year) %>% 
  mutate(avgr = avgr/1000000) %>% 
  melt(id.vars = "type") %>% 
  ggplot(aes(type, value, fill = type)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  facet_wrap(~variable, scales = "free_y") +
  ggtitle("Approach 3 - high, low, 2020 base") +
  ylab("Model outputs") +
  xlab("") +
    #scale_fill_discrete(name = "Type", labels = c("Retrospective", "MissingSurvey")) +
  .THEME +
  theme(axis.text.x = element_blank()) #theme_bw(base_size = 12, base_family = "")
ggsave(paste0(.FIGS, "app3_bar_graph_output.png"), width = 1.15*6, height = 5)


# 
