# katie.palof@alaska.gov
# 8-6-2020

# SMBKC model output
# model 1 from Sept. 2020

# Objective: Figures and summary of 2020 retrospective analysis, normal and with terminal survey year dropped.


# load --
source("./SMBKC/code/helper.R")
library(icesAdvice)
.FIGS = c(paste0("./SMBKC/smbkc_20/retrospective_model_1/figures/"))
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
.THEME    = list(theme_bw(base_size = 12, base_family = ""), scale_fill_manual(values=cbPalette), 
                 scale_colour_manual(values=cbPalette))
# data ---
mmb <- read.csv(paste0(here::here(), '/SMBKC/smbkc_20/retrospective_model_1/combined_data/ssb_all.csv'))
sum_stats <- read.csv(paste0(here::here(), '/SMBKC/smbkc_20/retrospective_model_1/combined_data/summary.csv'))

parms <- read.csv(paste0(here::here(), '/SMBKC/smbkc_20/retrospective_model_1/combined_data/error_summary.csv'))

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
  geom_line(aes(group = Model, colour = Model.end.yr, linetype = type), lwd = 0.75) +
    ylab("Mature male biomass (tons) on Feb 15th") +
  xlab("Year") +
  ylim(c(0,11500)) +
    theme_bw(base_size = 12, base_family = "") +
  scale_colour_discrete(name  ="Model end year")

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

# mohn's row -----
mmb2 %>% 
  filter(type == "retro") %>% 
  select(Model.end.yr, year, ssb) %>% 
  spread(Model.end.yr, ssb) %>% 
  select(year, `2020`, `2019`, `2018`, `2017`, `2016`, `2015`, `2014`, `2013`, `2012`) -> out2


mohn(out2)
mohn(out2, peels = 5, details = FALSE, plot = TRUE)
mohn(out2, peels = 7, details = FALSE, plot = TRUE)
mohn(out2, peels = 9, details = FALSE, plot = TRUE)


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

sum_stats %>% 
  select(year, OFL , type) %>% 
  spread(type, OFL ) %>% 
  mutate(OFL = (wo_term_sur_retro-retro)/retro*100) %>% 
  select(year, OFL) -> OFL

avgR %>% 
  left_join(Bmsy) %>% 
  left_join(Terminal_mmb) %>% 
  left_join(Status) %>% 
  left_join(F_ofl) %>% 
  left_join(OFL) -> stats_compare

stats_compare
write.csv(stats_compare, paste0(.FIGS, "stats_summary_compare_table.csv"), row.names = FALSE)

stats_compare %>% 
  gather(quant, measurement, avgR:OFL, factor_key=TRUE) %>% 
  summarise(mean(measurement))
#mean(measurement)
#1         0.9545492
stats_compare %>% 
  gather(quant, measurement, avgR:OFL, factor_key=TRUE) %>% 
  group_by(quant) %>% 
  summarise(mean(measurement))

#quant        `mean(measurement)`
#<fct>                      <dbl>
#1 avgR                     -0.112 
#2 Bmsy                      0.0723
#3 Terminal_mmb              0.554 
#4 Status                    0.381 
#5 F_ofl                     0.829 
#6 OFL                       4.00  

# MSE for each quanitity of interest ---------
head(sum_stats)
avgR %>% mutate(st1 = avgR^2) %>% summarise(RMSE = sqrt(sum(st1)/10))


stats_compare %>% 
  mutate(Rec = avgR^2, 
         Bs = Bmsy^2, 
         tmmb = Terminal_mmb^2, 
         sta = Status^2, 
         f = F_ofl^2, 
         ofl = OFL^2) %>% 
  select(avgR = Rec, Bmsy = Bs, Terminal_mmb = tmmb, Status = sta, 
         F_ofl = f, OFL = ofl) %>% 
  gather(quant, error_sq, avgR:OFL) %>% 
  group_by(quant) %>% 
  summarise(RMSE = sqrt(sum(error_sq)/10)) %>% 
  spread(quant, RMSE) %>% 
  mutate(year = "RMS") %>% 
  select(year, avgR, Bmsy, Terminal_mmb, Status, F_ofl, OFL)-> RSE


stats_compare %>% 
  bind_rows(RSE) -> stats_compare2
stats_compare2
write.csv(stats_compare2, paste0(.FIGS, "stats_summary_compare_table2.csv"), row.names = FALSE)


# parameter estimates and CV  ------------------
head(parms)

# avg CV retro cs. without terminal year -------
parms %>% 
  filter(value == "CV") %>% 
  group_by(type) %>% 
  summarise(Bmsy_cv = mean(Bmsy), 
            OFL_cv = mean(OFL), 
            status_cv = mean(status), 
            terminal_cv = mean(terminal_ssb)) %>% 
  mutate(type = ifelse(type == "retro", "retro", "missing-survey")) %>% 
  write.csv(paste0(.FIGS, "CV_summary_compare_table.csv"), row.names = FALSE)



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
ggsave(paste0(.FIGS, "app3_ssb_all_yrs.png"), width = 1.18*6, height = 5)

app3_ssb %>% 
  filter(year > 2010) %>% 
  ggplot(aes(year, ssb, group = Model)) +
  geom_line(aes(group = Model, colour = Model), lwd = 0.75) +
  ylab("Mature male biomass (tons) on Feb 15th") +
  xlab("Year") +
  #ylim(c(0, 3500)) +
  .THEME  #theme_bw(base_size = 12, base_family = "") +
  #geom_line(aes(year, Bmsy, group = Model, colour = Model))
ggsave(paste0(.FIGS, "app3_last_10yrs_ssb.png"), width = 1.18*6, height = 5)

app3_ssb %>% 
  filter(year > 2000) %>% 
  ggplot(aes(year, ssb, group = Model)) +
  geom_line(aes(group = Model, colour = Model), lwd = 0.75) +
  geom_ribbon(aes(x=year, ymax = ub, ymin = lb, fill = Model, col = NULL), alpha = 0.1) +
  ylab("Mature male biomass (tons) on Feb 15th") +
  xlab("Year") +
  #ylim(c(0, 3500)) +
  .THEME  #theme_bw(base_size = 12, base_family = "") +
#geom_line(aes(year, Bmsy, group = Model, colour = Model))
ggsave(paste0(.FIGS, "app3_last_20yrs_ssb_ribbons.png"), width = 1.18*6, height = 5)

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


# # comparison btn retro and leave out terminal yr survey -----------------
head(sum_stats2)

sum_stats2 %>% 
  select(year, avgr, type) %>% 
  spread(type, avgr) %>% 
  mutate(quant = "avgR", 
         L_base = (App3_L - `base model 1`)/`base model 1`*100, 
         H_base = (App3_H - `base model 1`)/`base model 1`*100) %>% 
  select(quant, L_base, H_base) -> avgR

sum_stats2 %>% 
  select(year, bmsy, type) %>% 
  spread(type, bmsy) %>% 
  mutate(quant = "Bmsy", 
         L_base = (App3_L - `base model 1`)/`base model 1`*100, 
         H_base = (App3_H - `base model 1`)/`base model 1`*100) %>% 
  select(quant, L_base, H_base) -> Bmsy

sum_stats2 %>% 
  select(year, mmb_terminal, type) %>% 
  spread(type, mmb_terminal) %>% 
  mutate(quant = "Terminal-MMB", 
         L_base = (App3_L - `base model 1`)/`base model 1`*100, 
         H_base = (App3_H - `base model 1`)/`base model 1`*100) %>% 
  select(quant, L_base, H_base) -> Terminal_mmb

sum_stats2 %>% 
  select(year, status, type) %>% 
  spread(type, status) %>% 
  mutate(quant = "Status", 
         L_base = (App3_L - `base model 1`)/`base model 1`*100, 
         H_base = (App3_H - `base model 1`)/`base model 1`*100) %>% 
  select(quant, L_base, H_base) -> Status

sum_stats2 %>% 
  select(year, f_ofl, type) %>% 
  spread(type, f_ofl) %>% 
  mutate(quant = "F-ofl", 
         L_base = (App3_L - `base model 1`)/`base model 1`*100, 
         H_base = (App3_H - `base model 1`)/`base model 1`*100) %>% 
  select(quant, L_base, H_base) -> F_ofl

sum_stats2 %>% 
  select(year, OFL, type) %>% 
  spread(type, OFL) %>% 
  mutate(quant = "OFL", 
         L_base = (App3_L - `base model 1`)/`base model 1`*100, 
         H_base = (App3_H - `base model 1`)/`base model 1`*100) %>% 
  select(quant, L_base, H_base) -> OFL

avgR %>% 
  bind_rows(Bmsy) %>% 
  bind_rows(Terminal_mmb) %>% 
  bind_rows(Status) %>% 
  bind_rows(F_ofl) %>% 
  bind_rows(OFL) -> stats_compare

stats_compare
write.csv(stats_compare, paste0(.FIGS, "stats_summary_app3_table.csv"), row.names = FALSE)
