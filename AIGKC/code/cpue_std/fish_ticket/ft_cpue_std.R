# notes ----
## fish ticket cpue standardization
## tyler jackson
## 11/6/2023

# load ----

source("./AIGKC/code/aigkc_functions.R")
library(MASS)
library(DHARMa)

# function for plotting model diagnostics
f_diagnostics <- function(x) {
  
  # qq plot
  ggplot()+
    stat_qq_line(aes(sample = resid(x, type = "deviance")), color = 2)+
    stat_qq(aes(sample = resid(x, type = "deviance")))+
    labs(x = "Theorical Quantiles", y = "Deviance Residuals", title = "Q-Q plot") -> p1
  
  # historgam
  ggplot()+
    geom_histogram(aes(x =  resid(x, type = "deviance")))+
    labs(x = "Residuals", y = "Count", title = "Histogram of residuals") -> p2
  
  # resid
  ggplot()+
    geom_point(aes(x = x$linear.predictors, y = resid(x, type = "deviance")))+
    geom_hline(yintercept = 0, linetype = 2)+
    labs(x = "Linear Predictor", y = "Residuals", title = "Resids. vs linear pred.") -> p3
  
  # resid vs fitted 
  ggplot()+
    geom_point(aes(x = x$fitted.values, y = x$model$cpue))+
    labs(x = "Fitted Values", y = "Response Values", title = "Response vs. Fitted") -> p4
  
  
  (p1 + p2) / (p3 + p4)
  
}

yraxis <- tickr(tibble(yr = 1985:1998), var = yr, to = 2)

# data ----

ft <- read_csv("AIGKC/data/observer/item2_linked_fish_ticket_dump.csv")

# core data ----

ft %>% 
  filter(crab_year %in% 1985:1998,
         # remove deadloss and personal use deliveries
         !delivery_condition_code %in% c(79, 95)) %>%
  group_by(adfg_number) %>%
  filter(length(unique(crab_year)) > 5) %>%
  ungroup %>%
  group_by(cfec_permit_holder_name) %>% 
  filter(length(unique(crab_year)) > 5) %>%
  ungroup %>%
  # add a cpue column
  mutate(cpue = ifelse(effort_sum != 0, number_of_crab / effort_sum, 0)) -> ft_core 

# ft core table
ft_core %>%
  group_by(crab_year, fishery) %>%
  add_count %>%
  group_by(crab_year, fishery) %>%
  summarise(adfg = length(unique(adfg_number)),
            cfec_permit_holder_name = length(unique(cfec_permit_holder_name)),
            month = length(unique(month_landed)),
            statarea = length(unique(stat_area))) %>%
  # write output
  write_csv(., "./AIGKC/output/cpue_std/fish_tickets_85_98_sample_size.csv")

# eag ----

ft_core %>%
  filter(fishery == "EAG") %>%
  transmute(crab_year = factor(crab_year),
            cfec_permit_holder_name,
            month = factor(month_landed),
            adfg = factor(adfg_number), 
            statarea = factor(stat_area),
            #number_of_crab, effort_sum,
            cpue) %>% 
  filter(complete.cases(.)) -> eag

# null model
eag_null <- glm(cpue ~ crab_year, family = neg.bin(9.169122), data = eag)
theta.ml(eag$cpue, eag_null$fitted.values)

# full model (a) includes year, captain, month, vessel, areagp
eag_best_aic <- stepAIC(eag_null, scope = list(upper= ~(crab_year + cfec_permit_holder_name + month + adfg + statarea), lower = ~crab_year),
                        direction = "forward", trace = 9, k = log(nrow(eag)) + 1.0)
theta.ml(eag$cpue, eag_best_aic$fitted.values)
eag_best <- f_stepCPUE(eag_null, scope = list(upper= ~(crab_year + adfg), lower = ~crab_year),
                       family = negative.binomial(9.169122), direction = "forward", trace = 9, r2.change = 0.01)
theta.ml(eag$cpue, eag_best$fitted.values)

# save diagnostic plots
ggsave("./AIGKC/figures/cpue_std/2024/may/eag_ft_diag.png",
       f_diagnostics(eag_best),
       width = 7, height = 6, units = "in")

f_dharma(eag_best, path = "./AIGKC/figures/cpue_std/2024/may/eag_ft_dharma.png")

# compare to a tweedie
gam(cpue ~ crab_year + adfg, family = tw(), data = eag) -> eag_tw
ggsave("./AIGKC/figures/cpue_std/2024/may/eag_tw_diag.png",
       f_diagnostics(eag_tw),
       width = 7, height = 6, units = "in")

f_dharma(eag_tw, path = "./AIGKC/figures/cpue_std/2024/may/eag_twt_dharma.png")

# extract index
eag_index <- f_getCPUE(eag_best, where = 2:14, years = 1985:1998)
# write output
write_csv(eag_index, "./AIGKC/output/cpue_std/eag_fish_tickets_85_98_std_index_may2024.csv")

# plot of index
eag %>%
  mutate(year = as.numeric(as.character(crab_year))) %>%
  group_by(year) %>%
  summarise(index = mean(cpue),
            se = sqrt(var(cpue) / n())) %>%
  mutate(index = index / (prod(index)^(1/n())),
            l95 = index - 1.96 * se,
            u95 = index + 1.96 * se) %>%
  mutate(type = "Nominal") %>%
  bind_rows(eag_index %>% mutate(type = "GLM (May)")) %>%
  bind_rows(read_csv("./AIGKC/output/cpue_std/eag_fish_tickets_85_98_std_index.csv") %>% mutate(type = "GLM (Jan)")) %>%
  # bind to siddeek data
  bind_rows(
    matrix(scan("./AIGKC/models/2024/jan/EAG/23.0/EAG_23_0.dat",
                skip = 289, nlines = length(290:303)), 
           nrow = length(290:303), ncol = 10, byrow = T) %>%
      as.data.frame %>%
      rename_all(~c("which", "year", "season", "fleet", "sex", 
                    "maturity", "index", "cv", "unit", "timing")) %>%
      transmute(year, index,
                type = "Status Quo GLM")
  ) %>%
  
  ggplot()+
  #geom_ribbon(aes(x = year, ymin = l95, ymax = u95, fill = type), alpha = 0.3)+
  geom_line(aes(x = year, y = index, color = type))+
  scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
  labs(x = NULL, y = "Fish Ticket CPUE", fill = NULL, color = NULL)+
  theme(legend.position = c(1, 1),
        legend.justification = c(1, 1)) -> x

ggsave("./AIGKC/figures/cpue_std/2024/may/eag_ft_index.png",
       plot = x,
       width = 5, height = 3, units = "in")


# step plot
f_step_plot(eag_best, term_labs = c("Year", "+ Vessel"))+
  scale_x_discrete(breaks = yraxis$breaks, labels = yraxis$labels) -> x
ggsave("./AIGKC/figures/cpue_std/2024/may/eag_ft_step_plot.png",
       plot = x,
       width = 5, height = 6, units = "in")

# partial effects plot
vis <- visreg::visreg(eag_best, "adfg")
left_join(vis$fit, vis$res) %>%
  ggplot()+
  geom_jitter(aes(x = adfg, y = visregRes), size = 0.4, color = "grey70")+
  geom_point(aes(x = adfg, y = visregFit))+
  geom_errorbar(aes(x = adfg,  ymin = visregLwr, ymax = visregUpr), width = 0)+
  labs(x = "Vessel", y = "f(Vessel)")+
  theme(axis.text.x = element_blank()) -> plot_pe
# sample size plot
eag %>%
  count(crab_year, adfg) %>%
  ggplot()+
  geom_point(aes(x = adfg, y = crab_year, size = n), color = "grey70")+
  labs(x = "Vessel", y = "Year", size = "Sample Size")+
  scale_y_discrete(breaks = yraxis$breaks, labels = yraxis$labels)+
  theme(axis.text.x = element_blank()) -> plot_d
ggsave("./AIGKC/figures/cpue_std/2024/may/eag_ft_effects.png",
       plot = plot_pe / plot_d,
       width = 6, height = 5.5, units = "in")


# wag ----

ft_core %>%
  filter(fishery == "WAG") %>%
  transmute(crab_year = factor(crab_year),
            cfec_permit_holder_name,
            month = factor(month_landed),
            adfg = factor(adfg_number), 
            statarea = factor(stat_area),
            cpue) %>% 
  filter(complete.cases(.)) -> wag

# null model
wag_null <- glm(cpue ~ crab_year, family = negative.binomial(4.136), data = wag)
theta.ml(wag$cpue, wag_null$fitted.values)

# full model (a) includes year, captain, month, vessel, areagp
wag_best_aic <- stepAIC(wag_null, scope = list(upper= ~(crab_year + cfec_permit_holder_name + month + adfg + statarea), lower = ~crab_year),
                        direction = "forward", trace = 9, k = log(nrow(wag)) + 1.0)
theta.ml(wag$cpue, wag_best_aic$fitted.values)
wag_best <- f_stepCPUE(wag_null, scope = list(upper= ~(crab_year + adfg), lower = ~crab_year),
                       family = negative.binomial(5.518119), direction = "forward", trace = 9, r2.change = 0.01)
theta.ml(wag$cpue, wag_best$fitted.values)

# save diagnostic plots
ggsave("./AIGKC/figures/cpue_std/2024/may/wag_ft_diag.png",
       f_diagnostics(wag_best),
       width = 7, height = 6, units = "in")
f_dharma(wag_best, path = "./AIGKC/figures/cpue_std/2024/may/wag_ft_dharma.png")

# compare to a tweedie
gam(cpue ~ crab_year + adfg, family = tw(), data = wag) -> wag_tw
ggsave("./AIGKC/figures/cpue_std/2024/may/wag_tw_diag.png",
       f_diagnostics(wag_tw),
       width = 7, height = 6, units = "in")
f_dharma(wag_tw, path = "./AIGKC/figures/cpue_std/2024/may/wag_tw_dharma.png")

# extract index
wag_index <- f_getCPUE(wag_best, where = 2:14, years = 1985:1998)

# write output
write_csv(wag_index, "./AIGKC/output/cpue_std/wag_fish_tickets_85_98_std_index_may2024.csv")
f_getCPUE_gam(wag_tw, where = 2:14, years = 1985:1998)

# plot of index
wag %>%
  mutate(year = as.numeric(as.character(crab_year))) %>%
  group_by(year) %>%
  summarise(index = mean(cpue),
            se = sqrt(var(cpue) / n())) %>%
  mutate(index = index / (prod(index)^(1/n())),
         l95 = index - 1.96 * se,
         u95 = index + 1.96 * se) %>%
  mutate(type = "Nominal") %>%
  bind_rows(wag_index %>% mutate(type = "GLM (May)")) %>%
  bind_rows(read_csv("./AIGKC/output/cpue_std/wag_fish_tickets_85_98_std_index.csv") %>% mutate(type = "GLM (Jan)")) %>%
  # bind to siddeek data
  bind_rows(
  matrix(scan("./AIGKC/models/2024/jan/WAG/23.0/WAG_23_0.dat",
              skip = 285, nlines = length(286:299)), 
         nrow = length(286:299), ncol = 10, byrow = T) %>%
  as.data.frame %>%
  rename_all(~c("which", "year", "season", "fleet", "sex", 
                "maturity", "index", "cv", "unit", "timing")) %>%
  transmute(year, index,
            type = "Status Quo GLM")
  ) %>%
  
  ggplot()+
  #geom_ribbon(aes(x = year, ymin = l95, ymax = u95, fill = type), alpha = 0.3)+
  geom_line(aes(x = year, y = index, color = type))+
  scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
  labs(x = NULL, y = "Fish Ticket CPUE", fill = NULL, color = NULL)+
  theme(legend.position = c(1, 1),
        legend.justification = c(1, 1)) -> x

ggsave("./AIGKC/figures/cpue_std/2024/may/wag_ft_index.png",
       plot = x,
       width = 5, height = 3, units = "in")

# step plot
f_step_plot(wag_best, term_labs = c("Year", "+ Vessel"))+
  scale_x_discrete(breaks = yraxis$breaks, labels = yraxis$labels) -> x
ggsave("./AIGKC/figures/cpue_std/2024/may/wag_ft_step_plot.png",
       plot = x,
       width = 5, height = 6, units = "in")


# partial effects plot
vis <- visreg::visreg(wag_best, "adfg")
left_join(vis$fit, vis$res) %>%
  ggplot()+
  geom_jitter(aes(x = adfg, y = visregRes), size = 0.4, color = "grey70")+
  geom_point(aes(x = adfg, y = visregFit))+
  geom_errorbar(aes(x = adfg,  ymin = visregLwr, ymax = visregUpr), width = 0)+
  labs(x = "Vessel", y = "f(Vessel)")+
  theme(axis.text.x = element_blank()) -> plot_pe
# sample size plot
wag %>%
  count(crab_year, adfg) %>%
  ggplot()+
  geom_point(aes(x = adfg, y = crab_year, size = n), color = "grey70")+
  labs(x = "Vessel", y = "Year", size = "Sample Size")+
  scale_y_discrete(breaks = yraxis$breaks, labels = yraxis$labels)+
  theme(axis.text.x = element_blank()) -> plot_d
ggsave("./AIGKC/figures/cpue_std/2024/may/wag_ft_effects.png",
       plot = plot_pe / plot_d,
       width = 6, height = 5.5, units = "in")

