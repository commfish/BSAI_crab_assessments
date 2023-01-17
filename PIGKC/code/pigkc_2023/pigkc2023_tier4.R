#  notes ----
## 2023 pigkc assessment tier 4 RE model
## tyler jackson
## 11/4/2022

# load ----

# libraries
library(rema)
library(tidyverse)
library(patchwork)

# graphic options
theme_sleek <- function(base_size = 12, base_family = "Times") {
  
  windowsFonts(Times=windowsFont("TT Times New Roman"))
  
  half_line <- base_size/2
  
  theme_light(base_size = base_size, base_family = base_family) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.ticks.length = unit(half_line / 2.2, "pt"),
      strip.background = element_rect(fill = NA, colour = NA),
      strip.text.x = element_text(colour = "black"),
      strip.text.y = element_text(colour = "black"),
      #axis.text = element_text(colour = "grey30"),
      #axis.title = element_text(colour = "grey30"),
      #legend.title = element_text(colour = "grey30"),#, size = rel(0.9)
      panel.border = element_rect(fill = NA),#, colour = "grey70", size = 1),
      legend.key.size = unit(0.9, "lines"),
      #legend.text = element_text(size = rel(0.7)),#, colour = "grey30"),
      legend.key = element_rect(colour = NA, fill = NA),
      legend.background = element_rect(colour = NA, fill = NA)#,
      #plot.title = element_text(colour = "grey30"),#, size = rel(1)
      #plot.subtitle = element_text(colour = "grey30")#, size = rel(.85)
    )
  
}

# Depends on dplyr
tickr <- function(
    data, # dataframe
    var, # column of interest
    to # break point definition
) {
  
  VAR <- enquo(var) # makes VAR a dynamic variable
  
  data %>%
    dplyr::filter(!is.na(!!VAR)) %>%
    distinct(!!VAR) %>%
    mutate(labels = ifelse(!!VAR %in% seq(to * round(min(!!VAR) / to), max(!!VAR), to),
                           !!VAR, "")) %>%
    dplyr::select(breaks = UQ(VAR), labels)
}
theme_set(theme_sleek())
yraxis <- tickr(tibble(yr = 1980:2100), yr, 5)

# custom functions ----

f_plot_rema_fit <- function(fits, confint = T) {
  
  colors = c("#999999", "#E69F00", "#56B4E9", "#009E73", 
             "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  
  # extract models
  nmod = length(fits)
  purrr::map(fits, function(x) {
    tidy_rema(x)$biomass_by_strata %>%
      mutate(model = x$model_name)
  }) -> reps
  
  if(confint == T) {
    do.call("rbind", reps) %>%
    ggplot()+
    geom_ribbon(aes(x = year, ymin = pred_lci, ymax = pred_uci, fill = model_name), alpha = 0.3)+
    geom_line(aes(x = year, y = pred, color = model_name))+
    geom_errorbar(aes(x = year, ymin = obs_lci, ymax = obs_uci), width = 0.2)+
    geom_point(aes(x = year, y = obs), shape = 21, fill = "white")+
    scale_y_continuous(breaks = seq(0,10000,200), labels = scales::comma)+
    scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
    scale_fill_manual(values = colors[1:nmod])+
    scale_color_manual(values = colors[1:nmod])+
    labs(x = NULL, y = "MMB (t)", fill = NULL, color = NULL)+
    coord_cartesian(ylim = c(NA, 1600), xlim = c(2002, 2023))+
    theme(legend.position = c(0,1),
          legend.justification = c(0,1)) -> p
  }
  if(confint == F) {
    do.call("rbind", reps) %>%
      ggplot()+
      geom_line(aes(x = year, y = pred, color = model_name))+
      geom_errorbar(aes(x = year, ymin = obs_lci, ymax = obs_uci), width = 0.2)+
      geom_point(aes(x = year, y = obs), shape = 21, fill = "white")+
      scale_y_continuous(breaks = seq(0,10000,200), labels = scales::comma)+
      scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
      scale_fill_manual(values = colors[1:nmod])+
      scale_color_manual(values = colors[1:nmod])+
      labs(x = NULL, y = "MMB (t)", color = NULL)+
      coord_cartesian(ylim = c(NA, 1600), xlim = c(2002, 2023))+
      theme(legend.position = c(0,1),
            legend.justification = c(0,1)) -> p
  }
  
  return(p)
  
}

f_specs <- function(fits, M = 0.18, alpha = 0.1) {
  
  # extract models
  nmod = length(fits)
  purrr::map(fits, function(x) {
    tidy_rema(x)$biomass_by_strata %>%
      mutate(model = x$model_name)
  }) %>% 
    do.call("rbind", .) %>%
    filter(year <= 2016) %>%
    group_by(model) %>%
    summarise(bmsy = mean(pred),
              mmb = pred[year == 2016],
              mmb_proj = mmb * exp(-0.625 * M),
              b_bmsy = mmb_proj / bmsy,
              f_ofl = case_when(b_bmsy >= 1 ~ M,
                                b_bmsy < 1 & b_bmsy > 0.25 ~ (M * (b_bmsy - alpha)) / (1 - alpha),
                                b_bmsy <= 0.25 ~ 0),
              OFL = (1 - exp(-f_ofl)) * mmb_proj)
  
}

# data ----

## slope survey mmb timeseries
survey <- read_csv("../../../PIGKC/output/survey_timeseries_surveyarea_pi_district.csv")
## survey estimate table
### total biomass 
filter(survey, group == "total") %>%
  transmute(survey_year, total = biomass) %>%
  ### mmb
  left_join(filter(survey, group == "male_mature") %>%
              transmute(survey_year, mmb = biomass, cv = cv_biomass)) %>%
  ### ratio
  mutate(ratio = ifelse(survey_year %in% 2008:2016, mmb / total, NA)) -> survey_est_tab


# rema base; 23.0 ----

## biomass input for rema
biomass <- data.frame(strata = "PI District",
                      year = survey_est_tab$survey_year,
                      biomass = survey_est_tab$mmb / 1000,
                      cv = survey_est_tab$cv)

## fit base rema model
prepare_rema_input(model_name = "23.0",
                   start_year = 2002,
                   end_year = 2023,
                   biomass_dat = biomass) %>%
  fit_rema(do.check = T) -> pigkc_23.0
### extract report
rep_23.0 <- tidy_rema(pigkc_23.0)
### plot fit
# f_plot_rema_fit(list(pigkc_23.0))
# ggsave("./PIGKC/figures/2023/pigkc_23.0_fit.png", height = 3, width = 5, units = "in")

# rema base w/ high 2002 CV; 23.0a ----
## fit base rema model w/ arbitrarily high CV (CPT commented 2002 was highly influential)
biomass_tmp <- data.frame(strata = "PI District",
                      year = survey_est_tab$survey_year,
                      biomass = survey_est_tab$mmb / 1000,
                      cv = c(0.4, survey_est_tab$cv[2:6]))
## PE goes to 0 when CV gets to ~ 0.5

## fit base rema model
prepare_rema_input(model_name = "23.0a",
                   start_year = 2002,
                   end_year = 2023,
                   biomass_dat = biomass_tmp) %>%
  fit_rema(MakeADFun.silent = T) -> pigkc_23.0a
### extract report
rep_23.0a <- tidy_rema(pigkc_23.0a)
### plot fit
# f_plot_rema_fit(list(pigkc_23.0a))
# ggsave("./PIGKC/figures/2023/pigkc_23.0a_fit.png", height = 3, width = 5, units = "in")
# f_plot_rema_fit(list(pigkc_23.0, pigkc_23.0a))
# ggsave("./PIGKC/figures/2023/pigkc_23.0_23.0a_compare.png", height = 3, width = 5, units = "in")



# rema base, 2008-2016; 23.1 ----

## biomass input for rema
biomass <- data.frame(strata = "PI District",
                      year = survey_est_tab$survey_year,
                      biomass = survey_est_tab$mmb / 1000,
                      cv = survey_est_tab$cv)

## fit base rema model
prepare_rema_input(model_name = "23.1",
                   start_year = 2008,
                   end_year = 2016,
                   biomass_dat = biomass) %>%
  fit_rema() -> pigkc_23.1
check_convergence(pigkc_23.1, ret = T)
### extract report
rep_23.0 <- tidy_rema(pigkc_23.1)
### plot fit
# f_plot_rema_fit(list(pigkc_23.1))
# ggsave("./PIGKC/figures/2023/pigkc_23.1_fit.png", height = 3, width = 5, units = "in")

# 23.1 with squared penalty; 23.1a ----

## fit 2008 - 2016 with square penalty 
biomass <- data.frame(strata = "PI District",
                      year = survey_est_tab$survey_year,
                      biomass = survey_est_tab$mmb / 1000,
                      cv = survey_est_tab$cv)

## fit rema model
prepare_rema_input(model_name = "23.1a",
                   start_year = 2008,
                   end_year = 2023,
                   biomass_dat = biomass,
                   PE_options = list(penalty_options = "squared_penalty",
                                     penalty_values = 1.5)) %>%
  fit_rema() -> pigkc_23.1a
### extract report
rep_23.1a <- tidy_rema(pigkc_23.1a)
### plot fit
# f_plot_rema_fit(list(pigkc_23.1a))
# ggsave("./PIGKC/figures/2023/pigkc_23.0a_fit.png", height = 3, width = 5, units = "in")

# 23.1 with prior; 23.1b ----

## fit 2008 - 2016 with prior
biomass <- data.frame(strata = "PI District",
                      year = survey_est_tab$survey_year,
                      biomass = survey_est_tab$mmb / 1000,
                      cv = survey_est_tab$cv)

## fit rema model
prepare_rema_input(model_name = "23.1b",
                   start_year = 2008,
                   end_year = 2023,
                   biomass_dat = biomass,
                   PE_options = list(penalty_options = "normal_prior",
                                     penalty_values = c(-2.3, 1))) %>%
  fit_rema() -> pigkc_23.1b
### extract report
rep_23.1b <- tidy_rema(pigkc_23.1b)
### plot fit
f_plot_rema_fit(list(pigkc_23.1b))

## prior sensitivity 

## plot prior distributions
ggplot(data = data.frame(x = c(-8,3)), aes(x)) +
  stat_function(fun = dnorm, 
                args = list(mean = -2.3, sd = 1), 
                geom = "area", alpha = 0.2, aes(fill = "\U00B5 = -2.3, \U03C3 = 1"), color = 1)+
  stat_function(fun = dnorm, 
                args = list(mean = -1.3, sd = 1), 
                geom = "area", alpha = 0.2, aes(fill = "\U00B5 = -1.3, \U03C3 = 1"), color = 1)+ 
  stat_function(fun = dnorm, 
                args = list(mean = -3.3, sd = 1), 
                geom = "area", alpha = 0.2, aes(fill = "\U00B5 = -3.3, \U03C3 = 1"), color = 1)+
  labs(x = NULL, y = "Probability Density", fill = NULL) +
  scale_fill_manual(values = c("#999999", "#E69F00", "#56B4E9"))+
  theme(legend.position = c(1, 1), legend.justification = c(1, 1)) -> prior_pdf
ggsave("../../../PIGKC/figures/2023/prior_pdf.png", plot = prior_pdf, height = 2.5, width = 5, units = "in")

# evaluate effect of different priors
prior = c(-1.3, -2.3, -3.3)
fits_23.1b <- list()
for(i in 1:3){
  prepare_rema_input(model_name = paste0("23.1b, ","\U00B5 = ", prior[i]),
                     start_year = 2008,
                     end_year = 2023,
                     biomass_dat = biomass,
                     PE_options = list(penalty_options = "normal_prior",
                                       penalty_values = c(prior[i], 1))) %>%
    fit_rema() -> fits_23.1b[[i]]
}
lapply(fits_23.1b, function(x) {
  tibble(est = x$sdrep$par.fixed,
         se = as.numeric(sqrt(x$sdrep$cov.fixed)))
}) %>% do.call("rbind", .) %>%
  mutate(mu = c(-1.3, -2.3, -3.3)) %>%
  dplyr::select(mu, est, se) -> prior_eval_tab

# compare fits ----

## plot fits with confidence band
ou_fit_plot <- f_plot_rema_fit(list(pigkc_23.0, pigkc_23.0a)) / f_plot_rema_fit(list(pigkc_23.1, pigkc_23.1a, pigkc_23.1b))
## all fits on one plot without confidence band
model_comp_plot <- f_plot_rema_fit(list(pigkc_23.0, pigkc_23.0a, pigkc_23.1, pigkc_23.1a, pigkc_23.1b), confint = F)

# model comparison table
fits = list(pigkc_23.0, pigkc_23.0a, pigkc_23.1, pigkc_23.1a, pigkc_23.1b)
tibble(nll = purrr::map_dbl(fits, function(x) {x$opt$objective}),
       sigma_pe = purrr::map(fits, function(x) {tidy_rema(x)$parameter_estimates}) ) %>%
  unnest(sigma_pe) %>%
  transmute(model = model_name, nll = nll, sigma_pe = estimate, se = std_err) -> result_tab



# compute specs ----

## management specs w/ M = 0.18
spec_tab_0.18 <- f_specs(list(pigkc_23.0, pigkc_23.0a, pigkc_23.1, pigkc_23.1a, pigkc_23.1b), M = 0.18)
## management specs w/ M = 0.21
spec_tab_0.21 <- f_specs(list(pigkc_23.0, pigkc_23.0a, pigkc_23.1, pigkc_23.1a, pigkc_23.1b), M = 0.21)

exp(-11)
