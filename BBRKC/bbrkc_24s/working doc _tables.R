# gmacs_get_lik_type_pen() ----

## output likelihood table by type plus some penalties needed for bbrkc

## args:
### all_out - output from gmacs_read_allout as nested list, example: all.out = list(mod_23.0a, mod_23.1b)
### file - file paths to Gmacsall.out for each model to compare, passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided
### model_name - character string passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided

gmacs_get_lik_type_pen <- function(all_out = NULL, file = NULL, model_name = NULL){
  
  # bring in all out data ----
  
  if(!is.null(file) && is.null(all_out)) {
    if(is.null(model_name)) {stop("Must supply model name(s)!!")}
    # read all out file
    all_out <- purrr::map2(file, model_name, gmacs_read_allout); names(all_out) <- paste0("model", model_name)
  }
  
  # extract catch data ----
  
  tibble(mod = names(all_out),
         all_out = all_out) %>% 
    mutate(data = purrr::map(all_out, function(x) {
      
      x$likelihoods_by_type %>%
        transmute(model = x$model_name, process, net_lik) %>%
        # add M deviation penalty
        add_row(model = x$model_name, process = "Mdevs", net_lik = x$penalties %>% filter(penalty == "Mdevs") %>% pull(net_lik)) %>%
        # add sex ratio penalty
        add_row(model = x$model_name, process = "Sex_ratio", net_lik = x$penalties %>% filter(penalty == "Sex_ratio") %>% pull(net_lik))
      
      
    })) %>% transmute(data) %>% unnest(data) %>%
    pivot_wider(names_from = model, values_from = net_lik) -> out
  
  return(out)
  
}


# Jie's old code 
source("./BBRKC/code/bbrkc_functions.R")

A <- read_rep("./BBRKC/bbrkc_24s/ADJ_model_211b_ph7/gmacs.rep")


## Table of abundance per model ------
# model 21.1b.p7
model <- "m211b_p7"
W <- m211b_p7 ### CHANGE HERE
Y <- m211b_p7_std ### change here 
# -- males - mature legal, females mature does NOT include projectino year!
temp <- W$n_matrix
temp %>% 
  select(year, size, males, females) %>% 
  mutate(size = as.numeric(size)) %>% 
  #filter(size >= 119) %>% 
  group_by(year) %>%  #  year
  summarise(mat_males = sum(males[size >= 119])/1000000, # mature males >119 mm
            leg_males = sum(males[size >= 134])/1000000, #legal males > 134 mm
            mat_fem = sum(females[size >= 90])/1000000) -> fmales1 #mature_females >90 mm
# -- MMB and sd and recruits - also does NOT include projection year!
temp2 <- W$derived_quant_summary
temp2 %>% 
  select(year, ssb, sd_log_ssb, recruit_male, recruit_female) %>% 
  group_by(year) %>% 
  mutate(recruits = sum(recruit_male, recruit_female)/1000000, # need to be moved down a year 
         MMB = ssb/1000, # mmb
         sd_mmb = MMB*(exp(sd_log_ssb^2)-1)^0.5) %>% # sd mmb
  select(year, MMB, sd_mmb) -> derived_m
# recruits needs to be moved down one year 
temp2 %>% 
  select(year, recruit_male, recruit_female) %>% 
  group_by(year) %>% 
  mutate(recruits = sum(recruit_male, recruit_female)/1000000) %>% 
  mutate(year = year +1) %>% 
  select(year, recruits) -> recruits

# survey obs and predicted - does include prj year
temp3 <- W$index_fit_summary
temp3 %>% 
  filter(fleet == "NMFS_Trawl") %>% 
  group_by(year) %>% 
  summarise(total_obs = round(sum(obs_index)/1000, 2), # total area swept
            total_pred = round(sum(pred_index)/1000, 2)) -> survey_est # total model est survey

# put them in the correct order for tables in doc -
#abun_tab <- cbind(fmales1[ ,1:3], derived_m[ ,2:3], recruit_tab, survey_est[ ,c(3,2)])
survey_est %>% #[1:47, ] %>% 
  merge(derived_m, all = T) %>% 
  merge(fmales1, all = T) %>% 
  merge(recruits, all = T) %>% 
  filter(year <= 2022) -> abun_tab2
# doesn't have 2023 values for anything but the survey 
mat_fem1 <- as.data.frame(A$N_females[ , 6:16]) # need only mature females 
mat_fem1 %>% 
  rowwise() %>% 
  mutate(mat_fem = sum(V6+V7+V8+V9+V10+V11+V12+V13+V14+V15+V16)/1000000) -> mat_fem2
# values match so really just need 2023 value saved here 
#mat_fem2[49,12] # 2023 projected mature females
mat_mal <- as.data.frame(A$N_males[ , 12:20]) # need only mature males 
mat_mal %>% 
  rowwise() %>% 
  mutate(mat_males = sum(V12+V13+V14+V15+V16+V17+V18+V19+V20)/1000000, 
         leg_males = sum(V15+V16+V17+V18+V19+V20)/1000000) -> mat_mal2
# values match so really just need 2023 value saved here 
#mat_mal2[49,10:11] # 2023 projected mature females
#mmb_proj = W$bmsy * W$b_bmsy
Y %>% filter(par == "sd_last_ssb") %>% 
  mutate(MMB = est/1000, 
         sd_mmb = se/1000) %>% 
  select(MMB, sd_mmb) -> ref_prj

# 2023 vector 
vec1 <- cbind(year = 2023, survey_est[48, 2:3] , ref_prj, mat_mal2[49,10:11], 
              mat_fem2[49,12], recruits[48,2])

abun_tab2 %>% 
  merge(vec1, all = T) %>% 
  select(year, mat_males, leg_males, MMB, sd_mmb, mat_fem, recruits, total_pred, total_obs) -> abun_tab3

write.csv(abun_tab3, paste0(.TABS, "_", model, "gmacs_sum_abun.csv"), row.names = FALSE)

# gmacs_plot_index() ----

## plot fits to gmacs index data

## args:
### all_out - output from gmacs_read_allout as nested list, example: all.out = list(mod_23.0a, mod_23.1b)
### save_plot - T/F save plots, default = T
### plot_dir - file directory in which to save plots
### y_labs - optional, custom y axis labels, as character vector
### data_summary - alternate way to bring in data, output of gmacs_get_index_summary()
### file - file paths to Gmacsall.out for each model to compare, passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided
### model_name - character string passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided

## example: gmacs_plot_index(all_out = list(mod_23.1b), plot_dir = "./put/file/here")

gmacs_plot_index <- function(all_out = NULL, save_plot = T, plot_dir = NULL, y_labs = NULL, 
                             data_summary = NULL, file = NULL, model_name = NULL) {
  # get summary data
  if(is.null(data_summary)){data_summary <- gmacs_get_index_summary(all_out, file, model_name)}
  
  # plots 
  # create output directories
  if(save_plot == T & is.null(plot_dir)) {plot_dir <- file.path(getwd(), "plots"); dir.create(plot_dir, showWarnings = F, recursive = TRUE)}
  if(!is.null(plot_dir) && !file.exists(plot_dir)) {dir.create(plot_dir, showWarnings = F, recursive = TRUE)}
  data_summary %>%
    nest_by(series, sex, units, .keep = T) %>% ungroup %>%
    mutate(plot = purrr::map(data, function(data) {
      # y label
      if(is.null(y_labs)) {
        y_labs <- paste0(gsub("_", " ", unique(data$fleet)), " Index (", unique(data$wt_units), ")")
        if(unique(data$units) == "Numbers") {
          if(is.na(unique(data$n_units)) == FALSE) {
            y_labs <- paste0(gsub("_", " ", unique(data$fleet)), " Index (", unique(data$n_units), ")")}
          if(is.na(unique(data$n_units)) == TRUE) {
            y_labs <- paste0(gsub("_", " ", unique(data$fleet)), " Index") 
          }
        }
      }
      # plot
      data %>%
        mutate(obs_l95 = obs_index * exp(-1.96 * sqrt(log(1 + obs_cv^2))),
               obs_u95 = obs_index * exp(1.96 * sqrt(log(1 + obs_cv^2))),
               tot_l95 = obs_index * exp(-1.96 * sqrt(log(1 + tot_cv^2))),
               tot_u95 = obs_index * exp(1.96 * sqrt(log(1 + tot_cv^2)))) %>%
        ggplot()+
        geom_errorbar(aes(x = year, ymin = tot_l95, ymax = tot_u95), width = 0, color = "grey70")+
        geom_errorbar(aes(x = year, ymin = obs_l95, ymax = obs_u95), width = 0, color = "grey20")+
        geom_point(aes(x = year, y = obs_index), color = "grey20")+
        geom_line(aes(x = year, y = pred_index, group = model, color = model))+
        labs(x = NULL, color = NULL, y = y_labs)+
        scale_y_continuous(labels = scales::comma)+
        scale_color_manual(values = cbpalette)+
        coord_cartesian(ylim = c(0, NA)) -> p
      #if(length(min(data$year):max(data$year)) > 10) { p + scale_x_discrete(labels = yraxis$labels, breaks = yraxis$breaks) -> p }
      if(save_plot == T) {
        pwidth <- min(max(length(min(data$year):max(data$year))*0.2, 5), 7)
        # save plot
        ggsave(plot = p, 
               filename = file.path(plot_dir, paste0("index_fit_", tolower(unique(data$fleet)), "_",
                                                     tolower(unique(data$sex)), ".png")), 
               width = pwidth, 
               height = pwidth * (3/5), units = "in")
      }
      return(p)
    })) -> plots
  # return ----
  if(save_plot == T) {
    # save plot of all stacked
    ggsave(plot = patchwork::wrap_plots(plots$plot, ncol = 2), 
           filename = file.path(plot_dir, "index_fit.png"),
           height = nrow(plots) / 2 * 4, width = 11, units = "in") 
    return("done")
  } else {return(plots$plot)}
  
}




# Selectivity fix for females ----------------------------------
gmacs_get_slx(all_out = base_models) %>%
  mutate(capture_block = case_when(fleet %in% c("BSFRF", "Bairdi_Fishery_Bycatch", "Fixed_Gear") ~ "1975 - 2023",
                                   fleet == "NMFS_Trawl" & year %in% 1975:1981 ~ "1975 - 1981",
                                   fleet == "NMFS_Trawl" & year %in% 1981:2023 ~ "1981 - 2023",
                                   fleet == "Pot_Fishery" ~ "1975 - 2022",
                                   fleet == "Trawl_Bycatch" ~ "1975 - 2022")) %>%
  gmacs_plot_slx(data_summary = ., save_plot = T, )



# NEED TO TRY THOSE BELOW ----------------
# From Caitlin for MMB with projected year ----------------------------------------
# modified version of gmacs_get_ref_points function that brings in SE on SSB
gmacs_get_ref_points <- function(all_out = NULL, file = NULL, model_name = NULL){
  # bring in all out data ----
  if(!is.null(file) && is.null(all_out)) {
    if(is.null(model_name)) {stop("Must supply model name(s)!!")}
    # read all out file
    all_out <- purrr::map2(file, model_name, gmacs_read_allout); names(all_out) <- paste0("model", model_name)
  }
  
  # extract catch data ----
  tibble(mod = names(all_out),
         all_out = all_out) %>%
    mutate(data = purrr::map(all_out, function(x) {
      x$reference_points %>%
        transmute(parameter_name, estimate, se) %>%
        pivot_wider(names_from = parameter_name, values_from = c(estimate, se)) %>%
        transmute(model = as.character(x$model_name),
                  mmb = estimate_BMSY * `estimate_Bcurr/BMSY`,
                  b35 = estimate_BMSY,
                  b_b35 = `estimate_Bcurr/BMSY`,
                  male_rbar = `estimate_Male_spr_rbar`,
                  rbar_yrs = paste(rep24.0$spr_syr, "-", rep24.0$spr_nyr),
                  f35 = `estimate_Fmsy_1`,
                  fofl = rep24.0$spr_fofl * f35,
                  ofl_tot = estimate_OFL_tot,
                  b35_se = se_BMSY,
                  b_b35_se = `se_Bcurr/BMSY`,
                  mmb_se = (`estimate_Bcurr/BMSY` * se_BMSY) + (estimate_BMSY * `se_Bcurr/BMSY`))
    })) %>% transmute(data) %>% unnest(data) -> out
  return(out)
}



# projections for models 16.0, 16.0a, 16.0b, and 24.0
stds.16.24 <- bind_rows(gmacs_read_std("./SMBKC/smbkc_24s/model_16_0/gmacs.std", sub_text = "sd_log_ssb", model_name = "16.0") %>% mutate(year = 1978:2022),
                        gmacs_read_std("./SMBKC/smbkc_24s/model_16_0_a/gmacs.std", sub_text = "sd_log_ssb", model_name = "16.0a") %>% mutate(year = 1978:2022),
                        gmacs_read_std("./SMBKC/smbkc_24s/model_16_0_b/gmacs.std", sub_text = "sd_log_ssb", model_name = "16.0b") %>% mutate(year = 1978:2022),
                        gmacs_read_std("./SMBKC/smbkc_24s/model_24_0/gmacs.std", sub_text = "sd_log_ssb", model_name = "24.0") %>% mutate(year = 1978:2022)) %>%
  mutate(ssb_se = se / (1 / exp(est)), 
         ssb_lci = exp(est) + ssb_se * qnorm(0.05 / 2), 
         ssb_uci = exp(est) + ssb_se * qnorm(1 - 0.05 / 2))

refs.16.24 <- gmacs_get_ref_points(all_out = list(rep16.0, rep16.0a, rep16.0b, rep24.0)) %>%
  rename(model_name = model) %>%
  mutate(ssb = mmb,
         ssb_se = mmb_se, 
         ssb_lci = mmb + ssb_se * qnorm(0.05 / 2), 
         ssb_uci = mmb + ssb_se * qnorm(1 - 0.05 / 2),
         year = 2023)

proj.plot.1 <- gmacs_get_derived_quantity_summary(all_out = list(rep16.0, rep16.0a, rep16.0b, rep24.0)) %>%
  rename(model_name = model) %>%
  left_join(stds.16.24) %>%
  bind_rows(refs.16.24)

plot1 <- proj.plot.1 %>%
  ggplot()+
  geom_ribbon(aes(x = as.numeric(as.factor(year)), ymin = ssb_lci, ymax = ssb_uci, fill = model_name), alpha = 0.2, show.legend = F)+
  geom_line(aes(x = factor(year), y = ssb, group = model_name, color = model_name))+
  geom_point(data = function(x) filter(x, year == max(year)),
             aes(x = factor(year), y = ssb, color = model_name))+
  scale_x_discrete(breaks = yraxis$breaks, labels = yraxis$labels)+
  scale_y_continuous(labels = scales::comma, limits = c(0, NA))+
  scale_color_manual(values = cbpalette)+
  scale_fill_manual(values = cbpalette)+
  labs(x = NULL, y = paste0("MMB (t)"), color = NULL)+
  theme(legend.position = c(1, 1),
        legend.justification = c(1, 1))

ggsave("./SMBKC/smbkc_24s/model_24_0/plots/mmb_trajectory_modelset1.png", plot1, height = 4.2, width = 7, units = "in")

# Tyler plots with projected SSB -------------------------------
gmacs_get_derived_quantity_summary(eag_ao) %>%
  left_join(gmacs_get_ref_points(eag_ao)) %>%
  left_join(gmacs_read_std("AIGKC/models/2024/may/EAG/23.1/gmacs.std", sub_text = "sd_log_ssb", model_name = "23.1") %>%
              transmute(model = "23.1", year = 1960:2023,
                        ssb_se = se / (1 / exp(est)), 
                        ssb_lci = exp(est) + ssb_se * qnorm(0.05 / 2), 
                        ssb_uci = exp(est) + ssb_se * qnorm(1 - 0.05 / 2)), by = c("model", "year")) %>%
  ggplot()+
  geom_ribbon(aes(x = as.numeric(as.factor(year)), ymin = ssb_lci, ymax = ssb_uci, fill = model), alpha = 0.2, show.legend = F)+
  geom_line(aes(x = factor(year), y = ssb, group = model, color = model))+
  geom_point(data = function(x) filter(x, year == max(year)),
             aes(x = factor(year), y = mmb, color = model))+
  scale_x_discrete(breaks = yraxis$breaks, labels = yraxis$labels)+
  scale_y_continuous(labels = scales::comma, limits = c(0, NA))+
  scale_color_manual(values = cbpalette)+
  scale_fill_manual(values = cbpalette)+
  labs(x = NULL, y = paste0("MMB (t)"), color = NULL)+
  theme(legend.position = c(1, 1),
        legend.justification = c(1, 1))


### plotting all F's on one plot from Caitlin -----------------------------

p1 <- gmacs_plot_f(all_out = list(rep24.0), save_plot = F, yrs = 1978:2022)[[1]]
p2 <- gmacs_plot_f(all_out = list(rep24.0), save_plot = F, yrs = 1978:2022)[[2]]
p3 <- gmacs_plot_f(all_out = list(rep24.0), save_plot = F, yrs = 1978:2022)[[3]]
p4 <- gmacs_plot_f(all_out = list(rep24.0), save_plot = F, yrs = 1978:2022)[[4]]
p5 <- gmacs_plot_f(all_out = list(rep24.0), save_plot = F, yrs = 1978:2022)[[5]]


f_comb <- plot_grid(
  p2, p4, p5,
  labels = "", ncol = 1
)

yrs <- 1978:2022
pwidth <- min(max(length(min(yrs):max(yrs))*0.2, 5), 7)
ggsave(file.path("./SMBKC/smbkc_24s/model_24_0/plots", "f_combined.png"), plot = f_comb, height = 0.6*pwidth, width = pwidth, units = "in")