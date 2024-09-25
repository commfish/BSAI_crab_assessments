## gmacs R functions
## updated to GMACS 2.20.16; Compiled 2024-09-11
## tyler jackson, caitlin stern
## last update - 9/21/2024

# load ----

if(!require(tidyverse, quietly = T)) {install.packages("tidyverse", dependencies = T); library(tidyverse)}
if(!require(ggpmisc, quietly = T)) {install.packages("ggpmisc", dependencies = T); library(ggpmisc)}
if(!require(janitor, quietly = T)) {install.packages("janitor", dependencies = T); library(janitor)}
if(!require(patchwork, quietly = T)) {install.packages("patchwork", dependencies = T); library(patchwork)}
if(!require(latex2exp, quietly = T)) {install.packages("latex2exp", dependencies = T); library(latex2exp)}

# plot options -----

# graphic options
theme_sleek <- function(base_size = 12) {
  
  half_line <- base_size/2
  
  theme_light(base_size = base_size) +
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
yraxis <- tickr(tibble(yr = 1900:2100), yr, 5)

# custom color scale
cbpalette <- colorRampPalette(colors = c("#009E73", "#0072B2","#E69F00" , "#56B4E9", "#D55E00", "#CC79A7","#F0E442", "black", "grey"))(9)
cbpalette <- c(cbpalette, "tomato3", "turquoise4", "orangered4", "pink", "green", "red")

# gmacs_read_allout() ----

## read GMACSall.out file

## args:
### file - file path to Gmacsall.out
### model_name - character string to save as object in output, later to be used for plot legends. example: "23.1b"

## output: list object
## example: gmacs_read_allout(file = "./AIGKC/models/2024/may/EAG/23.1b/Gmacsall.out", model_name = "23.1b")
## version: character string denoting gmacs version

gmacs_read_allout <- function(file, model_name = NULL, version = NULL) {
  
  if(is.null(version)){version = "2.20.16"}
  
  if(version == "2.01.M.10"){
  # setup ----
  # Suppress the NA message in the coercion to double
  options(warn = -1) 
  
  # read text file
  allout <- read.delim(file, sep = "", header = F, col.names = c(1:1000), fill = T, na.strings = "", colClasses = "character")
  # create out object
  out <- list()
  
  # version ----
  out$version <- str_flatten(allout[1,], collapse = " ", na.rm = T)
  # model name ----
  out$model_name <- model_name
  # stock ----
  out$stock <- gsub("#Stock being assessed: ", "", str_flatten(allout[2,], collapse = " ", na.rm = T))
  # general info ----
  ## years
  out$yr_range <- as.numeric(gsub(";", "", allout[grep("Year_range", allout[,1]), 2:3]))
  out$mod_yrs <- out$yr_range[1]:out$yr_range[2]
  last <- grep("Year_range", allout[,1]) # start saving last position in file
  ## number of seasons
  out$n_season <- as.numeric(str_extract(str_flatten(allout[last + 1,], na.rm = T), "([[:digit:]]+)")); last <- last + 1
  ## number of fleets
  out$n_fleets <- as.numeric(str_extract(str_flatten(allout[last + 1,], na.rm = T), "([[:digit:]]+)")); last <- last + 1
  ## fleet names
  out$fleet_names <- as.character(allout[last + 1, 2:(out$n_fleets + 1)]); last <- last + 1
  ## n sexes
  out$n_sex <- as.numeric(str_extract(str_flatten(allout[last + 1,], na.rm = T), "([[:digit:]]+)")); last <- last + 1
  ## n shell conidition
  out$n_shell <- as.numeric(str_extract(str_flatten(allout[last + 1,], na.rm = T), "([[:digit:]]+)")); last <- last + 1 
  ## n maturity states
  out$n_maturity <- as.numeric(str_extract(str_flatten(allout[last + 1,], na.rm = T), "([[:digit:]]+)")); last <- last + 1 
  ## units 
  out$wt_units <-  gsub("Weightunitis:", "", str_flatten(allout[last + 1,], na.rm = T)); last <- last + 1 
  out$n_units <- gsub("Numbersunitis:", "", str_flatten(allout[last + 1,], na.rm = T)); last <- last + 1
  case_when(out$n_units %in% c(1, "1", "1s", "one", "One", "ones", "Ones", "Numbers") ~ "1s",
            out$n_units %in% c(10, "10", "10s", "ten", "Ten", "Tens") ~ "10s",
            out$n_units %in% c(100, "100", "100s", "hundred", "Hundred", "Hundreds") ~ "100s",
            out$n_units %in% c(1000, "10-0", "1000s", "thousand", "Thousand", "Thousands", "thou", "Thou") ~ "1000s", 
            out$n_units %in% c(1000000, "10-0", "1000000s", "millions", "Million", "Millions", "mill", "Mill") ~ "1000000s") -> out$n_units

  # likelihoods by type ----
  
  # read lines individually
  catch = as.numeric(na.omit(as.numeric(allout[last + 2,]))); last <- last + 2 
  index = as.numeric(na.omit(as.numeric(allout[last + 1,]))); last <- last + 1 
  size = as.numeric(na.omit(as.numeric(allout[last + 1,]))); last <- last + 1 
  recruitment = as.numeric(na.omit(as.numeric(allout[last + 1,]))); last <- last + 1
  tagging =  as.numeric(na.omit(as.numeric(allout[last + 1,]))); last <- last + 1
  penalties =  as.numeric(na.omit(as.numeric(allout[last + 1,]))); last <- last + 1
  priors = as.numeric(na.omit(as.numeric(allout[last + 1,]))); last <- last + 1
  initial_size = as.numeric(na.omit(as.numeric(allout[last + 1,]))); last <- last + 1
  total = as.numeric(na.omit(as.numeric(allout[last + 1,]))); last <- last + 1
  
  # coerce to tibble
  rbind(catch, index, size, recruitment, tagging) %>% as_tibble() %>%
    transmute(process = c("catch", "index", "size", "recruitment", "tagging"), raw_lik = V1, net_lik = V2) %>%
    add_row(process = "penalites", raw_lik = penalties, net_lik = penalties) %>%
    add_row(process = "priors", raw_lik = priors, net_lik = priors) %>%
    add_row(process = "initial_size", raw_lik = initial_size, net_lik = initial_size) %>%
    add_row(process = "total", raw_lik = sum(.$raw_lik), net_lik = total) -> out$likelihoods_by_type
  
  # likelihoods by type and fleet ---- 
  ## catches
  tibble(raw_lik = as.numeric(na.omit(as.numeric(allout[last + 3,]))),
         emphasis = as.numeric(na.omit(as.numeric(allout[last + 4,]))),
         net_lik = as.numeric(na.omit(as.numeric(allout[last + 5,])))) %>%
    transmute(process = paste0("catch_", 1:nrow(.)), raw_lik, emphasis, net_lik) -> catch; last <- last + 5
  ## index
  tibble(raw_lik = as.numeric(na.omit(as.numeric(allout[last + 2,]))),
         emphasis = as.numeric(na.omit(as.numeric(allout[last + 3,]))),
         net_lik = as.numeric(na.omit(as.numeric(allout[last + 4,])))) %>%
    transmute(process = paste0("index_", 1:nrow(.)), raw_lik, emphasis, net_lik) -> index; last <- last + 4    
  ## size composition
  tibble(raw_lik = as.numeric(na.omit(as.numeric(allout[last + 2,]))),
         emphasis = as.numeric(na.omit(as.numeric(allout[last + 3,]))),
         net_lik = as.numeric(na.omit(as.numeric(allout[last + 4,])))) %>%
    transmute(process = paste0("size_comp_", 1:nrow(.)), raw_lik, emphasis, net_lik) -> size; last <- last + 4    
  ## recruitment penalties
  tibble(raw_lik = as.numeric(na.omit(as.numeric(allout[last + 2,]))),
         net_lik = as.numeric(na.omit(as.numeric(allout[last + 2,])))) %>%
    transmute(process = paste0("rec_pen_", 1:nrow(.)), raw_lik, net_lik) -> rec_pen; last <- last + 2
  ## tagging
  tibble(raw_lik = as.numeric(na.omit(as.numeric(allout[last + 2,]))),
         emphasis = as.numeric(na.omit(as.numeric(allout[last + 3,]))),
         net_lik = as.numeric(na.omit(as.numeric(allout[last + 4,])))) %>%
    transmute(process = paste0("tagging_", 1:nrow(.)), raw_lik, emphasis, net_lik) -> tagging; last <- last + 4   
  ## growth 
  tibble(raw_lik = as.numeric(na.omit(as.numeric(allout[last + 2,]))),
         net_lik = as.numeric(na.omit(as.numeric(allout[last + 2,])))) %>%
    transmute(process = paste0("growth_", 1:nrow(.)), raw_lik, net_lik) -> growth; last <- last + 2
  bind_rows(catch, index, size, rec_pen, tagging, growth) -> out$likelihoods_by_type_and_fleet
  
  
  
  # penalties ----
  
  tmp <- matrix(nrow = 12, ncol = 3)
  for(i in 1:12) {tmp[i, 1:3] <- as.numeric(na.omit(as.numeric(allout[last + 1 + i,])[-1]))}
  as_tibble(tmp) %>%
    mutate(penalty = c("Mean_Fbar = 0", "Mean_Fdev", "Mdevs", "Rec_ini", "Rec_dev", "Sex_ratio",
                       "Molt_prob", "Smooth_select", "Init_numbers", "Fdevs_(flt)", "Fdovs_(flt)",
                       "Seldevs")) %>%
    transmute(penalty, raw_lik = V1, emphasis = V2, net_lik = V3) -> tmp

  out$penalties <- tmp
  last <- last + 14
  
  # parameters ----
  
  ## par tibble
  tmp <- matrix(ncol = 11, nrow = length((last + 2):(grep("#---", allout[,1])[1]-1)))
  for(i in 1:nrow(tmp)) {
    if("*" %in% as.character(allout[last + 1 + i, 1:ncol(tmp)])) {
      as.character(allout[last + 1 + i, 1:13]) %>%
        .[!is.na(.)] %>% .[. != "*"] -> tmp[i,]
    } else{as.character(allout[last + 1 + i, 1:ncol(tmp)]) -> tmp[i,]}
  }
  as_tibble(tmp) %>%
    rename_all(~c("parameter_count", "parameter", "colon", "estimate", "phase", "lower_bound", "upper_bound", 
                  "penalty", "gradient", "standard_error", "est_count")) %>%
    #janitor::clean_names() %>%
    mutate_at(c(1, 4:11), as.numeric) %>% 
    dplyr::select(-colon) -> out$parameters; last <- grep("#---", allout[,1])[1]
  out$n_par <- out$parameters %>% filter(phase > 0) %>% nrow
  ## parameters at bounds
  out$parameters %>%
    mutate(range = upper_bound - lower_bound,
           status = ifelse(estimate < (lower_bound+range*0.01), 1,
                           ifelse(estimate > upper_bound-range*0.01, 1, 0))) %>%
    filter(status == 1) %>% dplyr::select(-range, -status) -> out$parameters_at_bounds
  
  # max gradient ----
  out$max_gradient <- max(abs(out$parameters$gradient), na.rm = T)
  # reference points ----
  
  ## ref tibble
  tmp <- matrix(ncol = 3, nrow = length((last + 2):(grep("#---", allout[,1])[2] - 2)))
  for(i in 1:nrow(tmp)) {
    tmp[i,] <- na.omit(as.numeric(allout[last + 1 + i,]))
  }
  as_tibble(tmp) %>%
    mutate(ref = c("Male_spr_rbar", "Female_spr_rbar", "SSSB/R_F=0", "BMSY", "Bcurr/BMSY", "OFL_tot",
                   paste0("Fmsy_", 1:out$n_fleets), paste0("Fofl_", 1:out$n_fleets), 
                   paste0("OFL_", 1:out$n_fleets))) %>%
    transmute(parameter_name = ref, estimate = V1, se = V2, est_quantity_count = V3) -> out$reference_points
  out$mmb_curr <- prod(out$reference_points$estimate[out$reference_points$parameter_name %in% c("BMSY", "Bcurr/BMSY")])
  out$ofl_tot <- out$reference_points$estimate[out$reference_points$parameter_name %in% c("OFL_tot")]
  out$bmsy <- out$reference_points$estimate[out$reference_points$parameter_name %in% c("BMSY")]
  out$b_bmsy <- out$reference_points$estimate[out$reference_points$parameter_name %in% c("Bcurr/BMSY")]
  out$f_msy_tot <- sum(out$reference_points$estimate[out$reference_points$parameter_name %in% paste0("Fmsy_", 1:out$n_fleets)])
  out$f_ofl_tot <- sum(out$reference_points$estimate[out$reference_points$parameter_name %in% paste0("Fofl_", 1:out$n_fleets)])
  out$rbar_tot <- sum(out$reference_points$estimate[out$reference_points$parameter_name %in% c("Male_spr_rbar", "Female_spr_rbar")])
  last <- grep("#---", allout[,1])[2] - 1
  ## ref sigma
  allout[last,] %>% dplyr::select_if(~ !any(is.na(.))) %>%
    mutate_all(., function(x){gsub(";", "", x)}) %>%
    .[1,] %>% as.numeric() %>%na.omit() %>% as.numeric() -> out$ref_sigmaR
  names(out$ref_sigmaR) <- c("sigmaR", "weight")
  last <- grep("#---", allout[,1])[2]
  
  # overall summary ----
  
  tmp <- matrix(ncol = 15 + (3*out$n_sex) + out$n_fleets, nrow = length(out$mod_yrs))
  for(i in 1:nrow(tmp)) {
    tmp[i,] <- as.numeric(allout[last+2+i,1:ncol(tmp)])
  }
  as_tibble(tmp) %>%
    rename_all(~make.unique(as.character(allout[last+2,1:ncol(tmp)]))) %>%
    janitor::clean_names(.) %>% rename(year = number_year, log_recruits_male = log_recruits, sd_log_recruits_male = sd_log_recruits) -> out$derived_quant_summary
  # do some renaming
  
  if(out$n_sex == 2) {
    out$derived_quant_summary %>%
      rename(log_recruits_female = log_recruits_1, sd_log_recruits_female = sd_log_recruits_1) -> out$derived_quant_summary
  }
  last <- grep("#---", allout[,1])[3]
  
  # mean wt ----
  
  ## add size bins
  out$size_bins <- as.numeric(na.omit(as.numeric(allout[last+2,])))
  ## number of bins
  out$n_size_bins <- length(out$size_bins)
  ## weight at size matrix
  tmp <- matrix(nrow = out$n_sex*out$n_maturity*length(out$mod_yrs), ncol = length(out$size_bins)+3)
  for(i in 1:nrow(tmp)) {
    tmp[i,] <- as.character(allout[last+3,1:ncol(tmp)])
  }
  as_tibble(tmp) %>%
    mutate_at(2:ncol(.), as.numeric) %>%
    rename_all(~c("sex", "maturity", "year", out$size_bins)) %>%
    pivot_longer(4:ncol(.), names_to = "size", values_to = "wt") %>%
    mutate(size = as.numeric(size)) -> out$wt_at_size
  last <- last + 3 + nrow(tmp)
  
  # maturity vector ----
  
  out$maturity_at_size_vector <- as.numeric(allout[last+1,1:length(out$size_bins)]); last <- grep("#---", allout[,1])[4]
  
  # catch fit summary ----
  
  ## catch summary
  tmp <- matrix(nrow = length((last+3):(grep("#---", allout[,1])[5]-3)), ncol = 14)
  for(i in 1:nrow(tmp)){
    tmp[i,] <- as.character(allout[last+2+i,1:14])
  }
  as_tibble(tmp) %>%
    mutate_at(c(1:2, 4, 6:7, 10:14), as.numeric) %>%
    rename_all(~c("series", "year", "fleet", "season", "sex", "obs_catch", "cv", "type", "units", "mult", "effort", "disc_m", 
                  "pred_catch", "residual")) -> out$catch_fit_summary; last <- grep("#---", allout[,1])[5]-3
  ## catch q
  tibble(series = unique(out$catch_fit_summary$series),
         log_q = as.numeric(allout[last+2,1:length(unique(out$catch_fit_summary$series))])) -> out$log_q_catch; last <- grep("#---", allout[,1])[5]
  
  # index fix_summary ----
  
  ## index summary
  tmp <- matrix(nrow = length((last+3):(grep("sdnr_MAR_cpue", allout[,1])-2)), ncol = 13)
  for(i in 1:nrow(tmp)){
    tmp[i,] <- as.character(allout[last+2+i,1:13])
  }
  as_tibble(tmp) %>%
    mutate_at(c(1:2, 4, 7:9, 11:13), as.numeric) %>%
    rename_all(~c("series", "year", "fleet", "season", "sex", "maturity", "obs_index", "obs_cv", 
                  "tot_cv", "units", "q", "timing", "pred_index")) -> out$index_fit_summary; last <- last + nrow(tmp) + 2
  ## sdnr_MAR_cpue
  tmp <- matrix(nrow = length(unique(out$index_fit_summary$series)), ncol = 2)
  for(i in 1:nrow(tmp)) {
    tmp[i,] <- as.numeric(allout[last+3, 1:2])
  }
  out$sdnr_MAR_cpue <- tmp; last <- grep("#---", allout[,1])[6]
  
  # size composition fit summary ----
  
  ## size composition fit summary
  ## get info first
  tmp <- matrix(ncol = 10, nrow = length((last+3):(grep("sdnr_MAR_lf", allout[,1])-2)))
  for(i in 1:nrow(tmp)) {
    tmp[i,] <- as.character(allout[last+2+i, 1:ncol(tmp)])
  }
  as_tibble(tmp) %>%
    rename_all(~c("org_series", "mod_series", "year", "fleet", "season", "sex", "type", "shell", "maturity", "nsamp_obs")) %>%
    mutate_at(c(1:3, 5, 10), as.numeric) -> tmp
  
  ## get comps
  last <- last + 2 # set last to start where the data is
  tmp %>%
    nest_by(mod_series, .keep = T) %>% ungroup() %>% 
    mutate(row = purrr::map_dbl(data, ~nrow(.)),
           row = lag(row),
           row = cumsum(ifelse(is.na(row), 0, row)) + last) %>% 
    mutate(comps = purrr::map2(data, row, function(data, row) {
      
      comp_tmp <- matrix(ncol = ncol(allout)-ncol(tmp), nrow = nrow(data))
      for(i in 1:nrow(comp_tmp)) {
        comp_tmp[i,] <- as.numeric(allout[row + i, 11:ncol(allout)])
      }
      as_tibble(comp_tmp) %>%
        dplyr::select(where(function(x)!all(is.na(x)))) -> comp_tmp
      if((ncol(comp_tmp)/out$n_size_bins) <= 2) {comp_agg <- F} else{comp_agg <- T}
      if(comp_agg == F){
        
        comp_tmp %>% 
          rename_all(~c(paste0("obs_", out$size_bins[1:(ncol(comp_tmp)/2)]), paste0("pred_", out$size_bins[1:(ncol(comp_tmp)/2)]))) %>%
          bind_cols(data, .) %>%
          pivot_longer((ncol(data)+1):ncol(.), names_to = "group", values_to = "prop") %>% 
          separate_wider_delim(group, "_", names_sep = "split") %>%
          pivot_wider(names_from = groupsplit1, values_from = prop) %>%
          rename(size = groupsplit2) %>%
          transmute(org_series, mod_series, year, fleet, season, sex, type, shell, maturity, size = as.numeric(size), 
                    nsamp_obs, obs = as.numeric(obs), pred = as.numeric(pred), residual = obs - pred) -> comp_out
      }
      if(comp_agg == T){
        nobs <- ncol(comp_tmp)/2
        group <- rep(1:50, each = out$n_size_bins)[1:nobs] # << probably a more elegant way to do this...
        comp_tmp %>% 
          rename_all(~c(paste0("obs_", group, "_", as.numeric(matrix(out$size_bins, ncol = nobs))), paste0("pred_", group, "_", as.numeric(matrix(out$size_bins, ncol = nobs))))) %>%
          bind_cols(data, .) %>%
          pivot_longer((ncol(data)+1):ncol(.), names_to = "group", values_to = "prop") %>% 
          separate_wider_delim(group, "_", names_sep = "split") %>%
          pivot_wider(names_from = groupsplit1, values_from = prop) %>%
          rename(size = groupsplit3, aggregate_series = groupsplit2) %>%
          transmute(org_series, mod_series, aggregate_series = as.numeric(aggregate_series), year, fleet, season, sex, type, shell, maturity, size = as.numeric(size),
                    nsamp_obs, obs = as.numeric(obs), pred = as.numeric(pred), residual = obs - pred) -> comp_out
      }
      
      return(comp_out)
      
    })) %>% transmute(comps) %>% unnest -> out$size_fit_summary
  
  last <- grep("sdnr_MAR_lf", allout[,1])
  ## sdnr_MAR_lf
  tmp <- matrix(ncol = 2, nrow = length(unique(out$size_fit_summary$mod_series)))
  for(i in 1:nrow(tmp)) {
    tmp[i,] <- as.numeric(allout[last+i, 1:2])
  }
  out$sdnr_MAR_lf <- tmp
  last <- grep("Francis_weights", allout[,1])
  ## francis weights
  out$francis_weights <- as.numeric(allout[last+1, 1:length(unique(out$size_fit_summary$mod_series))]); last <- grep("#---", allout[,1])[7]
  
  ## add stage two weights to fit summary
  out$size_fit_summary %>% 
    mutate(lambda = out$francis_weights[.$mod_series],
           nsamp_est = exp(out$parameters$estimate[grepl("Log_vn_comp", out$parameters$parameter)][.$mod_series]) * nsamp_obs * lambda) -> out$size_fit_summary
  
  # selectivity ----
  
  ## selex
  tmp <- matrix(ncol = 3 + length(out$size_bins), nrow = length(out$mod_yrs) * out$n_sex * out$n_fleets)
  for(i in 1:nrow(tmp)) {
    tmp[i,] <- as.character(allout[last+3+i, 1:ncol(tmp)])
  }
  as_tibble(tmp) %>%
    rename_all(~c("year", "sex", "fleet", out$size_bins)) %>%
    pivot_longer(4:ncol(.), values_to = "slx_capture", names_to = "size") %>%
    mutate_at(c(1, 4, 5), as.numeric) -> slx_cap; last <- last + 3 + nrow(tmp)
  ## retention
  tmp <- matrix(ncol = 3 + length(out$size_bins), nrow = length(out$mod_yrs) * out$n_sex * out$n_fleets)
  for(i in 1:nrow(tmp)) {
    tmp[i,] <- as.character(allout[last+2+i, 1:ncol(tmp)])
  }
  as_tibble(tmp) %>%
    rename_all(~c("year", "sex", "fleet", out$size_bins)) %>%
    pivot_longer(4:ncol(.), values_to = "slx_retention", names_to = "size") %>%
    mutate_at(c(1, 4, 5), as.numeric) -> slx_ret; last <- last + 2 + nrow(tmp)
  ## discard
  tmp <- matrix(ncol = 3 + length(out$size_bins), nrow = length(out$mod_yrs) * out$n_sex * out$n_fleets)
  for(i in 1:nrow(tmp)) {
    tmp[i,] <- as.character(allout[last+2+i, 1:ncol(tmp)])
  }
  as_tibble(tmp) %>%
    rename_all(~c("year", "sex", "fleet", out$size_bins)) %>%
    pivot_longer(4:ncol(.), values_to = "slx_discard", names_to = "size") %>%
    mutate_at(c(1, 4, 5), as.numeric) -> slx_disc; last <- grep("Select_control", allout[,2])
  ## slx control
  as_tibble(allout[(last+2):(grep("#----", allout[,1])[8]-1), 1:11]) %>%
    mutate_all(as.numeric) %>%
    rename_all(~c("gear", "par", "phase", "start_yr", "end_yr", "env_link",
                  "link_par", "rand_walk", "re_start_yr", "re_end_yr", "re_sigma")) %>%
    # add sex wherever there is two selectivity functions with the same gear and block - Andre should change the output back to what I had at some point
    group_by(gear, start_yr, end_yr) %>% 
    mutate(sex = c("male", "female")[row_number()]) %>% ungroup() %>%
    transmute(gear, sex, par, phase, start_yr, end_yr, env_link, link_par, rand_walk, re_start_yr, re_end_yr, re_sigma) -> out$slx_control
  
  out$slx_control %>%
    mutate(fleet = out$fleet_names[abs(as.numeric(.$gear))],
           type = ifelse(gear > 0, "capture", "retention")) %>%
    distinct(fleet, type, sex, start_yr, end_yr) %>%
    mutate(start_yr = ifelse(start_yr == 0, out$yr_range[1], start_yr),
           end_yr = ifelse(start_yr == 0, out$yr_range[2], end_yr),
           year = purrr::map2(start_yr, end_yr, function(start_yr, end_yr) {start_yr:end_yr})) %>%
    unnest(year) %>%
    mutate(block = paste(start_yr, "-", end_yr)) %>%
    dplyr::select(-start_yr, -end_yr) -> tmp
  slx_cap %>%
    left_join(tmp %>%
                filter(type == "capture") %>%
                transmute(year, fleet, sex, capture_block = block),
              by = join_by(year, sex, fleet)) %>%
    left_join(slx_ret %>%
                left_join(tmp %>%
                            filter(type == "retention") %>%
                            transmute(year, fleet, sex, ret_disc_block = block),
                          by = join_by(year, sex, fleet)),
              by = join_by(year, sex, fleet, size)) %>%
    left_join(slx_disc, by = join_by(year, sex, fleet, size)) %>%
    transmute(year, sex, fleet, size, slx_capture, capture_block, slx_retention,
              slx_discard, ret_disc_block) -> out$selectivity
  last <- grep("#----", allout[,1])[8]
  
  # mortality ----
  
  ## M by season
  tmp <- matrix(nrow = length(out$mod_yrs), ncol = 1 + out$n_season)
  for(i in 1:nrow(tmp)){
    tmp[i,] <- as.numeric(allout[last + 3 + i, 1:ncol(tmp)])
  }
  as_tibble(tmp) %>%
    rename_all(~c("year", paste0("season_", 1:out$n_season))) -> out$proportion_M_by_season; last <- last + nrow(tmp) + 4
  
  ## M by sex-maturity-size class
  tmp <- matrix(nrow = length(out$mod_yrs) * out$n_sex * out$n_maturity, ncol = 3 + length(out$size_bins))
  for(i in 1:nrow(tmp)){
    tmp[i,] <- as.character(allout[last+1+i, 1:ncol(tmp)])
  }
  as_tibble(tmp) %>%
    rename_all(~c("year", "sex", "maturity", out$size_bins)) %>%
    pivot_longer(4:ncol(.), names_to = "size", values_to = "M") %>%
    mutate_at(c(1, 3:5), as.numeric) -> out$M_by_class; last <- last + nrow(tmp) + 2
  
  ## fully selected F by season, sex, fleet
  tmp <- matrix(nrow = out$n_sex * out$n_fleets * length(out$mod_yrs), ncol = 3 + out$n_season)
  for(i in 1:nrow(tmp)){
    tmp[i,] <- as.character(allout[last+1+i, 1:ncol(tmp)])
  }
  as_tibble(tmp) %>%
    rename_all(~c("sex", "fleet", "year", 1:out$n_season)) %>%
    mutate_at(c(3:ncol(.)), as.numeric) %>%
    pivot_longer(4:ncol(.), names_to = "season", values_to = "F") -> out$F_by_sex_fleet_year_season;last <- last + nrow(tmp) + 2
  
  ## fully selected F by season, sex, fleet
  ## skip same as above
  last <- last + nrow(out$F_by_sex_fleet_year_season)/out$n_fleets + 2
  
  ## F by sex, year, season, size
  tmp <- matrix(nrow = out$n_sex * length(out$mod_yrs) * out$n_season, ncol = 3 + length(out$size_bins))
  for(i in 1:nrow(tmp)){
    tmp[i,] <- as.character(allout[last+1+i, 1:ncol(tmp)])
  }
  as_tibble(tmp) %>%
    rename_all(~c("sex", "year", "season", out$size_bins)) %>%
    pivot_longer(4:ncol(.), names_to = "size", values_to = "F_continuos") %>%
    mutate_at(2:ncol(.), as.numeric) -> cont; last <- last + nrow(tmp) + 2
  tmp <- matrix(nrow = out$n_sex * length(out$mod_yrs) * out$n_season, ncol = 3 + length(out$size_bins))
  for(i in 1:nrow(tmp)){
    tmp[i,] <- as.character(allout[last+1+i, 1:ncol(tmp)])
  }
  as_tibble(tmp) %>%
    rename_all(~c("sex", "year", "season", out$size_bins)) %>%
    pivot_longer(4:ncol(.), names_to = "size", values_to = "F_discrete") %>%
    mutate_at(2:ncol(.), as.numeric) -> disc; last <- last + nrow(tmp) + 2
  out$F_by_sex_maturity_year_season_size <- left_join(cont, disc, by = join_by(sex, year, season, size)); 
  
  ## Z by sex, year, maturity, season, size
  tmp <- matrix(nrow = out$n_sex * out$n_maturity * length(out$mod_yrs) * out$n_season, ncol = 4 + length(out$size_bins))
  for(i in 1:nrow(tmp)){
    tmp[i,] <- as.character(allout[last+1+i, 1:ncol(tmp)])
  }
  as_tibble(tmp) %>%
    rename_all(~c("sex", "maturity", "year", "season", out$size_bins)) %>%
    pivot_longer(5:ncol(.), names_to = "size", values_to = "Z_continuos") %>%
    mutate_at(2:ncol(.), as.numeric) -> cont; last <- last + nrow(tmp) + 2 
  for(i in 1:nrow(tmp)){
    tmp[i,] <- as.character(allout[last+1+i, 1:ncol(tmp)])
  }
  as_tibble(tmp) %>%
    rename_all(~c("sex", "maturity", "year", "season", out$size_bins)) %>%
    pivot_longer(5:ncol(.), names_to = "size", values_to = "Z_discrete") %>%
    mutate_at(2:ncol(.), as.numeric) -> disc; last <- grep("#---", allout[,1])[9]
  out$Z_by_sex_maturity_year_season_size <- left_join(cont, disc, by = join_by(sex, maturity, year, season, size))
  
  # n matrix ----
  
  nmats <- grep("#N(.)", apply(allout[(grep("#---", allout[,1])[9]+2):grep("#---", allout[,1])[10],], 1, str_flatten, na.rm = T), value = T)
  nmats_index <- as.numeric(names(nmats))
  
  list_tmp <- list()
  for(m in 1:length(nmats)) {
    tmp <- matrix(nrow = length(out$mod_yrs), ncol = 1 + length(out$size_bins))
    for(i in 1:nrow(tmp)) {
      tmp[i,] <- as.numeric(allout[nmats_index[m]+1+i, 1:ncol(tmp)])
    }
    as_tibble(tmp) %>% rename_all(~c("year", out$size_bins)) %>%
      mutate(type = gsub("[()]|#N", "", nmats[m])) -> list_tmp[[m]]
  }
  do.call("bind_rows", list_tmp) %>%
    pivot_longer(2:(ncol(.)-1), names_to = "size", values_to = "n") %>%
    pivot_wider(names_from = type, values_from = n) -> out$n_matrix; last <- grep("#---", allout[,1])[10]
  
  
  # growth ----
  
  ## molt probability
  tmp <- matrix(nrow = out$n_sex * length(out$mod_yrs), ncol = 2 + length(out$size_bins))
  for(i in 1:nrow(tmp)) {
    tmp[i,] <- as.character(allout[last+3+i,1:ncol(tmp)])
  }
  as_tibble(tmp) %>%
    rename_all(~c("sex", "year", out$size_bins)) %>%
    pivot_longer(3:ncol(.), names_to = "size", values_to = "molt_probability") %>%
    mutate_at(2:ncol(.), as.numeric) -> out$molt_probability; last <- last + 3 + nrow(tmp)
  
  ## growth transition matrix
  gmats <- grep("#growth_matrix", allout[,1]); last <- gmats[1]
  gmats_names <- lapply(apply(allout[gmats,], 1, str_flatten, na.rm = T), function(x){strsplit(x, ":")[[1]][2]}) %>% unlist
  list_tmp <- list()
  for(m in 1:length(gmats)) {
    tmp <- matrix(nrow = length(out$size_bins), ncol = length(out$size_bins))
    for(i in 1:nrow(tmp)) {
      tmp[i,] <- as.numeric(allout[gmats[m]+i, 1:ncol(tmp)])
    }
    row.names(tmp) <- out$size_bins; colnames(tmp) <- out$size_bins
    list_tmp[[m]] <- tmp
  }
  names(list_tmp) <- gmats_names
  out$growth_transition <- list_tmp
  
  ## size transition matrix
  smats <- grep("#size_matrix", allout[,1]); last <- gmats[1]
  smats_names <- lapply(apply(allout[smats,], 1, str_flatten, na.rm = T), function(x){strsplit(x, ":")[[1]][2]}) %>% unlist
  list_tmp <- list()
  for(m in 1:length(smats)) {
    tmp <- matrix(nrow = length(out$size_bins), ncol = length(out$size_bins))
    for(i in 1:nrow(tmp)) {
      tmp[i,] <- as.numeric(allout[smats[m]+i, 1:ncol(tmp)])
    }
    row.names(tmp) <- out$size_bins; colnames(tmp) <- out$size_bins
    list_tmp[[m]] <- tmp
  }
  names(list_tmp) <- smats_names
  out$size_transition <- list_tmp
  last <- grep("#---", allout[,1])[11]
  
  
  
  
  # reference points ----
  
  # combinations of seasons and fleets with Fs
  tmp <- matrix(nrow = out$n_season, ncol = out$n_fleets + 1)
  for(i in 1:nrow(tmp)){
    tmp[i,] <- as.numeric(allout[last+2+i, 1:ncol(tmp)])
  }
  as_tibble(tmp) %>%
    rename_all(~c("season", out$fleet_names)) -> out$seasons_fleets_w_F
  
  ##reference points and ofl
  out$spr_syr <- as.numeric(allout[grep("spr_syr", allout[,1])+1, 1])
  out$spr_nyr <- as.numeric(allout[grep("spr_nyr", allout[,1])+1, 1])
  out$spr_rbar <- as.numeric(allout[grep("spr_rbar", allout[,1])+1, 1:2])
  out$proj_rbar <- as.numeric(allout[grep("proj_rbar", allout[,1])+1, 1:2])
  out$spr_sexr <- as.numeric(allout[grep("spr_sexr", allout[,1])+1, 1])
  out$SR_alpha_prj <- as.numeric(allout[grep("SR_alpha_prj", allout[,1])+1, 1])
  out$SR_beta_prj <- as.numeric(allout[grep("SR_beta_prj", allout[,1])+1, 1])
  out$spr_fofl <- as.numeric(allout[grep("spr_fofl", allout[,1])+1, 1])
  out$spr_cofl_ret <- as.numeric(allout[grep("spr_cofl_ret", allout[,1])+1, 1])
  
  # simple likelihood stuff ----
  max(c(length(unique(out$catch_fit_summary$series)),
        length(unique(out$index_fit_summary$series)),
        length(unique(out$size_fit_summary$series)))) -> cols
  tmp <- matrix(nrow = 5, ncol = cols)
  for(i in 1:nrow(tmp)){
    tmp[i,] <- as.numeric(allout[grep("nloglike", allout[,1])+i, 1:ncol(tmp)])
  }
  out$nloglike <- tmp
  out$nlogPenalty <- as.numeric(na.omit(as.numeric(allout[grep("nlogPenalty", allout[,1])+1,])))
  out$priorDensity <- as.numeric(na.omit(as.numeric(allout[grep("priorDensity", allout[,1])+1,])))
  
  # objective function ----
  out$objective_function <- out$likelihoods_by_type %>% filter(process == "total") %>% pull(net_lik)

  }
  if(version == "2.20.14"){  
    # setup ----
    # Suppress the NA message in the coercion to double
    options(warn = -1) 
    
    # read text file
    allout <- read.delim(file, sep = "", header = F, col.names = c(1:1000), fill = T, na.strings = "", colClasses = "character")
    # create out object
    out <- list()
    
    # version ----
    out$version <- str_flatten(allout[1,], collapse = " ", na.rm = T)
    # model name ----
    out$model_name <- model_name
    # stock ----
    out$stock <- gsub("#Stock being assessed: ", "", str_flatten(allout[7,], collapse = " ", na.rm = T))
    # general info ----
    last <- 8
    ## units 
    out$wt_units <-  gsub("Weightunit:", "", str_flatten(allout[last + 1,], na.rm = T)); last <- last + 1 
    out$n_units <- gsub("Numbersunit:", "", str_flatten(allout[last + 1,], na.rm = T)); last <- last + 1
    case_when(out$n_units %in% c(1, "1", "1s", "one", "One", "ones", "Ones", "Numbers") ~ "1s",
              out$n_units %in% c(10, "10", "10s", "ten", "Ten", "Tens") ~ "10s",
              out$n_units %in% c(100, "100", "100s", "hundred", "Hundred", "Hundreds") ~ "100s",
              out$n_units %in% c(1000, "10-0", "1000s", "thousand", "Thousand", "Thousands", "thou", "Thou") ~ "1000s", 
              out$n_units %in% c(1000000, "10-0", "1000000s", "millions", "Million", "Millions", "mill", "Mill") ~ "1000000s") -> out$n_units
    ## years
    out$yr_range <- as.numeric(gsub(";", "", allout[grep("Year_range", allout[,1]), 2:3]))
    out$mod_yrs <- out$yr_range[1]:out$yr_range[2]
    last <- grep("Year_range", allout[,1]) # start saving last position in file
    ## number of seasons
    out$n_season <- as.numeric(str_extract(str_flatten(allout[last + 1,], na.rm = T), "([[:digit:]]+)")); last <- last + 1
    ## number of fleets
    out$n_fleets <- as.numeric(str_extract(str_flatten(allout[last + 1,], na.rm = T), "([[:digit:]]+)")); last <- last + 1
    ## fleet names
    out$fleet_names <- as.character(allout[last + 1, 2:(out$n_fleets + 1)]); last <- last + 1
    ## n sexes
    out$n_sex <- as.numeric(str_extract(str_flatten(allout[last + 1,], na.rm = T), "([[:digit:]]+)")); last <- last + 1
    ## n shell conidition
    out$n_shell <- as.numeric(str_extract(str_flatten(allout[last + 1,], na.rm = T), "([[:digit:]]+)")); last <- last + 1 
    ## n maturity states
    out$n_maturity <- as.numeric(str_extract(str_flatten(allout[last + 1,], na.rm = T), "([[:digit:]]+)")); last <- last + 1 
    ## size structure
    out$n_size_bins <- as.numeric(str_extract(str_flatten(allout[last + 1,], na.rm = T), "([[:digit:]]+)")); last <- last + 1 
    out$max_size_bins <- as.numeric(str_extract(str_flatten(allout[last + 1,], na.rm = T), "([[:digit:]]+)")); last <- last + 1 
    out$size_breaks <- as.numeric(allout[last + 1, 2:(out$n_size_bins+1)], na.rm = T); last <- last + 1 
    out$size_mid_points <- as.numeric(allout[last + 1, 2:(out$n_size_bins+1)], na.rm = T); last <- last + 1 
    
    # likelihoods by type ----
    
    # read lines individually
    catch = as.numeric(na.omit(as.numeric(allout[last + 3,]))); last <- last + 3 
    index = as.numeric(na.omit(as.numeric(allout[last + 1,]))); last <- last + 1 
    size = as.numeric(na.omit(as.numeric(allout[last + 1,]))); last <- last + 1 
    recruitment = as.numeric(na.omit(as.numeric(allout[last + 1,]))); last <- last + 1
    tagging =  as.numeric(na.omit(as.numeric(allout[last + 1,]))); last <- last + 1
    penalties =  as.numeric(na.omit(as.numeric(allout[last + 1,]))); last <- last + 1
    priors = as.numeric(na.omit(as.numeric(allout[last + 1,]))); last <- last + 1
    initial_size = as.numeric(na.omit(as.numeric(allout[last + 1,]))); last <- last + 1
    total = as.numeric(na.omit(as.numeric(allout[last + 1,]))); last <- last + 1
    
    # coerce to tibble
    rbind(catch, index, size, recruitment, tagging) %>% as_tibble() %>%
      transmute(process = c("catch", "index", "size", "recruitment", "tagging"), raw_lik = V1, net_lik = V2) %>%
      add_row(process = "penalites", raw_lik = penalties, net_lik = penalties) %>%
      add_row(process = "priors", raw_lik = priors, net_lik = priors) %>%
      add_row(process = "initial_size", raw_lik = initial_size, net_lik = initial_size) %>%
      add_row(process = "total", raw_lik = sum(.$raw_lik), net_lik = total) -> out$likelihoods_by_type
    
    # likelihoods by type and fleet ---- 
    ## catches
    tibble(raw_lik = as.numeric(na.omit(as.numeric(allout[last + 5,]))),
           emphasis = as.numeric(na.omit(as.numeric(allout[last + 6,]))),
           net_lik = as.numeric(na.omit(as.numeric(allout[last + 7,])))) %>%
      transmute(process = paste0("catch_", 1:nrow(.)), raw_lik, emphasis, net_lik) -> catch; last <- last + 8
    ## index
    tibble(raw_lik = as.numeric(na.omit(as.numeric(allout[last + 2,]))),
           emphasis = as.numeric(na.omit(as.numeric(allout[last + 3,]))),
           net_lik = as.numeric(na.omit(as.numeric(allout[last + 4,])))) %>%
      transmute(process = paste0("index_", 1:nrow(.)), raw_lik, emphasis, net_lik) -> index; last <- last + 5    
    ## size composition
    tibble(raw_lik = as.numeric(na.omit(as.numeric(allout[last + 2,]))),
           emphasis = as.numeric(na.omit(as.numeric(allout[last + 3,]))),
           net_lik = as.numeric(na.omit(as.numeric(allout[last + 4,])))) %>%
      transmute(process = paste0("size_comp_", 1:nrow(.)), raw_lik, emphasis, net_lik) -> size; last <- last + 5    
    ## recruitment penalties
    tibble(raw_lik = as.numeric(na.omit(as.numeric(allout[last + 2,]))),
           net_lik = as.numeric(na.omit(as.numeric(allout[last + 2,])))) %>%
      transmute(process = paste0("rec_pen_", 1:nrow(.)), raw_lik, net_lik) -> rec_pen; last <- last + 3
    ## tagging
    tibble(raw_lik = as.numeric(na.omit(as.numeric(allout[last + 2,]))),
           emphasis = as.numeric(na.omit(as.numeric(allout[last + 3,]))),
           net_lik = as.numeric(na.omit(as.numeric(allout[last + 4,])))) %>%
      transmute(process = paste0("tagging_", 1:nrow(.)), raw_lik, emphasis, net_lik) -> tagging; last <- last + 5   
    ## growth 
    tibble(raw_lik = as.numeric(na.omit(as.numeric(allout[last + 2,]))),
           net_lik = as.numeric(na.omit(as.numeric(allout[last + 2,])))) %>%
      transmute(process = paste0("growth_", 1:nrow(.)), raw_lik, net_lik) -> growth; last <- last + 4
    bind_rows(catch, index, size, rec_pen, tagging, growth) -> out$likelihoods_by_type_and_fleet
    
    
    
    # penalties ----
    
    tmp <- matrix(nrow = 10, ncol = 3)
    for(i in 1:10) {tmp[i, 1:3] <- as.numeric(na.omit(as.numeric(allout[last + 1 + i,])[-1]))}
    as_tibble(tmp) %>%
      mutate(penalty = c("Mean_Fbar = 0", "Mean_Fdev", "Rec_dev", "Sex_ratio",
                         "Molt_prob", "Smooth_select", "Init_numbers", "Fdevs_(flt)", "Fdovs_(flt)",
                         "Seldevs")) %>%
      transmute(penalty, raw_lik = V1, emphasis = V2, net_lik = V3) -> tmp
    
    out$penalties <- tmp
    last <- last + 14
    
    # parameters ----
    
    ## par tibble
    tmp <- matrix(ncol = 11, nrow = length((last + 1):(grep("#---", allout[,1])[1]-3)))
    for(i in 1:nrow(tmp)) {
      if("*" %in% as.character(allout[last + 1 + i, 1:ncol(tmp)])) {
        as.character(allout[last + 1 + i, 1:13]) %>%
          .[!is.na(.)] %>% .[. != "*"] -> tmp[i,]
      } else{as.character(allout[last + 1 + i, 1:ncol(tmp)]) -> tmp[i,]}
    }
    # oddly spaced column on one par
    log_vn_lines <- grep("Log_vn_size", tmp[,2])
    bind_rows( as_tibble(tmp[1:(log_vn_lines[1]-1), 1:11]),
               as_tibble(tmp[log_vn_lines, c(1, 2, 4:11)]),
               as_tibble(tmp[(log_vn_lines[length(log_vn_lines)]+1):nrow(tmp), 1:11]) ) %>%
      rename_all(~c("parameter_count", "parameter", "estimate", "phase", "lower_bound", "upper_bound", "status",
                    "penalty", "gradient", "standard_error", "est_count")) %>%
      #janitor::clean_names() %>%
      mutate_at(c(1, 3:11), as.numeric) %>% 
      dplyr::select(-status) -> out$parameters; last <- grep("#---", allout[,1])[1]
    out$n_par <- out$parameters %>% filter(phase > 0) %>% nrow
    ## parameters at bounds
    out$parameters %>%
      mutate(range = upper_bound - lower_bound,
             status = ifelse(estimate < (lower_bound+range*0.01), 1,
                             ifelse(estimate > upper_bound-range*0.01, 1, 0))) %>%
      filter(status == 1) %>% dplyr::select(-range, -status) -> out$parameters_at_bounds
    
    # max gradient ----
    out$max_gradient <- max(abs(out$parameters$gradient), na.rm = T)
    
    # reference points ----
    
    ## ref tibble
    tmp <- matrix(ncol = 3, nrow = length((last + 2):(grep("#---", allout[,1])[2] - 5)))
    for(i in 1:nrow(tmp)) {
      tmp[i,] <- na.omit(as.numeric(allout[last + 2 + i,]))
    }
    as_tibble(tmp) %>%
      mutate(ref = c("Male_spr_rbar", "Female_spr_rbar", "SSSB/R_F=0", "BMSY", "Bcurr/BMSY", "OFL_tot",
                     paste0("Fmsy_", 1:out$n_fleets), paste0("Fofl_", 1:out$n_fleets), 
                     paste0("OFL_", 1:out$n_fleets))) %>%
      transmute(parameter_name = ref, estimate = V1, se = V2, est_quantity_count = V3) -> out$reference_points
    out$mmb_curr <- prod(out$reference_points$estimate[out$reference_points$parameter_name %in% c("BMSY", "Bcurr/BMSY")])
    out$ofl_tot <- out$reference_points$estimate[out$reference_points$parameter_name %in% c("OFL_tot")]
    out$bmsy <- out$reference_points$estimate[out$reference_points$parameter_name %in% c("BMSY")]
    out$b_bmsy <- out$reference_points$estimate[out$reference_points$parameter_name %in% c("Bcurr/BMSY")]
    out$f_msy_tot <- sum(out$reference_points$estimate[out$reference_points$parameter_name %in% paste0("Fmsy_", 1:out$n_fleets)])
    out$f_ofl_tot <- sum(out$reference_points$estimate[out$reference_points$parameter_name %in% paste0("Fofl_", 1:out$n_fleets)])
    out$rbar_tot <- sum(out$reference_points$estimate[out$reference_points$parameter_name %in% c("Male_spr_rbar", "Female_spr_rbar")])
    last <- grep("#---", allout[,1])[2] - 2
    ## ref sigma
    allout[last:(last+1),] %>% dplyr::select_if(~ !any(is.na(.))) %>%
      mutate_all(., function(x){gsub(";", "", x)}) %>%
      dplyr::select(-2) %>%
      pivot_wider(names_from = X1, values_from = X3) -> out$ref_sigmaR
    last <- grep("#---", allout[,1])[2]
    
    # overall summary ----
    
    tmp <- matrix(ncol = 15 + (3*out$n_sex) + out$n_fleets, nrow = length(out$mod_yrs))
    for(i in 1:nrow(tmp)) {
      tmp[i,] <- as.numeric(allout[last+3+i,1:ncol(tmp)])
    }
    as_tibble(tmp) %>%
      rename_all(~make.unique(as.character(allout[last+3,1:ncol(tmp)]))) %>%
      janitor::clean_names(.) %>% rename(log_recruits_male = log_recruits, sd_log_recruits_male = sd_log_recruits) -> out$derived_quant_summary
    # do some renaming
    
    if(out$n_sex == 2) {
      out$derived_quant_summary %>%
        rename(log_recruits_female = log_recruits_1, sd_log_recruits_female = sd_log_recruits_1) -> out$derived_quant_summary
    }
    last <- grep("#---", allout[,1])[3]
    
    # mean wt ----
    
    ## weight at size matrix
    tmp <- matrix(nrow = out$n_sex*out$n_maturity*length(out$mod_yrs), ncol = length(out$size_mid_points)+3)
    for(i in 1:nrow(tmp)) {
      tmp[i,] <- as.character(allout[last+4,1:ncol(tmp)])
    }
    as_tibble(tmp) %>%
      mutate_at(2:ncol(.), as.numeric) %>%
      rename_all(~c("sex", "maturity", "year", out$size_mid_points)) %>%
      pivot_longer(4:ncol(.), names_to = "size", values_to = "wt") %>%
      mutate(size = as.numeric(size)) -> out$wt_at_size
    last <- last + 4 + nrow(tmp)
    
    # maturity vector ----
    
    out$maturity_at_size_vector <- as.numeric(allout[last+2,1:length(out$size_mid_points)]); last <- grep("#---", allout[,1])[4]
    
    # catch fit summary ----
    
    ## catch summary
    tmp <- matrix(nrow = length((last+3):(grep("#---", allout[,1])[5]-4)), ncol = 14)
    for(i in 1:nrow(tmp)){
      tmp[i,] <- as.character(allout[last+2+i,1:14])
    }
    as_tibble(tmp) %>%
      mutate_at(c(1:2, 4, 6:7, 10:14), as.numeric) %>%
      rename_all(~c("series", "year", "fleet", "season", "sex", "obs_catch", "cv", "type", "units", "mult", "effort", "disc_m", 
                    "pred_catch", "residual")) -> out$catch_fit_summary; last <- grep("#---", allout[,1])[5]-3
    ## catch q
    tibble(series = unique(out$catch_fit_summary$series),
           log_q = as.numeric(allout[last+2,1:length(unique(out$catch_fit_summary$series))])) -> out$log_q_catch; last <- grep("#---", allout[,1])[5]
    
    # index fix_summary ----
    
    ## index summary
    tmp <- matrix(nrow = length((last+3):(grep("sdnr_MAR_cpue", allout[,1])-2)), ncol = 13)
    for(i in 1:nrow(tmp)){
      tmp[i,] <- as.character(allout[last+2+i,1:13])
    }
    as_tibble(tmp) %>%
      mutate_at(c(1:2, 4, 7:9, 11:13), as.numeric) %>%
      rename_all(~c("series", "year", "fleet", "season", "sex", "maturity", "obs_index", "obs_cv", 
                    "tot_cv", "units", "q", "timing", "pred_index")) -> out$index_fit_summary; last <- last + nrow(tmp) + 2
    ## sdnr_MAR_cpue
    tmp <- matrix(nrow = length(unique(out$index_fit_summary$series)), ncol = 2)
    for(i in 1:nrow(tmp)) {
      tmp[i,] <- as.numeric(allout[last+3, 1:2])
    }
    out$sdnr_MAR_cpue <- tmp; last <- grep("#---", allout[,1])[6]
    
    # size composition fit summary ----
    
    ## size composition fit summary
    ## get info first
    tmp <- matrix(ncol = 12, nrow = length((last+3):(grep("sdnr_MAR_lf", allout[,1])-2)))
    for(i in 1:nrow(tmp)) {
      tmp[i,] <- as.character(allout[last+2+i, 1:ncol(tmp)])
    }
    as_tibble(tmp) %>%
      rename_all(~c("org_series", "mod_series", "year", "fleet", "season", "sex", "type", "shell", "maturity","shell_name", "maturity_name", "nsamp_obs")) %>%
      mutate_at(c(1:3, 5, 12), as.numeric) -> tmp
    
    ## get comps
    last <- last + 2 # set last to start where the data is
    tmp %>%
      nest_by(mod_series, .keep = T) %>% ungroup() %>% 
      mutate(row = purrr::map_dbl(data, ~nrow(.)),
             row = lag(row),
             row = cumsum(ifelse(is.na(row), 0, row)) + last) %>% 
      mutate(comps = purrr::map2(data, row, function(data, row) {
        
        comp_tmp <- matrix(ncol = ncol(allout)-ncol(tmp), nrow = nrow(data))
        for(i in 1:nrow(comp_tmp)) {
          comp_tmp[i,] <- as.numeric(allout[row + i, 13:ncol(allout)])
        }
        as_tibble(comp_tmp) %>%
          dplyr::select(where(function(x)!all(is.na(x)))) -> comp_tmp
        if((ncol(comp_tmp)/out$n_size_bins) <= 2) {comp_agg <- F} else{comp_agg <- T}
        if(comp_agg == F){
          
          comp_tmp %>% 
            rename_all(~c(paste0("obs_", out$size_mid_points[1:(ncol(comp_tmp)/2)]), paste0("pred_", out$size_mid_points[1:(ncol(comp_tmp)/2)]))) %>%
            bind_cols(data, .) %>%
            pivot_longer((ncol(data)+1):ncol(.), names_to = "group", values_to = "prop") %>% 
            separate_wider_delim(group, "_", names_sep = "split") %>%
            pivot_wider(names_from = groupsplit1, values_from = prop) %>%
            rename(size = groupsplit2) %>%
            transmute(org_series, mod_series, year, fleet, season, sex, type, shell, maturity, size = as.numeric(size), 
                      nsamp_obs, obs = as.numeric(obs), pred = as.numeric(pred), residual = obs - pred) -> comp_out
        }
        if(comp_agg == T){
          nobs <- ncol(comp_tmp)/2
          group <- rep(1:50, each = out$n_size_bins)[1:nobs] # << probably a more elegant way to do this...
          comp_tmp %>% 
            rename_all(~c(paste0("obs_", group, "_", as.numeric(matrix(out$size_mid_points, ncol = nobs))), paste0("pred_", group, "_", as.numeric(matrix(out$size_mid_points, ncol = nobs))))) %>%
            bind_cols(data, .) %>%
            pivot_longer((ncol(data)+1):ncol(.), names_to = "group", values_to = "prop") %>% 
            separate_wider_delim(group, "_", names_sep = "split") %>%
            pivot_wider(names_from = groupsplit1, values_from = prop) %>%
            rename(size = groupsplit3, aggregate_series = groupsplit2) %>%
            transmute(org_series, mod_series, aggregate_series = as.numeric(aggregate_series), year, fleet, season, sex, type, shell, maturity, size = as.numeric(size),
                      nsamp_obs, obs = as.numeric(obs), pred = as.numeric(pred), residual = obs - pred) -> comp_out
        }
        
        return(comp_out)
        
      })) %>% transmute(comps) %>% unnest -> out$size_fit_summary
    
    last <- grep("sdnr_MAR_lf", allout[,1])
    ## sdnr_MAR_lf
    tmp <- matrix(ncol = 2, nrow = length(unique(out$size_fit_summary$mod_series)))
    for(i in 1:nrow(tmp)) {
      tmp[i,] <- as.numeric(allout[last+i, 1:2])
    }
    out$sdnr_MAR_lf <- tmp
    last <- grep("Francis_weights", allout[,1])
    ## francis weights
    out$francis_weights <- as.numeric(allout[last+1, 1:length(unique(out$size_fit_summary$mod_series))]); last <- grep("#---", allout[,1])[7]
    
    ## add stage two weights to fit summary
    out$size_fit_summary %>% 
      mutate(vn = exp(out$parameters$estimate[grepl("Log_vn_size_comp", out$parameters$parameter)][.$mod_series]),
             out_lambda = out$francis_weights[.$mod_series],
             nsamp_est = exp(out$parameters$estimate[grepl("Log_vn_size_comp", out$parameters$parameter)][.$mod_series]) * nsamp_obs * out_lambda) -> out$size_fit_summary
    
    # selectivity ----
    
    ## selex
    tmp <- matrix(ncol = 3 + length(out$size_mid_points), nrow = length(out$mod_yrs) * out$n_sex * out$n_fleets)
    for(i in 1:nrow(tmp)) {
      tmp[i,] <- as.character(allout[last+3+i, 1:ncol(tmp)])
    }
    as_tibble(tmp) %>%
      rename_all(~c("year", "sex", "fleet", out$size_mid_points)) %>%
      pivot_longer(4:ncol(.), values_to = "slx_capture", names_to = "size") %>%
      mutate_at(c(1, 4, 5), as.numeric) -> slx_cap; last <- last + 3 + nrow(tmp)
    ## retention
    tmp <- matrix(ncol = 3 + length(out$size_mid_points), nrow = length(out$mod_yrs) * out$n_sex * out$n_fleets)
    for(i in 1:nrow(tmp)) {
      tmp[i,] <- as.character(allout[last+2+i, 1:ncol(tmp)])
    }
    as_tibble(tmp) %>%
      rename_all(~c("year", "sex", "fleet", out$size_mid_points)) %>%
      pivot_longer(4:ncol(.), values_to = "slx_retention", names_to = "size") %>%
      mutate_at(c(1, 4, 5), as.numeric) -> slx_ret; last <- last + 2 + nrow(tmp)
    ## discard
    tmp <- matrix(ncol = 3 + length(out$size_mid_points), nrow = length(out$mod_yrs) * out$n_sex * out$n_fleets)
    for(i in 1:nrow(tmp)) {
      tmp[i,] <- as.character(allout[last+2+i, 1:ncol(tmp)])
    }
    as_tibble(tmp) %>%
      rename_all(~c("year", "sex", "fleet", out$size_mid_points)) %>%
      pivot_longer(4:ncol(.), values_to = "slx_discard", names_to = "size") %>%
      mutate_at(c(1, 4, 5), as.numeric) -> slx_disc#; last <- grep("Select_control", allout[,2])
    # ## slx control
    # as_tibble(allout[(last+2):(grep("#----", allout[,1])[8]-1), 1:11]) %>%
    #   mutate_all(as.numeric) %>%
    #   rename_all(~c("gear", "par", "phase", "start_yr", "end_yr", "env_link",
    #                 "link_par", "rand_walk", "re_start_yr", "re_end_yr", "re_sigma")) %>%
    #   # add sex wherever there is two selectivity functions with the same gear and block - Andre should change the output back to what I had at some point
    #   group_by(gear, start_yr, end_yr) %>% 
    #   mutate(sex = c("male", "female")[row_number()]) %>% ungroup() %>%
    #   transmute(gear, sex, par, phase, start_yr, end_yr, env_link, link_par, rand_walk, re_start_yr, re_end_yr, re_sigma) -> out$slx_control
    # 
    # out$slx_control %>%
    #   mutate(fleet = out$fleet_names[abs(as.numeric(.$gear))],
    #          type = ifelse(gear > 0, "capture", "retention")) %>%
    #   distinct(fleet, type, sex, start_yr, end_yr) %>%
    #   mutate(start_yr = ifelse(start_yr == 0, out$yr_range[1], start_yr),
    #          end_yr = ifelse(start_yr == 0, out$yr_range[2], end_yr),
    #          year = purrr::map2(start_yr, end_yr, function(start_yr, end_yr) {start_yr:end_yr})) %>%
    #   unnest(year) %>%
    #   mutate(block = paste(start_yr, "-", end_yr)) %>%
    #   dplyr::select(-start_yr, -end_yr) -> tmp
    # slx_cap %>%
    #   left_join(tmp %>%
    #               filter(type == "capture") %>%
    #               transmute(year, fleet, sex, capture_block = block),
    #             by = join_by(year, sex, fleet)) %>%
    #   left_join(slx_ret %>%
    #               left_join(tmp %>%
    #                           filter(type == "retention") %>%
    #                           transmute(year, fleet, sex, ret_disc_block = block),
    #                         by = join_by(year, sex, fleet)),
    #             by = join_by(year, sex, fleet, size)) %>%
    #   left_join(slx_disc, by = join_by(year, sex, fleet, size)) %>%
    #   transmute(year, sex, fleet, size, slx_capture, capture_block, slx_retention,
    #             slx_discard, ret_disc_block) -> out$selectivity
    
    slx_cap %>% left_join(slx_ret, by = join_by(year, sex, fleet, size)) %>% left_join(slx_disc, by = join_by(year, sex, fleet, size)) -> out$selectivity
    last <- grep("#----", allout[,1])[8]
    
    # mortality ----
    
    ## M by season
    tmp <- matrix(nrow = length(out$mod_yrs), ncol = 1 + out$n_season)
    for(i in 1:nrow(tmp)){
      tmp[i,] <- as.numeric(allout[last + 4 + i, 1:ncol(tmp)])
    }
    as_tibble(tmp) %>%
      rename_all(~c("year", paste0("season_", 1:out$n_season))) -> out$proportion_M_by_season; last <- last + nrow(tmp) + 4
    
    ## M by sex-maturity-size class
    tmp <- matrix(nrow = length(out$mod_yrs) * out$n_sex * out$n_maturity, ncol = 3 + length(out$size_mid_points))
    for(i in 1:nrow(tmp)){
      tmp[i,] <- as.character(allout[last+3+i, 1:ncol(tmp)])
    }
    as_tibble(tmp) %>% 
      rename_all(~c("year", "sex", "maturity", out$size_mid_points)) %>%
      pivot_longer(4:ncol(.), names_to = "size", values_to = "M") %>%
      mutate_at(c(1, 4:5), as.numeric) -> out$M_by_class; last <- last + nrow(tmp) + 3
    
    ## fully selected F by season, sex, fleet
    tmp <- matrix(nrow = out$n_sex * out$n_fleets * length(out$mod_yrs), ncol = 3 + out$n_season)
    for(i in 1:nrow(tmp)){
      tmp[i,] <- as.character(allout[last+4+i, 1:ncol(tmp)])
    }
    as_tibble(tmp) %>%
      rename_all(~c("sex", "fleet", "year", 1:out$n_season)) %>%
      mutate_at(c(3:ncol(.)), as.numeric) %>% 
      pivot_longer(4:ncol(.), names_to = "season", values_to = "F") -> out$F_by_sex_fleet_year_season;last <- last + nrow(tmp) + 4
    
    ## fully selected F by season, sex, fleet
    ## skip same as above
    last <- last + nrow(out$F_by_sex_fleet_year_season)/out$n_fleets + 4
    
    ## F by sex, year, season, size
    tmp <- matrix(nrow = out$n_sex * length(out$mod_yrs) * out$n_season, ncol = 3 + length(out$size_mid_points))
    for(i in 1:nrow(tmp)){
      tmp[i,] <- as.character(allout[last+4+i, 1:ncol(tmp)])
    }
    as_tibble(tmp) %>%
      rename_all(~c("sex", "year", "season", out$size_mid_points)) %>%
      pivot_longer(4:ncol(.), names_to = "size", values_to = "F_continuous") %>% 
      mutate_at(2:ncol(.), as.numeric) -> cont; last <- last + nrow(tmp) + 4
    tmp <- matrix(nrow = out$n_sex * length(out$mod_yrs) * out$n_season, ncol = 3 + length(out$size_mid_points))
    for(i in 1:nrow(tmp)){
      tmp[i,] <- as.character(allout[last+4+i, 1:ncol(tmp)])
    }
    as_tibble(tmp) %>%
      rename_all(~c("sex", "year", "season", out$size_mid_points)) %>%
      pivot_longer(4:ncol(.), names_to = "size", values_to = "F_discrete") %>%
      mutate_at(2:ncol(.), as.numeric) -> disc; last <- last + nrow(tmp) + 4
    out$F_by_sex_maturity_year_season_size <- left_join(cont, disc, by = join_by(sex, year, season, size))
    
    ## Z by sex, year, maturity, season, size
    tmp <- matrix(nrow = out$n_sex * out$n_maturity * length(out$mod_yrs) * out$n_season, ncol = 4 + length(out$size_mid_points))
    for(i in 1:nrow(tmp)){
      tmp[i,] <- as.character(allout[last+4+i, 1:ncol(tmp)])
    }
    as_tibble(tmp) %>%
      rename_all(~c("sex", "maturity", "year", "season", out$size_mid_points)) %>%
      pivot_longer(5:ncol(.), names_to = "size", values_to = "Z_continuos") %>%
      mutate_at(3:ncol(.), as.numeric) -> cont; last <- last + nrow(tmp) + 4 
    for(i in 1:nrow(tmp)){
      tmp[i,] <- as.character(allout[last+4+i, 1:ncol(tmp)])
    }
    as_tibble(tmp) %>%
      rename_all(~c("sex", "maturity", "year", "season", out$size_mid_points)) %>%
      pivot_longer(5:ncol(.), names_to = "size", values_to = "Z_discrete") %>%
      mutate_at(3:ncol(.), as.numeric) %>%
      mutate(maturity = case_when(maturity == 1 ~ "mature",
                                  maturity == 2 ~ "immature",
                                  maturity == 0 ~ "undetermined")) -> disc; last <- grep("#---", allout[,1])[9]
    out$Z_by_sex_maturity_year_season_size <- left_join(cont, disc, by = join_by(sex, maturity, year, season, size))
    
    # n matrix ----
    
    ## n matrix by sex and maturity
    nmats <- grep("#N(.)", apply(allout[(grep("#---", allout[,1])[9]+2):grep("sex_maturity_shell_con", allout[,1]),], 1, str_flatten, na.rm = T), value = T)
    nmats_index <- as.numeric(names(nmats))
    
    list_tmp <- list()
    for(m in 1:length(nmats)) {
      tmp <- matrix(nrow = length(out$mod_yrs)+1, ncol = 1 + length(out$size_mid_points))
      for(i in 1:nrow(tmp)) {
        tmp[i,] <- as.numeric(allout[nmats_index[m]+2+i, 1:ncol(tmp)])
      }
      as_tibble(tmp) %>% rename_all(~c("year", out$size_mid_points)) %>%
        mutate(type = gsub("[()]|#N", "", nmats[m])) -> list_tmp[[m]]
    }
    bind_rows(list_tmp) %>%
      pivot_longer(2:(ncol(.)-1), names_to = "size", values_to = "n") %>%
      pivot_wider(names_from = type, values_from = n) -> out$n_matrix
    
    
    ## n matrix by sex, maturity and shell condition
    nmats <- grep("#N(.)", apply(allout[(grep("sex_maturity_shell_con", allout[,1])):grep("#---", allout[,1])[10],], 1, str_flatten, na.rm = T), value = T)
    nmats_index <- as.numeric(names(nmats))    
    
    list_tmp <- list()
    for(m in 1:length(nmats)) {
      tmp <- matrix(nrow = length(out$mod_yrs)+1, ncol = 1 + length(out$size_mid_points))
      for(i in 1:nrow(tmp)) {
        tmp[i,] <- as.numeric(allout[nmats_index[m]+i, c(1, 5:(ncol(tmp)+3))])
      }
      as_tibble(tmp) %>% rename_all(~c("year", out$size_mid_points)) %>%
        mutate(type = gsub("[()]|#N", "", nmats[m])) -> list_tmp[[m]]
    }
    bind_rows(list_tmp) %>%
      pivot_longer(2:(ncol(.)-1), names_to = "size", values_to = "n") %>%
      pivot_wider(names_from = type, values_from = n) %>% 
      right_join(out$n_matrix, ., by = join_by(year, size)) -> out$n_matrix
    
   last <- grep("#---", allout[,1])[10]
    
    
    # growth ----
    
    ## molt probability
    tmp <- matrix(nrow = out$n_sex * length(out$mod_yrs), ncol = 2 + length(out$size_mid_points))
    for(i in 1:nrow(tmp)) {
      tmp[i,] <- as.character(allout[last+3+i,1:ncol(tmp)])
    }
    as_tibble(tmp) %>% 
      rename_all(~c("sex", "year", out$size_mid_points)) %>%
      pivot_longer(3:ncol(.), names_to = "size", values_to = "molt_probability") %>%
      mutate_at(2:ncol(.), as.numeric) -> out$molt_probability; last <- last + 3 + nrow(tmp)
    
    ## growth transition matrix
    gmats <- grep("#growth_matrix", allout[,1]) + 1; last <- gmats[1]
    gmats_names <- lapply(apply(allout[gmats,], 1, str_flatten, na.rm = T), function(x){strsplit(x, ":")[[1]][2]}) %>% unlist
    list_tmp <- list()
    for(m in 1:length(gmats)) {
      tmp <- matrix(nrow = length(out$size_mid_points), ncol = length(out$size_mid_points))
      for(i in 1:nrow(tmp)) {
        tmp[i,] <- as.numeric(allout[gmats[m]+i, 1:ncol(tmp)])
      }
      row.names(tmp) <- out$size_mid_points; colnames(tmp) <- out$size_mid_points
      list_tmp[[m]] <- tmp
    }
    names(list_tmp) <- gmats_names
    out$growth_transition <- list_tmp
    
    ## size transition matrix
    smats <- grep("#size_matrix", allout[,1]) + 1; last <- gmats[1]
    smats_names <- lapply(apply(allout[smats,], 1, str_flatten, na.rm = T), function(x){strsplit(x, ":")[[1]][2]}) %>% unlist
    list_tmp <- list()
    for(m in 1:length(smats)) {
      tmp <- matrix(nrow = length(out$size_mid_points), ncol = length(out$size_mid_points))
      for(i in 1:nrow(tmp)) {
        tmp[i,] <- as.numeric(allout[smats[m]+i, 1:ncol(tmp)])
      }
      row.names(tmp) <- out$size_mid_points; colnames(tmp) <- out$size_mid_points
      list_tmp[[m]] <- tmp
    }
    names(list_tmp) <- smats_names
    out$size_transition <- list_tmp
    last <- grep("#---", allout[,1])[11]
    
    
    
    
    # reference points ----
    
    # combinations of seasons and fleets with Fs
    tmp <- matrix(nrow = out$n_season, ncol = out$n_fleets + 1)
    for(i in 1:nrow(tmp)){
      tmp[i,] <- as.numeric(allout[last+3+i, 1:ncol(tmp)])
    }
    as_tibble(tmp) %>%
      rename_all(~c("season", out$fleet_names)) -> out$seasons_fleets_w_F
    
    ##reference points and ofl
    out$spr_syr <- as.numeric(allout[grep("spr_syr", allout[,1])+1, 1])
    out$spr_nyr <- as.numeric(allout[grep("spr_nyr", allout[,1])+1, 1])
    out$spr_rbar <- as.numeric(allout[grep("spr_rbar", allout[,1])+1, 1:2])
    out$proj_rbar <- as.numeric(allout[grep("proj_rbar", allout[,1])+1, 1:2])
    out$spr_sexr <- as.numeric(allout[grep("spr_sexr", allout[,1])+1, 1])
    out$SR_alpha_prj <- as.numeric(allout[grep("SR_alpha_prj", allout[,1])+1, 1])
    out$SR_beta_prj <- as.numeric(allout[grep("SR_beta_prj", allout[,1])+1, 1])
    out$spr_fofl <- as.numeric(allout[grep("spr_fofl", allout[,1])+1, 1])
    out$spr_cofl_ret <- as.numeric(allout[grep("spr_cofl_ret", allout[,1])+1, 1])
    last <- grep("#---", allout[,1])[14]
    
    # simple likelihood stuff ----
    max(c(length(unique(out$catch_fit_summary$series)),
          length(unique(out$index_fit_summary$series)),
          length(unique(out$size_fit_summary$series)))) -> cols
    tmp <- matrix(nrow = 5, ncol = cols + 1)
    for(i in 1:nrow(tmp)){
      tmp[i,] <- as.numeric(allout[last+1+i, 1:ncol(tmp)])
    }
    out$nloglike <- tmp[,-1]
    out$nlogPenalty <- as.numeric(na.omit(as.numeric(allout[grep("nlogPenalty", allout[,1])+1,])))
    out$priorDensity <- as.numeric(na.omit(as.numeric(allout[grep("priorDensity", allout[,1])+1,])))
    
    # objective function ----
    out$objective_function <- out$likelihoods_by_type %>% filter(process == "total") %>% pull(net_lik)
  }
  if(version == "2.20.16"){  
    # setup ----
    # Suppress the NA message in the coercion to double
    options(warn = -1) 
    
    # read text file
    allout <- read.delim(file, sep = "", header = F, col.names = c(1:1000), fill = T, na.strings = "", colClasses = "character")
    # create out object
    out <- list()
    
    # version ----
    out$version <- str_flatten(allout[1,], collapse = " ", na.rm = T)
    # model name ----
    out$model_name <- model_name
    # stock ----
    out$stock <- gsub("#Stock being assessed: ", "", str_flatten(allout[7,], collapse = " ", na.rm = T))
    # general info ----
    last <- 8
    ## units 
    out$wt_units <-  gsub("Weightunit:", "", str_flatten(allout[last + 1,], na.rm = T)); last <- last + 1 
    out$n_units <- gsub("Numbersunit:", "", str_flatten(allout[last + 1,], na.rm = T)); last <- last + 1
    case_when(out$n_units %in% c(1, "1", "1s", "one", "One", "ones", "Ones", "Numbers") ~ "1s",
              out$n_units %in% c(10, "10", "10s", "ten", "Ten", "Tens") ~ "10s",
              out$n_units %in% c(100, "100", "100s", "hundred", "Hundred", "Hundreds") ~ "100s",
              out$n_units %in% c(1000, "10-0", "1000s", "thousand", "Thousand", "Thousands", "thou", "Thou") ~ "1000s", 
              out$n_units %in% c(1000000, "10-0", "1000000s", "millions", "Million", "Millions", "mill", "Mill") ~ "1000000s") -> out$n_units
    ## years
    out$yr_range <- as.numeric(gsub(";", "", allout[grep("Year_range", allout[,1]), 2:3]))
    out$mod_yrs <- out$yr_range[1]:out$yr_range[2]
    last <- grep("Year_range", allout[,1]) # start saving last position in file
    ## number of seasons
    out$n_season <- as.numeric(str_extract(str_flatten(allout[last + 1,], na.rm = T), "([[:digit:]]+)")); last <- last + 1
    ## number of fleets
    out$n_fleets <- as.numeric(str_extract(str_flatten(allout[last + 1,], na.rm = T), "([[:digit:]]+)")); last <- last + 1
    ## fleet names
    out$fleet_names <- as.character(allout[last + 1, 2:(out$n_fleets + 1)]); last <- last + 1
    ## n sexes
    out$n_sex <- as.numeric(str_extract(str_flatten(allout[last + 1,], na.rm = T), "([[:digit:]]+)")); last <- last + 1
    ## n shell conidition
    out$n_shell <- as.numeric(str_extract(str_flatten(allout[last + 1,], na.rm = T), "([[:digit:]]+)")); last <- last + 1 
    ## n maturity states
    out$n_maturity <- as.numeric(str_extract(str_flatten(allout[last + 1,], na.rm = T), "([[:digit:]]+)")); last <- last + 1 
    ## size structure
    out$n_size_bins <- as.numeric(str_extract(str_flatten(allout[last + 1,], na.rm = T), "([[:digit:]]+)")); last <- last + 1 
    out$max_size_bins <- as.numeric(str_extract(str_flatten(allout[last + 1,], na.rm = T), "([[:digit:]]+)")); last <- last + 1 
    out$size_breaks <- as.numeric(allout[last + 1, 2:(out$n_size_bins+1)], na.rm = T); last <- last + 1 
    out$size_mid_points <- as.numeric(allout[last + 1, 2:(out$n_size_bins+1)], na.rm = T); last <- last + 1 
    
    # likelihoods by type ----
    
    # read lines individually
    catch = as.numeric(na.omit(as.numeric(allout[last + 3,]))); last <- last + 3 
    index = as.numeric(na.omit(as.numeric(allout[last + 1,]))); last <- last + 1 
    size = as.numeric(na.omit(as.numeric(allout[last + 1,]))); last <- last + 1 
    recruitment = as.numeric(na.omit(as.numeric(allout[last + 1,]))); last <- last + 1
    tagging =  as.numeric(na.omit(as.numeric(allout[last + 1,]))); last <- last + 1
    penalties =  as.numeric(na.omit(as.numeric(allout[last + 1,]))); last <- last + 1
    priors = as.numeric(na.omit(as.numeric(allout[last + 1,]))); last <- last + 1
    initial_size = as.numeric(na.omit(as.numeric(allout[last + 1,]))); last <- last + 1
    total = as.numeric(na.omit(as.numeric(allout[last + 1,]))); last <- last + 1
    
    # coerce to tibble
    rbind(catch, index, size, recruitment, tagging) %>% as_tibble() %>%
      transmute(process = c("catch", "index", "size", "recruitment", "tagging"), raw_lik = V1, net_lik = V2) %>%
      add_row(process = "penalites", raw_lik = penalties, net_lik = penalties) %>%
      add_row(process = "priors", raw_lik = priors, net_lik = priors) %>%
      add_row(process = "initial_size", raw_lik = initial_size, net_lik = initial_size) %>%
      add_row(process = "total", raw_lik = sum(.$raw_lik), net_lik = total) -> out$likelihoods_by_type
    
    # likelihoods by type and fleet ---- 
    ## catches
    tibble(raw_lik = as.numeric(na.omit(as.numeric(allout[last + 5,]))),
           emphasis = as.numeric(na.omit(as.numeric(allout[last + 6,]))),
           net_lik = as.numeric(na.omit(as.numeric(allout[last + 7,])))) %>%
      transmute(process = paste0("catch_", 1:nrow(.)), raw_lik, emphasis, net_lik) -> catch; last <- last + 8
    ## index
    tibble(raw_lik = as.numeric(na.omit(as.numeric(allout[last + 2,]))),
           emphasis = as.numeric(na.omit(as.numeric(allout[last + 3,]))),
           net_lik = as.numeric(na.omit(as.numeric(allout[last + 4,])))) %>%
      transmute(process = paste0("index_", 1:nrow(.)), raw_lik, emphasis, net_lik) -> index; last <- last + 5    
    ## size composition
    tibble(raw_lik = as.numeric(na.omit(as.numeric(allout[last + 2,]))),
           emphasis = as.numeric(na.omit(as.numeric(allout[last + 3,]))),
           net_lik = as.numeric(na.omit(as.numeric(allout[last + 4,])))) %>%
      transmute(process = paste0("size_comp_", 1:nrow(.)), raw_lik, emphasis, net_lik) -> size; last <- last + 5    
    ## recruitment penalties
    tibble(raw_lik = as.numeric(na.omit(as.numeric(allout[last + 2,]))),
           net_lik = as.numeric(na.omit(as.numeric(allout[last + 2,])))) %>%
      transmute(process = paste0("rec_pen_", 1:nrow(.)), raw_lik, net_lik) -> rec_pen; last <- last + 3
    ## tagging
    tibble(raw_lik = as.numeric(na.omit(as.numeric(allout[last + 2,]))),
           emphasis = as.numeric(na.omit(as.numeric(allout[last + 3,]))),
           net_lik = as.numeric(na.omit(as.numeric(allout[last + 4,])))) %>%
      transmute(process = paste0("tagging_", 1:nrow(.)), raw_lik, emphasis, net_lik) -> tagging; last <- last + 5   
    ## growth 
    tibble(raw_lik = as.numeric(na.omit(as.numeric(allout[last + 2,]))),
           net_lik = as.numeric(na.omit(as.numeric(allout[last + 2,])))) %>%
      transmute(process = paste0("growth_", 1:nrow(.)), raw_lik, net_lik) -> growth; last <- last + 4
    bind_rows(catch, index, size, rec_pen, tagging, growth) -> out$likelihoods_by_type_and_fleet
    
    
    
    # penalties ----
    
    tmp <- matrix(nrow = 10, ncol = 3)
    for(i in 1:10) {tmp[i, 1:3] <- as.numeric(na.omit(as.numeric(allout[last + 1 + i,])[-1]))}
    as_tibble(tmp) %>%
      mutate(penalty = c("Mean_Fbar = 0", "Mean_Fdev", "Rec_dev", "Sex_ratio",
                         "Molt_prob", "Smooth_select", "Init_numbers", "Fdevs_(flt)", "Fdovs_(flt)",
                         "Seldevs")) %>%
      transmute(penalty, raw_lik = V1, emphasis = V2, net_lik = V3) -> tmp
    
    out$penalties <- tmp
    last <- last + 14
    
    # parameters ----
    
    ## par tibble
    tmp <- matrix(ncol = 11, nrow = length((last + 1):(grep("#---", allout[,1])[1]-3)))
    for(i in 1:nrow(tmp)) {
      if("*" %in% as.character(allout[last + 1 + i, 1:ncol(tmp)])) {
        as.character(allout[last + 1 + i, 1:13]) %>%
          .[!is.na(.)] %>% .[. != "*"] -> tmp[i,]
      } else{as.character(allout[last + 1 + i, 1:ncol(tmp)]) -> tmp[i,]}
    }
    # oddly spaced column on one par
    log_vn_lines <- grep("Log_vn_size", tmp[,2])
    bind_rows( as_tibble(tmp[1:(log_vn_lines[1]-1), 1:11]),
               as_tibble(tmp[log_vn_lines, c(1, 2, 4:11)]),
               as_tibble(tmp[(log_vn_lines[length(log_vn_lines)]+1):nrow(tmp), 1:11]) ) %>%
      rename_all(~c("parameter_count", "parameter", "estimate", "phase", "lower_bound", "upper_bound", "status",
                    "penalty", "gradient", "standard_error", "est_count")) %>%
      #janitor::clean_names() %>%
      mutate_at(c(1, 3:11), as.numeric) %>% 
      dplyr::select(-status) -> out$parameters; last <- grep("#---", allout[,1])[1]
    out$n_par <- out$parameters %>% filter(phase > 0) %>% nrow
    ## parameters at bounds
    out$parameters %>%
      mutate(range = upper_bound - lower_bound,
             status = ifelse(estimate < (lower_bound+range*0.01), 1,
                             ifelse(estimate > upper_bound-range*0.01, 1, 0))) %>%
      filter(status == 1) %>% dplyr::select(-range, -status) -> out$parameters_at_bounds
    
    # max gradient ----
    out$max_gradient <- max(abs(out$parameters$gradient), na.rm = T)
    
    # reference points ----
    
    ## ref tibble
    tmp <- matrix(ncol = 3, nrow = length((last + 2):(grep("#---", allout[,1])[2] - 5)))
    for(i in 1:nrow(tmp)) {
      tmp[i,] <- na.omit(as.numeric(allout[last + 2 + i,]))
    }
    as_tibble(tmp) %>%
      mutate(ref = c("Male_spr_rbar", "Female_spr_rbar", "SSSB/R_F=0", "BMSY", "Bcurr/BMSY", "OFL_tot",
                     paste0("Fmsy_", 1:out$n_fleets), paste0("Fofl_", 1:out$n_fleets), 
                     paste0("OFL_", 1:out$n_fleets))) %>%
      transmute(parameter_name = ref, estimate = V1, se = V2, est_quantity_count = V3) -> out$reference_points
    out$mmb_curr <- prod(out$reference_points$estimate[out$reference_points$parameter_name %in% c("BMSY", "Bcurr/BMSY")])
    out$ofl_tot <- out$reference_points$estimate[out$reference_points$parameter_name %in% c("OFL_tot")]
    out$bmsy <- out$reference_points$estimate[out$reference_points$parameter_name %in% c("BMSY")]
    out$b_bmsy <- out$reference_points$estimate[out$reference_points$parameter_name %in% c("Bcurr/BMSY")]
    out$f_msy_tot <- sum(out$reference_points$estimate[out$reference_points$parameter_name %in% paste0("Fmsy_", 1:out$n_fleets)])
    out$f_ofl_tot <- sum(out$reference_points$estimate[out$reference_points$parameter_name %in% paste0("Fofl_", 1:out$n_fleets)])
    out$rbar_tot <- sum(out$reference_points$estimate[out$reference_points$parameter_name %in% c("Male_spr_rbar", "Female_spr_rbar")])
    last <- grep("#---", allout[,1])[2] - 2
    ## ref sigma
    allout[last:(last+1),] %>% dplyr::select_if(~ !any(is.na(.))) %>%
      mutate_all(., function(x){gsub(";", "", x)}) %>%
      dplyr::select(-2) %>%
      pivot_wider(names_from = X1, values_from = X3) -> out$ref_sigmaR
    last <- grep("#---", allout[,1])[2]
    
    # overall summary ----
    
    tmp <- matrix(ncol = 15 + (3*out$n_sex) + out$n_fleets, nrow = length(out$mod_yrs))
    for(i in 1:nrow(tmp)) {
      tmp[i,] <- as.numeric(allout[last+3+i,1:ncol(tmp)])
    }
    as_tibble(tmp) %>%
      rename_all(~make.unique(as.character(allout[last+3,1:ncol(tmp)]))) %>%
      janitor::clean_names(.) %>% rename(log_recruits_male = log_recruits, sd_log_recruits_male = sd_log_recruits) -> out$derived_quant_summary
    # do some renaming
    
    if(out$n_sex == 2) {
      out$derived_quant_summary %>%
        rename(log_recruits_female = log_recruits_1, sd_log_recruits_female = sd_log_recruits_1) -> out$derived_quant_summary
    }
    last <- grep("#---", allout[,1])[3]
    
    # mean wt ----
    
    ## weight at size matrix
    tmp <- matrix(nrow = out$n_sex*out$n_maturity*length(out$mod_yrs), ncol = length(out$size_mid_points)+3)
    for(i in 1:nrow(tmp)) {
      tmp[i,] <- as.character(allout[last+4,1:ncol(tmp)])
    }
    as_tibble(tmp) %>%
      mutate_at(2:ncol(.), as.numeric) %>%
      rename_all(~c("sex", "maturity", "year", out$size_mid_points)) %>%
      pivot_longer(4:ncol(.), names_to = "size", values_to = "wt") %>%
      mutate(size = as.numeric(size)) -> out$wt_at_size
    last <- last + 4 + nrow(tmp)
    
    # maturity vector ----
    
    out$maturity_at_size_vector <- as.numeric(allout[last+2,1:length(out$size_mid_points)]); last <- grep("#---", allout[,1])[4]
    
    # catch fit summary ----
    
    ## catch summary
    tmp <- matrix(nrow = length((last+3):(grep("#---", allout[,1])[5]-4)), ncol = 14)
    for(i in 1:nrow(tmp)){
      tmp[i,] <- as.character(allout[last+2+i,1:14])
    }
    as_tibble(tmp) %>%
      mutate_at(c(1:2, 4, 6:7, 10:14), as.numeric) %>%
      rename_all(~c("series", "year", "fleet", "season", "sex", "obs_catch", "cv", "type", "units", "mult", "effort", "disc_m", 
                    "pred_catch", "residual")) -> out$catch_fit_summary; last <- grep("#---", allout[,1])[5]-3
    ## catch q
    tibble(series = unique(out$catch_fit_summary$series),
           log_q = as.numeric(allout[last+2,1:length(unique(out$catch_fit_summary$series))])) -> out$log_q_catch; last <- grep("#---", allout[,1])[5]
    
    # index fix_summary ----
    
    ## index summary
    tmp <- matrix(nrow = length((last+3):(grep("sdnr_MAR_cpue", allout[,1])-2)), ncol = 13)
    for(i in 1:nrow(tmp)){
      tmp[i,] <- as.character(allout[last+2+i,1:13])
    }
    as_tibble(tmp) %>%
      mutate_at(c(1:2, 4, 7:9, 11:13), as.numeric) %>%
      rename_all(~c("series", "year", "fleet", "season", "sex", "maturity", "obs_index", "obs_cv", 
                    "tot_cv", "units", "q", "timing", "pred_index")) -> out$index_fit_summary; last <- last + nrow(tmp) + 2
    ## sdnr_MAR_cpue
    tmp <- matrix(nrow = length(unique(out$index_fit_summary$series)), ncol = 2)
    for(i in 1:nrow(tmp)) {
      tmp[i,] <- as.numeric(allout[last+3, 1:2])
    }
    out$sdnr_MAR_cpue <- tmp; last <- grep("#---", allout[,1])[6]
    
    # size composition fit summary ----
    
    ## size composition fit summary
    ## get info first
    tmp <- matrix(ncol = 12, nrow = length((last+3):(grep("#----", allout[,1])[7]-2)))
    for(i in 1:nrow(tmp)) {
      tmp[i,] <- as.character(allout[last+2+i, 1:ncol(tmp)])
    }
    as_tibble(tmp) %>%
      rename_all(~c("org_series", "mod_series", "year", "fleet", "season", "sex", "type", "shell", "maturity","shell_name", "maturity_name", "nsamp_obs")) %>%
      mutate_at(c(1:3, 5, 12), as.numeric) -> tmp
    
    # save info for neff below
    tmp_neff <- tmp
    
    ## get comps
    last <- last + 2 # set last to start where the data is
    tmp %>%
      nest_by(mod_series, .keep = T) %>% ungroup() %>% 
      mutate(row = purrr::map_dbl(data, ~nrow(.)),
             row = lag(row),
             row = cumsum(ifelse(is.na(row), 0, row)) + last) %>% 
      mutate(comps = purrr::map2(data, row, function(data, row) {
        
        comp_tmp <- matrix(ncol = ncol(allout)-ncol(tmp), nrow = nrow(data))
        for(i in 1:nrow(comp_tmp)) {
          comp_tmp[i,] <- as.numeric(allout[row + i, 13:ncol(allout)])
        }
        as_tibble(comp_tmp) %>%
          dplyr::select(where(function(x)!all(is.na(x)))) -> comp_tmp
        if((ncol(comp_tmp)/out$n_size_bins) <= 2) {comp_agg <- F} else{comp_agg <- T}
        if(comp_agg == F){
          
          comp_tmp %>% 
            rename_all(~c(paste0("obs_", out$size_mid_points[1:(ncol(comp_tmp)/2)]), paste0("pred_", out$size_mid_points[1:(ncol(comp_tmp)/2)]))) %>%
            bind_cols(data, .) %>%
            pivot_longer((ncol(data)+1):ncol(.), names_to = "group", values_to = "prop") %>% 
            separate_wider_delim(group, "_", names_sep = "split") %>%
            pivot_wider(names_from = groupsplit1, values_from = prop) %>%
            rename(size = groupsplit2) %>%
            transmute(org_series, mod_series, year, fleet, season, sex, type, shell, maturity, size = as.numeric(size), 
                      nsamp_obs, obs = as.numeric(obs), pred = as.numeric(pred), residual = obs - pred) -> comp_out
        }
        if(comp_agg == T){
          nobs <- ncol(comp_tmp)/2
          group <- rep(1:50, each = out$n_size_bins)[1:nobs] # << probably a more elegant way to do this...
          comp_tmp %>% 
            rename_all(~c(paste0("obs_", group, "_", as.numeric(matrix(out$size_mid_points, ncol = nobs))), paste0("pred_", group, "_", as.numeric(matrix(out$size_mid_points, ncol = nobs))))) %>%
            bind_cols(data, .) %>%
            pivot_longer((ncol(data)+1):ncol(.), names_to = "group", values_to = "prop") %>% 
            separate_wider_delim(group, "_", names_sep = "split") %>%
            pivot_wider(names_from = groupsplit1, values_from = prop) %>%
            rename(size = groupsplit3, aggregate_series = groupsplit2) %>%
            transmute(org_series, mod_series, aggregate_series = as.numeric(aggregate_series), year, fleet, season, sex, type, shell, maturity, size = as.numeric(size),
                      nsamp_obs, obs = as.numeric(obs), pred = as.numeric(pred), residual = obs - pred) -> comp_out
        }
        
        return(comp_out)
        
      })) %>% transmute(comps) %>% unnest -> out$size_fit_summary
    
    ## sample size
    last <- grep("Sample_size_multipliers", allout[,1])
    tmp <- matrix(ncol = 5, nrow = (grep("sdnr_MAR_lf", allout[,1])-4) - last)
    for(i in 1:nrow(tmp)) {
      tmp[i,] <- as.numeric(allout[last+1+i, 1:5])
    }
    as_tibble(tmp) %>%
      rename_all(~c("mod_series", "row", "multiplier", "nsamp_est", "nsamp_obs")) -> out$effective_sample_size
    
    ## add estimated effectve sample size to output
    out$size_fit_summary %>%
      left_join(out$effective_sample_size %>% 
                  transmute(year = tmp_neff$year, mod_series, multiplier, nsamp_est, nsamp_obs),
                by = join_by(year, mod_series, nsamp_obs)) -> out$size_fit_summary
    
    ## sdnr_MAR_lf
    last <- grep("sdnr_MAR_lf", allout[,1])
    tmp <- matrix(ncol = 2, nrow = length(unique(out$size_fit_summary$mod_series)))
    for(i in 1:nrow(tmp)) {
      tmp[i,] <- as.numeric(allout[last+i, 1:2])
    }
    out$sdnr_MAR_lf <- tmp
    last <- grep("Francis_weights", allout[,1])
    ## francis weights
    out$francis_weights <- as.numeric(allout[last+1, 1:length(unique(out$size_fit_summary$mod_series))]); last <- grep("#---", allout[,1])[8]
    
    # selectivity ----
    
    ## selex
    tmp <- matrix(ncol = 3 + length(out$size_mid_points), nrow = length(out$mod_yrs) * out$n_sex * out$n_fleets)
    for(i in 1:nrow(tmp)) {
      tmp[i,] <- as.character(allout[last+3+i, 1:ncol(tmp)])
    }
    as_tibble(tmp) %>%
      rename_all(~c("year", "sex", "fleet", out$size_mid_points)) %>%
      pivot_longer(4:ncol(.), values_to = "slx_capture", names_to = "size") %>%
      mutate_at(c(1, 4, 5), as.numeric) -> slx_cap; last <- last + 3 + nrow(tmp)
    ## retention
    tmp <- matrix(ncol = 3 + length(out$size_mid_points), nrow = length(out$mod_yrs) * out$n_sex * out$n_fleets)
    for(i in 1:nrow(tmp)) {
      tmp[i,] <- as.character(allout[last+2+i, 1:ncol(tmp)])
    }
    as_tibble(tmp) %>%
      rename_all(~c("year", "sex", "fleet", out$size_mid_points)) %>%
      pivot_longer(4:ncol(.), values_to = "slx_retention", names_to = "size") %>%
      mutate_at(c(1, 4, 5), as.numeric) -> slx_ret; last <- last + 2 + nrow(tmp)
    ## discard
    tmp <- matrix(ncol = 3 + length(out$size_mid_points), nrow = length(out$mod_yrs) * out$n_sex * out$n_fleets)
    for(i in 1:nrow(tmp)) {
      tmp[i,] <- as.character(allout[last+2+i, 1:ncol(tmp)])
    }
    as_tibble(tmp) %>%
      rename_all(~c("year", "sex", "fleet", out$size_mid_points)) %>%
      pivot_longer(4:ncol(.), values_to = "slx_discard", names_to = "size") %>%
      mutate_at(c(1, 4, 5), as.numeric) -> slx_disc#; last <- grep("Select_control", allout[,2])
    # ## slx control
    # as_tibble(allout[(last+2):(grep("#----", allout[,1])[8]-1), 1:11]) %>%
    #   mutate_all(as.numeric) %>%
    #   rename_all(~c("gear", "par", "phase", "start_yr", "end_yr", "env_link",
    #                 "link_par", "rand_walk", "re_start_yr", "re_end_yr", "re_sigma")) %>%
    #   # add sex wherever there is two selectivity functions with the same gear and block - Andre should change the output back to what I had at some point
    #   group_by(gear, start_yr, end_yr) %>% 
    #   mutate(sex = c("male", "female")[row_number()]) %>% ungroup() %>%
    #   transmute(gear, sex, par, phase, start_yr, end_yr, env_link, link_par, rand_walk, re_start_yr, re_end_yr, re_sigma) -> out$slx_control
    # 
    # out$slx_control %>%
    #   mutate(fleet = out$fleet_names[abs(as.numeric(.$gear))],
    #          type = ifelse(gear > 0, "capture", "retention")) %>%
    #   distinct(fleet, type, sex, start_yr, end_yr) %>%
    #   mutate(start_yr = ifelse(start_yr == 0, out$yr_range[1], start_yr),
    #          end_yr = ifelse(start_yr == 0, out$yr_range[2], end_yr),
    #          year = purrr::map2(start_yr, end_yr, function(start_yr, end_yr) {start_yr:end_yr})) %>%
    #   unnest(year) %>%
    #   mutate(block = paste(start_yr, "-", end_yr)) %>%
    #   dplyr::select(-start_yr, -end_yr) -> tmp
    # slx_cap %>%
    #   left_join(tmp %>%
    #               filter(type == "capture") %>%
    #               transmute(year, fleet, sex, capture_block = block),
    #             by = join_by(year, sex, fleet)) %>%
    #   left_join(slx_ret %>%
    #               left_join(tmp %>%
    #                           filter(type == "retention") %>%
    #                           transmute(year, fleet, sex, ret_disc_block = block),
    #                         by = join_by(year, sex, fleet)),
    #             by = join_by(year, sex, fleet, size)) %>%
    #   left_join(slx_disc, by = join_by(year, sex, fleet, size)) %>%
    #   transmute(year, sex, fleet, size, slx_capture, capture_block, slx_retention,
    #             slx_discard, ret_disc_block) -> out$selectivity
    
    slx_cap %>% left_join(slx_ret, by = join_by(year, sex, fleet, size)) %>% left_join(slx_disc, by = join_by(year, sex, fleet, size)) -> out$selectivity
    last <- grep("#----", allout[,1])[9]
    
    # mortality ----
    
    ## M by season
    tmp <- matrix(nrow = length(out$mod_yrs), ncol = 1 + out$n_season)
    for(i in 1:nrow(tmp)){
      tmp[i,] <- as.numeric(allout[last + 4 + i, 1:ncol(tmp)])
    }
    as_tibble(tmp) %>%
      rename_all(~c("year", paste0("season_", 1:out$n_season))) -> out$proportion_M_by_season; last <- last + nrow(tmp) + 4
    
    ## M by sex-maturity-size class
    tmp <- matrix(nrow = length(out$mod_yrs) * out$n_sex * out$n_maturity, ncol = 3 + length(out$size_mid_points))
    for(i in 1:nrow(tmp)){
      tmp[i,] <- as.character(allout[last+3+i, 1:ncol(tmp)])
    }
    as_tibble(tmp) %>% 
      rename_all(~c("year", "sex", "maturity", out$size_mid_points)) %>%
      pivot_longer(4:ncol(.), names_to = "size", values_to = "M") %>%
      mutate_at(c(1, 4:5), as.numeric) -> out$M_by_class; last <- last + nrow(tmp) + 3
    
    ## fully selected F by season, sex, fleet
    tmp <- matrix(nrow = out$n_sex * out$n_fleets * length(out$mod_yrs), ncol = 3 + out$n_season)
    for(i in 1:nrow(tmp)){
      tmp[i,] <- as.character(allout[last+4+i, 1:ncol(tmp)])
    }
    as_tibble(tmp) %>%
      rename_all(~c("sex", "fleet", "year", 1:out$n_season)) %>%
      mutate_at(c(3:ncol(.)), as.numeric) %>% 
      pivot_longer(4:ncol(.), names_to = "season", values_to = "F") -> out$F_by_sex_fleet_year_season;last <- last + nrow(tmp) + 4
    
    ## fully selected F by season, sex, fleet
    ## skip same as above
    last <- last + nrow(out$F_by_sex_fleet_year_season)/out$n_fleets + 4
    
    ## F by sex, year, season, size
    tmp <- matrix(nrow = out$n_sex * length(out$mod_yrs) * out$n_season, ncol = 3 + length(out$size_mid_points))
    for(i in 1:nrow(tmp)){
      tmp[i,] <- as.character(allout[last+4+i, 1:ncol(tmp)])
    }
    as_tibble(tmp) %>%
      rename_all(~c("sex", "year", "season", out$size_mid_points)) %>%
      pivot_longer(4:ncol(.), names_to = "size", values_to = "F_continuous") %>% 
      mutate_at(2:ncol(.), as.numeric) -> cont; last <- last + nrow(tmp) + 4
    tmp <- matrix(nrow = out$n_sex * length(out$mod_yrs) * out$n_season, ncol = 3 + length(out$size_mid_points))
    for(i in 1:nrow(tmp)){
      tmp[i,] <- as.character(allout[last+4+i, 1:ncol(tmp)])
    }
    as_tibble(tmp) %>%
      rename_all(~c("sex", "year", "season", out$size_mid_points)) %>%
      pivot_longer(4:ncol(.), names_to = "size", values_to = "F_discrete") %>%
      mutate_at(2:ncol(.), as.numeric) -> disc; last <- last + nrow(tmp) + 4
    out$F_by_sex_maturity_year_season_size <- left_join(cont, disc, by = join_by(sex, year, season, size))
    
    ## Z by sex, year, maturity, season, size
    tmp <- matrix(nrow = out$n_sex * out$n_maturity * length(out$mod_yrs) * out$n_season, ncol = 4 + length(out$size_mid_points))
    for(i in 1:nrow(tmp)){
      tmp[i,] <- as.character(allout[last+4+i, 1:ncol(tmp)])
    }
    as_tibble(tmp) %>%
      rename_all(~c("sex", "maturity", "year", "season", out$size_mid_points)) %>%
      pivot_longer(5:ncol(.), names_to = "size", values_to = "Z_continuos") %>%
      mutate_at(3:ncol(.), as.numeric) -> cont; last <- last + nrow(tmp) + 4 
    for(i in 1:nrow(tmp)){
      tmp[i,] <- as.character(allout[last+4+i, 1:ncol(tmp)])
    }
    as_tibble(tmp) %>%
      rename_all(~c("sex", "maturity", "year", "season", out$size_mid_points)) %>%
      pivot_longer(5:ncol(.), names_to = "size", values_to = "Z_discrete") %>%
      mutate_at(3:ncol(.), as.numeric) %>%
      mutate(maturity = case_when(maturity == 1 ~ "mature",
                                  maturity == 2 ~ "immature",
                                  maturity == 0 ~ "undetermined")) -> disc; last <- grep("#---", allout[,1])[10]
    out$Z_by_sex_maturity_year_season_size <- left_join(cont, disc, by = join_by(sex, maturity, year, season, size))
    
    # n matrix ----
    
    ## n matrix by sex and maturity
    nmats <- grep("#N(.)", apply(allout[(grep("#---", allout[,1])[10]+2):grep("sex_maturity_shell_con", allout[,1]),], 1, str_flatten, na.rm = T), value = T)
    nmats_index <- as.numeric(names(nmats))
    
    list_tmp <- list()
    for(m in 1:length(nmats)) {
      tmp <- matrix(nrow = length(out$mod_yrs)+1, ncol = 1 + length(out$size_mid_points))
      for(i in 1:nrow(tmp)) {
        tmp[i,] <- as.numeric(allout[nmats_index[m]+2+i, 1:ncol(tmp)])
      }
      as_tibble(tmp) %>% rename_all(~c("year", out$size_mid_points)) %>%
        mutate(type = tolower(gsub("-", "_", gsub("#N[(]|[)]|#", "", nmats[m])))) -> list_tmp[[m]]
    }
    bind_rows(list_tmp) %>%
      filter(!is.na(year)) %>%
      pivot_longer(2:(ncol(.)-1), names_to = "size", values_to = "n") %>%
      pivot_wider(names_from = type, values_from = n) -> out$n_matrix
    
    
    ## n matrix by sex, maturity and shell condition
    nmats <- grep("#N(.)", apply(allout[(grep("sex_maturity_shell_con", allout[,1])):grep("#---", allout[,1])[11],], 1, str_flatten, na.rm = T), value = T)
    nmats_index <- as.numeric(names(nmats))    
    
    list_tmp <- list()
    for(m in 1:length(nmats)) {
      tmp <- matrix(nrow = length(out$mod_yrs)+1, ncol = 1 + length(out$size_mid_points))
      for(i in 1:nrow(tmp)) {
        tmp[i,] <- as.numeric(allout[nmats_index[m]+i, c(1, 5:(ncol(tmp)+3))])
      }
      as_tibble(tmp) %>% rename_all(~c("year", out$size_mid_points)) %>%
        mutate(type = tolower(gsub("-", "_", gsub("#N[(]|[)]|#", "", nmats[m])))) -> list_tmp[[m]]
    }
    bind_rows(list_tmp) %>%
      filter(!is.na(year)) %>%
      pivot_longer(2:(ncol(.)-1), names_to = "size", values_to = "n") %>%
      pivot_wider(names_from = type, values_from = n) %>% 
      right_join(out$n_matrix, ., by = join_by(year, size)) -> out$n_matrix
    
    last <- grep("#---", allout[,1])[11]
    
    
    # growth ----
    
    ## molt probability
    tmp <- matrix(nrow = out$n_sex * length(out$mod_yrs), ncol = 2 + length(out$size_mid_points))
    for(i in 1:nrow(tmp)) {
      tmp[i,] <- as.character(allout[last+3+i,1:ncol(tmp)])
    }
    as_tibble(tmp) %>% 
      rename_all(~c("sex", "year", out$size_mid_points)) %>%
      pivot_longer(3:ncol(.), names_to = "size", values_to = "molt_probability") %>%
      mutate_at(2:ncol(.), as.numeric) -> out$molt_probability; last <- last + 3 + nrow(tmp)
    
    ## growth transition matrix
    gmats <- grep("#growth_matrix", allout[,1]) + 1; last <- gmats[1]
    gmats_names <- lapply(apply(allout[gmats,], 1, str_flatten, na.rm = T), function(x){strsplit(x, ":")[[1]][2]}) %>% unlist
    list_tmp <- list()
    for(m in 1:length(gmats)) {
      tmp <- matrix(nrow = length(out$size_mid_points), ncol = length(out$size_mid_points))
      for(i in 1:nrow(tmp)) {
        tmp[i,] <- as.numeric(allout[gmats[m]+i, 1:ncol(tmp)])
      }
      row.names(tmp) <- out$size_mid_points; colnames(tmp) <- out$size_mid_points
      list_tmp[[m]] <- tmp
    }
    names(list_tmp) <- gmats_names
    out$growth_transition <- list_tmp
    
    ## size transition matrix
    smats <- grep("#size_matrix", allout[,1]) + 1; last <- gmats[1]
    smats_names <- lapply(apply(allout[smats,], 1, str_flatten, na.rm = T), function(x){strsplit(x, ":")[[1]][2]}) %>% unlist
    list_tmp <- list()
    for(m in 1:length(smats)) {
      tmp <- matrix(nrow = length(out$size_mid_points), ncol = length(out$size_mid_points))
      for(i in 1:nrow(tmp)) {
        tmp[i,] <- as.numeric(allout[smats[m]+i, 1:ncol(tmp)])
      }
      row.names(tmp) <- out$size_mid_points; colnames(tmp) <- out$size_mid_points
      list_tmp[[m]] <- tmp
    }
    names(list_tmp) <- smats_names
    out$size_transition <- list_tmp
    last <- grep("#---", allout[,1])[12]
    
    
    
    
    # reference points ----
    
    # combinations of seasons and fleets with Fs
    tmp <- matrix(nrow = out$n_season, ncol = out$n_fleets + 1)
    for(i in 1:nrow(tmp)){
      tmp[i,] <- as.numeric(allout[last+3+i, 1:ncol(tmp)])
    }
    as_tibble(tmp) %>%
      rename_all(~c("season", out$fleet_names)) -> out$seasons_fleets_w_F
    
    ##reference points and ofl
    out$spr_syr <- as.numeric(allout[grep("spr_syr", allout[,1])+1, 1])
    out$spr_nyr <- as.numeric(allout[grep("spr_nyr", allout[,1])+1, 1])
    out$spr_rbar <- as.numeric(allout[grep("spr_rbar", allout[,1])+1, 1:2])
    out$proj_rbar <- as.numeric(allout[grep("proj_rbar", allout[,1])+1, 1:2])
    out$spr_sexr <- as.numeric(allout[grep("spr_sexr", allout[,1])+1, 1])
    out$SR_alpha_prj <- as.numeric(allout[grep("SR_alpha_prj", allout[,1])+1, 1])
    out$SR_beta_prj <- as.numeric(allout[grep("SR_beta_prj", allout[,1])+1, 1])
    out$spr_fofl <- as.numeric(allout[grep("spr_fofl", allout[,1])+1, 1])
    out$spr_cofl_ret <- as.numeric(allout[grep("spr_cofl_ret", allout[,1])+1, 1])
    last <- grep("#---", allout[,1])[15]
    
    # simple likelihood stuff ----
    max(c(length(unique(out$catch_fit_summary$series)),
          length(unique(out$index_fit_summary$series)),
          length(unique(out$size_fit_summary$series)))) -> cols
    tmp <- matrix(nrow = 5, ncol = cols + 1)
    for(i in 1:nrow(tmp)){
      tmp[i,] <- as.numeric(allout[last+1+i, 1:ncol(tmp)])
    }
    out$nloglike <- tmp[,-1]
    out$nlogPenalty <- as.numeric(na.omit(as.numeric(allout[grep("nlogPenalty", allout[,1])+1,])))
    out$priorDensity <- as.numeric(na.omit(as.numeric(allout[grep("priorDensity", allout[,1])+1,])))
    
    # objective function ----
    out$objective_function <- out$likelihoods_by_type %>% filter(process == "total") %>% pull(net_lik)
  }
  
  # output ----
  return(out)
}


# gmacs_read_dat() ----

## read .dat file

## args:
### dat_file - file path to .dat
### model_name - character string to save as object in output, later to be used for plot legends. example: "23.1b"
### version - GMACS version, default latest

## output: list object
## example: gmacs_read_dat(dat_file = "./AIGKC/models/2024/may/EAG/23.1b/EAG_23_1b.dat", model_name = "23.1b")
## version: character string denoting gmacs version

gmacs_read_dat <- function(dat_file, model_name = NULL, version = NULL) {
  
  if(is.null(version)){version = "2.20.16"}
  if(version == "2.20.16"){
    # setup ---- 
    
    # Suppress the NA message in the coercion to double
    options(warn = -1) 
    
    # read text file
    dat <- read.delim(dat_file, sep = "", header = F, col.names = c(1:1000), fill = T, na.strings = "", colClasses = "character")
    # create out object
    out <- list()
    
    # get only data that rows start with a number
    dat_alpha <- filter(dat, grepl("^[A-Za-z]", dat[,1]))
    dat <- filter(dat, !is.na(as.numeric(dat[,1])))
    
    # model dimensions ----
    ## version 
    out$version <- version
    ## model_name 
    out$model_name <- model_name
    ## start year
    out$start_year <- as.numeric(dat[1, 1]); last <- 2
    ## terminal year
    out$terminal_year <- as.numeric(dat[last, 1]); last <- last + 1
    ## projection year
    ### need to add...
    ## number of seasons
    out$n_season <- as.numeric(dat[last, 1]); last <- last + 1
    ## number of fleets
    out$n_fleets <-  as.numeric(dat[last, 1]); last <- last + 1
    ## number of sexes
    out$n_sex <-  as.numeric(dat[last, 1]); last <- last + 1
    ## number of shell condition types
    out$n_shell <-  as.numeric(dat[last, 1]); last <- last + 1
    ## number of maturity types
    out$n_maturity <-  as.numeric(dat[last, 1]); last <- last + 1
    ## number of size classes
    out$n_size_bins <-  as.numeric(dat[last, 1]); last <- last + 1
    ## season recruitment occurs
    out$recruitment_season <-  as.numeric(dat[last, 1]); last <- last + 1
    ## season molt / growth occurs
    out$growth_season <-  as.numeric(dat[last, 1]); last <- last + 1
    ## season to calc mmb
    out$ssb_season <-  as.numeric(dat[last, 1]); last <- last + 1
    ## season to output N matrix
    out$n_matrix_season <-  as.numeric(dat[last, 1]); last <- last + 1
    ## max size class
    if(out$n_sex == 1) {out$max_size_bin <-  as.numeric(dat[last, 1]); last <- last + 1}
    if(out$n_sex == 2 & length(na.omit(as.numeric(dat[last,]))) > 1){
      out$max_size_bin <- as.numeric(c(dat[last,1], dat[last,2])); last <- last + 1
    }
    if(out$n_sex == 2 & length(na.omit(as.numeric(dat[last,]))) == 1){
      out$max_size_bin[1] <- as.numeric(dat[last, 1]); last <- last + 1
      out$max_size_bin[2] <- as.numeric(dat[last, 1]); last <- last + 1
    }
    ## size bin
    out$size_bins <- as.numeric(dat[last, 1:(out$n_size_bins+1)]); last <- last + 1
    
    # natural mortality taus ----
    
    ## natural mortality per season input
    out$nat_m_input_type <- as.numeric(dat[last, 1])
    tmp <- matrix(nrow = length(out$start_year:out$terminal_year), ncol = out$n_season)
    for(i in 1:nrow(tmp)){
      tmp[i,] <- as.numeric(dat[last+i, 1:out$n_season])
    }
    as_tibble(tmp) %>%
      mutate(year = out$start_year:out$terminal_year) %>%
      dplyr::select(ncol(.), 1:out$n_season) %>%
      rename_all(~c("year", paste("season", 1:out$n_season, sep = "_"))) -> out$tau; last <- last + i
    
    # fleets ----
    
    out$fleet_names <- as.character(na.omit(as.character(t(as.matrix(dat_alpha[,1:99])))))
    
    # season instant vs contin ----
    
    out$season_type <- as.numeric(dat[last+1, 1:out$n_season]); last <- last + 1
    
    # catch data ----
    
    ## input format
    out$catch_input_format <- as.numeric(dat[last+1, 1]); last <- last + 1
    ## now of catch series
    out$n_catch_series <- as.numeric(dat[last+1, 1]); last <- last + 1
    ## number of rows in each frame
    out$n_catch_rows <- as.numeric(dat[last+1, 1:out$n_catch_series]); last <- last + 1
    ## catch data
    tmp <- matrix(ncol = 11, nrow = sum(out$n_catch_rows))
    for(i in 1:nrow(tmp)){
      tmp[i,] <- as.numeric(dat[last + i, 1:11])
    }
    as_tibble(tmp) %>%
      rename_all(~c("year", "season", "fleet", "sex", "obs", "cv", "type", "units", "mult", "effort", "disc_m")) -> out$catch
    last <- last + nrow(tmp)
    
    
    # index data ----
    
    ## input format
    out$index_input_format <- as.numeric(dat[last+1, 1]); last <- last + 1
    ## number of index series
    out$n_index_series <- as.numeric(dat[last+1, 1]); last <- last + 1
    ## index type
    out$index_type <- as.numeric(dat[last+1, 1:out$n_index_series]); last <- last + 1
    ## number of rows in total
    out$n_index_rows <- as.numeric(dat[last+1, 1]); last <- last + 1
    ## index data
    tmp <- matrix(ncol = 10, nrow = out$n_index_rows)
    for(i in 1:nrow(tmp)){
      tmp[i,] <- as.numeric(dat[last + i, 1:10])
    }
    as_tibble(tmp) %>%
      rename_all(~c("series", "year", "season", "fleet", "sex", "maturity", "obs", "cv", "units", "timing")) -> out$index
    last <- last + nrow(tmp)
    
    # size comps ----
    
    ## input format
    out$size_input_format <- as.numeric(dat[last+1, 1]); last <- last + 1
    ## number of index series
    out$n_size_series <- as.numeric(dat[last+1, 1]); last <- last + 1
    ## number of rows in each series
    out$n_size_rows <- as.numeric(dat[last+1, 1:out$n_size_series]); last <- last + 1
    ## number of bins in each series
    out$n_size_bin_series <- as.numeric(dat[last+1, 1:out$n_size_series]); last <- last + 1
    org_series <- rep(1:out$n_size_series,out$n_size_rows)
    tmp <- matrix(nrow = sum(out$n_size_rows), ncol = 8 + max(out$n_size_bin_series))
    for(i in 1:nrow(tmp)){
      tmp[i,] <- as.numeric(dat[last + i, 1:ncol(tmp)])
    }
    as_tibble(tmp) %>%
      rename_all(~c("year", "season", "fleet", "sex", "type", "shell", "maturity",
                    "nsamp", out$size_bins[-length(out$size_bins)])) %>%
      mutate(org_series = org_series) %>%
      pivot_longer(9:(ncol(.)-1), names_to = "size", values_to = "obs") %>%
      transmute(org_series, year, season, fleet, sex, type, shell, maturity, nsamp, size, obs) -> out$size_comp
    
    last <- last + nrow(tmp)
    
    # growth data ----
    
    ## input format
    out$growth_data_type <- as.numeric(dat[last+1, 1]); last <- last + 1
    ## input format
    out$n_growth_obs <- as.numeric(dat[last+1, 1]); last <- last + 1
    ## growth data
    ### tagging data
    if(out$growth_data_type == 0){out$growth <- NULL}
    if(out$growth_data_type == 1){
      tmp <- matrix(nrow = out$n_growth_obs, ncol = 4)
      for(i in 1:nrow(tmp)){
        tmp[i,] <- as.numeric(dat[last+i, 1:4])
      }
      as_tibble(tmp) %>%
        rename_all(~c("size", "increment", "sex", "cv")) -> out$growth
      last <- last + nrow(tmp)
    }
    if(out$growth_data_type == 3){
      tmp <- matrix(nrow = out$n_growth_obs, ncol = 8)
      for(i in 1:nrow(tmp)){
        tmp[i,] <- as.numeric(dat[last+i, 1:8])
      }
      as_tibble(tmp) %>%
        rename_all(~c("release_class", "sex", "recapture_class", "years_at_liberty", "transition_matrix", 
                      "recapture_fleet", "recapture_year", "nsamp")) -> out$growth
      last <- last + nrow(tmp)
    }
    
    # environmental data ----
    
    ## input format
    out$env_format <- as.numeric(dat[last+1, 1]); last <- last + 1
    ## number of series
    out$n_env_series <- as.numeric(dat[last+1, 1]); last <- last + 1
    ## eof
    if(as.numeric(dat[last+1, 1]) == 9999){return(out)} else{stop("end of file not correct, debug")}
    
    
  }  
  if(version == "2.20.14"){
    # setup ---- 
    
    # Suppress the NA message in the coercion to double
    options(warn = -1) 
    
    # read text file
    dat <- read.delim(dat_file, sep = "", header = F, col.names = c(1:1000), fill = T, na.strings = "", colClasses = "character")
    # create out object
    out <- list()
    
    # get only data that rows start with a number
    dat_alpha <- filter(dat, grepl("^[A-Za-z]", dat[,1]))
    dat <- filter(dat, !is.na(as.numeric(dat[,1])))
    
    # model dimensions ----
    ## version 
    out$version <- version
    ## model_name 
    out$model_name <- model_name
    ## start year
    out$start_year <- as.numeric(dat[1, 1]); last <- 2
    ## terminal year
    out$terminal_year <- as.numeric(dat[last, 1]); last <- last + 1
    ## projection year
    ### need to add...
    ## number of seasons
    out$n_season <- as.numeric(dat[last, 1]); last <- last + 1
    ## number of fleets
    out$n_fleets <-  as.numeric(dat[last, 1]); last <- last + 1
    ## number of sexes
    out$n_sex <-  as.numeric(dat[last, 1]); last <- last + 1
    ## number of shell condition types
    out$n_shell <-  as.numeric(dat[last, 1]); last <- last + 1
    ## number of maturity types
    out$n_maturity <-  as.numeric(dat[last, 1]); last <- last + 1
    ## number of size classes
    out$n_size_bins <-  as.numeric(dat[last, 1]); last <- last + 1
    ## season recruitment occurs
    out$recruitment_season <-  as.numeric(dat[last, 1]); last <- last + 1
    ## season molt / growth occurs
    out$growth_season <-  as.numeric(dat[last, 1]); last <- last + 1
    ## season to calc mmb
    out$mmb_season <-  as.numeric(dat[last, 1]); last <- last + 1
    ## season to output N matrix
    out$n_matrix_season <-  as.numeric(dat[last, 1]); last <- last + 1
    ## max size class
    if(out$n_sex == 1) {out$max_size_bin <-  as.numeric(dat[last, 1]); last <- last + 1}
    if(out$n_sex == 2 & length(na.omit(as.numeric(dat[last,]))) > 1){
      out$max_size_bin <- as.numeric(c(dat[last,1], dat[last,2])); last <- last + 1
    }
    if(out$n_sex == 2 & length(na.omit(as.numeric(dat[last,]))) == 1){
      out$max_size_bin[1] <- as.numeric(dat[last, 1]); last <- last + 1
      out$max_size_bin[2] <- as.numeric(dat[last, 1]); last <- last + 1
    }
    ## size bin
    out$size_bins <- as.numeric(dat[last, 1:(out$n_size_bins+1)]); last <- last + 1
    
    # natural mortality taus ----
    
    ## natural mortality per season input
    out$nat_m_input_type <- as.numeric(dat[last, 1])
    tmp <- matrix(nrow = length(out$start_year:out$terminal_year), ncol = out$n_season)
    for(i in 1:nrow(tmp)){
      tmp[i,] <- as.numeric(dat[last+i, 1:out$n_season])
    }
    as_tibble(tmp) %>%
      mutate(year = out$start_year:out$terminal_year) %>%
      dplyr::select(ncol(.), 1:out$n_season) %>%
      rename_all(~c("year", paste("season", 1:out$n_season, sep = "_"))) -> out$tau; last <- last + i
    
    # fleets ----
    
    out$fleet_names <- as.character(na.omit(as.character(as.matrix(dat_alpha[,1:6]))))
    
    # season instant vs contin ----
    
    out$season_type <- as.numeric(dat[last+1, 1:out$n_season]); last <- last + 1
    
    # catch data ----
    
    ## input format
    out$catch_input_format <- as.numeric(dat[last+1, 1]); last <- last + 1
    ## now of catch series
    out$n_catch_series <- as.numeric(dat[last+1, 1]); last <- last + 1
    ## number of rows in each frame
    out$n_catch_rows <- as.numeric(dat[last+1, 1:out$n_catch_series]); last <- last + 1
    ## catch data
    tmp <- matrix(ncol = 11, nrow = sum(out$n_catch_rows))
    for(i in 1:nrow(tmp)){
      tmp[i,] <- as.numeric(dat[last + i, 1:11])
    }
    as_tibble(tmp) %>%
      rename_all(~c("year", "season", "fleet", "sex", "obs", "cv", "type", "units", "mult", "effort", "disc_m")) -> out$catch
    last <- last + nrow(tmp)
    
    
    # index data ----
    
    ## input format
    out$index_input_format <- as.numeric(dat[last+1, 1]); last <- last + 1
    ## number of index series
    out$n_index_series <- as.numeric(dat[last+1, 1]); last <- last + 1
    ## index type
    out$index_type <- as.numeric(dat[last+1, 1:out$n_index_series]); last <- last + 1
    ## number of rows in total
    out$n_index_rows <- as.numeric(dat[last+1, 1]); last <- last + 1
    ## index data
    tmp <- matrix(ncol = 10, nrow = out$n_index_rows)
    for(i in 1:nrow(tmp)){
      tmp[i,] <- as.numeric(dat[last + i, 1:10])
    }
    as_tibble(tmp) %>%
      rename_all(~c("series", "year", "season", "fleet", "sex", "maturity", "obs", "cv", "units", "timing")) -> out$index
    last <- last + nrow(tmp)
    
    # size comps ----
    
    ## input format
    out$size_input_format <- as.numeric(dat[last+1, 1]); last <- last + 1
    ## number of index series
    out$n_size_series <- as.numeric(dat[last+1, 1]); last <- last + 1
    ## number of rows in each series
    out$n_size_rows <- as.numeric(dat[last+1, 1:out$n_size_series]); last <- last + 1
    ## number of bins in each series
    out$n_size_bin_series <- as.numeric(dat[last+1, 1:out$n_size_series]); last <- last + 1
    org_series <- rep(1:out$n_size_series,out$n_size_rows)
    tmp <- matrix(nrow = sum(out$n_size_rows), ncol = 8 + max(out$n_size_bin_series))
    for(i in 1:nrow(tmp)){
      tmp[i,] <- as.numeric(dat[last + i, 1:ncol(tmp)])
    }
    as_tibble(tmp) %>%
      rename_all(~c("year", "season", "fleet", "sex", "type", "shell", "maturity",
                    "nsamp", out$size_bins[-length(out$size_bins)])) %>%
      mutate(org_series = org_series) %>%
      pivot_longer(9:(ncol(.)-1), names_to = "size", values_to = "obs") %>%
      transmute(org_series, year, season, fleet, sex, type, shell, maturity, nsamp, size, obs) -> out$size_comp
    
    last <- last + nrow(tmp)
    
    # growth data ----
    
    ## input format
    out$growth_data_type <- as.numeric(dat[last+1, 1]); last <- last + 1
    ## input format
    out$n_growth_obs <- as.numeric(dat[last+1, 1]); last <- last + 1
    ## growth data
    ### tagging data
    if(out$growth_data_type == 0){out$growth <- NULL}
    if(out$growth_data_type == 1){
      tmp <- matrix(nrow = out$n_growth_obs, ncol = 4)
      for(i in 1:nrow(tmp)){
        tmp[i,] <- as.numeric(dat[last+i, 1:4])
      }
      as_tibble(tmp) %>%
        rename_all(~c("size", "increment", "sex", "cv")) -> out$growth
      last <- last + nrow(tmp)
    }
    if(out$growth_data_type == 3){
      tmp <- matrix(nrow = out$n_growth_obs, ncol = 8)
      for(i in 1:nrow(tmp)){
        tmp[i,] <- as.numeric(dat[last+i, 1:8])
      }
      as_tibble(tmp) %>%
        rename_all(~c("release_class", "sex", "recapture_class", "years_at_liberty", "transition_matrix", 
                      "recapture_fleet", "recapture_year", "nsamp")) -> out$growth
      last <- last + nrow(tmp)
    }
    
    # environmental data ----
    
    ## number of series
    out$n_env_series <- as.numeric(dat[last+1, 1]); last <- last + 1
    ## eof
    if(as.numeric(dat[last+1, 1]) == 9999){return(out)} else{stop("end of file not correct, debug")}
    
    
  }  
  
}

# gmacs_read_ctl() ----

## read .ctl file ### MAY BE BUGGY YET

## args:
### ctl_file - file path to .ctl
### dat_file - file path to .dat
### version - GMACS version, default latest

## output: list object
## example: gmacs_read_dat(ctl_file = "./AIGKC/models/2024/may/EAG/23.1b/EAG_23_1b.ctl", dat_file = "./AIGKC/models/2024/may/EAG/23.1b/EAG_23_1b.dat", model_name = "23.1b")
## version: character string denoting gmacs version

gmacs_read_ctl <- function(ctl_file, dat_file, version = NULL){
  
  if(is.null(version)){version = "2.20.16"}
  if(version == "2.20.16"){
    # setup ---- 
    
    # Suppress the NA message in the coercion to double
    options(warn = -1) 
    
    # read text file
    ctl <- read.delim(ctl_file, sep = "", header = F, col.names = c(1:1000), fill = T, na.strings = "", 
                      colClasses = "character", comment.char = "#")
    dat <- gmacs_read_dat(dat_file) # read data file for sex, shell, fleet information
    # create out object
    out <- list()
    last <- 0
    
    # block structure ----
    
    n_block_groups <- as.numeric(ctl[last + 1, 1]); last <- last + 1
    blocks_per_group <- NULL
    for(i in 1:n_block_groups){
      blocks_per_group[i] <- as.numeric(ctl[last + i, 1])
    }; last <- last + n_block_groups
    blocks <- matrix(nrow = sum(blocks_per_group), ncol = 2)
    for(i in 1:sum(blocks_per_group)){
      blocks[i,1:2] <- as.numeric(ctl[last + i, 1:2])
    }; last <- last + nrow(blocks)
    # out
    tibble(block_group = rep(1:n_block_groups, blocks_per_group),
           l_blk = blocks[,1],
           u_blk = blocks[,2]) -> out$block_structure
    
    # other controls ----
    
    out$first_yr_rec <- as.numeric(ctl[last + 1, 1]); last <- last + 1
    out$last_yr_rec <- as.numeric(ctl[last + 1, 1]); last <- last + 1
    out$terminal_molt <- as.numeric(ctl[last + 1, 1]); last <- last + 1
    out$rec_phase <- as.numeric(ctl[last + 1, 1]); last <- last + 1
    out$rec_sex_ratio_phase <- as.numeric(ctl[last + 1, 1]); last <- last + 1
    out$init_sex_ratio <- as.numeric(ctl[last + 1, 1]); last <- last + 1
    out$init_conditions_type <- as.numeric(ctl[last + 1, 1]); last <- last + 1
    out$ref_size_class <- as.numeric(ctl[last + 1, 1]); last <- last + 1
    out$spr_lambda <- as.numeric(ctl[last + 1, 1]); last <- last + 1
    out$spr_type <- as.numeric(ctl[last + 1, 1]); last <- last + 1
    out$use_yr_avg_sex_ratio <- as.numeric(ctl[last + 1, 1]); last <- last + 1
    out$spr_yrs_equil <- as.numeric(ctl[last + 1, 1]); last <- last + 1
    out$dev_phase <- as.numeric(ctl[last + 1, 1]); last <- last + 1
    out$first_bias_correction <- as.numeric(ctl[last + 1, 1]); last <- last + 1
    out$first_full_bias_correction <- as.numeric(ctl[last + 1, 1]); last <- last + 1
    out$last_full_bias_correction <- as.numeric(ctl[last + 1, 1]); last <- last + 1
    out$last_bias_correction <- as.numeric(ctl[last + 1, 1]); last <- last + 1
    
    # thetas ----
    
    if(out$init_conditions_type == 2){
      tmp <- matrix(nrow = 3 + (2 * dat$n_sex) + 3 + (dat$n_sex * dat$n_shell * dat$n_maturity * dat$n_size_bins), ncol = 7)
      for(i in 1:nrow(tmp)) {
        tmp[i, 1:7] <- as.numeric(ctl[last + i, 1:7])
      }
      
      as_tibble(tmp) %>%
        rename_all(~c("initial", "lower_bound", "upper_bound", "phase", "prior_type", "prior_1", "prior_2")) %>%
        mutate(parameter = c("log_R0", "log_Rini", "log_Rbar", 
                             expand_grid(sex = 1:dat$n_sex, prefix = c("rec_a_sex_", "rec_b_sex_")) %>%
                               mutate(par = paste0(prefix, sex)) %>%
                               pull(par),
                             "log_sigmaR", "steepness", "rho", 
                             expand_grid(prefix = "scaled_log_N_dev",
                                         sex = paste0("_sex_", 1:dat$n_sex),
                                         maturity = paste0("_maturity_", 1:dat$n_maturity),
                                         shell = paste0("_shell_", 1:dat$n_shell),
                                         class = paste0("_class_", 1:dat$n_size_bins)) %>%
                               mutate(par = paste0(prefix, sex, shell, maturity, class)) %>%
                               pull(par) )) -> out$thetas
    }
    if(out$init_conditions_type == 3){
      tmp <- matrix(nrow = 3 + (2 * dat$n_sex) + 3 + (dat$n_sex * dat$n_shell * dat$n_maturity * dat$n_size_bins - 1), ncol = 7)
      for(i in 1:nrow(tmp)) {
        tmp[i, 1:7] <- as.numeric(ctl[last + i, 1:7])
      }
      
      as_tibble(tmp) %>%
        rename_all(~c("initial", "lower_bound", "upper_bound", "phase", "prior_type", "prior_1", "prior_2")) %>%
        mutate(parameter = c("log_R0", "log_Rini", "log_Rbar", 
                             expand_grid(sex = 1:dat$n_sex, prefix = c("rec_a_sex_", "rec_b_sex_")) %>%
                               mutate(par = paste0(prefix, sex)) %>%
                               pull(par),
                             "log_sigmaR", "steepness", "rho", 
                             expand_grid(prefix = "scaled_log_N_dev",
                                         sex = paste0("_sex_", 1:dat$n_sex),
                                         maturity = paste0("_maturity_", 1:dat$n_maturity),
                                         shell = paste0("_shell_", 1:dat$n_shell),
                                         class = paste0("_class_", 1:dat$n_size_bins)) %>%
                               mutate(par = paste0(prefix, sex, shell, maturity, class)) %>%
                               pull(par) %>% .[-out$ref_size_class])) -> out$thetas
    }
    last <- last + nrow(tmp)
    
    # allometry, maturity, legal ----
    
    ## allometry
    out$allometry_type <- as.numeric(ctl[last + 1, 1]); last <- last + 1
    if(out$allometry_type == 2) {
      tmp <- matrix(ncol = dat$n_size_bins, nrow = dat$n_sex)
      for(i in 1:nrow(tmp)) {
        tmp[i, 1:ncol(tmp)] <- as.numeric(ctl[last + i, 1:ncol(tmp)])
      }
      
      as_tibble(tmp) %>%
        rename_all(~as.character(dat$size_bins[-length(dat$size_bins)])) %>%
        mutate(sex = paste0("sex_", 1:dat$n_sex)) %>%
        pivot_longer(1:(ncol(.)-1), names_to = "size", values_to = "wt") %>%
        mutate(size = as.numeric(size)) -> out$allometry
      last <- last + nrow(tmp)
    }
    if(out$allometry_type == 3) {
      out$allometry <- list()
      for(i in 1:dat$n_sex){
        out$allometry[[i]] <- matrix(ncol = dat$n_size_bins, nrow = length(dat$start_year:dat$terminal_year)+1)
        for(j in 1:nrow(out$allometry[[i]])) {
          out$allometry[[i]][j, 1:ncol(out$allometry[[i]])] <- as.numeric(ctl[last + j, 1:ncol(out$allometry[[i]])])
        }
        out$allometry[[i]] %>%
          as_tibble() %>%
          rename_all(~as.character(dat$size_bins[-length(dat$size_bins)])) %>%
          bind_cols(tibble(year = c(dat$start_year:(dat$terminal_year+1))), .) -> out$allometry[[i]]
        last <- last + nrow(out$allometry[[i]])
        
      }
      names(out$allometry) <- paste0("sex_", 1:dat$n_sex)
      
    }
    
    
    ## maturity
    tmp <- matrix(ncol = dat$n_size_bins, nrow = dat$n_sex)
    for(i in 1:nrow(tmp)) {
      tmp[i, 1:ncol(tmp)] <- as.numeric(ctl[last + i, 1:ncol(tmp)])
    }
    as_tibble(tmp) %>%
      rename_all(~as.character(dat$size_bins[-length(dat$size_bins)])) %>%
      mutate(sex = paste0("sex_", 1:dat$n_sex)) %>%
      pivot_longer(1:(ncol(.)-1), names_to = "size", values_to = "maturity") %>%
      mutate(size = as.numeric(size)) -> out$maturity
    last <- last + nrow(tmp)
    
    ## legal status
    tmp <- matrix(ncol = dat$n_size_bins, nrow = dat$n_sex)
    for(i in 1:nrow(tmp)) {
      tmp[i, 1:ncol(tmp)] <- as.numeric(ctl[last + i, 1:ncol(tmp)])
    }
    as_tibble(tmp) %>%
      rename_all(~as.character(dat$size_bins[-length(dat$size_bins)])) %>%
      mutate(sex = paste0("sex_", 1:dat$n_sex)) %>%
      pivot_longer(1:(ncol(.)-1), names_to = "size", values_to = "legal") %>%
      mutate(size = as.numeric(size)) -> out$legal_status
    last <- last + nrow(tmp)
    
    # growth controls ----
    
    out$max_rec_class <- as.numeric(ctl[last + 1, 1:dat$n_sex]); last <- last + 1
    out$functional_maturity <- as.numeric(ctl[last + 1, 1]); last <- last + 1
    
    ## growth setup
    tmp <- matrix(ncol = 3, nrow = dat$n_sex)
    for(i in 1:nrow(tmp)) {
      tmp[i, 1:ncol(tmp)] <- as.numeric(ctl[last + i, 1:ncol(tmp)])
    }
    as_tibble(tmp) %>%
      rename_all(~c("growth_mat_type", "growth_inc_type", "block_group")) %>%
      mutate(sex = 1:dat$n_sex) %>%
      dplyr::select(4, 1:3) -> out$growth_options; last <- last + nrow(tmp)
    
    ## molt probability setup
    tmp <- matrix(ncol = 2, nrow = dat$n_sex)
    for(i in 1:nrow(tmp)) {
      tmp[i, 1:ncol(tmp)] <- as.numeric(ctl[last + i, 1:ncol(tmp)])
    }
    as_tibble(tmp) %>%
      rename_all(~c("molt_prob_type", "block_group")) %>%
      mutate(sex = 1:dat$n_sex) %>%
      dplyr::select(3, 1:2) -> out$molt_probability_options; last <- last + nrow(tmp)
    
    ## growth pars 
    ### number of pars per growth type
    tibble(growth_inc_type = 1:4,
           n_pars = c(3, dat$n_size_bins+1, dat$n_size_bins+1, 2)) -> growth_types
    ### get number of expected growth pars
    out$growth_options %>%
      left_join(growth_types, by = join_by(growth_inc_type)) -> growth_tmp
    ### read growth pars
    growth_pars <- list()
    for(i in 1:nrow(growth_tmp)){
      if(growth_tmp$growth_inc_type[i] < 0) {break}
      if(growth_tmp$growth_inc_type[i] == 1) {
        # growth pars main
        growth_pars[[i]] <- list()
        growth_pars[[i]]$main <- matrix(ncol = 14, nrow = as.numeric(growth_tmp[i,5]))
        for(j in 1:nrow(growth_pars[[i]]$main)) {
          growth_pars[[i]]$main[j, 1:14] <- as.numeric(ctl[last + j, 1:14])
        } 
        as_tibble(growth_pars[[i]]$main) %>%
          rename_all(~c("initial", "lower_bound", "upper_bound", "prior_type", "prior_1", "prior_2", "phase", "block", "blk_fn", "env_l", "env_vr", "rw", "rw_blk", "rw_sigma")) %>%
          mutate(par = c(paste0(c("alpha_sex_", "beta_sex_"), i), paste0("gscale_sex_", i))) -> growth_pars[[i]]$main
        last <- last + nrow(growth_pars[[i]]$main)
        # growth pars extra
        if(nrow(filter(growth_pars[[i]]$main, block != 0)) == 0){growth_pars[[i]]$extra <- tibble(initial = NA,lower_bound = NA,upper_bound = NA, prior_type = NA, prior_1 = NA, prior_2 = NA, phase = NA, relative = NA)[-1,]}
        if(nrow(filter(growth_pars[[i]]$main, block != 0)) > 0){
          
          # get names (and number) of extra pars
          growth_pars[[i]]$main %>%
            filter(block > 0) %>%
            mutate(n_blocks = blocks_per_group[block]) %>%
            mutate(extra_pars = purrr::map2(par, n_blocks, function(par, n_blocks){
              paste0(par, "_block_", 1:n_blocks)
            })) %>% pull(extra_pars) %>%
            unlist -> extra_pars
          
          growth_pars[[i]]$extra <- matrix(ncol = 8, nrow = length(extra_pars))
          for(j in 1:nrow(growth_pars[[i]]$extra)) {
            growth_pars[[i]]$extra[j, 1:8] <- as.numeric(ctl[last + j, 1:8])
          } 
          as_tibble(growth_pars[[i]]$extra) %>%
            rename_all(~c("initial", "lower_bound", "upper_bound", "prior_type", "prior_1", "prior_2", "phase", "relative")) %>%
            mutate(par = extra_pars) -> growth_pars[[i]]$extra
          last <- last + nrow(growth_pars[[i]]$extra)
        }
      }
      if(growth_tmp$growth_inc_type[i] == 3) {
        # growth pars main
        growth_pars[[i]] <- list()
        growth_pars[[i]]$main <- matrix(ncol = 14, nrow = as.numeric(growth_tmp[i,5]))
        for(j in 1:nrow(growth_pars[[i]]$main)) {
          growth_pars[[i]]$main[j, 1:14] <- as.numeric(ctl[last + j, 1:14])
        } 
        as_tibble(growth_pars[[i]]$main) %>%
          rename_all(~c("initial", "lower_bound", "upper_bound", "prior_type", "prior_1", "prior_2", "phase", "block", "blk_fn", "env_l", "env_vr", "rw", "rw_blk", "rw_sigma")) %>%
          mutate(par = c(paste0("molt_increment_base_sex_", i, "_class_", 1:dat$n_size_bins), paste0("gscale_base_sex_", i))) -> growth_pars[[i]]$main
        last <- last + nrow(growth_pars[[i]]$main)
        # growth pars extra
        if(nrow(filter(growth_pars[[i]]$main, block != 0)) == 0){growth_pars[[i]]$extra <- tibble(initial = NA,lower_bound = NA,upper_bound = NA, prior_type = NA, prior_1 = NA, prior_2 = NA, phase = NA, relative = NA)[-1,]}
        if(nrow(filter(growth_pars[[i]]$main, block != 0)) > 0){
          
          # get names (and number) of extra pars
          growth_pars[[i]]$main %>%
            filter(block > 0) %>%
            mutate(n_blocks = blocks_per_group[block]) %>%
            mutate(extra_pars = purrr::map2(par, n_blocks, function(par, n_blocks){
              paste0(par, "_block_", 1:n_blocks)
            })) %>% pull(extra_pars) %>%
            unlist -> extra_pars
          
          growth_pars[[i]]$extra <- matrix(ncol = 8, nrow = length(extra_pars))
          for(j in 1:nrow(growth_pars[[i]]$extra)) {
            growth_pars[[i]]$extra[j, 1:8] <- as.numeric(ctl[last + j, 1:8])
          } 
          as_tibble(growth_pars[[i]]$extra) %>%
            rename_all(~c("initial", "lower_bound", "upper_bound", "prior_type", "prior_1", "prior_2", "phase", "relative")) %>%
            mutate(par = extra_pars) -> growth_pars[[i]]$extra
          last <- last + nrow(growth_pars[[i]]$extra)
        }
      }
      
    }
    if(length(growth_pars) > 0) { names(growth_pars) <- paste0("sex_", 1:dat$n_sex) }
    
    ## molt pars 
    tibble(molt_prob_type = 0:3,
           n_pars = c(NA, 0, 2, NA)) -> molt_types
    ### get number of expected growth pars
    out$molt_probability_options %>%
      left_join(molt_types, by = join_by(molt_prob_type)) -> molt_tmp
    ### read molt pars
    molt_pars <- list()
    for(i in 1:nrow(molt_tmp)){
      if(molt_tmp$molt_prob_type[i] == 1) {
        molt_pars[[i]] <- list()
        molt_pars[[i]]$main <- tibble(initial = NA,lower_bound = NA,upper_bound = NA, prior_type = NA, prior_1 = NA, prior_2 = NA, phase = NA, block = NA,
                                      blk_fn = NA, env_l = NA, env_vr = NA, rw = NA, rw_blk = NA, rw_sigma = NA)[-1,]
        molt_pars[[i]]$extra <- tibble(initial = NA,lower_bound = NA,upper_bound = NA, prior_type = NA, prior_1 = NA, prior_2 = NA, phase = NA, relative = NA)[-1,]
      }
      if(molt_tmp$molt_prob_type[i] == 2) {
        # growth pars main
        molt_pars[[i]] <- list()
        molt_pars[[i]]$main <- matrix(ncol = 14, nrow = as.numeric(molt_tmp[i,4]))
        for(j in 1:nrow(molt_pars[[i]]$main)) {
          molt_pars[[i]]$main[j, 1:14] <- as.numeric(ctl[last + j, 1:14])
        } 
        as_tibble(molt_pars[[i]]$main) %>%
          rename_all(~c("initial", "lower_bound", "upper_bound", "prior_type", "prior_1", "prior_2", "phase", "block", "blk_fn", "env_l", "env_vr", "rw", "rw_blk", "rw_sigma")) %>%
          mutate(par = c(paste0("molt_probability_mu_base_sex_", i), paste0("molt_probability_cv_base_sex_", i) )) -> molt_pars[[i]]$main
        last <- last + nrow(molt_pars[[i]]$main)
        # growth pars extra
        if(nrow(filter(molt_pars[[i]]$main, block != 0)) == 0){molt_pars[[i]]$extra <- tibble(initial = NA,lower_bound = NA,upper_bound = NA, prior_type = NA, prior_1 = NA, prior_2 = NA, phase = NA, relative = NA)[-1,]}
        if(nrow(filter(molt_pars[[i]]$main, block != 0)) > 0){
          
          # get names (and number) of extra pars
          molt_pars[[i]]$main %>%
            filter(block > 0) %>%
            mutate(n_blocks = blocks_per_group[block]) %>%
            mutate(extra_pars = purrr::map2(par, n_blocks, function(par, n_blocks){
              paste0(par, "_block_", 1:n_blocks)
            })) %>% pull(extra_pars) %>%
            unlist -> extra_pars
          
          molt_pars[[i]]$extra <- matrix(ncol = 8, nrow = length(extra_pars))
          for(j in 1:nrow(molt_pars[[i]]$extra)) {
            molt_pars[[i]]$extra[j, 1:8] <- as.numeric(ctl[last + j, 1:8])
          } 
          as_tibble(molt_pars[[i]]$extra) %>%
            rename_all(~c("initial", "lower_bound", "upper_bound", "prior_type", "prior_1", "prior_2", "phase", "relative")) %>%
            mutate(par = extra_pars) -> molt_pars[[i]]$extra
          last <- last + nrow(molt_pars[[i]]$extra)
        }
      }
    }
    names(molt_pars) <- paste0("sex_", 1:dat$n_sex)
    
    # custom growth matrix
    out$growth_matrix <- list()
    for(i in 1:nrow(growth_tmp)){
      if(growth_tmp$growth_mat_type[i] == 1){
        out$growth_matrix[[i]] <- matrix(ncol = dat$n_size_bins, nrow = dat$n_size_bins)
        for(j in 1:nrow(out$growth_matrix[[i]] )) {
          out$growth_matrix[[i]][j, 1:ncol(out$growth_matrix[[i]])] <- as.numeric(ctl[last + j, 1:ncol(out$growth_matrix[[i]])])
        }
        names(out$growth_matrix)[i] <- paste0("sex_", i)
        last <- last + nrow(out$growth_matrix[[i]])
      }
    }
    # null if no custom growth matrix
    if(length(out$growth_matrix) == 0) {out$growth_matrix <- NULL}
    
    ### write to out
    out$growth_pars <- growth_pars
    out$molt_probability_pars <- molt_pars
    
    # natural mortality controls ----
    
    ## options
    out$natural_mortality_options <- matrix(ncol = 13, nrow = dat$n_sex * dat$n_maturity)
    for(i in 1:nrow(out$natural_mortality_options)){
      out$natural_mortality_options[i, 1:13] <- as.numeric(ctl[last + i, 1:13])
    }
    as_tibble(out$natural_mortality_options) %>%
      rename_all(~c("relative", "type", "extra", "brkpts", "mirror", "block", "blk_fn", "env_l", "env_vr", "rw", "rw_blk", "sigma_rw", "mirror_rw")) -> out$natural_mortality_options
    last <- last + nrow(out$natural_mortality_options)
    ## brkpts
    out$natural_mortality_brkpts <- list()
    for(i in 1:nrow(out$natural_mortality_options)){
      if(out$natural_mortality_options$brkpts[i] > 0){
        out$natural_mortality_brkpts[[i]] <- vector()
        out$natural_mortality_brkpts[[i]][1:out$natural_mortality_options$brkpts] <- as.numeric(ctl[last + i, 1:out$natural_mortality_options$brkpts])
        names(out$natural_mortality_brkpts)[i] <- paste0("sex_", i, "_maturity_", i)
      }
    }
    if(length(out$natural_mortality_brkpts) == 0) {out$natural_mortality_brkpts <- NULL}
    last <- last + length(out$natural_mortality_brkpts)
    
    ## types of natural morality
    tibble(type = 0:1,
           n_pars = c(1, NA)) -> nat_m_types
    ## get number of expected pars
    out$natural_mortality_options %>%
      left_join(nat_m_types, by = join_by(type)) %>%
      mutate(n_pars = n_pars + brkpts) %>%
      mutate(sex_mat = expand_grid(sex = 1:dat$n_sex, mat = 1:dat$n_maturity) %>%
               mutate(sex_mat = paste0("sex_", sex, "_maturity_", mat)) %>% pull(sex_mat)) -> nat_m_tmp
    
    nat_m_pars <- list()
    for(i in 1:nrow(out$natural_mortality_options)){
      nat_m_pars[[i]] <- list()
      if(nat_m_tmp$type[i] == 0){
        nat_m_pars[[i]]$main <- matrix(ncol = 7, nrow = nat_m_tmp$n_pars[i])
        for(j in 1:nrow(nat_m_pars[[i]]$main)) {
          nat_m_pars[[i]]$main[j,] <- as.numeric(ctl[last+j, 1:7])
        }
        
        as_tibble(nat_m_pars[[i]]$main) %>%
          rename_all(~c("initial", "lower_bound", "upper_bound", "prior_type", "prior_1", "prior_2", "phase")) %>%
          mutate(par = ifelse(nat_m_tmp$brkpts[i] == 0,
                              paste0("M_base_", nat_m_tmp$sex_mat[i]),
                              c(paste0("M_base_", nat_m_tmp$sex_mat[i]),
                                paste0("M_base_", nat_m_tmp$sex_mat[i], "_brk_", 1:nat_m_tmp$brkpts[i])))) -> nat_m_pars[[i]]$main
        last <- last + nrow(nat_m_pars[[i]]$main)
        if(nat_m_tmp$block[i] == 0) {nat_m_pars[[i]]$extra <- nat_m_pars[[i]]$main %>% dplyr::slice(-1:-nrow(.)) }
        if(nat_m_tmp$block[i] > 0) {
          # get names (and number) of extra pars
          nat_m_pars[[i]]$main %>%
            mutate(n_blocks = blocks_per_group[nat_m_tmp$block[i]]) %>%
            mutate(extra_pars = purrr::map2(par, n_blocks, function(par, n_blocks){
              paste0(par, "_block_", 1:n_blocks)
            })) %>% pull(extra_pars) %>%
            unlist -> extra_pars
          nat_m_pars[[i]]$extra <- matrix(ncol = 7, nrow = length(extra_pars))
          for(j in 1:nrow(nat_m_pars[[i]]$extra)) {
            nat_m_pars[[i]]$extra[j, 1:7] <- as.numeric(ctl[last + j, 1:7])
          } 
          as_tibble(nat_m_pars[[i]]$extra) %>%
            rename_all(~c("initial", "lower_bound", "upper_bound", "prior_type", "prior_1", "prior_2", "phase")) %>%
            mutate(par = extra_pars) -> nat_m_pars[[i]]$extra
          last <- last + nrow(nat_m_pars[[i]]$extra)
        }
      }
    }
    # rename list
    names(nat_m_pars) <- nat_m_tmp$sex_mat
    # write to out object
    out$natural_mortality_pars <- nat_m_pars
    
    # selectivity controls ----
    
    ## selex options
    out$selectivity_options <- matrix(ncol = dat$n_fleets, nrow = 2 + 4*dat$n_sex)
    for(i in 1:nrow(out$selectivity_options)){
      out$selectivity_options[i, 1:ncol(out$selectivity_options)] <- as.numeric(ctl[last+i, 1:ncol(out$selectivity_options)])
    }
    t(out$selectivity_options) %>%
      as_tibble %>%
      rename_all(~ c("sex_specific", paste0("type_sex_", 1:dat$n_sex), "sel_with_another_gear", paste0("extra_sex_", 1:dat$n_sex),
                     paste0("force_to_one_sex_", 1:dat$n_sex), paste0("force_class_sex_", 1:dat$n_sex))) %>%
      bind_cols(tibble(fleet = dat$fleet_names), .) -> out$selectivity_options
    last <- last + ncol(out$selectivity_options) - 1
    
    ## retention options
    out$retention_options <- matrix(ncol = dat$n_fleets, nrow = 1 + 4*dat$n_sex)
    for(i in 1:nrow(out$retention_options)){
      out$retention_options[i, 1:ncol(out$retention_options)] <- as.numeric(ctl[last+i, 1:ncol(out$retention_options)])
    }
    t(out$retention_options) %>%
      as_tibble %>%
      rename_all(~ c("sex_specific", paste0("type_sex_", 1:dat$n_sex), paste0("ret_flag_sex_", 1:dat$n_sex), paste0("extra_sex_", 1:dat$n_sex), paste0("est_max_sex_", 1:dat$n_sex))) %>%
      bind_cols(tibble(fleet = dat$fleet_names), .) -> out$retention_options
    last <- last + ncol(out$retention_options) - 1
    
    # selectivity pars
    ### number of pars per selectivity type
    tibble(type = 0:11,
           n_pars = c(dat$n_size_bins, dat$n_size_bins, 2, 2, 3, 0, 0, 4, 2, NA, 1, NA)) -> sel_types
    ### get number of expected selectivity pars
    out$selectivity_options %>%
      dplyr::select(fleet, sex_specific, grep("type", names(.))) %>%
      pivot_longer(3:ncol(.), names_to = "sex", values_to = "type") %>%
      mutate(sex = gsub("type_sex_", "", sex)) %>% arrange(sex) %>%
      left_join(out$selectivity_options %>%
                  dplyr::select(fleet, grep("extra", names(.))) %>%
                  pivot_longer(2:ncol(.), names_to = "sex", values_to = "extra") %>%
                  mutate(sex = gsub("extra_sex_", "", sex)) %>% arrange(sex), by = join_by(fleet, sex)) %>%
      left_join(sel_types, by = join_by(type)) %>%
      # add extra pars for spline knots??
      # remove parameters when selectivity isn't sex specific
      mutate(n_pars = ifelse(is.na(n_pars) & type == 9, extra, n_pars),
             n_pars = ifelse(sex > 1, sex_specific * (n_pars + extra), n_pars + extra)) %>%
      filter(n_pars != 0) -> sel_tmp
    
    ### read selectivity pars
    sel_pars <- list()
    for(i in 1:nrow(sel_tmp)){
      if(sel_tmp$type[i] == 0) {
        # sel pars main
        sel_pars[[i]] <- list()
        sel_pars[[i]]$main <- matrix(ncol = 14, nrow = as.numeric(sel_tmp[i,"n_pars"]))
        for(j in 1:nrow(sel_pars[[i]]$main)) {
          sel_pars[[i]]$main[j, 1:14] <- as.numeric(ctl[last + j, 1:14])
        } 
        as_tibble(sel_pars[[i]]$main) %>%
          rename_all(~c("initial", "lower_bound", "upper_bound", "prior_type", "prior_1", "prior_2", "phase", "block", "blk_fn", "env_l", "env_vr", "rw", "rw_blk", "rw_sigma")) %>%
          mutate(par = paste0("sel_", sel_tmp$fleet[i], "_sex_", sel_tmp$sex[i], "_class_", 1:dat$n_size_bins)) -> sel_pars[[i]]$main
        last <- last + nrow(sel_pars[[i]]$main)
        # growth pars extra
        if(nrow(filter(sel_pars[[i]]$main, block != 0)) == 0){sel_pars[[i]]$extra <- tibble(initial = NA,lower_bound = NA,upper_bound = NA, prior_type = NA, prior_1 = NA, prior_2 = NA, phase = NA, relative = NA)[-1,]}
        if(nrow(filter(sel_pars[[i]]$main, block != 0)) > 0){
          
          # get names (and number) of extra pars
          sel_pars[[i]]$main %>%
            filter(block > 0) %>%
            mutate(n_blocks = blocks_per_group[block]) %>%
            mutate(extra_pars = purrr::map2(par, n_blocks, function(par, n_blocks){
              paste0(par, "_block_", 1:n_blocks)
            })) %>% pull(extra_pars) %>%
            unlist -> extra_pars
          
          sel_pars[[i]]$extra <- matrix(ncol = 8, nrow = length(extra_pars))
          for(j in 1:nrow(sel_pars[[i]]$extra)) {
            sel_pars[[i]]$extra[j, 1:8] <- as.numeric(ctl[last + j, 1:8])
          } 
          as_tibble(sel_pars[[i]]$extra) %>%
            rename_all(~c("initial", "lower_bound", "upper_bound", "prior_type", "prior_1", "prior_2", "phase", "relative")) %>%
            mutate(par = extra_pars) -> sel_pars[[i]]$extra
          last <- last + nrow(sel_pars[[i]]$extra)
        }
      }
      if(sel_tmp$type[i] == 2) {
        # sel pars main
        sel_pars[[i]] <- list()
        sel_pars[[i]]$main <- matrix(ncol = 14, nrow = as.numeric(sel_tmp[i,"n_pars"]))
        for(j in 1:nrow(sel_pars[[i]]$main)) {
          sel_pars[[i]]$main[j, 1:14] <- as.numeric(ctl[last + j, 1:14])
        } 
        as_tibble(sel_pars[[i]]$main) %>%
          rename_all(~c("initial", "lower_bound", "upper_bound", "prior_type", "prior_1", "prior_2", "phase", "block", "blk_fn", "env_l", "env_vr", "rw", "rw_blk", "rw_sigma")) %>%
          mutate(par = paste0("sel_", sel_tmp$fleet[i], "_sex_", sel_tmp$sex[i], "_base_logistic_", c("mean", "cv"))) -> sel_pars[[i]]$main
        last <- last + nrow(sel_pars[[i]]$main)
        # growth pars extra
        if(nrow(filter(sel_pars[[i]]$main, block != 0)) == 0){sel_pars[[i]]$extra <- tibble(initial = NA,lower_bound = NA,upper_bound = NA, prior_type = NA, prior_1 = NA, prior_2 = NA, phase = NA, relative = NA)[-1,]}
        if(nrow(filter(sel_pars[[i]]$main, block != 0)) > 0){
          
          # get names (and number) of extra pars
          sel_pars[[i]]$main %>%
            filter(block > 0) %>%
            mutate(n_blocks = blocks_per_group[block]) %>%
            mutate(extra_pars = purrr::map2(par, n_blocks, function(par, n_blocks){
              paste0(par, "_block_", 1:n_blocks)
            })) %>% pull(extra_pars) %>%
            unlist -> extra_pars
          
          sel_pars[[i]]$extra <- matrix(ncol = 8, nrow = length(extra_pars))
          for(j in 1:nrow(sel_pars[[i]]$extra)) {
            sel_pars[[i]]$extra[j, 1:8] <- as.numeric(ctl[last + j, 1:8])
          } 
          as_tibble(sel_pars[[i]]$extra) %>%
            rename_all(~c("initial", "lower_bound", "upper_bound", "prior_type", "prior_1", "prior_2", "phase", "relative")) %>%
            mutate(par = extra_pars) -> sel_pars[[i]]$extra
          last <- last + nrow(sel_pars[[i]]$extra)
        }
      }
      if(sel_tmp$type[i] == 3) {
        # sel pars main
        sel_pars[[i]] <- list()
        sel_pars[[i]]$main <- matrix(ncol = 14, nrow = as.numeric(sel_tmp[i,"n_pars"]))
        for(j in 1:nrow(sel_pars[[i]]$main)) {
          sel_pars[[i]]$main[j, 1:14] <- as.numeric(ctl[last + j, 1:14])
        } 
        as_tibble(sel_pars[[i]]$main) %>%
          rename_all(~c("initial", "lower_bound", "upper_bound", "prior_type", "prior_1", "prior_2", "phase", "block", "blk_fn", "env_l", "env_vr", "rw", "rw_blk", "rw_sigma")) %>%
          mutate(par = paste0("sel_", sel_tmp$fleet[i], "_sex_", sel_tmp$sex[i], "_base_logistic_", c("sel50", "sel95"))) -> sel_pars[[i]]$main
        last <- last + nrow(sel_pars[[i]]$main)
        # growth pars extra
        if(nrow(filter(sel_pars[[i]]$main, block != 0)) == 0){sel_pars[[i]]$extra <- tibble(initial = NA,lower_bound = NA,upper_bound = NA, prior_type = NA, prior_1 = NA, prior_2 = NA, phase = NA, relative = NA)[-1,]}
        if(nrow(filter(sel_pars[[i]]$main, block != 0)) > 0){
          
          # get names (and number) of extra pars
          sel_pars[[i]]$main %>%
            filter(block > 0) %>%
            mutate(n_blocks = blocks_per_group[block]) %>%
            mutate(extra_pars = purrr::map2(par, n_blocks, function(par, n_blocks){
              paste0(par, "_block_", 1:n_blocks)
            })) %>% pull(extra_pars) %>%
            unlist -> extra_pars
          
          sel_pars[[i]]$extra <- matrix(ncol = 8, nrow = length(extra_pars))
          for(j in 1:nrow(sel_pars[[i]]$extra)) {
            sel_pars[[i]]$extra[j, 1:8] <- as.numeric(ctl[last + j, 1:8])
          } 
          as_tibble(sel_pars[[i]]$extra) %>%
            rename_all(~c("initial", "lower_bound", "upper_bound", "prior_type", "prior_1", "prior_2", "phase", "relative")) %>%
            mutate(par = extra_pars) -> sel_pars[[i]]$extra
          last <- last + nrow(sel_pars[[i]]$extra)
        }
      }
      if(sel_tmp$type[i] == 8) {
        # sel pars main
        sel_pars[[i]] <- list()
        sel_pars[[i]]$main <- matrix(ncol = 14, nrow = as.numeric(sel_tmp[i,"n_pars"]))
        for(j in 1:nrow(sel_pars[[i]]$main)) {
          sel_pars[[i]]$main[j, 1:14] <- as.numeric(ctl[last + j, 1:14])
        } 
        as_tibble(sel_pars[[i]]$main) %>%
          rename_all(~c("initial", "lower_bound", "upper_bound", "prior_type", "prior_1", "prior_2", "phase", "block", "blk_fn", "env_l", "env_vr", "rw", "rw_blk", "rw_sigma")) %>%
          mutate(par = ifelse(sel_tmp$extra[i] == 0,
                              paste0("sel_", sel_tmp$fleet[i], "_sex_", sel_tmp$sex[i], "_des_logistic_", c("sel50", "sel95")),
                              c(paste0("sel_", sel_tmp$fleet[i], "_sex_", sel_tmp$sex[i], "_des_logistic_", c("sel50", "sel95")),
                                paste0("sel_", sel_tmp$fleet[i], "_sex_", sel_tmp$sex[i], "_extra_par_", 1:sel_tmp$extra[i] )))) -> sel_pars[[i]]$main
        last <- last + nrow(sel_pars[[i]]$main)
        # sel pars extra
        if(nrow(filter(sel_pars[[i]]$main, block != 0)) == 0){sel_pars[[i]]$extra <- tibble(initial = NA,lower_bound = NA,upper_bound = NA, prior_type = NA, prior_1 = NA, prior_2 = NA, phase = NA, relative = NA)[-1,]}
        if(nrow(filter(sel_pars[[i]]$main, block != 0)) > 0){
          
          # get names (and number) of extra pars
          sel_pars[[i]]$main %>%
            filter(block > 0) %>%
            mutate(n_blocks = blocks_per_group[block]) %>%
            mutate(extra_pars = purrr::map2(par, n_blocks, function(par, n_blocks){
              paste0(par, "_block_", 1:n_blocks)
            })) %>% pull(extra_pars) %>%
            unlist -> extra_pars
          
          sel_pars[[i]]$extra <- matrix(ncol = 8, nrow = length(extra_pars))
          for(j in 1:nrow(sel_pars[[i]]$extra)) {
            sel_pars[[i]]$extra[j, 1:8] <- as.numeric(ctl[last + j, 1:8])
          } 
          as_tibble(sel_pars[[i]]$extra) %>%
            rename_all(~c("initial", "lower_bound", "upper_bound", "prior_type", "prior_1", "prior_2", "phase", "relative")) %>%
            mutate(par = extra_pars) -> sel_pars[[i]]$extra
          last <- last + nrow(sel_pars[[i]]$extra)
        }
      }
      if(sel_tmp$type[i] == 10) {
        # sel pars main
        sel_pars[[i]] <- list()
        sel_pars[[i]]$main <- matrix(ncol = 14, nrow = as.numeric(sel_tmp[i,"n_pars"]))
        for(j in 1:nrow(sel_pars[[i]]$main)) {
          sel_pars[[i]]$main[j, 1:14] <- as.numeric(ctl[last + j, 1:14])
        } 
        as_tibble(sel_pars[[i]]$main) %>%
          rename_all(~c("initial", "lower_bound", "upper_bound", "prior_type", "prior_1", "prior_2", "phase", "block", "blk_fn", "env_l", "env_vr", "rw", "rw_blk", "rw_sigma")) %>%
          mutate(par = paste0("sel_", sel_tmp$fleet[i], "_sex_", sel_tmp$sex[i], "_des_logistic_mean")) -> sel_pars[[i]]$main
        last <- last + nrow(sel_pars[[i]]$main)
        # growth pars extra
        if(nrow(filter(sel_pars[[i]]$main, block != 0)) == 0){sel_pars[[i]]$extra <- tibble(initial = NA,lower_bound = NA,upper_bound = NA, prior_type = NA, prior_1 = NA, prior_2 = NA, phase = NA, relative = NA)[-1,]}
        if(nrow(filter(sel_pars[[i]]$main, block != 0)) > 0){
          
          # get names (and number) of extra pars
          sel_pars[[i]]$main %>%
            filter(block > 0) %>%
            mutate(n_blocks = blocks_per_group[block]) %>%
            mutate(extra_pars = purrr::map2(par, n_blocks, function(par, n_blocks){
              paste0(par, "_block_", 1:n_blocks)
            })) %>% pull(extra_pars) %>%
            unlist -> extra_pars
          
          sel_pars[[i]]$extra <- matrix(ncol = 8, nrow = length(extra_pars))
          for(j in 1:nrow(sel_pars[[i]]$extra)) {
            sel_pars[[i]]$extra[j, 1:8] <- as.numeric(ctl[last + j, 1:8])
          } 
          as_tibble(sel_pars[[i]]$extra) %>%
            rename_all(~c("initial", "lower_bound", "upper_bound", "prior_type", "prior_1", "prior_2", "phase", "relative")) %>%
            mutate(par = extra_pars) -> sel_pars[[i]]$extra
          last <- last + nrow(sel_pars[[i]]$extra)
        }
      }
    }
    names(sel_pars) <- sel_tmp %>%
      mutate(name = paste0(fleet, "_sex_", sex)) %>% pull(name)
    
    # retention pars
    out$retention_options %>%
      dplyr::select(fleet, sex_specific, grep("type", names(.))) %>%
      pivot_longer(3:ncol(.), names_to = "sex", values_to = "type") %>%
      mutate(sex = gsub("type_sex_", "", sex)) %>% arrange(sex) %>%
      left_join(out$retention_options %>%
                  dplyr::select(fleet, grep("extra", names(.))) %>%
                  pivot_longer(2:ncol(.), names_to = "sex", values_to = "extra") %>%
                  mutate(sex = gsub("extra_sex_", "", sex)) %>% arrange(sex), by = join_by(fleet, sex)) %>%
      left_join(sel_types, by = join_by(type)) %>%
      # add extra pars for spline knots??
      # remove parameters when selectivity isn't sex specific
      mutate(n_pars = ifelse(is.na(n_pars) & type == 9, extra, n_pars),
             n_pars = ifelse(sex > 1, sex_specific * n_pars, n_pars)) %>%
      filter(n_pars != 0) -> ret_tmp
    
    ### read retention pars
    ret_pars <- list()
    for(i in 1:nrow(ret_tmp)){
      if(ret_tmp$type[i] == 0) {
        # ret pars main
        ret_pars[[i]] <- list()
        ret_pars[[i]]$main <- matrix(ncol = 14, nrow = as.numeric(ret_tmp[i,"n_pars"]))
        for(j in 1:nrow(ret_pars[[i]]$main)) {
          ret_pars[[i]]$main[j, 1:14] <- as.numeric(ctl[last + j, 1:14])
        } 
        as_tibble(ret_pars[[i]]$main) %>%
          rename_all(~c("initial", "lower_bound", "upper_bound", "prior_type", "prior_1", "prior_2", "phase", "block", "blk_fn", "env_l", "env_vr", "rw", "rw_blk", "rw_sigma")) %>%
          mutate(par = paste0("sel_", ret_tmp$fleet[i], "_sex_", ret_tmp$sex[i], "_class_", 1:dat$n_size_bins)) -> ret_pars[[i]]$main
        last <- last + nrow(ret_pars[[i]]$main)
        # growth pars extra
        if(nrow(filter(ret_pars[[i]]$main, block != 0)) == 0){ret_pars[[i]]$extra <- tibble(initial = NA,lower_bound = NA,upper_bound = NA, prior_type = NA, prior_1 = NA, prior_2 = NA, phase = NA, relative = NA)[-1,]}
        if(nrow(filter(ret_pars[[i]]$main, block != 0)) > 0){
          
          # get names (and number) of extra pars
          ret_pars[[i]]$main %>%
            filter(block > 0) %>%
            mutate(n_blocks = blocks_per_group[block]) %>%
            mutate(extra_pars = purrr::map2(par, n_blocks, function(par, n_blocks){
              paste0(par, "_block_", 1:n_blocks)
            })) %>% pull(extra_pars) %>%
            unlist -> extra_pars
          
          ret_pars[[i]]$extra <- matrix(ncol = 8, nrow = length(extra_pars))
          for(j in 1:nrow(sel_pars[[i]]$extra)) {
            ret_pars[[i]]$extra[j, 1:8] <- as.numeric(ctl[last + j, 1:8])
          } 
          as_tibble(ret_pars[[i]]$extra) %>%
            rename_all(~c("initial", "lower_bound", "upper_bound", "prior_type", "prior_1", "prior_2", "phase", "relative")) %>%
            mutate(par = extra_pars) -> ret_pars[[i]]$extra
          last <- last + nrow(ret_pars[[i]]$extra)
        }
      }
      if(ret_tmp$type[i] == 2) {
        # ret pars main
        ret_pars[[i]] <- list()
        ret_pars[[i]]$main <- matrix(ncol = 14, nrow = as.numeric(ret_tmp[i,"n_pars"]))
        for(j in 1:nrow(ret_pars[[i]]$main)) {
          ret_pars[[i]]$main[j, 1:14] <- as.numeric(ctl[last + j, 1:14])
        } 
        as_tibble(ret_pars[[i]]$main) %>%
          rename_all(~c("initial", "lower_bound", "upper_bound", "prior_type", "prior_1", "prior_2", "phase", "block", "blk_fn", "env_l", "env_vr", "rw", "rw_blk", "rw_sigma")) %>%
          mutate(par = paste0("sel_", ret_tmp$fleet[i], "_sex_", ret_tmp$sex[i], "_base_logistic_", c("mean", "cv"))) -> ret_pars[[i]]$main
        last <- last + nrow(ret_pars[[i]]$main)
        # growth pars extra
        if(nrow(filter(ret_pars[[i]]$main, block != 0)) == 0){ret_pars[[i]]$extra <- tibble(initial = NA,lower_bound = NA,upper_bound = NA, prior_type = NA, prior_1 = NA, prior_2 = NA, phase = NA, relative = NA)[-1,]}
        if(nrow(filter(ret_pars[[i]]$main, block != 0)) > 0){
          
          # get names (and number) of extra pars
          ret_pars[[i]]$main %>%
            filter(block > 0) %>%
            mutate(n_blocks = blocks_per_group[block]) %>%
            mutate(extra_pars = purrr::map2(par, n_blocks, function(par, n_blocks){
              paste0(par, "_block_", 1:n_blocks)
            })) %>% pull(extra_pars) %>%
            unlist -> extra_pars
          
          ret_pars[[i]]$extra <- matrix(ncol = 8, nrow = length(extra_pars))
          for(j in 1:nrow(ret_pars[[i]]$extra)) {
            ret_pars[[i]]$extra[j, 1:8] <- as.numeric(ctl[last + j, 1:8])
          } 
          as_tibble(ret_pars[[i]]$extra) %>%
            rename_all(~c("initial", "lower_bound", "upper_bound", "prior_type", "prior_1", "prior_2", "phase", "relative")) %>%
            mutate(par = extra_pars) -> ret_pars[[i]]$extra
          last <- last + nrow(ret_pars[[i]]$extra)
        }
      }
      if(ret_tmp$type[i] == 3) {
        # ret pars main
        ret_pars[[i]] <- list()
        ret_pars[[i]]$main <- matrix(ncol = 14, nrow = as.numeric(ret_tmp[i,"n_pars"]))
        for(j in 1:nrow(ret_pars[[i]]$main)) {
          ret_pars[[i]]$main[j, 1:14] <- as.numeric(ctl[last + j, 1:14])
        } 
        as_tibble(ret_pars[[i]]$main) %>%
          rename_all(~c("initial", "lower_bound", "upper_bound", "prior_type", "prior_1", "prior_2", "phase", "block", "blk_fn", "env_l", "env_vr", "rw", "rw_blk", "rw_sigma")) %>%
          mutate(par = paste0("sel_", ret_tmp$fleet[i], "_sex_", ret_tmp$sex[i], "_base_logistic_", c("ret50", "ret95"))) -> ret_pars[[i]]$main
        last <- last + nrow(ret_pars[[i]]$main)
        # growth pars extra
        if(nrow(filter(ret_pars[[i]]$main, block != 0)) == 0){ret_pars[[i]]$extra <- tibble(initial = NA,lower_bound = NA,upper_bound = NA, prior_type = NA, prior_1 = NA, prior_2 = NA, phase = NA, relative = NA)[-1,]}
        if(nrow(filter(ret_pars[[i]]$main, block != 0)) > 0){
          
          # get names (and number) of extra pars
          ret_pars[[i]]$main %>%
            filter(block > 0) %>%
            mutate(n_blocks = blocks_per_group[block]) %>%
            mutate(extra_pars = purrr::map2(par, n_blocks, function(par, n_blocks){
              paste0(par, "_block_", 1:n_blocks)
            })) %>% pull(extra_pars) %>%
            unlist -> extra_pars
          
          ret_pars[[i]]$extra <- matrix(ncol = 8, nrow = length(extra_pars))
          for(j in 1:nrow(ret_pars[[i]]$extra)) {
            ret_pars[[i]]$extra[j, 1:8] <- as.numeric(ctl[last + j, 1:8])
          } 
          as_tibble(ret_pars[[i]]$extra) %>%
            rename_all(~c("initial", "lower_bound", "upper_bound", "prior_type", "prior_1", "prior_2", "phase", "relative")) %>%
            mutate(par = extra_pars) -> ret_pars[[i]]$extra
          last <- last + nrow(ret_pars[[i]]$extra)
        }
      }
    }
    names(ret_pars) <- ret_tmp %>%
      mutate(name = paste0(fleet, "_sex_", sex)) %>% pull(name)
    
    # write to out
    out$selectivity_pars <- sel_pars
    out$retention_pars <- ret_pars
    
    # catchability controls ----
    
    out$catchability_options <- matrix(nrow = length(unique(dat$index$series)), ncol = 10) 
    for(i in 1:nrow(out$catchability_options)){
      out$catchability_options[i, 1:10] <- as.numeric(ctl[last+i, 1:10])
    }
    as_tibble(out$catchability_options) %>%
      rename_all(~c("analytic", "lambda", "emphasis", "mirror", "block", "env_l", "env_vr", "rw", "rw_blk", "sigma_rw")) %>%
      bind_cols(tibble(index_series = unique(dat$index$series)), .) -> out$catchability_options
    last <- last + nrow(out$catchability_options)
    
    q_pars <- list()
    # main pars
    q_pars$main <- matrix(ncol = 7, nrow = filter(out$catchability_options, mirror == 0) %>% nrow)
    for(i in 1:nrow(out$catchability_options)){
      # q pars main
      q_pars$main[i, 1:7] <- as.numeric(ctl[last + i, 1:7])
    }
    as_tibble(q_pars$main) %>%
      rename_all(~c("initial", "lower_bound", "upper_bound", "prior_type", "prior_1", "prior_2", "phase")) %>%
      mutate(par = paste0("survey_q_", filter(out$catchability_options, mirror == 0)$index_series)) -> q_pars$main
    last <- last + nrow(q_pars$main)
    # extra pars
    if(filter(out$catchability_options, block > 0) %>% nrow(.) == 0) {q_pars$extra <- NULL} else {stop("Can't yet read extra catchability pars")}
    # write to out
    out$catchability_pars <- q_pars
    
    # additional cv ----
    
    out$additional_cv_options <- matrix(nrow = length(unique(dat$index$series)), ncol = 7) 
    for(i in 1:nrow(out$additional_cv_options)){
      out$additional_cv_options[i, 1:7] <- as.numeric(ctl[last+i, 1:7])
    }
    as_tibble(out$additional_cv_options) %>%
      rename_all(~c("mirror", "block", "env_l", "env_vr", "rw", "rw_blk", "sigma_rw")) %>%
      bind_cols(tibble(index_series = unique(dat$index$series)), .) -> out$additional_cv_options
    last <- last + nrow(out$additional_cv_options)
    
    addcv_pars <- list()
    # main pars
    addcv_pars$main <- matrix(ncol = 7, nrow = filter(out$additional_cv_options, mirror == 0) %>% nrow)
    for(i in 1:nrow(addcv_pars$main)){
      # addcv pars main
      addcv_pars$main[i, 1:7] <- as.numeric(ctl[last + i, 1:7])
    }
    as_tibble(addcv_pars$main) %>%
      rename_all(~c("initial", "lower_bound", "upper_bound", "prior_type", "prior_1", "prior_2", "phase")) %>%
      mutate(par = paste0("add_cv_", filter(out$additional_cv_options, mirror == 0)$index_series)) -> addcv_pars$main
    last <- last + nrow(addcv_pars$main)
    # extra pars
    if(filter(out$catchability_options, block > 0) %>% nrow(.) == 0) {addcv_pars$extra <- NULL} else {stop("Can't yet read extra catchability pars")}
    # write to out
    out$additional_cv_pars <- addcv_pars
    
    # fishing mortality controls ----
    
    out$fishing_mortality_options <- matrix(nrow = dat$n_fleets, ncol = 12)
    for(i in 1:nrow(out$fishing_mortality_options)){
      out$fishing_mortality_options[i, 1:12] <- as.numeric(ctl[last + i, 1:12])
    }
    as_tibble(out$fishing_mortality_options) %>%
      rename_all(~c("init_sex_1_F", "init_sex_2_F", "pen_sd_sex_1", "pen_sd_sex_2", "phz_mean_F_sex_1", "phz_mean_F_sex_2", "lower_mean_F", "upper_mean_F",
                    "lower_ann_F_sex_1", "upper_ann_F_sex_1", "lower_ann_F_sex_2", "upper_ann_F_sex_2")) %>%
      bind_cols(tibble(index_series = length(unique(dat$index$series))), .) -> out$fishing_mortality_options
    last <- last + nrow(out$fishing_mortality_options)
    
    # size composition controls ----
    
    out$size_composition_options <- matrix(ncol = dat$n_size_series, nrow = 8)
    for(i in 1:nrow(out$size_composition_options)){
      out$size_composition_options[i, 1:ncol(out$size_composition_options)] <- as.numeric(ctl[last + i, 1:ncol(out$size_composition_options)])
    }
    t(out$size_composition_options) %>%
      as_tibble() %>%
      rename_all(~c("lik_type", "tail_compression", "pmin", "aggregator_code", "prediction_type", "lambda_sample_size", "lambda", "survey_q")) %>%
      bind_cols(dat$size_comp %>%
                  distinct(org_series, fleet, sex, shell, maturity, type) %>%
                  transmute(fleet = dat$fleet_names[fleet], sex, shell, maturity, type), .) -> out$size_composition_options
    last <- last + 8
    
    # dispersion pars
    comp_pars <- matrix(nrow = max(out$size_composition_options$aggregator_code), ncol = 7)
    for(i in 1:nrow(comp_pars)){
      comp_pars[i, 1:7] <- as.numeric(ctl[last + i, 1:7])
    }
    as_tibble(comp_pars) %>%
      rename_all(~c("initial", "lower_bound", "upper_bound", "prior_type", "prior_1", "prior_2", "phase")) %>%
      mutate(par = paste0("overdispersion_for_size_comp_", 1:max(out$size_composition_options$aggregator_code))) -> out$size_composition_pars
    last <- last + nrow(out$size_composition_pars)
    
    # emphasis factors ----
    
    ## tagging data
    out$emphasis_tagging <- as.numeric(ctl[last + 1, 1]);last <- last + 1
    ## catch data
    out$emphasis_catch <- as.numeric(ctl[last + 1, 1:dat$n_catch_series]);last <- last + 1
    ## weights for penalties
    out$fishing_mortality_pen <- matrix(nrow = dat$n_fleets, ncol = 4)
    for(i in 1:dat$n_fleets) {
      out$fishing_mortality_pen[i, 1:4] <- as.numeric(ctl[last+i, 1:4])
    }
    as_tibble(out$fishing_mortality_pen) %>%
      rename_all(~c("mean_sex_1_fdevs", "mean_sex_2_fdevs", "ann_sex_1_fdevs", "ann_sex_2_fdevs")) %>%
      bind_cols(tibble(fleet = dat$fleet_names), .) -> out$fishing_mortality_pen
    last <- last + nrow(out$fishing_mortality_pen)
    ## emphasis factors
    out$emphasis_factors <- NULL
    for(i in 1:13) {
      out$emphasis_factors[i] <- as.numeric(ctl[last+i, 1])
    }
    tibble(penalty = c("log_fdev", "mean_F", "not_used", "not_used", "not_used", "rec_dev_smoothness", "mean_sex_ratio",
                       "molt_prob_smoothness", "selex_smoothness", "init_n_smoothness", "ann_fdevs_sex_1", "ann_fdevs_sex_2", "dev_pars"),
           emphasis = out$emphasis_factors) -> out$emphasis_factors
    last <- last + 13
    
    # eof ----
    
    if(as.numeric(ctl[last+1, 1]) == 9999){return(out)} else{stop("end of file not correct, debug")}
    
  }
  
}

# gmacs_read_files_dat() ----

## read gmacs.dat file

## args:
### file - file path to gmacs.dat
### model_name - character string to save as object in output, later to be used for plot legends. example: "23.1b"
### version - GMACS version, default latest

## output: list object
## example: gmacs_read_dat(file = "./AIGKC/models/2024/may/EAG/23.1b/gmacs.dat", model_name = "23.1b")

gmacs_read_files_dat <- function(gmacs.dat, model_name = NULL, version = NULL) {
  
  if(is.null(version)){version = "2.20.16"}
  if(version %in% c("2.20.16", "2.20.14")){
    # setup ---- 
    
    # Suppress the NA message in the coercion to double
    options(warn = -1) 
    
    # read text file
    dat <- read.delim(gmacs.dat, sep = "", header = F, col.names = c(1:1000), fill = T, 
                      na.strings = "", colClasses = "character", comment.char = "#")
    # create out object
    out <- list()
    
    # model dimensions ----
    
    ## version 
    out$version <- version
    ## model_name 
    out$model_name <- model_name
    
    # files ----
    out$dat_file <- dat[1,1]
    out$ctl_file <- dat[2,1]
    out$prj_file <- dat[3,1]
    
    # units ----
    out$wt_unit <- dat[4,1]
    out$n_unit <- dat[5,1]
    
    # stock ----
    out$stock <- dat[6,1]
    
    # jittering ----
    out$jitter <- as.numeric(dat[7,1])
    out$jitter_sd <- as.numeric(dat[8,1])
    
    # out options ----
    out$out_ref_pars <- as.numeric(dat[9,1])
    out$out_recruit <- as.numeric(dat[10,1])
    out$out_ssb <- as.numeric(dat[11,1])
    out$out_fbar <- as.numeric(dat[12,1])
    out$out_dynb0 <- as.numeric(dat[13,1])
    
    # retro peels ----
    out$nyr_retro <- as.numeric(dat[14,1])
    
    # run options ----
    out$max_phase <- as.numeric(dat[15,1])
    out$max_function_calls <- as.numeric(dat[16,1])
    out$calc_ref_points <- as.numeric(dat[17,1])
    out$use_pin <- as.numeric(dat[18,1])
    out$verbose <- as.numeric(dat[19,1])
    
    ## eof
    if(as.numeric(dat[20,1]) == 9999){return(out)} else{stop("end of file not correct, debug")}
    
    
  }  
  
}

# gmacs_read_rep() ----

## args: file = file path to report file from GMACS, gmacs.rep (from Jie Zheng)

gmacs_read_rep <- function(file) {
  fn <- file
  options(warn = -1) # Suppress the NA message in the coercion to double
  repfile <- scan(fn, what = "character", flush = TRUE, blank.lines.skip = FALSE, quiet = TRUE, na.strings = c("nan","-nan"))
  #repfile <- scan(fn, what = "character", flush = TRUE, blank.lines.skip = FALSE, quiet = TRUE)
  inan <- which(is.na(repfile)) # Identify any nan entries so that they are not picked up as objects
  idx <- sapply(as.double(repfile), is.na)
  idx[inan] <- FALSE
  vnam <- repfile[idx] # list names
  nv <- length(vnam) # number of objects
  A <- list()
  ir <- 0
  for (i in 1:nv)
  {
    ir <- match(vnam[i], repfile)
    if (i != nv)
    {
      irr <- match(vnam[i+1], repfile)
    } else {
      irr <- length(repfile) + 1 # next row
    }
    dum <- NA
    if (irr-ir == 2)
    {
      dum <- as.double(scan(fn, skip = ir, nlines = 1, quiet = TRUE, what = ""))
    }
    if (irr-ir > 2)
    {
      # ncols <- 0
      # irows <- ir:irr-1
      # for(j in irows)
      # {
      #       tmp=as.double(scan(fn,skip=j,nlines=1,quiet=TRUE,what=""))
      #       if(length(tmp)>ncols) ncols <- length(tmp)
      #       #print(paste(1:ncols))
      # }
      # cname <- paste(1:ncols)
      # dum=as.matrix(read.table(fn,skip=ir,nrow=irr-ir-1,fill=TRUE,col.names=cname))
      # cat("\n ir ",ir," irr ",irr)
      dum <- as.matrix(read.table(fn, skip = ir, nrow = irr-ir-1, fill = TRUE, row.names = NULL))
    }
    if (is.numeric(dum)) # Logical test to ensure dealing with numbers
    {
      A[[vnam[i]]] <- dum
    }
  }
  
  options(warn = 0)
  return(A)
}

# gmacs_read_std() ----

## args: file = file path to gmacs.std file
#        sub_text = parameter name string used for filtering, Default = NULL

gmacs_read_std <- function(file, model_name = NULL, sub_text = NULL) {
  
  std <- read.delim(file, sep = "", skip = 2, header = F) 
  
  std %>%
    as_tibble() %>%
    rename_all(~c("est_no","par", "est", "se")) %>%
    mutate(model_name = model_name) -> out
  
  if(!is.null(sub_text)) {
    out %>% filter(grepl(sub_text, par)) -> out
  }
  
  return(out)
  
}

# gmacs_read_mcoutref() ----

## args: file = file path to mcoutREF.rep file
#        sub_text = parameter name string used for filtering, Default = NULL

gmacs_read_mcoutREF <- function(file, model_name = NULL, version = NULL){
  
  mcout <- read.delim(file, sep = "", skip = 1, header = F)
  ao <- gmacs_read_allout(file.path(dirname(file), "Gmacsall.out"), version = version)
  
  tibble(model = ifelse(is.null(model_name), NA, model_name),
         mcout) %>%
    rename_all(~c("model", "draw", "mean_rec", "f", "mmb", "bmsy", "bmsy_b0", "ofl",
                  paste0("fmsy_", ao$fleet_names), paste0("fofl_", ao$fleet_names))) -> out
  
  return(out)
  
}

# gmacs_write_dat() ----
## write .dat file

## args:
### input - named list of data needed to write file (need more info here), look at output of gmacs_read_dat()
### file - file path to write .dat file

## output: file saved in specified location
## example: gmacs_write_dat(input, file = "./AIGKC/models/2024/may/EAG/23.1b/EAG_23_1b.dat")

gmacs_write_dat <- function(input, file){
  
  # create output matrix
  out <- matrix(nrow = 1e6)
  last <- 0 # location tracker
  
  # version ----
  out[last + 1,] <- "##==============================================================="; last <- last + 1
  if(!is.null(input$version)){out[last + 1,] <- paste("# GMACS", input$version, "Data File"); last <- last + 1}
  if(!is.null(input$model_name)){out[last + 1,] <- paste("# Model", input$model_name); last <- last + 1}
  if(is.null(version)){out[last + 1,] <- "GMACS Data File"; last <- last + 1}
  out[last + 1,] <- "##==============================================================="; last <- last + 1
  # dimensions ---- 
  out[last + 1,] <- "  "; last <- last + 1
  out[last + 1,] <- "  "; last <- last + 1
  out[last + 1,] <- paste(input$start_year, "# initial (start year)"); last <- last + 1
  out[last + 1,] <- paste(input$terminal_year, "# terminal (end year)"); last <- last + 1
  out[last + 1,] <- paste(input$n_season, "# Number of seasons"); last <- last + 1
  out[last + 1,] <- paste(input$n_fleets, "# Number of distinct data groups (fleet, among fishing fleets and surveys)"); last <- last + 1
  out[last + 1,] <- paste(input$n_sex, "# Number of sexes"); last <- last + 1
  out[last + 1,] <- paste(input$n_shell, "# Number of shell condition types"); last <- last + 1
  out[last + 1,] <- paste(input$n_maturity, "# Number of maturity types"); last <- last + 1
  out[last + 1,] <- paste(input$n_size_bins, "# Number of size-classes in the model"); last <- last + 1
  out[last + 1,] <- paste(input$recruitment_season, "# Season recruitment occurs"); last <- last + 1
  out[last + 1,] <- paste(input$growth_season, "# Season molting and growth occurs"); last <- last + 1
  out[last + 1,] <- paste(input$ssb_season, "# Season to calculate SSB (changed to match Feb mating)"); last <- last + 1
  out[last + 1,] <- paste(input$n_matrix_season, "# Season for N output"); last <- last + 1
  out[last + 1,] <- "# maximum size-class (males then females)"; last <- last + 1
  out[last + 1,] <- input$max_size_bin; last <- last + 1
  out[last + 1,] <- "# size_breaks (a vector giving the break points between size intervals with dimension nclass+1, lower limits of bins)"; last <- last + 1
  out[last + 1,] <- paste(input$size_bins, collapse = " "); last <- last + 1
  
  # taus ----
  out[last + 1,] <- "# Natural mortality per season input type (1 = vector by season, 2 = matrix by season/year)"; last <- last + 1
  out[last + 1,] <- input$nat_m_input_type; last <- last + 1
  out[last + 1,] <- "# Proportion of the total natural mortality to be applied each season"; last <- last + 1
  for (i in 1:nrow(input$tau)){
    
    input$tau %>%
      dplyr::select(2:ncol(.), year) %>%
      mutate(year = paste("#", year)) %>%
      t %>%
      .[,i] %>%
      str_c(., collapse = " ") -> out[last + i,]
  }
  last <- last + nrow(input$tau)
  
  # fleet names ----
  out[last + 1,] <- "# fleetname"; last <- last + 1
  out[last + 1,] <- paste(input$fleet_names, collapse = " "); last <- last + 1
  
  # season type ----
  out[last + 1,] <- "#Season type: Set to 1 for continuous F and 0 for instantanous F"; last <- last + 1
  out[last + 1,] <- paste(input$season_type, collapse = " "); last <- last + 1
  
  # catch data ----
  out[last + 1,] <- "  "; last <- last + 1
  out[last + 1,] <- "  "; last <- last + 1
  out[last + 1,] <- "##------------------------------------------------------------"; last <- last + 1
  out[last + 1,] <- "##--CATCH DATA------------------------------------------------"; last <- last + 1
  out[last + 1,] <- "##------------------------------------------------------------"; last <- last + 1
  out[last + 1,] <- paste(input$catch_input_format, "#--input catch data format (0: old format, 1: new format)"); last <- last + 1
  out[last + 1,] <- paste(input$n_catch_series, "#--Number of catch data frames"); last <- last + 1
  out[last + 1,] <- "# Number of lines for each dataframe (this is not correct for retrospective analyses)"; last <- last + 1
  out[last + 1,] <- paste(input$n_catch_rows, collapse = " "); last <- last + 1
  out[last + 1,] <- "##  Type of catch: 1 = retained, 2 = discard, 0= total "; last <- last + 1
  out[last + 1,] <- "##  Units of catch: 1 = biomass, 2 = numbers"; last <- last + 1
  out[last + 1,] <- "## Mult: 1= use data as thy are, 2 = multiply by this number (e.g., lbs to kg)"; last <- last + 1
  # catch data frame
  if(input$catch_input_format == 0){
    #for(i in 1:input$n_catch_series){
    for(i in 1:input$n_catch_series){
      out[last + 1,] <- "#year season fleet sex obs cv type units mult effort discard_mortality"; last <- last + 1
      for(j in 1:input$n_catch_rows[i]){
        
        input$catch %>%
          mutate(series =  rep(1:input$n_catch_series, input$n_catch_rows)) %>%
          filter(series == i) %>%
          dplyr::select(-series) %>%
          t %>%
          .[,j] %>%
          str_c(., collapse = " ") -> out[last + j,]
        
      }
      last <- last + input$n_catch_rows[i]
    }
  }
  
  # index data ----
  
  out[last + 1,] <- "  "; last <- last + 1
  out[last + 1,] <- "  "; last <- last + 1
  out[last + 1,] <- "##------------------------------------------------------------"; last <- last + 1
  out[last + 1,] <- "##--RELATIVE ABUNDANCE DATA-----------------------------------"; last <- last + 1
  out[last + 1,] <- "##------------------------------------------------------------"; last <- last + 1
  out[last + 1,] <- paste(input$index_input_format, "#--input format type (0: old format, 1: new format)"); last <- last + 1
  out[last + 1,] <- paste(input$n_index_series, "#--Number of dataframes"); last <- last + 1
  out[last + 1,] <- "# Type of 'survey' catchability (1=Selectivity; 2=Selectivity+Retention), by data frame"; last <- last + 1
  out[last + 1,] <- paste(input$index_type, collapse = " "); last <- last + 1
  out[last + 1,] <- "# Number of data rows, by data frame"; last <- last + 1
  out[last + 1,] <- "#   NOTE: this is not correct for retrospective analyses"; last <- last + 1
  out[last + 1,] <- paste(input$n_index_rows, collapse = " "); last <- last + 1
  out[last + 1,] <- "#series year season fleet sex maturity obs cv units timing"; last <- last + 1
  # index data frame
  if(input$index_input_format == 0){
    for(i in 1:input$n_index_rows){
      input$index %>%
        t %>%
        .[,i] %>%
        str_c(., collapse = " ") -> out[last + i,]
    }
  }
  last <- last + nrow(input$index)
  
  # size data ----
  
  out[last + 1,] <- "  "; last <- last + 1
  out[last + 1,] <- "  "; last <- last + 1
  out[last + 1,] <- "##------------------------------------------------------------"; last <- last + 1
  out[last + 1,] <- "##--SIZE COMPOSITION DATA-------------------------------------"; last <- last + 1
  out[last + 1,] <- "##------------------------------------------------------------"; last <- last + 1
  out[last + 1,] <- paste(input$size_input_format, "#--input format type (0: old format, 1: new format)"); last <- last + 1
  out[last + 1,] <- paste(input$n_size_series, "#--Number of dataframes"); last <- last + 1
  out[last + 1,] <- "# nSizeCompRows_in"; last <- last + 1
  out[last + 1,] <- paste(input$n_size_rows, collapse = " "); last <- last + 1
  out[last + 1,] <- "# nSizeCompCols_in"; last <- last + 1
  out[last + 1,] <- paste(input$n_size_bin_series, collapse = " "); last <- last + 1
  out[last + 1,] <- "## Sex: 1 = male, 2 = female, 0 = both"; last <- last + 1
  out[last + 1,] <- "## Type of catch: 1 = retained, 2 = discard, 0 = total"; last <- last + 1
  out[last + 1,] <- "## Shell: 1 = newshell, 2 = oldshell, 0 = both"; last <- last + 1
  out[last + 1,] <- "## Maturity: 1 = immature, 2 = mature, 0 = both"; last <- last + 1
  out[last + 1,] <- "## Stage1_EffN (nsamp): the stage-1 effective sample size (this can be modified in the CTL file)"; last <- last + 1
  out[last + 1,] <- " "; last <- last + 1
  
  # size data frame
  if(input$size_input_format == 0){
    for(i in 1:input$n_size_series){
      out[last + 1,] <- "# year season fleet sex type shell maturity nsamp data"; last <- last + 1
      for(j in 1:input$n_size_rows[i]){
        
        input$size_comp %>%
          pivot_wider(names_from = size, values_from = obs) %>%
          filter(org_series == i) %>%
          dplyr::select(-org_series) %>%
          t %>%
          .[,j] %>%
          str_c(., collapse = " ") -> out[last + j,]
        
      }
      last <- last + input$n_size_rows[i]
    }
  }
  
  # growth data ----
  
  out[last + 1,] <- "  "; last <- last + 1
  out[last + 1,] <- "  "; last <- last + 1
  out[last + 1,] <- "##------------------------------------------------------------"; last <- last + 1
  out[last + 1,] <- "##--GROWTH DATA-----------------------------------------------"; last <- last + 1
  out[last + 1,] <- "##------------------------------------------------------------"; last <- last + 1
  out[last + 1,] <- paste(input$growth_data_type, "# GrowthObsType"); last <- last + 1
  out[last + 1,] <- paste(input$n_growth_obs, "# nGrowthObs"); last <- last + 1
  # growth data frame
  if(input$growth_data_type == 3){
    out[last + 1,] <- "# size-class-at-release, sex, size-class-at-recapture, and time-at-liberty fleet recapture_year number"; last <- last + 1
    for(i in 1:input$n_growth_obs){
      input$growth %>%
        t %>%
        .[,i] %>%
        str_c(., collapse = " ") -> out[last + i,]
    }
    last <- last + nrow(input$growth)
  }
  
  # environmental data ----
  
  out[last + 1,] <- "  "; last <- last + 1
  out[last + 1,] <- "  "; last <- last + 1
  out[last + 1,] <- "##------------------------------------------------------------"; last <- last + 1
  out[last + 1,] <- "##--ENVIRONMENTAL DATA----------------------------------------"; last <- last + 1
  out[last + 1,] <- "##------------------------------------------------------------"; last <- last + 1
  out[last + 1,] <- paste(input$env_format, "#--input format type (0: old format, 1: new format)"); last <- last + 1
  out[last + 1,] <- paste(input$n_env_series, "#--number of environmental indices "); last <- last + 1
  
  if(input$n_env_series > 0) {stop("Don't know how to read environmental indices!!!")}
  
  # eof ----
  
  out[last + 1,] <- "eof"; last <- last + 1
  out[last + 1,] <- "9999"; last <- last + 1
  # write
  writeLines(out[!is.na(out[,1]),], file)
  
}

# gmacs_write_files_dat() ----

## write gmacs.dat file

## args:
### input - named list of data needed to write file (need more info here), look at output of gmacs_read_files_dat()
### file - file path to write .dat file

## output: file saved in specified location
## example: gmacs_write_files_dat(input, file = "./AIGKC/models/2024/may/EAG/23.1b/EAG_23_1b.dat")

gmacs_write_files_dat <- function(input, file) {
  
  if(is.null(input$version)){input$version = "2.20.16"}
  if(input$version %in% c("2.20.16", "2.20.14")){
    # setup ---- 
    
    # create output matrix
    out <- matrix(nrow = 1e6)
    last <- 0 # location tracker
    
    # version ----
    out[last + 1,] <- "##==============================================================="; last <- last + 1
    out[last + 1,] <- paste("# GMACS", input$version, "Setup File"); last <- last + 1
    if(!is.null(input$model_name)){out[last + 1,] <- paste("# Model", input$model_name); last <- last + 1}
    out[last + 1,] <- "##==============================================================="; last <- last + 1
    out[last + 1,] <- " "; last <- last + 1
    out[last + 1,] <- " "; last <- last + 1
    
    # files ----
    out[last + 1,] <- "# datafile"; last <- last + 1
    out[last + 1,] <- input$dat_file; last <- last + 1
    out[last + 1,] <- "# controlfile"; last <- last + 1
    out[last + 1,] <- input$ctl_file; last <- last + 1
    out[last + 1,] <- "# projectfile"; last <- last + 1
    out[last + 1,] <- input$prj_file; last <- last + 1
    
    # units ----
    out[last + 1,] <- "# weightunit"; last <- last + 1
    out[last + 1,] <- input$wt_unit; last <- last + 1
    out[last + 1,] <- "# numbersunit"; last <- last + 1
    out[last + 1,] <- input$n_unit; last <- last + 1
    
    # stock ----
    out[last + 1,] <- "# StockName"; last <- last + 1
    out[last + 1,] <- input$stock; last <- last + 1
    
    # jittering ----
    out[last + 1,] <- "# IsJittered"; last <- last + 1
    out[last + 1,] <- input$jitter; last <- last + 1
    out[last + 1,] <- "# sdJitter"; last <- last + 1
    out[last + 1,] <- input$jitter_sd; last <- last + 1
    
    # out options ----
    out[last + 1,] <- "# OutRefPars"; last <- last + 1
    out[last + 1,] <- input$out_ref_pars; last <- last + 1
    out[last + 1,] <- "# OutRecruit"; last <- last + 1
    out[last + 1,] <- input$out_recruit; last <- last + 1
    out[last + 1,] <- "# OutSSB"; last <- last + 1
    out[last + 1,] <- input$out_ssb; last <- last + 1
    out[last + 1,] <- "# Outfbar"; last <- last + 1
    out[last + 1,] <- input$out_fbar; last <- last + 1
    out[last + 1,] <- "# OutDynB0"; last <- last + 1
    out[last + 1,] <- input$out_dynb0; last <- last + 1
    
    
    # retro peels ----
    out[last + 1,] <- "# nyrRetro"; last <- last + 1
    out[last + 1,] <- input$nyr_retro; last <- last + 1
    
    # run options ----
    out[last + 1,] <- paste(input$max_phase, "# Maximum phase (stop the estimation after this phase)", sep = " "); last <- last + 1
    if(is.null(input$max_function_calls)){input$max_function_calls <- -1}
    out[last + 1,] <- paste(input$max_function_calls, "# Maximum number of function calls", sep = " "); last <- last + 1
    if(is.null(input$calc_ref_points)){input$calc_ref_points <- 1}
    out[last + 1,] <- paste(input$calc_ref_points, "# Calculate reference points (0=no)", sep = " "); last <- last + 1
    if(is.null(input$use_pin)){input$use_pin <- 0}
    out[last + 1,] <- paste(input$use_pin, "# use pin file (0=no, 1=yes)", sep = " "); last <- last + 1
    if(is.null(input$verbose)){input$verbose <- 1}
    out[last + 1,] <- paste(input$verbose, "# VERBOSE FLAG (0 = off, 1 = on, 2 = objective func; 3 diagnostics)", sep = " "); last <- last + 1
    
    ## eof
    out[last + 1,] <- "eof"
    out[last + 1,] <- "9999"
    
    
  }  
  
  # write
  writeLines(out[!is.na(out[,1]),], file)
  
}

# gmacs_do_exe() ----

## run gmacs.exe program and tune length composition weights

## args:
### gmacs.dat - file path to gmacs.dat file
### pin - T/F use pin file
### wait - passed to shell(): a logical (not NA) indicating whether the R interpreter should wait for the command to finish, or run it asynchronously.
### reweight - T/F tune size composition weights
### level - level of convergence for size comp wts
### max_iter - maximum iterations, convergence shouldn't take forever
### reweight_only - T/F no initial gmacs run

gmacs_do_exe <- function(gmacs.dat, pin = F, wait = T, reweight = T, level = 0.01, max_iter = 100, reweight_only = F) {
  
  options(warn = -1) 
  # get parent directory
  dir <- dirname(gmacs.dat)
  # save working dir
  wd <- getwd()
  setwd(dir) # change wd
  # check for other needed inputs
  if(!file.exists("gmacs.exe")){stop("Cannot find gmacs.exe!!")}
  dat <- readLines("./gmacs.dat")
  if(!file.exists(file.path(dat[grep("\\.dat", dat)]))) {stop(paste("Cannot find", file.path(dat[grep("\\.dat", dat)]), "!!"))}
  if(!file.exists(file.path(dat[grep("\\.ctl", dat)]))) {stop(paste("Cannot find", file.path(dat[grep("\\.ctl", dat)]), "!!"))}
  if(!file.exists(file.path(dat[grep("\\.prj", dat)]))) {stop(paste("Cannot find", file.path(dat[grep("\\.prj", dat)]), "!!"))}
  if(pin == T){
    dat[grep("pin", dat)] <- "1 # use pin file (0=no, 1=yes)"   
    writeLines(dat, "./gmacs.dat")
    if(!file.exists("gmacs.pin")) {stop(paste("Cannot find gmacs.pin!!"))}
  }
  if(pin == F){
    dat[grep("pin", dat)] <- "0 # use pin file (0=no, 1=yes)"   
    writeLines(dat, "./gmacs.dat")
  }
  if(reweight_only == F){
    # run gmacs.exe
    if(wait == F){shell("gmacs.exe", wait = F, intern = F)}else{shell("gmacs.exe")}
  }
  # do reweighting
  if(reweight == T) {
    
    # check wts convergence first time
    # get lambdas from ctl file
    readLines("gmacs_in.ctl")[grep("# Lambda for effective sample size", readLines("gmacs_in.ctl"))] %>%
      str_split(" ") %>% unlist %>% as.numeric %>% na.omit -> ctl_wts
    # get lambdas from allout file
    readLines("Gmacsall.out")[grep("Francis_weights", readLines("Gmacsall.out"))+1] %>%
      str_split(" ") %>% unlist %>% as.numeric %>% na.omit -> rep_wts
    tibble(ctl_wts, rep_wts) %>%
      mutate(diff = abs(ctl_wts - rep_wts) >= level) %>%
      pull(diff) %>% sum -> test
    if(test == 0){setwd(wd); return(paste0("wts convergence reached level = ", level))}
    if(test > 0){
      converged <- F
      # turn off reference point calculation
      dat <- readLines("./gmacs_files_in.dat")
      dat[33] <- "0 # Calculate reference points (0=no)" 
      writeLines(dat, "./gmacs.dat")
      ctl_file <- dat[6] # ctl file path
      # start a counter
      iteration <- 0
      while(iteration < max_iter && converged == F){
        # change ctl wts
        ctl <- readLines("gmacs_in.ctl") 
        ctl[grep("# Lambda for effective sample size", ctl)] <- paste(str_flatten(rep_wts, collapse = " "), "# Lambda for effective sample size")
        writeLines(ctl, ctl_file)
        # run gmacs
        shell("gmacs.exe")
        # test convergence
        readLines("gmacs_in.ctl")[grep("# Lambda for effective sample size", readLines("gmacs_in.ctl"))] %>%
          str_split(" ") %>% unlist %>% as.numeric %>% na.omit -> ctl_wts
        # get lambdas from allout file
        readLines("Gmacsall.out")[grep("Francis_weights", readLines("Gmacsall.out"))+1] %>%
          str_split(" ") %>% unlist %>% as.numeric %>% na.omit -> rep_wts
        tibble(ctl_wts, rep_wts) %>%
          mutate(diff = abs(ctl_wts - rep_wts) >= level) %>%
          pull(diff) %>% sum -> test
        if(sum(test) == 0) {converged = T}
        iteration <- iteration + 1
      }
      # turn on reference point calculation, run gmacs once more
      dat[33] <- "1 # Calculate reference points (0=no)" 
      writeLines(dat, "./gmacs.dat")
      shell("gmacs.exe")
      setwd(wd)
      # done
      if(converged == F) {return(paste0("wts did not reach convergence level = ", level))}
      if(converged == T) {return(paste0("wts convergence reached level = ", level))}
      
    }
    
  } else{setwd(wd); return("done!")}
  
}

# gmacs_do_jitter() ----

# do gmacs jitter runs

### gmacs.dat - file path to gmacs.dat file
### sd - jitter standard deviation
### iter - number of iteration of jittering to run
### wait - passed to shell(): a logical (not NA) indicating whether the R interpreter should wait for the command to finish, or run it asynchronously.
### save_csv - T/F, save csv file output
### csv_dir - file directory in which to save output
### save_plot - T/F, create histograms, default = T
### plot_dir - file directory in which to save plots
### model_name - character string to save as object in output, later to be used for plot legends. example: "23.1b"

gmacs_do_jitter <- function(gmacs.dat, sd, iter, wait = T,
                            save_csv = T, csv_dir = NULL, save_plot = T, plot_dir = NULL, model_name = NULL, plot_only = F, version = NULL) {
  
  # create output directories
  if(save_csv == T & is.null(csv_dir)) {csv_dir <- file.path(dirname(gmacs.dat), "output"); dir.create(csv_dir, showWarnings = F, recursive = TRUE)}
  if(!is.null(csv_dir) && !file.exists(csv_dir)) {dir.create(csv_dir, showWarnings = F, recursive = TRUE)}
  if(save_plot == T & is.null(plot_dir)) {plot_dir <- file.path(dirname(gmacs.dat), "plots"); dir.create(plot_dir, showWarnings = F, recursive = TRUE)}
  if(!is.null(plot_dir) && !file.exists(plot_dir)) {dir.create(plot_dir, showWarnings = F, recursive = TRUE)}
  
  # directory ----
  
  options(warn = -1) 
  # get parent directory
  dir <- dirname(gmacs.dat)
  # save working dir
  wd <- getwd()
  setwd(dir) # change wd
  
  if(plot_only == F){
    
    # set up ----
    
    # check for other needed inputs
    if(!file.exists("gmacs.exe")){setwd(wd); stop("Cannot find gmacs.exe!!")}
    dat <- gmacs_read_files_dat("./gmacs.dat", version = version)
    
    if(!file.exists(file.path(dat[grep("\\.dat", dat)]))) {setwd(wd); stop(paste("Cannot find", file.path(dat[grep("\\.dat", dat)]), "!!"))}
    if(!file.exists(file.path(dat[grep("\\.ctl", dat)]))) {setwd(wd); stop(paste("Cannot find", file.path(dat[grep("\\.ctl", dat)]), "!!"))}
    if(!file.exists(file.path(dat[grep("\\.prj", dat)]))) {setwd(wd); stop(paste("Cannot find", file.path(dat[grep("\\.prj", dat)]), "!!"))}
    # make sure pin file is being not being usedused as expected
    dat$use_pin <- 0  
    # turn on reference points
    dat$calc_ref_points <- 1
    # set up jitter
    dat$jitter <- 1
    dat$jitter_sd <- sd
    
    # do jitter ----
    
    # create subdirectory for jitter run files
    dir.create("./jitter")
    # rewrite gmacs.dat
    gmacs_write_files_dat(dat, file = "./jitter/gmacs.dat")
    # put files in - this likely will not work with relative pathes
    file.copy(c(dat$dat_file, dat$ctl_file, dat$prj_file, "gmacs.exe"), 
              to = "./jitter")
    # set working 
    setwd("./jitter")
    # names of necessary gmacs files
    gfiles <- list.files()
    
    # do jitter runs
    out <- tibble(iteration = 1:iter,
                  obj_function = NA,
                  max_gradient = NA,
                  catch_lik = NA,
                  index_lik = NA,
                  size_lik = NA,
                  mmb_curr = NA,
                  bmsy = NA,
                  ofl = NA)
    for (i in 1:iter) {
      rundir <- paste0("./run_", i)
      dir.create(rundir)
      file.copy(from = gfiles, to = rundir)
      # do gmacs run
      setwd(rundir)
      while(!("gmacs.rep" %in% list.files())){shell("gmacs.exe", wait = wait)}
      ao <- gmacs_read_allout("./Gmacsall.out", version = version)
      out$obj_function[i] <- ao$objective_function
      out$max_gradient[i] <- ao$max_gradient
      out$catch_lik[i] <- ao$likelihoods_by_type$net_lik[ao$likelihoods_by_type$process == "catch"]
      out$index_lik[i] <- ao$likelihoods_by_type$net_lik[ao$likelihoods_by_type$process == "index"]
      out$size_lik[i] <- ao$likelihoods_by_type$net_lik[ao$likelihoods_by_type$process == "size"]
      if(ref_points == T) {
        out$mmb_curr[i] <- ao$mmb_curr
        out$bmsy[i] <- ao$bmsy
        out$ofl[i] <- ao$ofl_tot
      }
      setwd("..")
    }
    out <- out %>% dplyr::select(where(function(x) !all(is.na(x))))
    # return to model directory
    setwd("..")
    
  }
  
  # get mle estimates of objects
  mle_ao <- gmacs_read_allout("./Gmacsall.out", model_name = model_name, version = version)
  # set wd back to original
  setwd(wd)
  
  # plots ----
  
  if(plot_only == T){out <- read_csv(paste0(csv_dir, "/", mle_ao$model_name, "_jitter_sd_", sd, ".csv"))}
  
  # obj fxn
  ggplot()+
    geom_histogram(data = out, aes(x = obj_function), color = 1, fill = "grey80", 
                   width = 1)+
    geom_vline(xintercept = mle_ao$objective_function, linetype = 2, color = 2)+
    scale_x_continuous(labels = scales::comma)+
    labs(x = "Negative Log-likelihood", y = "Jitter Runs") -> p_obj
  
  ggplot()+
    geom_point(aes(x = out$obj_function, y = out$mmb_curr))+
    geom_point(aes(x = mle_ao$objective_function, y = mle_ao$mmb_curr), size = 2, shape = 21, fill = "white")+
    scale_y_continuous(labels = scales::comma, limits = c(0, NA))+
    labs(x = "Negative Log-likelihood", y = paste0("MMB (", gub("_", " ", mle_ao$wt_units), ")") ) -> p_mmb
  
  ggplot()+
    geom_point(aes(x = out$obj_function, y = out$bmsy))+
    geom_point(aes(x = mle_ao$objective_function, y = mle_ao$bmsy), size = 2, shape = 21, fill = "white")+
    scale_y_continuous(labels = scales::comma, limits = c(0, NA))+
    labs(x = "Negative Log-likelihood", y = bquote(B["MSY"]~"("~.(gub("_", " ", mle_ao$wt_units))~")") ) -> p_bmsy
  
  ggplot()+
    geom_point(aes(x = out$obj_function, y = out$ofl))+
    geom_point(aes(x = mle_ao$objective_function, y = mle_ao$ofl_tot), size = 2, shape = 21, fill = "white")+
    scale_y_continuous(labels = scales::comma, limits = c(0, NA))+
    labs(x = "Negative Log-likelihood", y = paste0("OFL (", gub("_", " ", mle_ao$wt_units), ")") ) -> p_ofl
  
  
  if(save_plot == T){ggsave(filename = paste0(plot_dir, "/", mle_ao$model_name, "_jitter_sd_", sd, ".png"),
                            plot = (p_obj + p_mmb) / (p_bmsy + p_ofl),
                            height = 6, width = 8, units = "in")}
  
  # output ----
  plots <- list(p_obj, p_mmb, p_bmsy, p_ofl)
  if(save_csv == T) {write_csv(out, paste0(csv_dir, "/", mle_ao$model_name, "_jitter_sd_", sd, ".csv"))}
  
  if(save_plot == F){return(c(list(out), plots))}else{return(out)}
  
}

# gmacs_do_retrospective() ----

## run retrospective analysis

### gmacs.dat - file path to gmacs.dat file
### n_peel - number of retrospective peels
### wait - passed to shell(): a logical (not NA) indicating whether the R interpreter should wait for the command to finish, or run it asynchronously.
### pin - T/F use pin file
### plot_only - T/F, only make plot (i.e. already ran retrospectives)
### plot_mmb - T/F, make plot of mmb by peel
### save_plot - T/F, create histograms, default = T
### plot_dir - file directory in which to save plots
### model_name - character string to save as object in output, later to be used for plot legends. example: "23.1b"

gmacs_do_retrospective <- function(gmacs.dat, n_peel, wait = T, pin = F, plot_only = F, plot_mmb = T, save_plot = T, 
                                   plot_dir = NULL, model_name = NULL, version = NULL) {
  # create output directories
  if(save_plot == T & is.null(plot_dir)) {plot_dir <- file.path(dirname(gmacs.dat), "plots"); dir.create(plot_dir, showWarnings = F, recursive = TRUE)}
  if(!is.null(plot_dir) && !file.exists(plot_dir)) {dir.create(plot_dir, showWarnings = F, recursive = TRUE)}
  
  # directory ----
  
  options(warn = -1) 
  # get parent directory
  dir <- dirname(gmacs.dat)
  # save working dir
  wd <- getwd()
  setwd(dir) # change wd

  # analysis ----
  if(plot_only == F) {
    # set up ----
    
    # check for other needed inputs
    if(!file.exists("gmacs.exe")){setwd(wd); stop("Cannot find gmacs.exe!!")}
    # look for gmacs_file_in.dat - if not present, run gmacs
    # if(!file.exists("./gmacs_files_in.dat")) {setwd(wd); gmacs_do_exe(gmacs.dat, pin = pin, reweight = F)}
    ao_full <- gmacs_read_allout("Gmacsall.out", version = version)
    dat <- gmacs_read_files_dat("gmacs.dat", version = version)
    if(!file.exists(file.path(dat$dat_file))) {setwd(wd); stop(paste("Cannot find", file.path(dat$dat_file), "!!"))}
    if(!file.exists(file.path(dat$ctl_file))) {setwd(wd); stop(paste("Cannot find", file.path(dat$dat_file), "!!"))}
    if(!file.exists(file.path(dat$prj_file))) {setwd(wd); stop(paste("Cannot find", file.path(dat$prj_file), "!!"))}
    # create retrospectives dir
    dir.create("./retrospectives",recursive = T, showWarnings = F)
    files_to_copy <- c(dat$dat_file, dat$ctl_file, dat$prj_file, "gmacs.exe")
    # make sure pin file is being used as expected
    if(pin == T){
      dat$use_pin <- 1
      if(!file.exists("gmacs.pin")) {setwd(wd); stop("Cannot find gmacs.pin!!"); files_to_copy <- c(files_to_copy, "gmacs.pin")}
    }
    # copy files to retro dir
    file.copy(files_to_copy, "./retrospectives",overwrite = T, recursive = T)
    # save dat file there
    gmacs_write_files_dat(dat, file = "./retrospectives/gmacs.dat")
    
    # do retrospective runs ----
    
    setwd("./retrospectives")
    gfiles <- list.files()
    for (i in 1:n_peel){
      # create peel sub-directory
      dir.create(paste0("retro_", i))
      file.copy(gfiles, paste0("retro_", i))
      setwd(paste0("retro_", i))
      # set up gmacs.dat for retro analysis
      dat <- gmacs_read_files_dat("gmacs.dat", version = version)
      dat$nyr_retro <- i
      gmacs_write_files_dat(dat, "gmacs.dat")
      # run gmacs
      shell("gmacs.exe", wait = wait)
      setwd("..")
    }      
    
  }
  
  if(plot_only == T) {
    ao_full <- gmacs_read_allout("Gmacsall.out", version = version)
    setwd("./retrospectives")  
  }

  
  # plot ----
  
  if(plot_mmb == F){setwd(wd); return(mohn_rho)}

  if(plot_mmb == T){
    ao <- list()
    for(i in 1:n_peel){
      ao[[i]] <- gmacs_read_allout(file.path(paste0("retro_", i), "Gmacsall.out"), i, version = version)
    }
    setwd(wd) # return to base working directory
    data_summary <- gmacs_get_derived_quantity_summary(ao)
    
    data_summary %>% 
      group_by(model) %>%
      mutate(terminal_yr = as.character(max(year))) %>% ungroup %>%
      left_join(ao_full$derived_quant_summary %>% transmute(year, ssb_full = ssb)) %>%
      filter(year == terminal_yr) %>%
      mutate(rho = (ssb - ssb_full) / ssb_full) %>%
      pull(rho) %>% mean -> mohn_rho
    
    data_summary %>%
      group_by(model) %>%
      mutate(terminal_yr = as.character(max(year))) %>% ungroup %>%
      ggplot()+
      geom_line(aes(x = factor(year), y = ssb, group = terminal_yr, color = terminal_yr))+
      geom_text_npc(aes(npcx = "right", npcy = "top"),
                    label = latex2exp::TeX(paste("Mohn's $\\rho$ = ", round(mohn_rho, 3))),
                    check_overlap = T, size = 3)+
      scale_x_discrete(breaks = yraxis$breaks, labels = yraxis$labels)+
      scale_y_continuous(labels = scales::comma, limits = c(0, NA))+
      scale_color_manual(values = cbpalette)+
      labs(x = NULL, y = paste0("MMB (", unique(data_summary$wt_units), ")"), color = "Terminal Year") -> p_mmb
    
    if(save_plot == T){
      ggsave(file.path(plot_dir, paste0(model_name,"_retrospective_mmb.png")), plot = p_mmb, height = 3, width = 6)
      return("done")
    }
    if(save_plot == F){return(list(mohn_rho, p_mmb))}
    
  }
}


# gmacs_do_ofl_dist() ----

## run gmacs with MCMC of reference points

### gmacs.dat - file path to gmacs.dat file
### n_replicates - length of MCMC chain
### n_draws - number of draws to save
### wait - passed to shell(): a logical (not NA) indicating whether the R interpreter should wait for the command to finish, or run it asynchronously.
### save_plot - T/F, create histograms, default = T
### plot_dir - file directory in which to save plots
### model_name - character string to save as object in output, later to be used for plot legends. example: "23.1b"
### version - gmacs version, example:  "2.20.16"

gmacs_do_ofl_dist <- function(gmacs.dat, n_replicates, n_draws, wait = T, model_name = NULL, version = NULL, 
                              save_plot = T, plot_dir = NULL){
  
  # create output directories
  if(save_plot == T & is.null(plot_dir)) {plot_dir <- file.path(dirname(gmacs.dat), "plots"); dir.create(plot_dir, showWarnings = F, recursive = TRUE)}
  if(!is.null(plot_dir) && !file.exists(plot_dir)) {dir.create(plot_dir, showWarnings = F, recursive = TRUE)}
  
  options(warn = -1) 
  # get parent directory
  dir <- dirname(gmacs.dat)
  # save working dir
  wd <- getwd()
  setwd(dir) # change wd
  
  # check for other needed inputs
  #if(!file.exists("gmacs.exe")){setwd(wd); stop("Cannot find gmacs.exe!!")}
  dat <- gmacs_read_files_dat("./gmacs.dat", version = version)
  
  if(!file.exists(file.path(dat[grep("\\.dat", dat)]))) {setwd(wd); stop(paste("Cannot find", file.path(dat[grep("\\.dat", dat)]), "!!"))}
  if(!file.exists(file.path(dat[grep("\\.ctl", dat)]))) {setwd(wd); stop(paste("Cannot find", file.path(dat[grep("\\.ctl", dat)]), "!!"))}
  if(!file.exists(file.path(dat[grep("\\.prj", dat)]))) {setwd(wd); stop(paste("Cannot find", file.path(dat[grep("\\.prj", dat)]), "!!"))}
  
  # set up a separate subdirectory for a run with the mcmc
  dir.create("ofl_distribution")
  files_to_copy <- c("gmacs.exe", dat[2:4], "gmacs.dat")
  file.copy(as.character(files_to_copy), to = file.path("ofl_distribution", files_to_copy), recursive = T)
  setwd("./ofl_distribution")
  
  # run gmacs with mcmc
  call <- paste0("gmacs -mcmc ", n_replicates, " -mcsave ", n_draws)
  shell(call, wait = wait)
  shell("gmacs -mceval", wait = wait)
  
  # read output
  mcout <- gmacs_read_mcoutREF(file = "mcoutREF.rep")
  ao <- gmacs_read_allout("../Gmacsall.out", model_name = model_name, version = version)
  
  # make a plot
  ## ofl pdf
  ggplot()+
    geom_density(data = mcout, aes(x = ofl), fill = "grey80", color = "grey30", alpha = 0.5)+
    scale_x_continuous(labels = scales::comma)+
    labs(x = paste0("OFL (", gsub("_", " ", ao$wt_units),")"), y = "Probability Denisty", fill = NULL)+
    scale_fill_manual(values = cbpalette)+
    theme(legend.position = c(1, 1),
          legend.justification = c(1, 1)) -> ofl_pdf
  ## stock status cdf
  mcout %>%
    mutate(mmbprj = ao$mmb_curr,
           status = mmbprj /bmsy) %>%
    ggplot()+
    stat_ecdf(aes(x = status, geom = "step", group = model, color = model))+
    geom_vline(xintercept = 0.5, linetype = 2, color = "firebrick")+
    geom_text(aes(x = 0.48, y = 0.5, label = "Overfished"), size = 4, angle = 90)+
    labs(x = bquote(MMB[prj] ~"/"~B["35%"]), y = "Cumulative Density", color = NULL)+
    scale_color_manual(values = cbpalette)+
    theme(legend.position = c(0.2, 1),
          legend.justification = c(0.2, 1)) -> status_cdf
  
  plot_out <- ofl_pdf / status_cdf
  
  # return to base dir
  setwd(wd)
  
  if(save_plot == F) {
    return(list(mcout = mcout, plot = plot_out))
  }
  if(save_plot == T){ggsave(filename = paste0(plot_dir, "/ref_point_dist.png"),
                            plot = plot_out,
                            height = 6, width = 5, units = "in")
    return("done")}
  
}

# gmacs_get_catch_summary() ----

## isolate catch summary data by model

## args:
### all_out - output from gmacs_read_allout as nested list, example: all.out = list(mod_23.0a, mod_23.1b)
### file - file paths to Gmacsall.out for each model to compare, passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided
### model_name - character string passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided

gmacs_get_catch_summary <- function(all_out = NULL, file = NULL, model_name = NULL, version = NULL){
  
  # bring in all out data ----
  
  if(!is.null(file) && is.null(all_out)) {
    if(is.null(model_name)) {stop("Must supply model name(s)!!")}
    # read all out file
    all_out <- purrr::map2(file, model_name, gmacs_read_allout, version = version); names(all_out) <- paste0("model", model_name)
  }
  # extract catch data ----
  
  tibble(mod = names(all_out),
         all_out = all_out) %>%
    mutate(data = purrr::map(all_out, function(x) {
      x$catch_fit_summary %>% 
        mutate(wt_units = gsub("_", " ", x$wt_units),
               n_units = gsub("_", " ", x$n_units),
               model = as.character(x$model_name)) 
    })) %>% transmute(data) %>% unnest(data) %>%
    dplyr::select(ncol(.), 1:(ncol(.)-1))-> out
  
  return(out)
  
  
}

# gmacs_get_index_summary() ----

## isolate index summary data by model

## args:
### all_out - output from gmacs_read_allout as nested list, example: all.out = list(mod_23.0a, mod_23.1b)
### file - file paths to Gmacsall.out for each model to compare, passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided
### model_name - character string passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided

gmacs_get_index_summary <- function(all_out = NULL, file = NULL, model_name = NULL){
  
  # bring in all out data ----
  if(!is.null(file) && is.null(all_out)) {
    if(is.null(model_name)) {stop("Must supply model name(s)!!")}
    # read all out file
    all_out <- purrr::map2(file, model_name, gmacs_read_allout); names(all_out) <- paste0("model", model_name)
  }
  # extract index data ----
  
  tibble(mod = names(all_out),
         all_out = all_out) %>% 
    mutate(data = purrr::map(all_out, function(x) {
      x$index_fit_summary %>% 
        mutate(wt_units = gsub("_", " ", x$wt_units),
               n_units = gsub("_", " ", x$n_units),
               model = as.character(x$model_name))
    })) %>% transmute(data) %>% unnest(data) %>%
    dplyr::select(ncol(.), 1:(ncol(.)-1)) -> out
  
  return(out)
  
}

# gmacs_get_size_summary() ----

## isolate size comp summary data by model

## args:
### all_out - output from gmacs_read_allout as nested list, example: all.out = list(mod_23.0a, mod_23.1b)
### file - file paths to Gmacsall.out for each model to compare, passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided
### model_name - character string passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided

gmacs_get_size_summary <- function(all_out = NULL, file = NULL, model_name = NULL){
  
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
      x$size_fit_summary %>% 
        mutate(model = as.character(x$model_name))
    })) %>% transmute(data) %>% unnest(data) %>%
    dplyr::select(ncol(.), 1:(ncol(.)-1)) -> out
  
  
  return(out)
  
}


# gmacs_get_effective_n() ----

## isolate effective sample size summary data by model

## args:
### all_out - output from gmacs_read_allout as nested list, example: all.out = list(mod_23.0a, mod_23.1b)
### file - file paths to Gmacsall.out for each model to compare, passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided
### model_name - character string passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided

gmacs_get_effective_n <- function(all_out = NULL, file = NULL, model_name = NULL){
  
  # bring in all out data ----
  
  if(!is.null(file) && is.null(all_out)) {
    if(is.null(model_name)) {stop("Must supply model name(s)!!")}
    # read all out file
    all_out <- purrr::map2(file, model_name, gmacs_read_allout); names(all_out) <- paste0("model", model_name)
  }
  
  # extract neff ----
  
  tibble(mod = names(all_out),
         all_out = all_out) %>% 
    mutate(data = purrr::map(all_out, function(x) {
      x$effective_sample_size %>% 
        mutate(model = as.character(x$model_name)) %>%
        bind_cols(distinct(x$size_fit_summary, mod_series, year) %>% transmute(year))
    })) %>% transmute(data) %>% unnest(data) %>%
    dplyr::select(ncol(.) - 1, ncol(.),  1:(ncol(.)-2)) -> out
  
  
  return(out)
  
}


# gmacs_get_derived_quantity_summary() ----

## isolate derived quantity summary data by model

## args:
### all_out - output from gmacs_read_allout as nested list, example: all.out = list(mod_23.0a, mod_23.1b)
### file - file paths to Gmacsall.out for each model to compare, passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided
### model_name - character string passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided

gmacs_get_derived_quantity_summary <- function(all_out = NULL, file = NULL, model_name = NULL, version = NULL){
  
  # bring in all out data ----
  
  if(!is.null(file) && is.null(all_out)) {
    if(is.null(model_name)) {stop("Must supply model name(s)!!")}
    # read all out file
    all_out <- purrr::map2(file, model_name, gmacs_read_allout, version = version); names(all_out) <- paste0("model", model_name)
  }
  
  # extract data ----
  
  tibble(mod = names(all_out),
         all_out = all_out) %>% 
    mutate(data = purrr::map(all_out, function(x) {
      x$derived_quant_summary %>%
        mutate(model = as.character(x$model_name),
               wt_units = gsub("_", " ", x$wt_units),
               n_units = gsub("_", " ", x$n_units))
    })) %>% transmute(data) %>% unnest(data) -> out
  
  return(out)
  
}

# gmacs_get_f() ----

## isolate fully selected fishing mortality by model

## args:
### all_out - output from gmacs_read_allout as nested list, example: all.out = list(mod_23.0a, mod_23.1b)
### file - file paths to Gmacsall.out for each model to compare, passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided
### model_name - character string passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided

gmacs_get_f <- function(all_out = NULL, file = NULL, model_name = NULL){
  
  # bring in all out data ----
  
  if(!is.null(file) && is.null(all_out)) {
    if(is.null(model_name)) {stop("Must supply model name(s)!!")}
    # read all out file
    all_out <- purrr::map2(file, model_name, gmacs_read_allout); names(all_out) <- paste0("model", model_name)
  }
  
  # extract data ----
  
  tibble(mod = names(all_out),
         all_out = all_out) %>% 
    mutate(data = purrr::map(all_out, function(x) {
      x$F_by_sex_fleet_year_season %>%
        mutate(model = as.character(x$model_name),
               n_sex = x$n_sex)
    })) %>% transmute(data) %>% unnest(data) -> out
  
  return(out)
  
}


# gmacs_get_m() ----

## isolate fully selected fishing mortality by model

## args:
### all_out - output from gmacs_read_allout as nested list, example: all.out = list(mod_23.0a, mod_23.1b)
### file - file paths to Gmacsall.out for each model to compare, passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided
### model_name - character string passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided

gmacs_get_m <- function(all_out = NULL, file = NULL, model_name = NULL){
  
  # bring in all out data ----
  
  if(!is.null(file) && is.null(all_out)) {
    if(is.null(model_name)) {stop("Must supply model name(s)!!")}
    # read all out file
    all_out <- purrr::map2(file, model_name, gmacs_read_allout); names(all_out) <- paste0("model", model_name)
  }
  
  # extract data ----
  
  tibble(mod = names(all_out),
         all_out = all_out) %>% 
    mutate(data = purrr::map(all_out, function(x) {
      x$M_by_class %>%
        mutate(model = as.character(x$model_name))
    })) %>% transmute(data) %>% unnest(data) -> out
  
  return(out)
  
}
# gmacs_get_molt_probability() ----

## isolate molt probability by model

## args:
### all_out - output from gmacs_read_allout as nested list, example: all.out = list(mod_23.0a, mod_23.1b)
### file - file paths to Gmacsall.out for each model to compare, passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided
### model_name - character string passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided

gmacs_get_molt_probability <- function(all_out = NULL, file = NULL, model_name = NULL){
  
  # bring in all out data ----
  
  if(!is.null(file) && is.null(all_out)) {
    if(is.null(model_name)) {stop("Must supply model name(s)!!")}
    # read all out file
    all_out <- purrr::map2(file, model_name, gmacs_read_allout); names(all_out) <- paste0("model", model_name)
  }
  
  # extract data ----
  
  tibble(mod = names(all_out),
         all_out = all_out) %>% 
    mutate(data = purrr::map(all_out, function(x) {
      x$molt_probability %>%
        mutate(model = as.character(x$model_name)) %>%
        left_join(x$molt_probability %>%
                    distinct(sex, size, molt_probability, .keep_all = T) %>% 
                    distinct(sex, year) %>% group_by(sex) %>%
                    mutate(year_lead = lead(year)) %>% ungroup() %>%
                    replace_na(list(year_lead = max(x$molt_probability$year))) %>%
                    transmute(sex, year, block = paste0(year, " - ", year_lead)), by = c("sex", "year"))
      })) %>% transmute(data) %>% unnest(data) %>%
    transmute(model, sex, year, size, molt_probability, block) -> out
  
  return(out)
  
}

# gmacs_get_n_matrix() ----

## isolate n matrix data by model

## args:
### all_out - output from gmacs_read_allout as nested list, example: all.out = list(mod_23.0a, mod_23.1b)
### file - file paths to Gmacsall.out for each model to compare, passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided
### model_name - character string passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided

gmacs_get_n_matrix <- function(all_out = NULL, file = NULL, model_name = NULL){
  
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
      x$n_matrix %>% 
        mutate(model = as.character(x$model_name)) 
    })) %>% transmute(data) %>% unnest(data) %>%
    dplyr::select(ncol(.), 1:(ncol(.)-1))-> out
  
  return(out)
  
}

# gmacs_get_slx() ----

## isolate n matrix data by model

## args:
### all_out - output from gmacs_read_allout as nested list, example: all.out = list(mod_23.0a, mod_23.1b)
### file - file paths to Gmacsall.out for each model to compare, passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided
### model_name - character string passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided

gmacs_get_slx <- function(all_out = NULL, file = NULL, model_name = NULL){
  
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
      x$selectivity %>% 
        mutate(model = as.character(x$model_name)) 
    })) %>% transmute(data) %>% unnest(data) %>%
    dplyr::select(ncol(.), 1:(ncol(.)-1))-> out
  
  return(out)
  
}

# gmacs_get_pars() ----

## isolate parameters data by model

## args:
### all_out - output from gmacs_read_allout as nested list, example: all.out = list(mod_23.0a, mod_23.1b)
### file - file paths to Gmacsall.out for each model to compare, passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided
### model_name - character string passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided

gmacs_get_pars <- function(all_out = NULL, file = NULL, model_name = NULL, version = NULL){
  
  # bring in all out data ----
  
  if(!is.null(file) && is.null(all_out)) {
    if(is.null(model_name)) {stop("Must supply model name(s)!!")}
    # read all out file
    all_out <- purrr::map2(file, model_name, gmacs_read_allout, version = version); names(all_out) <- paste0("model", model_name)
  }
  
  # extract catch data ----
  
  tibble(mod = names(all_out),
         all_out = all_out) %>% 
    mutate(data = purrr::map(all_out, function(x) {
      x$parameters %>% 
        mutate(model = as.character(x$model_name)) 
    })) %>% transmute(data) %>% unnest(data) %>%
    dplyr::select(ncol(.), 1:(ncol(.)-1))-> out
  
  return(out)
  
}

# gmacs_get_ref_points() ----

## isolate reference points table by model

## args:
### all_out - output from gmacs_read_allout as nested list, example: all.out = list(mod_23.0a, mod_23.1b)
### file - file paths to Gmacsall.out for each model to compare, passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided
### model_name - character string passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided

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
        transmute(parameter_name, estimate) %>%
        pivot_wider(names_from = parameter_name, values_from = estimate) %>%
        transmute(model = as.character(x$model_name),
                  mmb = BMSY * `Bcurr/BMSY`,
                  b35 = BMSY,
                  b_b35 = `Bcurr/BMSY`,
                  male_rbar = `Male_spr_rbar`,
                  rbar_yrs = paste(x$spr_syr, "-", x$spr_nyr),
                  f35 = `Fmsy_1`,
                  fofl = x$spr_fofl * f35,
                  ofl_tot = OFL_tot)
    
    })) %>% transmute(data) %>% unnest(data) -> out
  
  return(out)
  
}

# gmacs_get_lik() ----

## output likelihood table

## args:
### all_out - output from gmacs_read_allout as nested list, example: all.out = list(mod_23.0a, mod_23.1b)
### file - file paths to Gmacsall.out for each model to compare, passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided
### model_name - character string passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided

gmacs_get_lik <- function(all_out = NULL, file = NULL, model_name = NULL, version = NULL){
  
  # bring in all out data ----
  
  if(!is.null(file) && is.null(all_out)) {
    if(is.null(model_name)) {stop("Must supply model name(s)!!")}
    # read all out file
    all_out <- purrr::map2(file, model_name, gmacs_read_allout, version = version); names(all_out) <- paste0("model", model_name)
  }
  
  # extract catch data ----
  
  tibble(mod = names(all_out),
         all_out = all_out) %>% 
    mutate(data = purrr::map(all_out, function(x) {
      
      x$likelihoods_by_type_and_fleet %>%
        transmute(model = x$model_name, process, net_lik) %>%
        # add number of parameters
        add_row(model = x$model_name, process = "n_pars", net_lik = x$parameters %>% filter(phase > 0) %>% nrow()) %>%
        # add total lik
        add_row(model = x$model_name, process = "total", net_lik = x$likelihoods_by_type %>% filter(process == "total") %>% pull(net_lik))
      
      
    })) %>% transmute(data) %>% unnest(data) %>%
    pivot_wider(names_from = model, values_from = net_lik) -> out
  
  return(out)
  
}

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

# gmacs_get_recruitment_distribution() ----

## compute size distribution of recruit classes

## args:
### all_out - output from gmacs_read_allout as nested list, example: all.out = list(mod_23.0a, mod_23.1b)
### file - file paths to Gmacsall.out for each model to compare, passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided
### model_name - character string passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided
### n_rec_class - number of recruitment classes as list with first element vector for males, second element vector for females

gmacs_get_recruitment_distribution <- function(all_out = NULL, file = NULL, model_name = NULL, n_rec_class = NULL){
  
  if(is.null(n_rec_class)){stop("Provide number of recruitment classes; its not in Gmacsall.out")}
  # bring in all out data ----
  
  if(!is.null(file) && is.null(all_out)) {
    if(is.null(model_name)) {stop("Must supply model name(s)!!")}
    # read all out file
    all_out <- purrr::map2(file, model_name, gmacs_read_allout); names(all_out) <- paste0("model", model_name)
  }
  
  # extract rec dist data ----
  tibble(mod = names(all_out),
         all_out = all_out) %>% 
    mutate(sex = purrr::map(all_out, function(x){if(x$n_sex == 2){return(c("male", "female"))}  else{return("male")}}),
           mod = purrr::map_chr(all_out, function(x){x$model_name})) %>%
    unnest(sex) %>% group_by(sex) %>% nest() %>%
    bind_cols(tibble(n_rec_class)) %>% unnest(data, n_rec_class) %>% arrange(mod) %>% #pull(all_out) %>% .[[1]] -> x
    mutate(rec_dist = purrr::pmap(list(all_out, sex, n_rec_class), function(x, sex, n_rec_class){
      
      if(sex == "male") {
        # pull parameters
        x$parameters %>%
          filter(parameter %in% c("Recruitment_ra-males:", "Recruitment_rb-males:")) %>% pull(estimate) -> pars
        # compute rec dist
        ra <- pars[1]; rbeta <- pars[2]
        size_breaks <- x$size_mid_points - ((x$size_mid_points[2]-x$size_mid_points[1])/2)
        ralpha <- ra / rbeta
        z <- pgamma(size_breaks / rbeta, ralpha)
        rec_sdd <- z - lag(z)
        rec_sdd <- rec_sdd[-1]
        rec_sdd[(n_rec_class + 1):length(rec_sdd)] <- 0
        dist <- c(rec_sdd / sum(rec_sdd, na.rm = T), 0)
        return(tibble(size = x$size_mid_points, rec_dist = dist))
      }
      if(sex == "female") {
        # pull parameters
        x$parameters %>%
          filter(parameter %in% c("Recruitment_ra-males:", "Recruitment_rb-males:", "Recruitment_ra-females:", "Recruitment_rb-females:")) %>% pull(estimate) -> pars
        # compute rec dist
        ra <- pars[1]*exp(pars[3]); rbeta <- pars[2]*exp(pars[4])
        size_breaks <- x$size_mid_points - ((x$size_mid_points[2]-x$size_mid_points[1])/2)
        ralpha <- ra / rbeta
        z <- pgamma(size_breaks / rbeta, ralpha)
        rec_sdd <- z - lag(z)
        rec_sdd <- rec_sdd[-1]
        rec_sdd[(n_rec_class + 1):length(rec_sdd)] <- 0
        dist <- c(rec_sdd / sum(rec_sdd, na.rm = T), 0)
        return(tibble(size = x$size_mid_points, rec_dist = dist))
      }
      
    })) %>%
    transmute(model = mod, sex, rec_dist) %>%
    unnest(rec_dist) %>% ungroup -> out
  
  return(out)
  
}

# gmacs_plot_data_range() ----

# plot data range by process, fleet, type and sex

## args:
### all_out - output from gmacs_read_allout as nested list, example: all.out = list(mod_23.0a, mod_23.1b)
### save_plot - T/F save plots, default = T
### plot_dir - file directory in which to save plots
### file - file paths to Gmacsall.out for each model to compare, passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided
### model_name - character string passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided

## example: gmacs_plot_data_range(all_out = list(output))

gmacs_plot_data_range <- function(all_out = NULL, save_plot = T, plot_dir = NULL, file = NULL, model_name = NULL) {
  # setup ----
  # read all out 
  if(is.null(all_out)) {
    tibble(file = file,
           model_name = model_name) %>%
      mutate(ao = purrr::map2(file, model_name, function(file, model_name) {gmacs_read_allout(file, model_name)})) -> ao
  }
  if(!is.null(all_out)) {
    tibble(ao = all_out,
           model_name = purrr::map_chr(ao, function(ao) {ao$model_name})) -> ao
  }
  
  # create output directories
  if(save_plot == T & is.null(plot_dir)) {plot_dir <- file.path(getwd(), "plots"); dir.create(plot_dir, showWarnings = F, recursive = TRUE)}
  if(!is.null(plot_dir) && !file.exists(plot_dir)) {dir.create(plot_dir, showWarnings = F, recursive = TRUE)}
  
  # get data pieces and plot ----
  
  # catch 
  ao %>% 
    mutate(catch = purrr::map(ao, function(data){
      data$catch_fit_summary %>%
        mutate(type = gsub("All", "Total", type)) %>%
        rowwise %>%
        mutate(group = gsub("_", " ", ifelse(data$n_sex > 1, paste(fleet, type, sex), paste(fleet, type)))) %>%
        ungroup %>%
        distinct(group, year, series, sex, fleet, type) %>%
        mutate(fleet = factor(fleet, levels = data$fleet_names),
               type = factor(type, levels = c("Retained", "Total", "Discarded")),
               sex = factor(sex, levels = c("Male", "Female", "Both"))) %>%
        arrange(type, fleet, sex)
      
    }),
    index = purrr::map(ao, function(data){
      data$index_fit_summary %>%
        rowwise %>%
        mutate(group = gsub("_", " ", ifelse(data$n_sex > 1, paste(fleet, series, sex), paste(fleet, series)))) %>%
        ungroup %>%
        distinct(group, year, series, sex, fleet) %>%
        mutate(fleet = factor(fleet, levels = data$fleet_names),
               sex = factor(sex, levels = c("Male", "Female", "Both"))) %>%
        arrange(fleet, sex) 
    }),
    size_composition = purrr::map(ao, function(data){
      data$size_fit_summary %>%
        mutate(type = gsub("All", "Total", type)) %>%
        rename(series = mod_series) %>%
        rowwise %>%
        mutate(group = gsub("_", " ", ifelse(data$n_sex > 1, paste(fleet, type, sex), paste(fleet, type)))) %>%
        ungroup %>%
        distinct(group, year, series, sex, fleet, type) %>%
        mutate(fleet = factor(fleet, levels = data$fleet_names),
               type = factor(type, levels = c("Retained", "Total", "Discarded")),
               sex = factor(sex, levels = c("Male", "Female", "Both"))) %>%
        arrange(type, fleet, sex) 
    })) %>%
    transmute(model_name, catch, index, size_composition) %>%
    pivot_longer(2:4, names_to = "process", values_to = "data") %>%
    unnest(data) %>%
    mutate(process = factor(str_to_title(gsub("_", " ", process)), level = c("Catch", "Index", "Size Composition"))) %>%
    arrange(model_name, process, type, fleet, sex) %>%
    mutate(group = factor(group, levels = unique(group))) %>%
    nest_by(model_name) %>% ungroup %>% #pull(data) %>% .[[1]] -> data
    mutate(plot = purrr::map2(model_name, data, function(model_name, data) {
      data %>%
        filter(year > 1980) %>%
        ggplot()+
        geom_point(aes(y = group, x = year, color = fleet), shape = 15, size = 3.75, show.legend = F)+
        facet_wrap(~process, ncol = 1, scales = "free_y")+
        scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
        scale_y_discrete(limits = rev)+
        scale_color_manual(values = cbpalette)+
        labs(y = NULL, x = NULL)+
        theme(panel.border = element_blank(),
              axis.line = element_line()) -> p_dat
      
      if(save_plot == T) {
        ggsave(plot = p_dat, 
               filename = file.path(plot_dir, paste0(model_name, "_data_range.png")), 
               width = 7, 
               height = 5, units = "in")
      }
      return(p_dat)
    })) -> out
  
  # output ----
  
  if(save_plot == T){ return("done")} else{return(transmute(out, model_name, plot))}
}

# gmacs_plot_catch() ----

## plot fits to gmacs catch data

## args:
### all_out - output from gmacs_read_allout as nested list, example: all.out = list(mod_23.0a, mod_23.1b)
### save_plot - T/F save plots, default = T
### plot_dir - file directory in which to save plots
### y_labs - optional, custom y axis labels, as character vector
### data_summary - alternate way to bring in data, output of gmacs_get_catch_summary()
### file - file paths to Gmacsall.out for each model to compare, passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided
### model_name - character string passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided

## example: gmacs_plot_catch(all_out = list(mod_23.1b), plot_dir = "./put/file/here")

gmacs_plot_catch <- function(all_out = NULL, save_plot = T, plot_dir = NULL, y_labs = NULL, 
                             data_summary = NULL, file = NULL, model_name = NULL, version = NULL) {
  
  # get summary data
  if(is.null(data_summary)){data_summary <- gmacs_get_catch_summary(all_out, file, model_name, version = version)}
  
  # plots 
  
  # create output directories
  if(save_plot == T & is.null(plot_dir)) {plot_dir <- file.path(getwd(), "plots"); dir.create(plot_dir, showWarnings = F, recursive = TRUE)}
  if(!is.null(plot_dir) && !file.exists(plot_dir)) {dir.create(plot_dir, showWarnings = F, recursive = TRUE)}
  
  data_summary %>%
    nest_by(series, units, .keep = T) %>% ungroup %>%
    mutate(y_lab = ifelse(is.null(y_labs), NA, y_labs)) %>%
    #dplyr::slice(1) %>% # pull(data) %>% .[[1]]-> data
    mutate(plot = purrr::map2(data, y_lab, function(data, y_lab) {
      
      # y label
      if(is.null(y_labs)) {
      y_lab <- paste0(gsub("_", " ", unique(data$fleet)), " ", gsub("All", "Total", unique(data$type)), " Catch (", unique(data$wt_units), ")")
      if(unique(data$units) == "Numbers") {
        y_lab <- paste0(gsub("_", " ", unique(data$fleet)), " ", gsub("All", "Total", unique(data$type)), " Catch (", unique(data$n_units), ")")
      }
      }
      
      # plot
      data %>%
        right_join(expand_grid(distinct(., model, series, units),
                              year = min(data$year):max(data$year)),
                  by = join_by(model, series, year, units)) %>%
        mutate(obs_l95 = obs_catch * exp(-1.96 * sqrt(log(1 + cv^2))),
               obs_u95 = obs_catch * exp(1.96 * sqrt(log(1 + cv^2)))) %>%
        ggplot()+
        geom_point(aes(x = year, y = obs_catch), color = "grey40")+
        geom_errorbar(aes(x = year, ymin = obs_l95, ymax = obs_u95), width = 0, color = "grey40")+
        geom_line(aes(x = year, y = pred_catch, group = model, color = model))+
        labs(x = NULL, color = NULL, y = y_lab)+
        scale_y_continuous(labels = scales::comma)+
        scale_color_manual(values = cbpalette)+
        coord_cartesian(ylim = c(0, NA)) -> p
      
      if(length(min(data$year):max(data$year)) > 10) { p + scale_x_continuous(labels = yraxis$labels, breaks = yraxis$breaks) -> p }
      
      if(save_plot == T) {
        
        pwidth <- min(max(length(min(data$year):max(data$year))*0.2, 5), 7)
        # save plot
        ggsave(plot = p, 
               filename = file.path(plot_dir, paste0("catch_fit_", tolower(unique(data$fleet)), "_", tolower(unique(data$units)), ".png")), 
               width = pwidth, 
               height = pwidth * (3/5), units = "in")
      }
      
      return(p)
      
    })) -> plots
  
  # return ----
  if(save_plot == T) {
    # save plot of all stacked
    ggsave(plot = patchwork::wrap_plots(plots$plot, ncol = 2), 
           filename = file.path(plot_dir, "catch_fit.png"),
           height = nrow(plots) / 2 * 4, width = 11, units = "in") 
    
    return("done")
    
  } else {return(plots$plot)}
  
}

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
    mutate(y_lab = ifelse(is.null(y_labs), NA, y_labs)) %>%  
    mutate(plot = purrr::map2(data, y_lab, function(data, y_lab) {
      # y label
      if(is.null(y_labs)) {
        y_lab <- paste0(gsub("_", " ", unique(data$fleet)), " ", unique(data$sex), " Index (", unique(data$wt_units), ")")
        if(unique(data$units) == "Numbers") {
          if(is.na(unique(data$n_units)) == FALSE) {
            y_lab <- paste0(gsub("_", " ", unique(data$fleet)), " ", unique(data$sex), " Index (", unique(data$n_units), ")")}
          if(is.na(unique(data$n_units)) == TRUE) {
            y_lab <- paste0(gsub("_", " ", unique(data$fleet)), " ", unique(data$sex), " Index") 
          }
        }
      }
      # plot
      data %>%
        right_join(expand_grid(distinct(., model, series, sex, units),
                               year = min(data$year):max(data$year)),
                   by = join_by(model, series, sex, year, units)) %>%
        mutate(obs_l95 = obs_index * exp(-1.96 * sqrt(log(1 + obs_cv^2))),
               obs_u95 = obs_index * exp(1.96 * sqrt(log(1 + obs_cv^2))),
               tot_l95 = obs_index * exp(-1.96 * sqrt(log(1 + tot_cv^2))),
               tot_u95 = obs_index * exp(1.96 * sqrt(log(1 + tot_cv^2)))) %>%
        ggplot()+
        geom_errorbar(aes(x = factor(year), ymin = tot_l95, ymax = tot_u95), width = 0, color = "grey70")+
        geom_errorbar(aes(x = factor(year), ymin = obs_l95, ymax = obs_u95), width = 0, color = "grey20")+
        geom_point(aes(x = factor(year), y = obs_index), color = "grey20")+
        geom_line(aes(x = factor(year), y = pred_index, group = model, color = model))+
        labs(x = NULL, color = NULL, y = y_lab)+
        scale_y_continuous(labels = scales::comma)+
        scale_color_manual(values = cbpalette)+
        coord_cartesian(ylim = c(0, NA)) -> p
      if(length(min(data$year):max(data$year)) > 10) { p + scale_x_discrete(labels = yraxis$labels, breaks = yraxis$breaks) -> p }
      if(save_plot == T) {
        pwidth <- min(max(length(min(data$year):max(data$year))*0.3, 6), 7)
        # save plot
        ggsave(plot = p, 
               filename = file.path(plot_dir, paste0("index_fit_", tolower(unique(data$fleet)), "_",
                                                     tolower(unique(data$sex)), ".png")), 
               width = pwidth, 
               height = pwidth * (4/6), units = "in")
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



# gmacs_plot_sizecomp() ----

## plot fits to gmacs size comp data

## args:
### all_out - output from gmacs_read_allout as nested list, example: all.out = list(mod_23.0a, mod_23.1b)
### save_plot - T/F save plots, default = T
### plot_dir - file directory in which to save plots
### size_lab - optional, custom size axis label, as character vector, example: "Carapace Length (mm)", default = "Size"
### plot_nsamp_est - show stage 2 effective sample sizes on plots? T/F
### nsamp_est_model - name of model to display nsamp_est for. Only required if plotting more than a single model. Defaults to first in list.
### aggregate_series_labels - character vector of labels for aggregate series, ex: c("Male", "Female") or c("New Shell", "Old Shell");
###                           or list with elements being character vectors for each aggregate series (if you want to use different labels)
### data_summary - alternate way to bring in data, output of gmacs_get_index_summary()
### file - file paths to Gmacsall.out for each model to compare, passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided
### model_name - character string passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided


gmacs_plot_sizecomp <- function(all_out = NULL, save_plot = T, plot_dir = NULL, size_lab = "Size",
                                plot_nsamp_est = F, nsamp_est_model = NULL, aggregate_series_label = NULL, data_summary = NULL, file = NULL, 
                                model_name = NULL) {
  
  # get size summary data
  if(is.null(data_summary)){data_summary <- gmacs_get_size_summary(all_out, file, model_name)}
  
  # check dir
  if(save_plot == T & is.null(plot_dir)) {dir.create("./plots", showWarnings = F, recursive = TRUE); plot_dir <- "./plots"}
  if(!is.null(plot_dir) && !file.exists(plot_dir)) {dir.create(plot_dir, showWarnings = F, recursive = TRUE)}
  
  # make plots with all models
  data_summary %>% 
    nest_by(mod_series, .keep = T) %>% ungroup %>%# pull(data) %>% .[[5]] -> data
    mutate(agg = purrr::map_lgl(data, function(data) {
      # check for aggregated comp
      data <- dplyr::select(data, where(function(x) !all(is.na(x))))
      agg <- "aggregate_series" %in% names(data)
      return(agg)
    }),
    agg_series_order = rank(mod_series * ifelse(agg==F, NA, 1), na.last = "keep"),
    aggregate_series_label = purrr::map2(agg, agg_series_order, function(agg, agg_series_order){
      if(agg == T & class(aggregate_series_label) == "list"){return(aggregate_series_label[[agg_series_order]])}
      if(agg == T & class(aggregate_series_label) != "list"){return(aggregate_series_label)}
      if(agg == F) {return(NA)}
    }),
    plots = purrr::pmap(list(data, agg, aggregate_series_label), function(data, agg, aggregate_series_label){
      
      ### check nsamp_est_model
      if(is.null(nsamp_est_model)){nsamp_est_model <- unique(data$model)[1]}
      
      ## comp, agg comp, and residual plots
      if(agg == T) {
        ## setup for plotting aggregate series ----
        # get some detail about size bins
        size_bins <- data %>% pull(size) %>% unique 
        n_bins <- length(size_bins)
        n_yr <- length(unique(data$year))
        bin_width <- size_bins[2] - size_bins[1]
        # adjust size bin for the secondary series
        data <- mutate(data, plot_size = (aggregate_series-1)*(max(size_bins)-min(size_bins)+bin_width*2) + size) 
        # get size breaks and labels for the plot
        brks <- labeling::extended(1, n_bins, m = 3); brks <- brks[brks != 0]
        data %>%
          distinct(aggregate_series, plot_size) %>% 
          nest_by(aggregate_series) %>% ungroup %>%
          mutate(breaks = purrr::map(data, function(data){data %>% dplyr::slice(brks)})) %>%
          pull(breaks) %>% unlist %>% as.numeric -> breaks
        data %>%
          distinct(size, plot_size) %>%
          filter(plot_size %in% breaks) %>% pull(size) -> labels
        data %>%
          filter(aggregate_series > 1) %>%
          group_by(aggregate_series) %>%
          summarise(divider = min(plot_size) - bin_width) %>% pull(divider) -> divider
        if(is.null(aggregate_series_label)) {aggregate_series_label <- unique(data$aggregate_series)}
        
        ## comp by year ----
        ### plot dimensions
        cols <- ifelse(n_yr >= 12, 3, ifelse(n_yr >= 6, 2, 1))
        rows <- ifelse(cols == 1, n_yr, ifelse(cols == 2, ceiling(n_yr/2), ceiling(n_yr/3))) 
        ### plot
        data %>%  
          rowwise %>%
          mutate(nsamp_annotate = ifelse(plot_nsamp_est == T,
                                         paste0("N = ", round(nsamp_obs), "\nN est = ", round(nsamp_est, 1)),
                                         paste0("N = ", round(nsamp_obs))),
                 nsamp_annotate = ifelse(model == nsamp_est_model, nsamp_annotate, NA)) %>% ungroup %>%
          mutate(aggregate_series_label = factor(aggregate_series_label[aggregate_series], levels = aggregate_series_label)) %>%
          ggplot()+
          geom_bar(aes(x = plot_size, y = obs, fill = aggregate_series_label), stat = "identity", position = "identity", color = NA, width = bin_width)+
          geom_line(aes(x = plot_size, y = pred, group = interaction(aggregate_series, model), color = model))+
          geom_vline(xintercept = divider, linetype = 2, color = "grey70")+
          scale_x_continuous(breaks = breaks, labels = labels)+
          scale_y_continuous(expand = expand_scale(mult = c(0, 0.1), add = c(0, 0)))+
          labs(x = size_lab, y = NULL, color = NULL, fill = NULL)+
          geom_text_npc(aes(npcx = "left", npcy = 0.6, label = year), check_overlap = T, size = 3)+
          geom_text_npc(aes(npcx = "right", npcy = 0.9, label = nsamp_annotate),
                        check_overlap = T, size = 3)+
          facet_wrap(~year, nrow = rows, ncol = cols, dir = "v")+
          scale_color_manual(values = cbpalette)+
          scale_fill_grey()+
          theme(panel.spacing.x = unit(0.2, "lines"),
                panel.spacing.y = unit(0, "lines"),
                panel.border = element_blank(),
                axis.line.x = element_line(color = "grey70", size = 0.2),
                axis.ticks.y = element_blank(),
                axis.text.y = element_blank(),
                axis.text.x = element_text(size = 8),
                plot.title = element_text(hjust = 0.5),
                strip.background = element_blank(),
                strip.text.x = element_blank(),
                panel.background = element_blank()) -> p_comp_year
        ### save
        if(save_plot == T){
          height = min(rows, 10)
          width = min(cols*3, 9)
          ggsave(file.path(plot_dir, paste0("comp_fit_",tolower(unique(data$fleet)),"_", tolower(unique(data$sex)),"_", tolower(unique(data$type)),".png")),
                 plot = p_comp_year, height = height, width = width, units = "in") 
        }
        
        
        ## aggregate size comp ----
        ### plot
        data %>%  
          group_by(model, aggregate_series, size, plot_size) %>%
          summarise(obs = sum(obs), pred = sum(pred),
                    nsamp_obs = sum(nsamp_obs), nsamp_est = sum(nsamp_est)) %>% ungroup %>%
          
          mutate(nsamp_annotate = ifelse(plot_nsamp_est == T,
                                         paste0("N = ", prettyNum(round(nsamp_obs), big.mark = ","), "\nN est = ", prettyNum(round(nsamp_est, 1), big.mark = ",")),
                                         paste0("N = ", prettyNum(round(nsamp_obs), big.mark = ","))),
                 nsamp_annotate = ifelse(model == nsamp_est_model, nsamp_annotate, NA)) %>%
          mutate(aggregate_series_label = factor(aggregate_series_label[aggregate_series], levels = aggregate_series_label)) %>%
          ggplot()+
          geom_bar(aes(x = plot_size, y = obs, fill = aggregate_series_label), stat = "identity", position = "identity", color = NA, width = bin_width)+
          geom_line(aes(x = plot_size, y = pred, group = interaction(aggregate_series, model), color = model))+
          geom_vline(xintercept = divider, linetype = 2, color = "grey70")+
          scale_x_continuous(breaks = breaks, labels = labels)+
          scale_y_continuous(expand = expand_scale(mult = c(0, 0.1), add = c(0, 0)))+
          labs(x = size_lab, y = NULL, color = NULL, fill = NULL)+
          geom_text_npc(aes(npcx = "left", npcy = 0.9, label = nsamp_annotate),
                        check_overlap = T, size = 3)+
          scale_color_manual(values = cbpalette)+
          scale_fill_grey()+
          theme(panel.spacing.x = unit(0.2, "lines"),
                panel.spacing.y = unit(0, "lines"),
                panel.border = element_blank(),
                axis.line.x = element_line(color = "grey70", size = 0.2),
                axis.ticks.y = element_blank(),
                axis.text.y = element_blank(),
                axis.text.x = element_text(size = 8),
                plot.title = element_text(hjust = 0.5),
                strip.background = element_blank(),
                strip.text.x = element_blank(),
                panel.background = element_blank()) -> p_comp_agg
        ### save
        if(save_plot == T){
          ggsave(file.path(plot_dir, paste0("aggregated_comp_fit_",tolower(unique(data$fleet)),"_", tolower(unique(data$sex)),"_", tolower(unique(data$type)),".png")),
                 plot = p_comp_agg, height = 3, width = 5, units = "in") 
        }
        
        ## line residual plot ----
        
        ### plot dimensions
        cols <- ifelse(n_yr >= 12, 3, ifelse(n_yr >= 6, 2, 1))
        rows <- ifelse(cols == 1, n_yr, ifelse(cols == 2, ceiling(n_yr/2), ceiling(n_yr/3)))  
        ### plot
        data %>%  
          ggplot()+
          geom_line(aes(x = plot_size, y = residual, group = interaction(aggregate_series, model), color = model))+
          geom_hline(yintercept = 0, linetype = 2, color = "grey70")+
          geom_vline(xintercept = divider, linetype = 2, color = "grey70")+
          scale_x_continuous(breaks = breaks, labels = labels)+
          scale_y_continuous(expand = expand_scale(mult = c(0, 0.1), add = c(0, 0)))+
          labs(x = size_lab, y = NULL, color = NULL, fill = NULL)+
          geom_text_npc(aes(npcx = "left", npcy = 0.8, label = year), check_overlap = T, size = 3)+
          facet_wrap(~year, nrow = rows, ncol = cols, dir = "v")+
          scale_color_manual(values = cbpalette)+
          scale_fill_grey()+
          theme(panel.spacing.x = unit(0.2, "lines"),
                panel.spacing.y = unit(0, "lines"),
                panel.border = element_blank(),
                axis.line.x = element_line(color = "grey70", size = 0.2),
                axis.ticks.y = element_blank(),
                axis.text.y = element_blank(),
                axis.text.x = element_text(size = 8),
                plot.title = element_text(hjust = 0.5),
                strip.background = element_blank(),
                strip.text.x = element_blank(),
                panel.background = element_blank()) -> p_resid_line
        ### save
        if(save_plot == T){
          height = min(rows, 10)
          width = min(cols*3, 9)
          ggsave(file.path(plot_dir, paste0("resid_line_",tolower(unique(data$fleet)),"_", tolower(unique(data$sex)),"_", tolower(unique(data$type)),".png")),
                 plot = p_resid_line, height = height, width = width, units = "in") 
        }
        
      }
      if(agg == F) {
        ## setup for plotting aggregate series ----
        # get some detail about size bins
        size_bins <- data %>% pull(size) %>% unique 
        n_bins <- length(size_bins)
        n_yr <- length(unique(data$year))
        bin_width <- size_bins[2] - size_bins[1]
        
        ## comp by year ----
        ### plot dimensions
        cols <- ifelse(n_yr >= 12, 3, ifelse(n_yr >= 6, 2, 1))
        rows <- ifelse(cols == 1, n_yr, ifelse(cols == 2, ceiling(n_yr/2), ceiling(n_yr/3)))  
        ### plot
        data %>%  
          rowwise() %>%
          mutate(nsamp_annotate = ifelse(plot_nsamp_est == T,
                                         paste0("N = ", round(nsamp_obs), "\nN est = ", round(nsamp_est, 1)),
                                         paste0("N = ", round(nsamp_obs))),
                 nsamp_annotate = ifelse(model == nsamp_est_model, nsamp_annotate, NA)) %>% ungroup %>%
          ggplot()+
          geom_bar(aes(x = size, y = obs), stat = "identity", position = "identity", color = NA, fill = "grey70", width = bin_width, alpha = 0.5)+
          geom_line(aes(x = size, y = pred, color = model))+
          scale_y_continuous(expand = expand_scale(mult = c(0, 0.1), add = c(0, 0)))+
          labs(x = size_lab, y = NULL, color = NULL, fill = NULL)+
          geom_text_npc(aes(npcx = "left", npcy = 0.6, label = year), check_overlap = T, size = 3)+
          geom_text_npc(aes(npcx = "right", npcy = 0.9, label = nsamp_annotate),
                        check_overlap = T, size = 3)+
          facet_wrap(~year, nrow = rows, ncol = cols, dir = "v")+
          scale_color_manual(values = cbpalette)+
          theme(panel.spacing.x = unit(0.2, "lines"),
                panel.spacing.y = unit(0, "lines"),
                panel.border = element_blank(),
                axis.line.x = element_line(color = "grey70", size = 0.2),
                axis.ticks.y = element_blank(),
                axis.text.y = element_blank(),
                axis.text.x = element_text(size = 8),
                plot.title = element_text(hjust = 0.5),
                strip.background = element_blank(),
                strip.text.x = element_blank(),
                panel.background = element_blank()) -> p_comp_year
        ### save
        if(save_plot == T){
          height = min(rows, 10)
          width = min(cols*3, 9)
          ggsave(file.path(plot_dir, paste0("comp_fit_",tolower(unique(data$fleet)),"_", tolower(unique(data$sex)),"_", tolower(unique(data$type)),".png")),
                 plot = p_comp_year, height = height, width = width, units = "in") 
        }
        
        
        ## aggregate size comp ----
        ### plot
        data %>%  
          group_by(model, size) %>%
          summarise(obs = sum(obs), pred = sum(pred),
                    nsamp_obs = sum(nsamp_obs), nsamp_est = sum(nsamp_est)) %>% ungroup %>%
          mutate(nsamp_annotate = ifelse(plot_nsamp_est == T,
                                         paste0("N = ", prettyNum(round(nsamp_obs), big.mark = ","), "\nN est = ", prettyNum(round(nsamp_est, 1), big.mark = ",")),
                                         paste0("N = ", prettyNum(round(nsamp_obs), big.mark = ","))),
                 nsamp_annotate = ifelse(model == nsamp_est_model, nsamp_annotate, NA)) %>%
          ggplot()+
          geom_bar(aes(x = size, y = obs), stat = "identity", position = "identity", color = NA, fill = "grey70", width = bin_width, alpha = 0.5)+
          geom_line(aes(x = size, y = pred, color = model))+
          scale_y_continuous(expand = expand_scale(mult = c(0, 0.1), add = c(0, 0)))+
          labs(x = size_lab, y = NULL, color = NULL, fill = NULL)+
          geom_text_npc(aes(npcx = "left", npcy = 0.9, label = nsamp_annotate),
                        check_overlap = T, size = 3)+
          scale_color_manual(values = cbpalette)+
          theme(panel.spacing.x = unit(0.2, "lines"),
                panel.spacing.y = unit(0, "lines"),
                panel.border = element_blank(),
                axis.line.x = element_line(color = "grey70", size = 0.2),
                axis.ticks.y = element_blank(),
                axis.text.y = element_blank(),
                axis.text.x = element_text(size = 8),
                plot.title = element_text(hjust = 0.5),
                strip.background = element_blank(),
                strip.text.x = element_blank(),
                panel.background = element_blank()) -> p_comp_agg
        ### save
        if(save_plot == T){
          ggsave(file.path(plot_dir, paste0("aggregated_comp_fit_",tolower(unique(data$fleet)),"_", tolower(unique(data$sex)),"_", tolower(unique(data$type)),".png")),
                 plot = p_comp_agg, height = 3, width = 5, units = "in") 
        }
        
        ## line residual plot ----
        
        ### plot dimensions
        cols <- ifelse(n_yr >= 12, 3, ifelse(n_yr >= 6, 2, 1))
        rows <- ifelse(cols == 1, n_yr, ifelse(cols == 2, ceiling(n_yr/2), ceiling(n_yr/3)))  
        ### plot
        data %>%  
          ggplot()+
          geom_line(aes(x = size, y = residual, color = model))+
          geom_hline(yintercept = 0, linetype = 2, color = "grey70")+
          scale_y_continuous(expand = expand_scale(mult = c(0, 0.1), add = c(0, 0)))+
          labs(x = size_lab, y = NULL, color = NULL, fill = NULL)+
          geom_text_npc(aes(npcx = "left", npcy = 0.8, label = year), check_overlap = T, size = 3)+
          facet_wrap(~year, nrow = rows, ncol = cols, dir = "v")+
          scale_color_manual(values = cbpalette)+
          theme(panel.spacing.x = unit(0.2, "lines"),
                panel.spacing.y = unit(0, "lines"),
                panel.border = element_blank(),
                axis.line.x = element_line(color = "grey70", size = 0.2),
                axis.ticks.y = element_blank(),
                axis.text.y = element_blank(),
                axis.text.x = element_text(size = 8),
                plot.title = element_text(hjust = 0.5),
                strip.background = element_blank(),
                strip.text.x = element_blank(),
                panel.background = element_blank()) -> p_resid_line
        ### save
        if(save_plot == T){
          height = min(rows, 10)
          width = min(cols*3, 9)
          ggsave(file.path(plot_dir, paste0("resid_line_",tolower(unique(data$fleet)),"_", tolower(unique(data$sex)),"_", tolower(unique(data$type)),".png")),
                 plot = p_resid_line, height = height, width = width, units = "in") 
        }
        
      }
    })) -> out
  
  # make plots by model
  data_summary %>% 
    nest_by(model, mod_series, .keep = T) %>% ungroup %>% #pull(data) %>% .[[1]] -> data
    mutate(agg = purrr::map_lgl(data, function(data) {
      # check for aggregated comp
      data <- dplyr::select(data, where(function(x) !all(is.na(x))))
      agg <- "aggregate_series" %in% names(data)
      return(agg)
    }),
    agg_series_order = rank(mod_series * ifelse(agg==F, NA, 1), na.last = "keep"),
    aggregate_series_label = purrr::map2(agg, agg_series_order, function(agg, agg_series_order){
      if(agg == T & class(aggregate_series_label) == "list"){return(aggregate_series_label[[agg_series_order]])}
      if(agg == T & class(aggregate_series_label) != "list"){return(aggregate_series_label)}
      if(agg == F) {return(NA)}
    }),
    plots = purrr::pmap(list(data, agg, aggregate_series_label, model), function(data, agg, aggregate_series_label, model){
      
      ### check nsamp_est_model
      if(length(unique(data$model)) > 1 & is.null(nsamp_est_model)){nsamp_est_model <- unique(data$model)[1]}
      if(length(unique(data$model)) > 1 & is.null(nsamp_est_model)){nsamp_est_model <- unique(data$model)[1]}
      
      if(agg == T) {
        ## setup for plotting aggregate series ----
        # get some detail about size bins
        size_bins <- data %>% pull(size) %>% unique 
        n_bins <- length(size_bins)
        n_yr <- length(unique(data$year))
        bin_width <- size_bins[2] - size_bins[1]
        # adjust size bin for the secondary series
        data <- mutate(data, plot_size = (aggregate_series-1)*(max(size_bins)-min(size_bins)+bin_width*2) + size) 
        # get size breaks and labels for the plot
        brks <- labeling::extended(1, n_bins, m = 3); brks <- brks[brks != 0]
        data %>%
          distinct(aggregate_series, plot_size) %>% 
          nest_by(aggregate_series) %>% ungroup %>%
          mutate(breaks = purrr::map(data, function(data){data %>% dplyr::slice(brks)})) %>%
          pull(breaks) %>% unlist %>% as.numeric -> breaks
        data %>%
          distinct(size, plot_size) %>%
          filter(plot_size %in% breaks) %>% pull(size) -> labels
        data %>%
          filter(aggregate_series > 1) %>%
          group_by(aggregate_series) %>%
          summarise(divider = min(plot_size) - bin_width) %>% pull(divider) -> divider
        if(is.null(aggregate_series_label)) {aggregate_series_label <- unique(data$aggregate_series)}
        
        ## dot plot ----
        
        data %>%
          # compute residual
          mutate(pos = case_when(residual >= 0 ~ "> 0", 
                                 residual < 0 ~ "< 0"),
                 residual = ifelse(residual == 0, NA, residual),
                 aggregate_series_label = factor(aggregate_series_label[aggregate_series], levels = aggregate_series_label)) %>%
          ggplot()+
          geom_point(aes(x = year, y = size, size = abs(residual), fill = pos), 
                     shape = 21, alpha = 0.5)+
          scale_fill_manual(values = c("black", "white", NA))+
          scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels, limits = range(data$year))+
          labs(x = NULL, y = size_lab, size = NULL, fill = NULL)+
          theme(legend.position = "top")+
          facet_wrap(~aggregate_series_label, ncol = 1, scales = "free_y") -> p_dot
        ### save
        if(save_plot == T){
          height = unique(data$aggregate_series) * 6
          width = min(6, n_yr * 0.5)
          ggsave(file.path(plot_dir, paste0(model, "_resid_dot_",tolower(unique(data$fleet)),"_", tolower(unique(data$sex)),"_", tolower(unique(data$type)),".png")),
                 plot = p_dot, height = height, width = width, units = "in") 
        }
      }
      if(agg == F) {
        ## setup for plotting aggregate series ----
        # get some detail about size bins
        size_bins <- data %>% pull(size) %>% unique 
        n_bins <- length(size_bins)
        n_yr <- length(unique(data$year))
        bin_width <- size_bins[2] - size_bins[1]
        
        ## dot plot ----
        
        data %>%
          # compute residual
          mutate(pos = case_when(residual >= 0 ~ "> 0", 
                                 residual < 0 ~ "< 0"),
                 residual = ifelse(residual == 0, NA, residual)) %>%
          ggplot()+
          geom_point(aes(x = year, y = size, size = abs(residual), fill = pos), 
                     shape = 21, alpha = 0.5)+
          scale_fill_manual(values = c("black", "white", NA))+
          scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels, limits = range(data$year))+
          labs(x = NULL, y = size_lab, size = NULL, fill = NULL)+
          theme(legend.position = "top") -> p_dot
        ### save
        if(save_plot == T){
          height = 6
          width = min(6, n_yr * 0.5)
          ggsave(file.path(plot_dir, paste0(model, "_resid_dot_",tolower(unique(data$fleet)),"_", tolower(unique(data$sex)),"_", tolower(unique(data$type)),".png")),
                 plot = p_dot, height = height, width = width, units = "in") 
        }
      }
      ## mean size (francis) plot ---- 
      
      data %>%
        group_by(year) %>%
        summarise(obs_mean_size = weighted.mean(size, obs),
                  pred_mean_size = weighted.mean(size, pred), 
                  sd = sqrt(sum((size - pred_mean_size)^2 * pred) / sum(pred)) / sqrt(mean(nsamp_obs)),
                  l95 = pred_mean_size + sd * qnorm(0.025),
                  u95 = pred_mean_size + sd * qnorm(0.975),
                  sd_est = sqrt(sum((size - pred_mean_size)^2 * pred) / sum(pred)) / sqrt(mean(nsamp_est)),
                  l95_est = pred_mean_size + sd_est * qnorm(0.025),
                  u95_est = pred_mean_size + sd_est * qnorm(0.975)) %>% ungroup %>%
        ggplot()-> tmp
      if(plot_nsamp_est == T){tmp+geom_ribbon(aes(x = year, ymin = l95_est, ymax = u95_est, group = 1), fill = "grey80", alpha = 0.5)->tmp}
      tmp+geom_ribbon(aes(x = year, ymin = l95, ymax = u95, group = 1), fill = "grey60", alpha = 0.5)+
        geom_line(aes(x = year, y = pred_mean_size, group = 1))+
        geom_point(aes(x = year, y = obs_mean_size))+
        scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
        labs(x = NULL, y = paste0("Mean ", size_lab)) -> p_mean_size
      ### save
      if(save_plot == T){
        height = 4
        width = 6
        ggsave(file.path(plot_dir, paste0(model, "_mean_size_",tolower(unique(data$fleet)),"_", tolower(unique(data$sex)),"_", tolower(unique(data$type)),".png")),
               plot = p_mean_size, height = height, width = width, units = "in") 
      }
      
      return(list(p_dot, p_mean_size))
      
    })) -> out2
  
  # output
  if(save_plot == F) {return(c(out %>% pull(plots), out2 %>% pull(plots)))} else{"done"}
  
}


# gmacs_plot_mmb() ----

## plot mature male biomass and mature male abundance trajectories

## args:
### all_out - output from gmacs_read_allout as nested list, example: all.out = list(mod_23.0a, mod_23.1b)
### save_plot - T/F save plots, default = T
### plot_ci - T/F add confidence interval ribbon to ssb
### ci_alpha - alpha value for confidence interval, a = 0.05 is 95% CI
### yrs - subset a specific year range, example: c(1990:2022)
### plot_proj - T/F add point to plot for projection year MMB
### plot_dir - file directory in which to save plots
### data_summary - alternate way to bring in data, output of gmacs_get_derived_quantity_summary()
### file - file paths to Gmacsall.out for each model to compare, passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided
### model_name - character string passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided
### std_file - file path to gmacs.std file. Optional, if plot_ci = T, both std_file and std_list cannot be NULL.
### std_list -  output from gmacs_read_std() as nested list, e.g., std = list(std.24.0, std.16.0). Optional, if plot_ci = T, both std_file and std_list cannot be NULL.

gmacs_plot_mmb <- function(all_out = NULL, save_plot = T, plot_ci = F, ci_alpha = 0.05, yrs = NULL, plot_proj = T, plot_dir = NULL, data_summary = NULL, 
                           file = NULL, model_name = NULL, version = NULL, std_file = NULL, std_list = NULL) {
  
  # get summary data
  if(is.null(data_summary)){data_summary <- gmacs_get_derived_quantity_summary(all_out, file, model_name, version)}
  
  # plots ----
  if(save_plot == T & is.null(plot_dir)) {plot_dir <- file.path(getwd(), "plots"); dir.create(plot_dir, showWarnings = F, recursive = TRUE)}
  if(!is.null(plot_dir) && !file.exists(plot_dir)) {dir.create(plot_dir, showWarnings = F, recursive = TRUE)}
  # add ci if plot_ci is on
  if(plot_ci == T){
    if(is.null(model_name)){model_name <- unique(data_summary$model)}
    if(is.null(std_list)){std_list <- purrr::map2(std_file, model_name, gmacs_read_std)}
    bind_rows(std_list) %>%
      filter(grepl("sd_log_ssb", par)) %>%
      transmute(ssb_se = se / (1 / exp(est)), 
                ssb_lci = exp(est) + ssb_se * qnorm(ci_alpha / 2), 
                ssb_uci = exp(est) + ssb_se * qnorm(1 - ci_alpha / 2)) %>%
      bind_cols(data_summary, .) -> data_summary
  }
  if(plot_ci == F){
    data_summary %>%
      mutate(ssb_se = NA, ssb_lci = NA, ssb_uci = NA) -> data_summary
  }
  # filter for years if specified
  if(!is.null(yrs)){data_summary %>% filter(year %in% yrs) -> data_summary}
  # add line for projection year
  if(plot_proj == T){
    proj <- gmacs_get_ref_points(all_out) %>% 
      mutate(year = max(data_summary$year) + 1)
  }
  
  # plot ssb
  data_summary %>%
    ggplot()+
    {if(plot_ci == T){geom_ribbon(aes(x = factor(year), ymin = ssb_lci, ymax = ssb_uci, group = model, fill = model), alpha = 0.2, show.legend = F)}}+
    geom_line(aes(x = factor(year), y = ssb, group = model, color = model))+
    {if(plot_proj == T){geom_point(data = proj,
                                   aes(x = factor(year), y = mmb, color = model))}}+
    scale_x_discrete(breaks = yraxis$breaks, labels = yraxis$labels)+
    scale_y_continuous(labels = scales::comma, limits = c(0, NA))+
    scale_color_manual(values = cbpalette)+
    scale_fill_manual(values = cbpalette)+
    labs(x = NULL, y = paste0("MMB (", unique(data_summary$wt_units), ")"), color = NULL)+
    theme(legend.position = c(1, 1),
          legend.justification = c(1, 1)) -> mmb
  # plot ssa
  data_summary %>%
    ggplot()+
    geom_line(aes(x = factor(year), y = ssa, group = model, color = model))+
    scale_x_discrete(breaks = yraxis$breaks, labels = yraxis$labels)+
    scale_y_continuous(labels = scales::comma, limits = c(0, NA))+
    scale_color_manual(values = cbpalette)+
    labs(x = NULL, 
         y = ifelse(unique(data_summary$n_units) %in% c(1, "1", "1s", "one", "One", "ones", "Ones"),
                    "MMA", paste0("MMA (", unique(data_summary$n_units), ")")),
         color = NULL)+
    theme(legend.position = c(1, 1),
          legend.justification = c(1, 1)) -> mma
  if(save_plot == T){
    ggsave(file.path(plot_dir, "mmb_trajectory.png"), plot = mmb, height = 4.2, width = 7, units = "in")
    ggsave(file.path(plot_dir, "mma_trajectory.png"), plot = mma, height = 4.2, width = 7, units = "in")
    return("done")
  }
  if(save_plot == F){return(list(mmb, mma))}
  
}

# gmacs_plot_recruitment() ----

## plot recruitment trajectories

## args:
### all_out - output from gmacs_read_allout as nested list, example: all.out = list(mod_23.0a, mod_23.1b)
### save_plot - T/F save plots, default = T
### plot_ci - T/F add confidence interval ribbon to ssb
### ci_alpha - alpha value for confidence interval, a = 0.05 is 95% CI
### yrs - subset a specific year range, example: c(1990:2022)
### plot_dir - file directory in which to save plots
### data_summary - alternate way to bring in data, output of gmacs_get_derived_quantity_summary()
### file - file paths to Gmacsall.out for each model to compare, passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided
### model_name - character string passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided
### std_file - file path to gmacs.std file. Optional, if plot_ci = T, both std_file and std_list cannot be NULL.
### std_list -  output from gmacs_read_std() as nested list, e.g., std = list(std.24.0, std.16.0). Optional, if plot_ci = T, both std_file and std_list cannot be NULL.
gmacs_plot_recruitment <- function(all_out = NULL, save_plot = T, plot_ci = F, ci_alpha = 0.05, yrs = NULL, plot_dir = NULL, data_summary = NULL, file = NULL, model_name = NULL, std_file = NULL, std_list = NULL, version = NULL) {
  
  # get summary data
  if(is.null(data_summary)){data_summary <- gmacs_get_derived_quantity_summary(all_out, file, model_name, version)}
  
  # plots ----
  if(save_plot == T & is.null(plot_dir)) {plot_dir <- file.path(getwd(), "plots"); dir.create(plot_dir, showWarnings = F, recursive = TRUE)}
  if(!is.null(plot_dir) && !file.exists(plot_dir)) {dir.create(plot_dir, showWarnings = F, recursive = TRUE)}
  
  # add ci if plot_ci is on
  if(plot_ci == T){
    if(is.null(model_name)){model_name <- unique(data_summary$model)}
    if(is.null(std_list)){std_list <- purrr::map2(std_file, model_name, gmacs_read_std)}
    bind_rows(std_list) %>%
      filter(grepl("sd_log_recruits", par)) %>%
      transmute(rec_se = se / (1 / exp(est)), 
                rec_lci = exp(est) + rec_se * qnorm(ci_alpha / 2), 
                rec_uci = exp(est) + rec_se * qnorm(1 - ci_alpha / 2)) %>%
      bind_cols(data_summary, .) -> data_summary
  }
  if(plot_ci == F){
    data_summary %>%
      mutate(rec_se = NA, rec_lci = NA, rec_uci = NA) -> data_summary
  }
  # filter for years if specified
  if(!is.null(yrs)){data_summary %>% filter(year %in% yrs) -> data_summary}
  
  # male and female
  if("recruit_female" %in% names(data_summary)) {
    
    if(unique(data_summary$n_units) %in% c(1, "1", "1s", "one", "One", "ones", "Ones")){y_labs <- c("Total Recruitment", "Female Recruitment", "Male Recruitment")} else{
      y_labs <- paste0(c("Total Recruitment", "Female Recruitment", "Male Recruitment"), " (", unique(data_summary$n_units), ")")
    }
    data_summary %>%
      mutate(tot_recruit = recruit_male + recruit_female) %>%
      ggplot()+
      geom_line(aes(x = factor(year), y = tot_recruit, group = model, color = model))+
      scale_x_discrete(breaks = yraxis$breaks, labels = yraxis$labels)+
      scale_y_continuous(labels = scales::comma, limits = c(0, NA))+
      scale_color_manual(values = cbpalette)+
      labs(x = NULL, y = y_labs[1], color = NULL)+
      theme(legend.position = c(1, 1),
            legend.justification = c(1, 1)) -> tot
    data_summary %>%
      ggplot()+
      geom_line(aes(x = factor(year), y = recruit_female, group = model, color = model))+
      scale_x_discrete(breaks = yraxis$breaks, labels = yraxis$labels)+
      scale_y_continuous(labels = scales::comma, limits = c(0, NA))+
      labs(x = NULL, y = y_labs[2], color = NULL)+
      scale_color_manual(values = cbpalette)+
      theme(legend.position = c(1, 1),
            legend.justification = c(1, 1)) -> fem
    data_summary %>%
      ggplot()+
      geom_line(aes(x = factor(year), y = recruit_male, group = model, color = model))+
      scale_x_discrete(breaks = yraxis$breaks, labels = yraxis$labels)+
      scale_y_continuous(labels = scales::comma, limits = c(0, NA))+
      labs(x = NULL, y = y_labs[3], color = NULL)+
      scale_color_manual(values = cbpalette)+
      theme(legend.position = c(1, 1),
            legend.justification = c(1, 1)) -> mal
    
    if(save_plot == T){
      ggsave(file.path(plot_dir, "total_recruitment.png"), plot = tot, height = 4.2, width = 7, units = "in")
      ggsave(file.path(plot_dir, "female_recruitment.png"), plot = fem, height = 4.2, width = 7, units = "in")
      ggsave(file.path(plot_dir, "male_recruitment.png"), plot = mal, height = 4.2, width = 7, units = "in")
    }
    if(save_plot == F){plots = c(tot, fem, mal)}
  }
  # only male
  if(!("recruit_female" %in% names(data_summary))) {
    if(unique(data_summary$n_units) %in% c(1, "1", "1s", "one", "One", "ones", "Ones")){y_labs <- "Recruitment"} else{
      if(is.na(data_summary$n_units)[1] == TRUE){y_labs <- "Recruitment"} else{
        y_labs <- paste0("Recruitment", " (", unique(data_summary$n_units), ")")}
    }
    data_summary %>%
      ggplot()+
      geom_line(aes(x = factor(year), y = recruit_male, group = model, color = model))+
      {if(plot_ci == T){geom_ribbon(aes(x = factor(year), ymin = rec_lci, ymax = rec_uci, group = model, fill = model), alpha = 0.2, show.legend = F)}}+
      scale_x_discrete(breaks = yraxis$breaks, labels = yraxis$labels)+
      scale_y_continuous(labels = scales::comma, limits = c(NA, NA))+
      labs(x = NULL, y = y_labs, color = NULL)+
      scale_color_manual(values = cbpalette)+
      {if(plot_ci == T){scale_fill_manual(values = cbpalette)}}+
      theme(legend.position = c(1, 1),
            legend.justification = c(1, 1)) -> mal
    
    if(save_plot == T){
      ggsave(file.path(plot_dir, "recruitment.png"), plot = mal, height = 4.2, width = 7, units = "in")
    }
    if(save_plot == F){plots = mal}
  }
  if(save_plot == F){plots} else{"done"}
  
}


# gmacs_plot_f() ----

## plot fishing mortality by fleet

## args:
### all_out - output from gmacs_read_allout as nested list, example: all.out = list(mod_23.0a, mod_23.1b)
### save_plot - T/F save plots, default = T
### plot_dir - file directory in which to save plots
### data_summary - alternate way to bring in data, output of gmacs_get_f()
### file - file paths to Gmacsall.out for each model to compare, passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided
### model_name - character string passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided
### yrs - subset a specific year range, example: c(1990:2022)

gmacs_plot_f <- function(all_out = NULL, save_plot = T, plot_dir = NULL, data_summary = NULL, file = NULL, model_name = NULL, yrs = NULL){
  
  # get summary data
  if(is.null(data_summary)){data_summary <- gmacs_get_f(all_out, file, model_name)}
  
  # save plot dirs ----
  if(save_plot == T & is.null(plot_dir)) {plot_dir <- file.path(getwd(), "plots"); dir.create(plot_dir, showWarnings = F, recursive = TRUE)}
  if(!is.null(plot_dir) && !file.exists(plot_dir)) {dir.create(plot_dir, showWarnings = F, recursive = TRUE)}
  
  # plot f by fleet and sex ----
  
  if(!is.null(yrs)){data_summary <- filter(data_summary, year %in% yrs)} # filter year range if specified
  data_summary %>%
    group_by(model, year, sex, fleet, n_sex) %>%
    summarize(f = sum(`F`)) %>% ungroup %>%
    mutate(fleet_name = gsub("_", " ", fleet),
           sex_name = str_to_title(sex)) %>%
    nest_by(sex_name, fleet_name, .keep = T) %>%
    mutate(ylab = ifelse(length(unique(data$sex_name)) > 1, 
                         paste(unique(data$fleet_name), unique(data$sex_name), "F"), 
                         paste(unique(data$fleet_name), "F"))) %>% ungroup %>% #pull(data) %>% .[[1]] -> data
    mutate(plot = purrr::map2(data, ylab, function(data, ylab) {
      
      data %>%
        ggplot()+
        geom_line(aes(x = factor(year), y = f, color = model, group = model))+
        labs(x = NULL, y = ylab, color = NULL)+
        scale_x_discrete(breaks = yraxis$breaks, labels = yraxis$labels)+
        scale_color_manual(values = cbpalette) -> x
      if(save_plot == T){
        pwidth <- min(max(length(min(data$year):max(data$year))*0.2, 5), 7)
        ggsave(file.path(plot_dir, paste0(tolower(gsub(" ", "_", ylab)), ".png")), plot = x, 
               height = 0.6*pwidth, width = pwidth, units = "in")
      }
      return(x)
    })) -> by_fleet
  
  return(if(save_plot == T){"done"}else{by_fleet$plot})
  
}


# gmacs_plot_slx() ----

## args:
### all_out - output from gmacs_read_allout as nested list, example: all.out = list(mod_23.0a, mod_23.1b)
### save_plot - T/F save plots, default = T
### plot_dir - file directory in which to save plots
### size_lab - optional, custom size axis label, as character vector, example: "Carapace Length (mm)", default = "Size"
### data_summary - alternate way to bring in data, output of gmacs_get_slx()
### file - file paths to Gmacsall.out for each model to compare, passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided
### model_name - character string passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided

gmacs_plot_slx <- function(all_out = NULL, save_plot = T, plot_dir = NULL, size_lab = "Size", data_summary = NULL, file = NULL, model_name = NULL){
  
  # get summary data
  if(is.null(data_summary)){data_summary <- gmacs_get_slx(all_out, file, model_name)}
  
  # create output directories
  if(save_plot == T & is.null(plot_dir)) {plot_dir <- file.path(getwd(), "plots"); dir.create(plot_dir, showWarnings = F, recursive = TRUE)}
  if(!is.null(plot_dir) && !file.exists(plot_dir)) {dir.create(plot_dir, showWarnings = F, recursive = TRUE)}
  
  
  # plots
  data_summary %>%
    mutate(sex = str_to_title(sex)) %>%
    nest_by(fleet, .keep = T) %>% ungroup %>% 
    mutate(capture_plot = purrr::map2(data, fleet, function(data, fleet){
      
      data %>% 
        ggplot()+
        geom_line(aes(x = size, y = slx_capture, color = model))+
        {if(length(unique(data$sex)) == 1){facet_wrap(~capture_block, nrow = 1)}}+
        {if(length(unique(data$sex)) > 1){facet_grid(cols = vars(capture_block), rows = vars(sex))}}+
        scale_color_manual(values = cbpalette)+
        labs(x = size_lab, y = "Selectivity", color = NULL) -> x
      
      if(save_plot == T) {
        # save plot of all stacked
        ggsave(plot = x, 
               filename = file.path(plot_dir, paste0(tolower(fleet), "_slx_capture.png")),
               height = length(unique(data$sex)) * 3, width = min(length(unique(data$capture_block)) * 4, 8), units = "in") 
      }
      return(x)
      
    }),
    retention_plot = purrr::map2(data, fleet, function(data, fleet){
      
      data %>%
        ggplot()+
        geom_line(aes(x = size, y = slx_retention, color = model))+
        {if(length(unique(data$sex)) == 1){facet_wrap(~ret_disc_block, nrow = 1)}}+
        {if(length(unique(data$sex)) > 1){facet_grid(cols = vars(ret_disc_block), rows = vars(sex))}}+
        scale_color_manual(values = cbpalette)+
        labs(x = size_lab, y = "Prob. Retention", color = NULL) -> x
      
      if(save_plot == T) {
        # save plot of all stacked
        ggsave(plot = x, 
               filename = file.path(plot_dir, paste0(tolower(fleet), "_slx_retention.png")),
               height = length(unique(data$sex)) * 3, width = min(length(unique(data$ret_disc_block)) * 4, 8), units = "in") 
      }
      return(x)
      
    }),
    discard_plot = purrr::map2(data, fleet, function(data, fleet){
      
      data %>%
        ggplot()+
        geom_line(aes(x = size, y = slx_discard, color = model))+
        {if(length(unique(data$sex)) == 1){facet_wrap(~ret_disc_block, nrow = 1)}}+
        {if(length(unique(data$sex)) > 1){facet_grid(cols = vars(ret_disc_block), rows = vars(sex))}}+
        scale_color_manual(values = cbpalette)+
        labs(x = size_lab, y = "Prob. Discard", color = NULL) -> x
      
      if(save_plot == T) {
        # save plot of all stacked
        ggsave(plot = x, 
               filename = file.path(plot_dir, paste0(tolower(fleet), "_slx_discard.png")),
               height = length(unique(data$sex)) * 3, width = min(length(unique(data$ret_disc_block)) * 4, 8), units = "in") 
      }
      return(x)
      
    })) -> out
  
  # output
  if(save_plot == T) {return("done")}
  if(save_plot == F) {c(out$capture_plot, out$retention_plot, out$discard_plot)}
  

  }


# gmacs_plot_m() ----

## ## plot natural mortality

## args:
### all_out - output from gmacs_read_allout as nested list, example: all.out = list(mod_23.0a, mod_23.1b)
### save_plot - T/F save plots, default = T
### plot_dir - file directory in which to save plots
### by - vector of grouping variable. Example = c("year", "sex"). Optional, if NULL, the function will determine how M varies
### size_lab - optional, custom size axis label, as character vector, example: "Carapace Length (mm)", default = "Size"
### data_summary - alternate way to bring in data, output of gmacs_get_m()
### file - file paths to Gmacsall.out for each model to compare, passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided
### model_name - character string passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided
### yrs - subset a specific year range, example: c(1990:2022)

gmacs_plot_m <- function(all_out = NULL, save_plot = T, plot_dir = NULL, by = NULL, size_lab = "Size", data_summary = NULL, file = NULL, model_name = NULL, yrs = NULL){
  
  # get summary data
  if(is.null(data_summary)){data_summary <- gmacs_get_m(all_out, file, model_name)}
  
  # save plot dirs ----
  if(save_plot == T & is.null(plot_dir)) {plot_dir <- file.path(getwd(), "plots"); dir.create(plot_dir, showWarnings = F, recursive = TRUE)}
  if(!is.null(plot_dir) && !file.exists(plot_dir)) {dir.create(plot_dir, showWarnings = F, recursive = TRUE)}
  
  # plot m ----
  
  if(!is.null(yrs)){data_summary <- filter(data_summary, year %in% yrs)} # filter year range if specified
  
  if(is.null(by)){
    data_summary %>%
      distinct(M, .keep_all = T) %>%
      dplyr::select_if(function(x){length(unique(x)) > 1}) %>%
      dplyr::select(-M) %>% names -> by
  }
  by <- by[by!="model"]
  
  #plot dimensions
  data_summary %>%
    distinct(M, .keep_all = T) %>%
    pull(sex) %>% unique %>% length -> nsex
  data_summary %>%
    distinct(M, .keep_all = T) %>%
    pull(maturity) %>% unique %>% length -> nmat
  
  if(!("size" %in% by) & "year" %in% by) {
    
    data_summary %>%
      mutate(sex = str_to_title(sex),
             sex = factor(sex, levels = c("Male", "Female"))) %>%
      mutate(maturity = case_when(maturity == 1 ~ "Mature",
                                  maturity == 2 ~ "Immature"),
             maturity = factor(maturity, levels = c("Mature", "Immature"))) %>%
      distinct(model, year, !!!syms(by), M) %>%
      ggplot()+
      geom_line(aes(x = factor(year), y = M, group = model, color = model))+
      geom_point(data = function(x) filter(x, year == max(year)),
                 aes(x = factor(year), y = M, color = model))+
      scale_x_discrete(breaks = yraxis$breaks, labels = yraxis$labels)+
      scale_y_continuous(labels = scales::comma, limits = c(0, NA))+
      scale_color_manual(values = cbpalette)+
      labs(x = NULL, y = paste0("Natural mortality (M)"), color = NULL)+
      facet_wrap(by[by!="year"], ncol = 1, scales = "free_y") -> x
    
  }
  if("size" %in% by & "year" %in% by){
    
    data_summary %>%
      mutate(sex = str_to_title(sex),
             sex = factor(sex, levels = c("Male", "Female"))) %>%
      mutate(maturity = case_when(maturity == 1 ~ "Mature",
                                  maturity == 2 ~ "Immature"),
             maturity = factor(maturity, levels = c("Mature", "Immature"))) %>%
      distinct(model, year, !!!syms(by), M) %>%
      ggplot()+
      geom_line(aes(x = size, y = M, color = factor(year), linetype = model))+
      labs(x = size_lab, y = "Natural Mortaity (M)", color = NULL, linetype = NULL)+
      facet_wrap(by[by != "size"]) -> x
    
  }
  if("size" %in% by & !("year" %in% by)){
    
    data_summary %>%
      mutate(sex = str_to_title(sex),
             sex = factor(sex, levels = c("Male", "Female"))) %>%
      mutate(maturity = case_when(maturity == 1 ~ "Mature",
                                  maturity == 2 ~ "Immature"),
             maturity = factor(maturity, levels = c("Mature", "Immature"))) %>%
      distinct(model, !!!syms(by), M) %>%
      ggplot()+
      geom_line(aes(x = size, y = M, color = model))+
      labs(x = size_lab, y = "Natural Mortaity (M)", color = NULL)+
      facet_wrap(by[!(by %in% c("size", "year"))], ncol = 1) -> x
    
  }
  
  if(save_plot == T){
    ggsave(file.path(plot_dir, "natural_mortality.png"), plot = x, height = 4.2 * nsex * nmat, width = 7, units = "in")
  }
  
  return(if(save_plot == T){"done"}else{x})
  
}

# gmacs_plot_recruitment_distribution() ----

## plot recruitment distribution

## args:
### all_out - output from gmacs_read_allout as nested list, example: all.out = list(mod_23.0a, mod_23.1b)
### save_plot - T/F save plots, default = T
### plot_dir - file directory in which to save plots
### n_rec_class - number of recruitment classes as list with first element vector for males, second element vector for females
### size_lab - optional, custom size axis label, as character vector, example: "Carapace Length (mm)", default = "Size"
### data_summary - alternate way to bring in data, output of gmacs_get_derived_quantity_summary()
### file - file paths to Gmacsall.out for each model to compare, passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided
### model_name - character string passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided

gmacs_plot_recruitment_distribution <- function(all_out = NULL, save_plot = T, plot_dir = NULL, n_rec_class = NULL, size_lab = "Size", data_summary = NULL, file = NULL, model_name = NULL) {
  # get summary data
  if(is.null(data_summary)){data_summary <- gmacs_get_recruitment_distribution(all_out, file, model_name, n_rec_class = n_rec_class)}
  
  # plots ----
  if(save_plot == T & is.null(plot_dir)) {plot_dir <- file.path(getwd(), "plots"); dir.create(plot_dir, showWarnings = F, recursive = TRUE)}
  if(!is.null(plot_dir) && !file.exists(plot_dir)) {dir.create(plot_dir, showWarnings = F, recursive = TRUE)}
  
  data_summary %>%
    mutate(sex = str_to_title(sex)) %>%
    ggplot()+
    geom_line(aes(x = size, y = rec_dist, color = model))+
    labs(x = size_lab, y = "Recruitment Proportion", color = NULL)+
    theme(legend.position = c(1, 1), legend.justification = c(1, 1))+
    scale_color_manual(values = cbpalette)+
    {if(length(unique(data_summary$sex)) > 1) {facet_wrap(~sex, ncol = 1)}} -> p
  if(save_plot == T){
    ggsave(file.path(plot_dir, "recruitment_distribution.png"), plot = p, height = 4.2, width = 7, units = "in")
    return("done")
  }
  if(save_plot == F) {return(p)}
}




# gmacs_plot_molt_probability() ----

## args:
### all_out - output from gmacs_read_allout as nested list, example: all.out = list(mod_23.0a, mod_23.1b)
### save_plot - T/F save plots, default = T
### plot_dir - file directory in which to save plots
### size_lab - optional, custom size axis label, as character vector, example: "Carapace Length (mm)", default = "Size"
### data_summary - alternate way to bring in data, output of gmacs_get_molt_probability()
### file - file paths to Gmacsall.out for each model to compare, passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided
### model_name - character string passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided

gmacs_plot_molt_probability <- function(all_out = NULL, save_plot = T, plot_dir = NULL, size_lab = "Size", data_summary = NULL, file = NULL, model_name = NULL){
  
  # get summary data
  if(is.null(data_summary)){data_summary <- gmacs_get_molt_probability(all_out, file, model_name)}
  
  # save plot dirs ----
  if(save_plot == T & is.null(plot_dir)) {plot_dir <- file.path(getwd(), "plots"); dir.create(plot_dir, showWarnings = F, recursive = TRUE)}
  if(!is.null(plot_dir) && !file.exists(plot_dir)) {dir.create(plot_dir, showWarnings = F, recursive = TRUE)}
  
  # plot molt probability ----
  
  data_summary %>%
    distinct(model, sex, block, size, molt_probability, block) %>%
    nest_by(sex) %>% ungroup %>% 
    mutate(plot = purrr::map2(sex, data, function(sex, data){
      
      data %>%
        filter(!is.na(block)) %>%
        ggplot()+
        geom_line(aes(x = size, y = molt_probability, color = model))+
        {if(length(unique(data$block)) > 1) {facet_wrap(~block, nrow = 1)}}+
        scale_color_manual(values = cbpalette)+
        labs(x = size_lab, y = "Molt Probability", color = NULL) -> x
      
      if(save_plot == T) {
        # save plot of all stacked
        ggsave(plot = x, 
               filename = file.path(plot_dir, paste0(sex, "_molt_probability.png")),
               height = length(unique(sex)) * 3, width = min(length(unique(data$block[!is.na(data$block)]))*4, 8), units = "in") 
      }
      return(x)
    })) -> out
  
  # output
  if(save_plot == T) {return("done")}
  if(save_plot == F) {out$plot} 
  
}


# gmacs_plot_f_mmb() ----

## plot relationship between f and mmb

## args:
### all_out - output from gmacs_read_allout as nested list, example: all.out = list(mod_23.0a, mod_23.1b)
### save_plot - T/F save plots, default = T
### plot_dir - file directory in which to save plots
### beta- spr beta. Default 0.25
### alpha - spr alpha. Default 0.1
### spr_targ - target spr ratio. Default 0.35
### file - file paths to Gmacsall.out for each model to compare, passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided
### model_name - character string passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided

gmacs_plot_f_mmb <- function(all_out = NULL, save_plot = T, plot_dir = NULL, beta = 0.25, alpha = 0.1, spr_targ = 0.35, data_summary = NULL, file = NULL, model_name = NULL){
  
  # bring in all out data ----
  
  if(!is.null(file) && is.null(all_out)) {
    if(is.null(model_name)) {stop("Must supply model name(s)!!")}
    # read all out file
    all_out <- purrr::map2(file, model_name, gmacs_read_allout); names(all_out) <- paste0("model", model_name)
  }
  
  # save plot dirs ----
  if(save_plot == T & is.null(plot_dir)) {plot_dir <- file.path(getwd(), "plots"); dir.create(plot_dir, showWarnings = F, recursive = TRUE)}
  if(!is.null(plot_dir) && !file.exists(plot_dir)) {dir.create(plot_dir, showWarnings = F, recursive = TRUE)}
  
  # extract data
  tibble(mod = names(all_out),
         all_out = all_out) %>%
    mutate(model = purrr::map_chr(all_out, function(x) {x$model_name}),
           plot = purrr::map(all_out, function(x) {
             
             
             # get directed fs
             gmacs_get_f(list(x)) %>%
               filter(sex == "male") %>%
               group_by(year) %>%
               summarise(f = sum(F)) -> fs
             
             # get mmb time series
             gmacs_get_derived_quantity_summary(list(x)) %>%
               transmute(year, mmb = ssb) -> mmb
             
             # set up mmb vector for fofl control rule line
             b <- 0:max(mmb$mmb)
             
             # get biomass target and fmsy
             btarg <- x$bmsy
             ftarg <- x$f_msy_tot
             
             # control rule
             tibble(b = b) %>%
               mutate(f_ofl = case_when(b / btarg <= beta ~ 0,
                                        b / btarg > beta & b/btarg <= 1 ~ ftarg * ((b/btarg - alpha) / (1 - alpha)),
                                        b / btarg > 1 ~ ftarg)) -> control_rule
             
             # plot annotation
             annotation <- paste0("F", spr_targ*100, " = ", round(ftarg, 3), "\nFOFL = ", round(x$f_ofl_tot, 3))
             
             # plot
             left_join(fs, mmb, by = "year") %>%
               filter(f > 0) %>%
               ggplot()+
               geom_line(data = control_rule, aes(x = b, y = f_ofl), size = 1)+
               geom_text(aes(x = mmb, y = f, label = substring(year, 3, 4)), size = 2.5)+
               geom_text(data = function(x){filter(x, year == max(x$year))}, 
                         aes(x = mmb, y = f, label = substring(year, 3, 4)), size = 2.5, color = "firebrick")+
               geom_text_npc(aes(npcx = "right", npcy = "top", label = annotation),
                             check_overlap = T, size = 3)+
               scale_x_continuous(labels = scales::comma)+
               labs(x = paste0("MMB (", x$wt_units, ")"), y = "F") -> p
             
             if(save_plot == T) {
               ggsave(plot = p, 
                      filename = file.path(plot_dir, paste0(x$model_name, "_f_mmb.png")),
                      height = 4, width = 5, units = "in") 
             }
             
             return(p)
             
           })) -> out
  # out ----
  if(save_plot == T) {return("done")}
  if(save_plot == F) {return(out$plot)}
  
  
}

# gmacs_plot_kobe() ----

## kobe plot

## args:
### all_out - output from gmacs_read_allout as nested list, example: all.out = list(mod_23.0a, mod_23.1b)
### save_plot - T/F save plots, default = T
### plot_dir - file directory in which to save plots
### beta- spr beta. Default 0.25
### alpha - spr alpha. Default 0.1
### spr_targ - target spr ratio. Default 0.35
### file - file paths to Gmacsall.out for each model to compare, passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided
### model_name - character string passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided

gmacs_plot_kobe <- function(all_out = NULL, save_plot = T, plot_dir = NULL, beta = 0.25, alpha = 0.1, spr_targ = 0.35, data_summary = NULL, file = NULL, model_name = NULL){
  
  # bring in all out data ----
  
  if(!is.null(file) && is.null(all_out)) {
    if(is.null(model_name)) {stop("Must supply model name(s)!!")}
    # read all out file
    all_out <- purrr::map2(file, model_name, gmacs_read_allout); names(all_out) <- paste0("model", model_name)
  }
  
  # save plot dirs ----
  if(save_plot == T & is.null(plot_dir)) {plot_dir <- file.path(getwd(), "plots"); dir.create(plot_dir, showWarnings = F, recursive = TRUE)}
  if(!is.null(plot_dir) && !file.exists(plot_dir)) {dir.create(plot_dir, showWarnings = F, recursive = TRUE)}
  
  # extract data
  tibble(mod = names(all_out),
         all_out = all_out) %>%
    mutate(model = purrr::map_chr(all_out, function(x) {x$model_name}),
           plot = purrr::map(all_out, function(x) {
             
             
             # get directed fs
             gmacs_get_f(list(x)) %>%
               filter(sex == "male") %>%
               group_by(year) %>%
               summarise(f = sum(F)) -> fs
             
             # get mmb time series
             gmacs_get_derived_quantity_summary(list(x)) %>%
               transmute(year, mmb = ssb) -> mmb
             
             # first year of catch
             gmacs_get_catch_summary(list(x)) %>% pull(year) %>% min -> first_yr
             
             # get biomass target and fmsy
             btarg <- x$bmsy
             ftarg <- x$f_msy_tot
             
             # control rule
             tibble(status = seq(0, max(mmb$mmb[mmb$year >= first_yr])/btarg * 1.1, 0.0001)) %>%
               mutate(f_ofl = case_when(status <= beta ~ 0,
                                        status > beta & status <= 1 ~ ((status - alpha) / (1 - alpha)),
                                        status > 1 ~ 1)) -> control_rule
             # bacground polygons
             # all coords are top left clockwise
             p_xlim <- max(mmb$mmb[mmb$year >= first_yr])/btarg * 1.1
             p_ylim <- max(fs$f[mmb$year >= first_yr])/ftarg * 1.1
             glite <- tibble(x = c(1, p_xlim, p_xlim, 1), 
                             y = c(1, 1, 0, 0))
             ylite <- tibble(x = c(1, 1, beta, beta, 1, p_xlim, p_xlim, 1), 
                             y = c(1, 0, 0, ((beta - alpha) / (1 - alpha)), p_ylim, p_ylim, 1, 1), 
                             group = c(1, 1, 1, 1, 2, 2, 2, 2))
             rlite <- tibble(x = c(0, 1, 1, beta, beta, 0), 
                             y = c(p_ylim, p_ylim, 1, ((beta - alpha) / (1 - alpha)), 0, 0))
             vline <- tibble(x = c(1, 1), y = c(0, p_ylim))
             
             # plot
             left_join(fs, mmb, by = "year") %>%
               filter(year >= first_yr) %>%
               ggplot()+
               geom_line(data = control_rule, aes(x = status, y = f_ofl), linetype = 1, size = 1)+
               geom_line(data = vline, aes(x = x, y= y), linetype = 2)+
               geom_polygon(data = glite, aes(x = x, y = y), color = NA, fill = "green", alpha = 0.3)+
               geom_polygon(data = ylite, aes(x = x, y = y, group = group), color = NA, fill = "yellow", alpha = 0.3)+
               geom_polygon(data = rlite, aes(x = x, y = y), color = NA, fill = "red", alpha = 0.3)+
               geom_point(aes(x = mmb/btarg, y = f/ftarg))+
               geom_text(aes(x = mmb/btarg, y = f/ftarg, label = substring(year, 3, 4)), size = 2, nudge_y = 0.05)+
               geom_path(aes(x = mmb/btarg, y = f/ftarg), size = 0.2)+
               geom_point(data = function(x){filter(x, year == max(x$year))},
                          aes(x = mmb/btarg, y = f/ftarg), size = 3, shape = 21, fill = "white")+
               labs(x = bquote(B/B[.(spr_targ*100)~"%"]), y = bquote("F/"~F[.(spr_targ*100)~"%"])) -> p 
             
             if(save_plot == T) {
               ggsave(plot = p, 
                      filename = file.path(plot_dir, paste0(x$model_name, "_kobe.png")),
                      height = 4, width = 5, units = "in") 
             }
             
             return(p)
             
           })) -> out
  # out ----
  if(save_plot == T) {return("done")}
  if(save_plot == F) {return(out$plot)}
  
  
}


