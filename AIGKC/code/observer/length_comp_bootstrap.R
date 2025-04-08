# notes ----

# bootstrap size composition sample size
# tyler jackson
# tyler.jackson@alaska.gov
# 3/18/2025

# load ----

library(tidyverse)

# data ----

obs_meas <- read_csv("./AIGKC/data/observer/2025/linked_meas_dump.csv")

dock <- read_csv("./AIGKC/data/observer/2025/cleaned_dockside.csv")

# observed size comp ----

# total size
obs_meas %>%
  filter(sex == 1, size > 100) %>% 
  group_by(crab_year, subdistrict) %>%
  mutate(bin = ifelse(size <= 183, ceiling(size / 5) * 5 - 2, 183),
         total = n()) %>%
  group_by(crab_year, subdistrict, bin) %>%
  summarise(obs_p_tot = n() / mean(total),
            nmeas = mean(total)) %>% ungroup -> obs_p_tot

# retained size
dock %>%
  filter(size > 100) %>%
  group_by(crab_year, subdistrict) %>%
  mutate(bin = ifelse(size <= 183, ceiling(size / 5) * 5 - 2, 183),
         total = n()) %>%
  group_by(crab_year, subdistrict, bin) %>%
  summarise(obs_p_ret = n() / mean(total),
            nmeas = mean(total)) %>% ungroup -> obs_p_ret

# total composition boot ----

yrs <- 1995:1997
# eag
for(z in 1:10){
for (i in yrs){
  # random number seed
  # set.seed(1624)
  
  obs_meas %>%
    filter(subdistrict == "EAG",
           crab_year == i) %>%
    mutate(pot = paste0(trip, adfg, spn),
           n_meas_pots = length(unique(pot))) %>%
    # bootstrap pots
    expand_grid(., pot_boot = 1:100) %>%
    nest_by(crab_year, pot_boot, n_meas_pots, .keep = T) %>% ungroup() %>%  #pull(data) %>% .[[1]] -> data
    # resample crab
    mutate(data = purrr::map2(data, n_meas_pots, function(data, n_meas_pots){
      
      
      data %>% group_by(pot) %>% nest %>% ungroup %>%
        sample_n(., n_meas_pots, replace = T) %>%
        unnest(data) %>%
        group_by(pot) %>% nest %>% ungroup %>% #pull(data) %>% .[[1]] -> data
        # sample lengths
        mutate(length_sample = purrr::map(data, function(data) {
          data %>%
            filter(sex == 1) %>%
            sample_n(., nrow(.), replace = T)
        })) %>%
        transmute(pot, length_sample) %>% unnest(length_sample)
      
      
    })) %>%
    ungroup %>% 
    # compute neff
    mutate(n = purrr::map_dbl(data, function(data){
      data %>%
        filter(size > 100) %>%
        group_by(crab_year) %>%
        mutate(bin = ifelse(size <= 183, ceiling(size / 5) * 5 - 2, 183),
               total = n()) %>%
        group_by(crab_year, bin) %>%
        summarise(p = n() / mean(total)) %>% 
        left_join(obs_p_tot %>% filter(subdistrict == "EAG"), by = c("crab_year", "bin")) %>% ungroup %>%
        summarise(n = sum(obs_p_tot * (1 - obs_p_tot)) / sum((obs_p_tot - p)^2)) %>% pull(n)
      
    })) -> out
  
  saveRDS(out, paste0("./AIGKC/output/observer/length_comp_boot/eag_total_comp_boot_", i, "_", z, ".RDS"))
  
}
  }
# wag
for (i in yrs){
  # random number seed
  # set.seed(1624)
  
  obs_meas %>%
    filter(subdistrict == "WAG",
           crab_year == i) %>%
    mutate(pot = paste0(trip, adfg, spn),
           n_meas_pots = length(unique(pot))) %>%
    # bootstrap pots
    expand_grid(., pot_boot = 1:1000) %>%
    nest_by(crab_year, pot_boot, n_meas_pots, .keep = T) %>% ungroup() %>%  #pull(data) %>% .[[1]] -> data
    # resample crab
    mutate(data = purrr::map2(data, n_meas_pots, function(data, n_meas_pots){
      
      
      data %>% group_by(pot) %>% nest %>% ungroup %>%
        sample_n(., n_meas_pots, replace = T) %>%
        unnest(data) %>%
        group_by(pot) %>% nest %>% ungroup %>% #pull(data) %>% .[[1]] -> data
        # sample lengths
        mutate(length_sample = purrr::map(data, function(data) {
          data %>%
            filter(sex == 1) %>%
            sample_n(., nrow(.), replace = T)
        })) %>%
        transmute(pot, length_sample) %>% unnest(length_sample)
      
      
    })) %>%
    ungroup %>% 
    # compute neff
    mutate(n = purrr::map_dbl(data, function(data){
      data %>%
        filter(size > 100) %>%
        group_by(crab_year) %>%
        mutate(bin = ifelse(size <= 183, ceiling(size / 5) * 5 - 2, 183),
               total = n()) %>%
        group_by(crab_year, bin) %>%
        summarise(p = n() / mean(total)) %>% 
        left_join(obs_p_tot %>% filter(subdistrict == "WAG"), by = c("crab_year", "bin")) %>% ungroup %>%
        summarise(n = sum(obs_p_tot * (1 - obs_p_tot)) / sum((obs_p_tot - p)^2)) %>% pull(n)
      
    })) -> out
  
  saveRDS(out, paste0("./AIGKC/output/observer/length_comp_boot/wag_total_comp_boot_", i, ".RDS"))
  
}

# lapply(list.files("./AIGKC/output/observer", pattern = "eag_total_comp_boot", full.names = T), readRDS) %>% 
#   bind_rows %>%
#   group_by(crab_year) %>%
#   summarise(neff = mean(n)) -> tmp

# retained composition boot ----

yrs <- 1981:2024
# eag
for (i in yrs){
  # random number seed
  #set.seed(1624)
  
  dock %>%
    filter(subdistrict == "EAG",
           crab_year == i) %>%
    mutate(delivery = paste0(adfg, sample_date),
           n_deliveries =  length(unique(delivery))) %>%
    # bootstrap pots
    expand_grid(., del_boot = 1:1000) %>%
    nest_by(crab_year, del_boot, n_deliveries, .keep = T) %>% ungroup() %>%  #pull(data) %>% .[[1]] -> data
    # resample crab
    mutate(data = purrr::map2(data, n_deliveries, function(data, n_deliveries){
      
      
      data %>% group_by(delivery) %>% nest %>% ungroup %>%
        sample_n(., n_deliveries, replace = T) %>%
        unnest(data) %>%
        group_by(delivery) %>% nest %>% ungroup %>% #pull(data) %>% .[[1]] -> data
        # sample lengths
        mutate(length_sample = purrr::map(data, function(data) {
          data %>%
            sample_n(., nrow(.), replace = T)
        })) %>%
        transmute(delivery, length_sample) %>% unnest(length_sample)
      
      
    })) %>%
    ungroup %>%
    # compute neff
    mutate(n = purrr::map_dbl(data, function(data){
      data %>%
        filter(size > 100) %>%
        group_by(crab_year) %>%
        mutate(bin = ifelse(size <= 183, ceiling(size / 5) * 5 - 2, 183),
               total = n()) %>%
        group_by(crab_year, bin) %>%
        summarise(p = n() / mean(total)) %>% 
        left_join(obs_p_ret %>% filter(subdistrict == "EAG"), by = c("crab_year", "bin")) %>% ungroup %>%
        summarise(n = sum(obs_p_ret * (1 - obs_p_ret)) / sum((obs_p_ret - p)^2)) %>% pull(n)
      
    })) -> out
  
  saveRDS(out, paste0("./AIGKC/output/observer/eag_retained_comp_boot_", i, ".RDS"))
  
}
# wag
for (i in yrs){
  # random number seed
  # set.seed(1624)
  
  dock %>%
    filter(subdistrict == "WAG",
           crab_year == i) %>%
    mutate(delivery = paste0(adfg, sample_date),
           n_deliveries =  length(unique(delivery))) %>%
    # bootstrap pots
    expand_grid(., del_boot = 1:1000) %>%
    nest_by(crab_year, del_boot, n_deliveries, .keep = T) %>% ungroup() %>%  #pull(data) %>% .[[1]] -> data
    # resample crab
    mutate(data = purrr::map2(data, n_deliveries, function(data, n_deliveries){
      
      
      data %>% group_by(delivery) %>% nest %>% ungroup %>%
        sample_n(., n_deliveries, replace = T) %>%
        unnest(data) %>%
        group_by(delivery) %>% nest %>% ungroup %>% #pull(data) %>% .[[1]] -> data
        # sample lengths
        mutate(length_sample = purrr::map(data, function(data) {
          data %>%
            sample_n(., nrow(.), replace = T)
        })) %>%
        transmute(delivery, length_sample) %>% unnest(length_sample)
      
      
    })) %>%
    ungroup %>%
    # compute neff
    mutate(n = purrr::map_dbl(data, function(data){
      data %>%
        filter(size > 100) %>%
        group_by(crab_year) %>%
        mutate(bin = ifelse(size <= 183, ceiling(size / 5) * 5 - 2, 183),
               total = n()) %>%
        group_by(crab_year, bin) %>%
        summarise(p = n() / mean(total)) %>% 
        left_join(obs_p_ret %>% filter(subdistrict == "WAG"), by = c("crab_year", "bin")) %>% ungroup %>%
        summarise(n = sum(obs_p_ret * (1 - obs_p_ret)) / sum((obs_p_ret - p)^2)) %>% pull(n)
      
    })) -> out
  
  saveRDS(out, paste0("./AIGKC/output/observer/wag_retained_comp_boot_", i, ".RDS"))
  
}

# table output ----


wag_ret_tab <- data.frame()
wag_ret_files <- list.files("./AIGKC/output/observer/length_comp_boot", pattern = "wag_retained_comp_boot", full.names = T)
for(file in 1:length(wag_ret_files)) {
  
  readRDS(wag_ret_files[file]) %>%
    mutate(fishery = "WAG") %>%
    left_join(dock %>%
                filter(size > 100, subdistrict == "WAG") %>% 
                group_by(crab_year) %>%
                summarise(n_meas = n()) %>% ungroup) %>% 
    group_by(fishery, crab_year) %>%
    summarise(n_deliveries = mean(n_deliveries),
              n_meas = mean(n_meas),
              neff_min = min(n),
              neff_mean = mean(n),
              neff_max = max(n)) -> wag_ret_tab[file, 1:7]
  
}
write_csv(wag_ret_tab, "./AIGKC/output/observer/length_comp_boot/retained_neff_boot_table_wag.csv")


eag_ret_tab <- data.frame()
eag_ret_files <- list.files("./AIGKC/output/observer/length_comp_boot", pattern = "eag_retained_comp_boot", full.names = T)
for(file in 1:length(eag_ret_files)) {
  
  readRDS(eag_ret_files[file]) %>%
    mutate(fishery = "EAG") %>%
    left_join(dock %>%
                filter(size > 100, subdistrict == "EAG") %>% 
                group_by(crab_year) %>%
                summarise(n_meas = n()) %>% ungroup) %>% 
    group_by(fishery, crab_year) %>%
    summarise(n_deliveries = mean(n_deliveries),
              n_meas = mean(n_meas),
              neff_min = min(n),
              neff_mean = mean(n),
              neff_max = max(n)) -> eag_ret_tab[file, 1:7]
  
}
write_csv(eag_ret_tab, "./AIGKC/output/observer/length_comp_boot/retained_neff_boot_table_eag.csv")



wag_tot_tab <- data.frame()
wag_tot_files <- list.files("./AIGKC/output/observer/length_comp_boot", pattern = "wag_total_comp_boot", full.names = T)
for(file in 1:length(wag_tot_files)) {
  
  readRDS(wag_tot_files[file]) %>%
    mutate(fishery = "WAG") %>%
    left_join(obs_meas %>%
                filter(size > 100, subdistrict == "WAG") %>% 
                group_by(crab_year) %>%
                summarise(n_meas = n()) %>% ungroup) %>% 
    group_by(fishery, crab_year) %>%
    summarise(n_meas_pots = mean(n_meas_pots),
              n_meas = mean(n_meas),
              neff_min = min(n),
              neff_mean = mean(n),
              neff_max = max(n)) -> wag_tot_tab[file, 1:7]
  
}
write_csv(wag_tot_tab, "./AIGKC/output/observer/length_comp_boot/total_neff_boot_table_wag.csv")


eag_tot_tab <- data.frame()
eag_tot_files <- list.files("./AIGKC/output/observer/length_comp_boot", pattern = "eag_total_comp_boot", full.names = T)
for(file in 1:length(eag_tot_files)) {
  
  readRDS(eag_tot_files[file]) %>%
    mutate(fishery = "EAG") %>%
    left_join(obs_meas %>%
                filter(size > 100, subdistrict == "EAG") %>% 
                group_by(crab_year) %>%
                summarise(n_meas = n()) %>% ungroup) %>% 
    group_by(fishery, crab_year) %>%
    summarise(n_meas_pots = mean(n_meas_pots),
              n_meas = mean(n_meas),
              neff_min = min(n),
              neff_mean = mean(n),
              neff_max = max(n)) -> eag_tot_tab[file, 1:7]
  
}
eag_tot_tab %>%
  group_by(fishery, crab_year) %>%
  summarise(n_meas_pots = mean(n_meas_pots),
            n_meas = mean(n_meas),
            neff_min = min(neff_min),
            neff_mean = mean(neff_mean),
            neff_max = max(neff_max)) %>% ungroup %>%
write_csv("./AIGKC/output/observer/length_comp_boot/total_neff_boot_table_eag.csv")
