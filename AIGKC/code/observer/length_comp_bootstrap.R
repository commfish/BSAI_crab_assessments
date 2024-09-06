# notes ----

# bootstrap size composition sample size
# tyler jackson
# tyler.jackson@alaska.gov

# load ----

library(tidyverse)

# data ----

obs_meas <- read_csv("./AIGKC/data/observer/item12_linked_meas_dump.csv")

dock <- read_csv("./AIGKC/data/observer/item13_cleaned_dockside.csv")

# observed size comp ----

# total size
obs_meas %>%
  filter(sex == 1, size > 100) %>% 
  group_by(crab_year, subdistrict) %>%
  mutate(bin = ifelse(size <= 183, ceiling(size / 5) * 5 - 2, 183),
         total = n()) %>%
  group_by(crab_year, subdistrict, bin) %>%
  summarise(obs_p_tot = n() / mean(total)) %>% ungroup -> obs_p_tot

# retained size
dock %>%
  filter(size > 100) %>%
  group_by(crab_year, subdistrict) %>%
  mutate(bin = ifelse(size <= 183, ceiling(size / 5) * 5 - 2, 183),
         total = n()) %>%
  group_by(crab_year, subdistrict, bin) %>%
  summarise(obs_p_ret = n() / mean(total)) %>% ungroup -> obs_p_ret

# total composition boot ----

yrs <- 1990:2023
# eag
for (i in yrs){
  # random number seed
  set.seed(1624)
  
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
  
  saveRDS(out, paste0("./AIGKC/output/observer/eag_total_comp_boot_", i, ".RDS"))
  
}
# wag
for (i in yrs){
  # random number seed
  set.seed(1624)
  
  obs_meas %>%
    filter(subdistrict == "WAG",
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
        left_join(obs_p_tot %>% filter(subdistrict == "WAG"), by = c("crab_year", "bin")) %>% ungroup %>%
        summarise(n = sum(obs_p_tot * (1 - obs_p_tot)) / sum((obs_p_tot - p)^2)) %>% pull(n)
      
    })) -> out
  
  saveRDS(out, paste0("./AIGKC/output/observer/wag_total_comp_boot_", i, ".RDS"))
  
}

# lapply(list.files("./AIGKC/output/observer", pattern = "eag_total_comp_boot", full.names = T), readRDS) %>% 
#   bind_rows %>%
#   group_by(crab_year) %>%
#   summarise(neff = mean(n)) -> tmp

# retained composition boot ----

yrs <- 1985:2023
# eag
for (i in yrs){
  # random number seed
  set.seed(1624)
  
  dock %>%
    filter(subdistrict == "EAG",
           crab_year == i) %>%
    mutate(delivery = paste0(adfg, sample_date),
           n_deliveries =  length(unique(delivery))) %>%
    # bootstrap pots
    expand_grid(., del_boot = 1:500) %>%
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
  set.seed(1624)
  
  dock %>%
    filter(subdistrict == "WAG",
           crab_year == i) %>%
    mutate(delivery = paste0(adfg, sample_date),
           n_deliveries =  length(unique(delivery))) %>%
    # bootstrap pots
    expand_grid(., del_boot = 1:500) %>%
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

# table output

## eag
lapply(list.files("./AIGKC/output/observer", pattern = "eag_retained_comp_boot", full.names = T), readRDS) %>%
  bind_rows %>%
  dplyr::select(-data) %>%
  mutate(subdistrict = "EAG") %>% 
  left_join(dock %>% 
              filter(subdistrict == "EAG") %>%
              group_by(crab_year) %>%
              summarise(nmeas = sum(numcrab))) -> eag_ret

## wag
lapply(list.files("./AIGKC/output/observer", pattern = "wag_retained_comp_boot", full.names = T), readRDS) %>%
  bind_rows %>%
  dplyr::select(-data) %>%
  mutate(subdistrict = "WAG") %>% 
  left_join(dock %>% 
              filter(subdistrict == "WAG") %>%
              group_by(crab_year) %>%
              summarise(nmeas = sum(numcrab))) -> wag_ret

eag_ret %>%
  group_by(crab_year) %>%
  summarise(eag_nmeas = mean(nmeas),
            eag_neff_min = min(n),
            eag_neff_mean = mean(n),
            eag_neff_max = max(n)) %>%
  mutate(gap = NA) %>%
  left_join(wag_ret %>%
              group_by(crab_year) %>%
              summarise(wag_nmeas = mean(nmeas),
                        wag_neff_min = min(n),
                        wag_neff_mean = mean(n),
                        wag_neff_max = max(n)) ) %>%
  write_csv("./AIGKC/output/observer/retained_neff_boot_table.csv")
  
  



## eag
lapply(list.files("./AIGKC/output/observer", pattern = "eag_total_comp_boot", full.names = T), readRDS) %>%
  bind_rows %>%
  dplyr::select(-data) %>%
  mutate(subdistrict = "EAG") %>% 
  left_join(obs_meas %>% 
              filter(subdistrict == "EAG",
                     sex == 1, size > 100) %>%
              group_by(crab_year) %>%
              summarise(nmeas = n())) -> eag_tot


## wag
lapply(list.files("./AIGKC/output/observer", pattern = "wag_total_comp_boot", full.names = T), readRDS) %>%
  bind_rows %>%
  dplyr::select(-data) %>%
  mutate(subdistrict = "WAG") %>% 
  left_join(obs_meas %>% 
              filter(subdistrict == "WAG",
                     sex == 1, size > 100) %>%
              group_by(crab_year) %>%
              summarise(nmeas = n())) -> wag_tot


eag_tot %>%
  group_by(crab_year) %>%
  summarise(eag_nmeas = mean(nmeas),
            eag_neff_min = min(n),
            eag_neff_mean = mean(n),
            eag_neff_max = max(n)) %>%
  mutate(gap = NA) %>%
  left_join(wag_tot %>%
              group_by(crab_year) %>%
              summarise(wag_nmeas = mean(nmeas),
                        wag_neff_min = min(n),
                        wag_neff_mean = mean(n),
                        wag_neff_max = max(n)) ) %>%
  write_csv("./AIGKC/output/observer/total_neff_boot_table.csv")




