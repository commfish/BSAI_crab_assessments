library(gmacsr)

gmacs_get_hs_in <- function(allout) {
  
  gmacs_get_derived_quantity_summary(allout) %>%
    transmute(model, year, mmb = ssb, mma = ssa, rec = recruit_male) %>%
     bind_rows(gmacs_get_ref_points(allout) %>%
                transmute(model, year = max(gmacs_get_derived_quantity_summary(allout)$year) + 1, mmb)) %>%
    full_join(gmacs_get_n_matrix(allout) %>%
                filter(size >= 136) %>%
                group_by(model, year) %>%
                summarise(lma = sum(total))) %>%
    arrange(model, year) %>%
    rename_all(~c("model", "crab_year", "mmb_t", "mma_thou", "rec_thou", "lma_thou"))
  
  
}




gmacs_get_hs_in(list(gmacs_read_allout("AIGKC/models/2025/may/EAG/23.1c_complete/Gmacsall.out", "23.1c"))) %>%
  mutate(fishery = "EAG") %>%
  write_csv("AIGKC/output/models/2025/may/aigkc_hs_in_eag_23_1c.csv")
  
  
gmacs_get_hs_in(list(gmacs_read_allout("AIGKC/models/2025/may/WAG/23.1c_complete/Gmacsall.out", "23.1c"))) %>%
  mutate(fishery = "WAG") %>% 
  write_csv("AIGKC/output/models/2025/may/aigkc_hs_in_wag_23_1c.csv")
  
  
