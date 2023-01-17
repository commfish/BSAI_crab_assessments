# notes ----
## pigkc groundfish bycatch
## tyler jackson
## 11/7/2022

# load ----

library(tidyverse)

# data ----

## paths meant for sourcing in safe document
gf19912009 <- read.csv("../../data/Crab Bycatch Estimates 1991-2009.csv")
gf2009present <- read.csv("../../data/pig_crab_bycatch_2009_present.csv", skip = 7)


# estimate ----

# groundfish bycatch 1991/92-2008/09
gf19912009 %>%
  rename_at(1, ~"year") %>%
  filter(Reporting.Area.Code %in% c(521, 513, 517, 523)) %>%
  group_by(year, Gear) %>%
  summarise(est = round(sum(Estimate.Wt..Sum. / 1000), 2)) %>%
  pivot_wider(names_from = Gear, values_from = est) %>%
  rename_all(~c("year", "fixed", "trawl")) %>%
  mutate(total = round(fixed + trawl, 2),
         total_mortality = round((fixed * 0.5) + (trawl * 0.8), 2)) %>%
  
  bind_rows(
    
  # groundfish bycatch 2009 - present
  gf2009present %>%
  mutate(type = case_when(Agency.Gear.Code %in% c("HAL", "POT") ~ "fixed",
                          Agency.Gear.Code %in% c("NPT", "PTR") ~ "trawl")) %>%
  group_by(Calendar.Year, type) %>%
  summarise(est = round(sum(Estimated.Crab.Weight..kg. / 1000), 2)) %>%
  ungroup %>%
  rename(year = Calendar.Year) %>%
  pivot_wider(names_from = type, values_from = est) %>%
  mutate(year = as.character(year),
         total = fixed + trawl,
         total_mortality = round((fixed * 0.5) + (trawl * 0.8), 2))
  
  ) %>% as.data.frame() -> gf_ts

# compute averages and tack on last row
gf_ts %>%
  summarise_if(is.numeric, function(x){round(mean(x), 2)}) %>%
  mutate(year = "Average") %>%
  bind_rows(gf_ts, .) -> gf_bycatch
