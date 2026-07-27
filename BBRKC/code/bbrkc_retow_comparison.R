# notes ----

# retow biomass and size composition data difference 
# tyler jackson
# 7 27 2026

# load ---- 

library(crabpack)
library(tidyverse)

# data ---

## pull specimen data
specimen_data <- crabpack::get_specimen_data(species = "RKC",
                                             region = "EBS",
                                             district = "BB",
                                             channel = "API")


# estimates with retow
survey_est_female <- calc_bioabund(crab_data = specimen_data,
                                   species = "RKC",
                                   region = "EBS",
                                   district = "BB",
                                   sex = "female",
                                   bin_1mm = T) %>% janitor::clean_names() 

# estimates without retow
survey_est_female_wort  <- calc_bioabund(crab_data = specimen_data,
                                   species = "RKC",
                                   region = "EBS",
                                   district = "BB",
                                   sex = "female",
                                   replace_retow = F,
                                   bin_1mm = T) %>% janitor::clean_names() 

# plots ----

# cutline mature female abundance timeseries
survey_est_female %>%
  transmute(year, which = "Re-tow", size = size_1mm, abundance) %>%
  bind_rows(survey_est_female_wort %>%
              transmute(year, which = "w/o Re-tow", size = size_1mm, abundance) ) %>%
  filter(size >= 90) %>%
  group_by(year, which) %>%
  summarise(mfa = sum(abundance)) %>%
  ggplot()+
  geom_point(aes(x = year, y = mfa / 1e6, color = which))+
  geom_line(aes(x = year, y = mfa / 1e6, color = which))+
  scale_x_continuous(breaks = gmacsr::yraxis$breaks, labels = gmacsr::yraxis$labels)+
  labs(x = NULL, y = "Abundance (Millions)", color = NULL)

# cutline mature female biomass timeseries
survey_est_female %>%
  transmute(year, which = "Re-tow", size = size_1mm, biomass_mt) %>%
  bind_rows(survey_est_female_wort %>%
              transmute(year, which = "w/o Re-tow", size = size_1mm, biomass_mt) ) %>%
  filter(size >= 90) %>%
  group_by(year, which) %>%
  summarise(mfb = sum(biomass_mt)) %>%
  ggplot()+
  geom_point(aes(x = year, y = mfb, color = which))+
  geom_line(aes(x = year, y = mfb, color = which))+
  scale_x_continuous(breaks = gmacsr::yraxis$breaks, labels = gmacsr::yraxis$labels)+
  scale_y_continuous(labels = scales::comma)+
  labs(x = NULL, y = "Biomass (t)", color = NULL)


# female size composition
survey_est_female %>%
  transmute(year, which = "Re-tow", size = size_1mm, abundance) %>%
  bind_rows(survey_est_female_wort %>%
              transmute(year, which = "w/o Re-tow", size = size_1mm, abundance) ) %>%
  # 5 mm bins
  mutate(bin = floor(size / 5) * 5) %>%
  group_by(year, which) %>%
  mutate(abund = sum(abundance)) %>% ungroup %>%
  group_by(year, which, bin) %>%
  summarise(prop = sum(abundance) / mean(abund)) %>%
  ggplot()+
  geom_bar(aes(x = bin, y = prop, fill = which), stat = "identity", position = "identity", alpha = 0.4, width = 5)+
  facet_wrap(~year, ncol = 4)+
  geom_text_npc(aes(npcx = "left", npcy = 0.9, label = year),
                check_overlap = T, size = 3)+
  labs(x = "Carapce Length (mm)", y = NULL, fill = NULL)+
  theme(panel.spacing.x = unit(0, "lines"),
        panel.spacing.y = unit(0, "lines"),
        axis.line.x = element_line(color = "grey70", linewidth = 0.2),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 8),
        plot.title = element_text(hjust = 0.5),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        panel.background = element_blank()) 



