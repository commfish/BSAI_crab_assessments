#Northern Unstratified to BB ratio
# 8-22-2025
# author: Erin Fedewa/ Shannon Hennnessay / 

# load ------------
library(crabpack)
library(tidyverse)
library(patchwork)

cur_yr <- 2025 # update annually
folder <- "bbrkc_25f" # update annually
.FIGS     = c(paste0("./BBRKC/", folder, "/doc/figures/"))
# y axis minor ticks
yr_axis = tickr(tibble(yr = 1975:2050), yr, 5)


## Pull specimen data
specimen_data <- crabpack::get_specimen_data(species = "RKC",
                                             region = "EBS",
                                             channel = "API")


# males -------------
# Calculate total abundance/biomass of BBRKC and Northern district RKC by category
bio <- calc_bioabund(crab_data = specimen_data,
                              species = "RKC",
                              region = "EBS",
                              crab_category = "all_categories",
                              years = c(1985:2025)) %>%
	     filter(CATEGORY %in% c("mature_male", "legal_male"))

#Plot BB
bio %>%
  filter(DISTRICT == "BB") %>%
  ggplot(aes(x = YEAR, y = BIOMASS_MT, linetype = CATEGORY, shape = CATEGORY)) +
  geom_point() +
  geom_line()+
  #labs(y = "Bristol Bay Biomass (mt)", x = "") +
  scale_linetype_manual(values = c(2, 1), labels = c("Legal Males", "Mature males"))+
  scale_shape_manual(values = c(1, 16), labels = c("Legal Males", "Mature males"))+
  scale_x_continuous(labels = yr_axis$labels, breaks = yr_axis$breaks)+
  scale_y_continuous(labels = scales::comma)+
  labs(x = NULL, y = "Bristol Bay \n Biomass (mt)", linetype = NULL, shape = NULL)+
  theme(legend.justification = c(1,1), legend.position = c(1,1)) -> bb_bio_plot
  #theme_bw() 


#Plot Northern district
bio %>%
  filter(DISTRICT == "NORTH") %>%
  ggplot(aes(x = YEAR, y = BIOMASS_MT, linetype = CATEGORY, shape = CATEGORY)) +
  geom_point() +
  geom_line()+
  #labs(y = "Northern Unstratified Biomass (mt)", x = "") +
  scale_linetype_manual(values = c(2, 1), labels = c("Legal Males", "Mature males"))+
  scale_shape_manual(values = c(1, 16), labels = c("Legal Males", "Mature males"))+
  scale_x_continuous(labels = yr_axis$labels, breaks = yr_axis$breaks)+
  scale_y_continuous(labels = scales::comma)+
  labs(x = NULL, y = "Northern Unstratified \n Biomass (t)", linetype = NULL, shape = NULL)+
  theme(legend.justification = c(0,1), legend.position = c(0,1)) -> northern_bio_plot
  #theme_bw() -> northern_bio_plot


#join and calculate ratio
bio %>% 
  filter(DISTRICT %in% c("BB", "NORTH")) %>%
  select(YEAR, DISTRICT, CATEGORY, BIOMASS_MT) %>%
  pivot_wider(names_from = DISTRICT, values_from = BIOMASS_MT) %>%
  mutate(ratio = NORTH/BB) -> rkc_ratio2

#plot
rkc_ratio2 %>%
  ggplot(aes(YEAR, ratio, linetype = CATEGORY, shape = CATEGORY)) +
  geom_point() +
  geom_line() +
  geom_hline(aes(yintercept=mean(ratio))) +
  #labs(y = "Proportion Total Biomass in Northern Unstratified District", x = "") +
  scale_linetype_manual(values = c(2, 1), labels = c("Legal Males", "Mature males"))+
  scale_shape_manual(values = c(1, 16), labels = c("Legal Males", "Mature males"))+
  scale_x_continuous(labels = yr_axis$labels, breaks = yr_axis$breaks)+
  scale_y_continuous(limit = c(0, 0.25))+
  labs(x = NULL, y = "Proportion Total Biomass in \n North Unstratified District", linetype = NULL, shape = NULL)+
  theme(legend.justification = c(0,1), legend.position = c(0,1)) -> ratio_plot
  #theme_bw() -> ratio_plot

#combine plots 
bb_bio_plot / northern_bio_plot / ratio_plot
ggsave(paste0(.FIGS, "figure_35_alternate.png"), plot = bb_bio_plot / northern_bio_plot / ratio_plot, height = 8, width = 5, units = "in")


# females ----------------
# Calculate total abundance/biomass of BBRKC and Northern district RKC by category
bio <- calc_bioabund(crab_data = specimen_data,
                     species = "RKC",
                     region = "EBS",
                     crab_category = "all_categories",
                     years = c(1985:2025)) %>%
  filter(CATEGORY %in% c("immature_female", "mature_female"))

#Plot BB  - abundance
bio %>%
  filter(DISTRICT == "BB") %>%
  ggplot(aes(x = YEAR, y = ABUNDANCE/1000000, linetype = CATEGORY, shape = CATEGORY)) +
  geom_point() +
  geom_line()+
  #labs(y = "Bristol Bay Biomass (mt)", x = "") +
  scale_linetype_manual(values = c(2, 1), labels = c("Immature Females", "Mature Females"))+
  scale_shape_manual(values = c(1, 16), labels = c("Immature Females", "Mature Females"))+
  scale_x_continuous(labels = yr_axis$labels, breaks = yr_axis$breaks)+
  scale_y_continuous(limit = c(0,40), labels = scales::comma)+
  labs(x = NULL, y = "Bristol Bay Females \n (millions of crab)", linetype = NULL, shape = NULL)+
  theme(legend.justification = c(0,1), legend.position = c(0,1)) -> bb_bio_plot

#Plot Northern district
bio %>%
  filter(DISTRICT == "NORTH") %>%
  ggplot(aes(x = YEAR, y = ABUNDANCE/1000000, linetype = CATEGORY, shape = CATEGORY)) +
  geom_point() +
  geom_line()+
  #labs(y = "Northern Unstratified Biomass (mt)", x = "") +
  scale_linetype_manual(values = c(2, 1), labels = c("Immature Females", "Mature Females"))+
  scale_shape_manual(values = c(1, 16), labels = c("Immature Females", "Mature Females"))+
  scale_x_continuous(labels = yr_axis$labels, breaks = yr_axis$breaks)+
  scale_y_continuous(limit = c(0, 10), labels = scales::comma)+
  labs(x = NULL, y = "Northern Unstratified \n (millions of crab)", linetype = NULL, shape = NULL)+
  theme(legend.justification = c(0,1), legend.position = c(0,1)) -> northern_bio_plot
#theme_bw() -> northern_bio_plot


#join and calculate ratio
bio %>% 
  filter(DISTRICT %in% c("BB", "NORTH")) %>%
  select(YEAR, DISTRICT, CATEGORY, ABUNDANCE) %>%
  pivot_wider(names_from = DISTRICT, values_from = ABUNDANCE) %>%
  mutate(ratio = NORTH/BB) -> rkc_ratio2

#plot
rkc_ratio2 %>%
  ggplot(aes(YEAR, ratio, linetype = CATEGORY, shape = CATEGORY)) +
  geom_point() +
  geom_line() +
  geom_hline(aes(yintercept=mean(ratio))) +
  #labs(y = "Proportion Total Biomass in Northern Unstratified District", x = "") +
  scale_linetype_manual(values = c(2, 1), labels = c("Immature Females", "Mature Females"))+
  scale_shape_manual(values = c(1, 16), labels = c("Immature Females", "Mature Females"))+
  scale_x_continuous(labels = yr_axis$labels, breaks = yr_axis$breaks)+
  scale_y_continuous(limit = c(0, 0.5))+
  labs(x = NULL, y = "Proportion Total Abundance in \n North Unstratified District Females", linetype = NULL, shape = NULL)+
  theme(legend.justification = c(0,1), legend.position = c(0,1)) -> ratio_plot
#theme_bw() -> ratio_plot

#combine plots 
bb_bio_plot / northern_bio_plot / ratio_plot
ggsave(paste0(.FIGS, "figure_35_FEMALE_alternate.png"), plot = bb_bio_plot / northern_bio_plot / ratio_plot, height = 8, width = 5, units = "in")

