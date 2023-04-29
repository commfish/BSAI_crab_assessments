# 4-28-23 from Tyler Jackson
# originall size composition for bbrkc 

# section below for size composition without retows for May 2023 models.

#read.csv("./NMFS_survey/data/rkc_specimen.csv", skip = 5) %>%
#  rename_all(tolower) -> specimen
# specimen data (haul data dump)
read.csv("./BBRKC/data/2022/survey/rkc_specimen.csv", skip = 5) %>%
  rename_all(tolower) -> specimen


# effective sample size
specimen %>%
  filter(haul_type == 3, !is.na(sex)) %>%
  group_by(akfin_survey_year) %>%
  summarise(n = n()) %>%
  mutate(Nsamp = min(0.5*n, 200)) %>%
  print(n = 100)

# akfin abundance report ----

read.csv("EBSCrab_Abundance_Biomass.csv", skip = 7) %>%
  rename_all(tolower) -> est_1mm

# male index ----

est_1mm %>%
  filter(size_class_mm > 65) %>%
  mutate(se = (biomass_mt_cv * biomass_mt)^2) %>%
  group_by(survey_year) %>%
  
  summarise(cv = sqrt(sum(se))) %>%
  print(n = 1000)

est_1mm %>%
  # filter(sex == "MALE", size_class_mm >= 65) %>%
  # 
  # 
  # group_by(survey_year) %>%
  # summarise(B = sum(biomass_mt),
  #           cv = mean(biomass_mt_cv)) %>%
  # print(n = 1000)

  # filter only for animals in the model
  filter(size_class_mm >= 65) %>%
  # create length comp bin
  mutate(cl_bin = ifelse(size_class_mm > 160, 160, floor(size_class_mm / 5) * 5),
         cl_bin = ifelse(sex == "FEMALE" & cl_bin >= 140, 140, cl_bin)) %>%
  # get abundance estimates by bin
  group_by(survey_year, sex, cl_bin) %>%
  summarise(N_hat = sum(abundance)) %>%
  # get total abundance estimate of all
  group_by(survey_year) %>%
  mutate(total = sum(N_hat)) %>% ungroup %>%
  # compute proportion
  mutate(p = round(N_hat / total, 4)) -> size_comp

# males
size_comp %>% 
  filter(sex == "MALE") %>%
  mutate(season = 1, fleet = 5, sex = 1, type = 0.00, shell = 0, maturity = 0, Nsamp = 200, p =  sprintf('%.4f', p)) %>%
  dplyr::select(survey_year, season, fleet, sex, type, shell, maturity, Nsamp, cl_bin, p) %>%
  pivot_wider(names_from = cl_bin, values_from = p) %>%
  rbind(c("#year", "season", "fleet", "sex", "type", "shell", "maturity", "nsamp", "datavector", rep(NA, 20)), .) %>%
  write_delim("male_race_size_comp.txt", delim = "\t", col_names = F, na = "")

# females
size_comp %>% 
  filter(sex == "FEMALE") %>%
  mutate(season = 1, fleet = 5, sex = 1, type = 0.00, shell = 0, maturity = 0, Nsamp = 0, p =  sprintf('%.4f', p)) %>%
  dplyr::select(survey_year, season, fleet, sex, type, shell, maturity, Nsamp, cl_bin, p) %>%
  pivot_wider(names_from = cl_bin, values_from = p) %>%
  rbind(c("#year", "season", "fleet", "sex", "type", "shell", "maturity", "nsamp", "datavector", rep(NA, 20)), .) %>%
  write_delim("female_race_size_comp.txt", delim = "\t", col_names = F, na = "")
