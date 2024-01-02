# notes ----
# cooperative survey cpue standardization (based on siddeek's 2023 script)
# tyler jackson
# 4/17/2023

# load ----

source("./AIGKC/code/aigkc_functions.R")

# data ---- 

## siddeeks co-op survey file 
coop_raw <- read_csv("./AIGKC/data/coop_survey/AIGKC_SurveyData_2015_19_21_22May2023.csv")

LegalCatchSurvey15_19_21_22<- read_csv("./AIGKC/data/coop_survey/AIGKCSurvey15_19_21_22Aggregated_Combined_LegalMaleSumCatchbyPotIDWorkMay2023.csv")

# data mgmt ----

coop_raw %>%
  rename_all(tolower) %>%
  # retain only complete entries
  transmute(eag1wag2, year, vesselid, captainid, stringid, meshid, subsamplerate, 
            sex, size, potid, potcatch, soakdays, midlat, midlong, middepth) %>%
  filter(complete.cases(.)) %>%
  # aggregate catch by size
  group_by(eag1wag2, year, vesselid, captainid, stringid, meshid, sex, size, potid, soakdays, midlat, midlong, middepth) %>%
  summarise(potcatch = sum(potcatch, na.rm = T)) %>% ungroup() %>%
  # set female catches to 0 (line 76 Siddeek)
  mutate(potcatch = ifelse(sex == 2, 0, potcatch)) %>%
  # filter for legal males
  filter(size > 135)  %>% 
  # summarise catch
  group_by(eag1wag2, year, vesselid, captainid, stringid, meshid, potid, soakdays, midlat, midlong, middepth) %>%
  summarise(potcatch = sum(potcatch, na.rm = T)) %>% ungroup() %>%
  # create columns vesstring and vesstringpot
  mutate(vesstring = paste0(vesselid, stringid),
         vesstringpot = paste0(vesselid, stringid, potid)) %>%
  # add block
  rename(lat = midlat, lon = midlong) %>%
  f_add_blocks() -> coop
  
  

# eag ----

coop %>%
  # filter for: eag; not small mesh; 
  filter(eag1wag2 == 1,
         meshid != 2) %>%
  # filter for quantiles of soak time and depth
  filter(soakdays >= quantile(soakdays, 0.05),
         soakdays <= quantile(soakdays, 0.95),
         middepth >= quantile(middepth, 0.01),
         middepth <= quantile(middepth, 0.99)) %>% 
  # figure why block is not being asined correctly some oher time
  mutate(block = ifelse(is.na(block), 4, block)) %>%
  # set several columns as factors
  mutate_at(vars(year, vesselid, captainid, vesstring, vesstringpot, block), factor) -> eag_coop
  
# fit re model for eag ----

# run 2 from siddek's script
eag_lme2 <- glmer(potcatch ~ year + ns(middepth, df = 9) + ns(soakdays, df = 3) + captainid + (1 | block / vesstring), 
                  family = negative.binomial(3.01), control = glmerControl(optimizer = "bobyqa", check.scaleX = c("silent.rescale"), tolPwrss = 2e-5), 
                  data = eag_coop)

# extract year coefficients
fix_effects <- fixef(eag_lme2)
eag_lme2_sum <- summary(eag_lme2) 
eag_lme2_sum$coefficients



loc <- grep("year", names(fixef(eag_lme2)))
yrs <- unique(eag_coop$year)

rel <- as.numeric(c(0, fix_effects[loc]))
std_cpue <- exp(rel - mean())

## Extract model components
relative <- as.numeric(c(0, model$coefficients[where,1]))  # shave off names
std_cpue <- exp(relative - mean(relative))
if(class(model)=="summary.lm")
  
V <- eag_lme2_sum$cov.unscaled[loc, loc] * model$sigma^2

eag_lme2_sum$vcov


## Compute confidence limits
Q <- matrix(-1 / n, nrow = n, ncol = n - 1)
Q[-1,] <- Q[-1,] + diag(rep(1, n - 1))
V0 <- (Q %*% V) %*% t(Q)
SE <- sqrt(diag(V0))
Upper <- exp(log(std_cpue) + 2 * SE)
Lower <- exp(log(std_cpue) - 2 * SE)

out <- tibble(year = years, 
              index = std_cpue, 
              se = SE, 
              l95 = Lower,
              u95 = Upper)



where 










