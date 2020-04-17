# notes ----
## Compute OFL for each model scenario
## Tyler Jackson
## tyler.jackson@alaska.gov
## last updated: 2020/4/7

# load ----
library(tidyverse)

## global option
YEAR <- 2020
## model scenarios
subdir <- c("2020a", "2020b", "2020c", "2020d", "2020e", "2020f")

# OFL table ----

lapply(subdir, function(subdir){
  
  ## read model output
  ### load file
  output <- read.table(paste0("./PIGKC/model/", YEAR, "/", subdir, "/rwout.rep"), fill = T, sep = "\t")
  ### survey ests (sd is cv)
  tibble(yrs = na.omit(as.numeric(str_split(output[2,], pattern = " ", simplify = T))),
         survey_est = na.omit(as.numeric(str_split(output[4,], pattern = " ", simplify = T))),
         survey_sd = na.omit(as.numeric(str_split(output[6,], pattern = " ", simplify = T)))) -> tmp1
  ### model est
  tibble(yrs = na.omit(as.numeric(str_split(output[8,], pattern = " ", simplify = T))),
         fit = na.omit(as.numeric(str_split(output[12,], pattern = " ", simplify = T))),
         l95 = na.omit(as.numeric(str_split(output[10,], pattern = " ", simplify = T))),
         u95 = na.omit(as.numeric(str_split(output[14,], pattern = " ", simplify = T))),
         l90 = na.omit(as.numeric(str_split(output[16,], pattern = " ", simplify = T))),
         u90 = na.omit(as.numeric(str_split(output[18,], pattern = " ", simplify = T))),
         fit_sd = na.omit(as.numeric(str_split(output[20,], pattern = " ", simplify = T))),
         fit_sd_sd = na.omit(as.numeric(str_split(output[22,], pattern = " ", simplify = T)))) -> tmp2
  ### join temporary objects results
  full_join(tmp1, tmp2, by = "yrs") %>%
    arrange(yrs) %>%
    filter(yrs <= 2016) -> model_est
  
  
  ## compute OFL
  tibble(model = subdir,
         bmsy = mean(model_est$fit),
         yr_est = filter(model_est, yrs == 2016)$fit,
         proj_b = yr_est * exp(-0.625 * 0.18),
         b_bmsy = proj_b / bmsy,
         f_ofl = ifelse(b_bmsy > 1, 0.18, (0.18 * (b_bmsy - 0.1)) / (0.90)),
         OFL_t = (1 - exp(-f_ofl)) * proj_b,
         OFL_lbs = OFL_t * 1000 * 2.20462262)
  
}) %>%
  do.call("rbind", .) -> ofl_table

# svae to output folder
write_csv(ofl_table, "./PIGKC/output/2020_ofl_table.csv")


