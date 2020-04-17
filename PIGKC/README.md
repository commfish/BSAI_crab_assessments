# Pribilof Islands Golden King Crab

This directory includes materials related to the Pribilof Islands Golden King Crab assessment.

## Authors (2020):
* Ben Daly, ben.daly@alaska.gov
* Tyler Jackson, tyler.jackson@alaska.gov (code author)

## Layout: 
* **code** - R scripts used to 1) compile survey specimen data, 2) estimates survey biomass and CV, and 3) run and summary random effect model outputs.
* **data** - Haul, strata, and specimen data from the NMFS EBS slope survey
* **model** - Contains re-model executable file, raw inputs and outputs. Delineated by assessment year.
* **output** - Tables generated from input data or model results. Delineated by assessment year.
* **figures** - Figures generated from input data or model resilts. Delineated by assessment year.  

## Tier 4 assessment basic workflow:
1. Run nmfs_slope_biomass_est.R script to obtain survey biomass estimates.
2. Run re_model_run_summary.R to run random effects model (by J. Ianelli) and evaluate model output (in R).
3. Run compute_ofl.R to compute reference points

## 2020 model scenarios
Model scenarios for the 2020 tier 4 assessment involve different methods of computing biomass inputs to the random effects models. They include:
* **2020a** - mature male biomass (MMB) and variance in MMB 2008-2016 computed among strata within subareas 2-4, summed within subareas, and then across subareas
* **2020b** - MMB and variance in MMB 2008-2016 computed among strata within the survey area bounded by the Pribilof Islands district and summed across strata
* **2020c** - MMB density and variance in MMB 2008-2016 density computed among strata within subareas 2-4 and summed across strata
* **2020d** - mature male biomass (MMB) and variance in MMB 2008-2016 computed among strata within subareas 2-4, summed within subareas, and then across subareas. MMB in 2002 and 2004 was computed using the mean ratio of MMB:total biomass from 2008-2016 
* **2020e** - MMB and variance in MMB 2008-2016 computed among strata within the survey area bounded by the Pribilof Islands district and summed across strata. MMB in 2002 and 2004 was computed using the mean ratio of MMB:total biomass from 2008-2016
* **2020f** - MMB density and variance in MMB 2008-2016 density computed among strata within subareas 2-4 and summed across strata. MMB in 2002 and 2004 was computed using the mean ratio of MMB:total biomass from 2008-2016

