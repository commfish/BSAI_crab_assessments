# Pribilof Islands Golden King Crab

This directory includes materials related to the Pribilof Islands Golden King Crab assessment.

## Authors (2020):
* Ben Daly, ben.daly@alaska.gov
* Tyler Jackson, tyler.jackson@alaska.gov

## Layout: 
* **code** - R scripts used to 1) compile survey specimen data, 2) estimates survey biomass and CV, and 3) run and summary random effect model outputs.
* **data** - Haul, strata, and specimen data from the NMFS EBS slope survey
* **output** - Tables generated from input data or model results. Delineated by assessment year.
* **figures** - Figures generated from input data or model resilts. Delineated by assessment year.  

## Tier 4 assessment basic workflow:
1. Run nmfs_slope_biomass_est.R script to obtain survey biomass estimates.
2. Run re_model_run_summary.R to run random effects model (by J. Ianelli) and evaluate model output (in R).
