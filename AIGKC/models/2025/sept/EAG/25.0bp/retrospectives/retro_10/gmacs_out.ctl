## GMACS Version 2.20.16; ** AEP & WTS **; Compiled 2024-07-23

# Block structure
# Number of block groups
2
# Block structure (number of blocks per block group)
2 # block group 1
1 # block group 2
# The blocks
#Block 1: 
2005 2010 # block_group_1_block_1
2018 2023 # block_group_1_block_2
#Block 2: 
2005 2023 # block_group_2_block_1

##  ------------------------------------------------------------------------------------ ##
##  OTHER  CONTROLS
##  ------------------------------------------------------------------------------------ ##
1981 # First year of recruitment estimation
2013 # Last year of recruitment estimation
   0 # Consider terminal molting (0 = off, 1 = on). If on, the calc_stock_recruitment_relationship() isn't called in the procedure
   1 # Phase for recruitment estimation
  -2 # Phase for recruitment sex-ratio estimation
0.50 # Initial value for recruitment sex-ratio
   3 # Initial conditions (0 = Unfished, 1 = Steady-state fished, 2 = Free parameters, 3 = Free parameters (revised))
   8 # ILambda (proportion of mature male biomass for SPR reference points)
1.00 # Lambda (proportion of mature male biomass for SPR reference points)
   0 # Stock-Recruit-Relationship (0 = none, 1 = Beverton-Holt)
   1 # Use years specified to computed average sex ratio in the calculation of average recruitment for reference points (0 = off -i.e. Rec based on End year, 1 = on)
 200 # Years to compute equilibria
   5 # Phase for deviation parameters
1981 # First year of bias-correction
1981 # First full bias-correction
2023 # Last full bias-correction
2023 # Last year of bias-correction

# Expecting 24 theta parameters
# Core parameters
## Initial: Initial value for the parameter (must lie between lower and upper)
## Lower & Upper: Range for the parameter
## Phase: Set equal to a negative number not to estimate
## Prior type:
## 0: Uniform   - parameters are the range of the uniform prior
## 1: Normal    - parameters are the mean and sd
## 2: Lognormal - parameters are the mean and sd of the log
## 3: Beta      - parameters are the two beta parameters [see dbeta]
## 4: Gamma     - parameters are the two gamma parameters [see dgamma]
# Initial_value    Lower_bound    Upper_bound Phase Prior_type        Prior_1        Prior_2
     7.79033949   -10.00000000    20.00000000    -1          0   -10.00000000    20.00000000 # Log(R0)
     9.10037359   -10.00000000    20.00000000     3          1   -10.00000000    20.00000000 # Log(Rinitial)
     8.04408906   -10.00000000    20.00000000     1          0   -10.00000000    20.00000000 # Log(Rbar)
   110.00000000   103.00000000   165.00000000    -2          1    72.50000000     7.25000000 # Recruitment_ra-males
     0.49026315     0.00100000    20.00000000     3          0     0.10000000     5.00000000 # Recruitment_rb-males
    -0.69314718   -10.00000000     0.75000000    -1          0   -10.00000000     0.75000000 # log(SigmaR)
     0.73000000     0.20000000     1.00000000    -2          3     3.00000000     2.00000000 # Steepness
     0.00100000     0.00000000     1.00000000    -3          3     1.01000000     1.01000000 # Rho
     0.08015802   -10.00000000     4.00000000     4          0    10.00000000    20.00000000 # Scaled_logN_for_male_mature_mature_newshell_class_1
     0.12353061   -10.00000000     4.00000000     4          0    10.00000000    20.00000000 # Scaled_logN_for_male_mature_mature_newshell_class_2
     0.15568251   -10.00000000     4.00000000     4          0    10.00000000    20.00000000 # Scaled_logN_for_male_mature_mature_newshell_class_3
     0.18273786   -10.00000000     4.00000000     4          0    10.00000000    20.00000000 # Scaled_logN_for_male_mature_mature_newshell_class_4
     0.20592347   -10.00000000     4.00000000     4          0    10.00000000    20.00000000 # Scaled_logN_for_male_mature_mature_newshell_class_5
     0.21891330   -10.00000000     4.00000000     4          0    10.00000000    20.00000000 # Scaled_logN_for_male_mature_mature_newshell_class_6
     0.21091155   -10.00000000     4.00000000     4          0    10.00000000    20.00000000 # Scaled_logN_for_male_mature_mature_newshell_class_7
     0.19332725   -10.00000000     4.00000000     4          0    10.00000000    20.00000000 # Scaled_logN_for_male_mature_mature_newshell_class_9
     0.13214703   -10.00000000     4.00000000     4          0    10.00000000    20.00000000 # Scaled_logN_for_male_mature_mature_newshell_class_10
     0.07739240   -10.00000000     4.00000000     4          0    10.00000000    20.00000000 # Scaled_logN_for_male_mature_mature_newshell_class_11
     0.05320630   -10.00000000     4.00000000     4          0    10.00000000    20.00000000 # Scaled_logN_for_male_mature_mature_newshell_class_12
    -0.07515285   -10.00000000     4.00000000     4          0    10.00000000    20.00000000 # Scaled_logN_for_male_mature_mature_newshell_class_13
    -0.24451033   -10.00000000     4.00000000     4          0    10.00000000    20.00000000 # Scaled_logN_for_male_mature_mature_newshell_class_14
    -0.33118983   -10.00000000     4.00000000     4          0    10.00000000    20.00000000 # Scaled_logN_for_male_mature_mature_newshell_class_15
    -0.57537391   -10.00000000     4.00000000     4          0    10.00000000    20.00000000 # Scaled_logN_for_male_mature_mature_newshell_class_16
    -0.56634700   -10.00000000     4.00000000     4          0    10.00000000    20.00000000 # Scaled_logN_for_male_mature_mature_newshell_class_17

 ##Allometry
# weight-at-length input  method  (1 = allometry  [w_l = a*l^b],  2 = vector by sex; 3= matrix by sex)
3
     0.58151571     0.67932817     0.78803235     0.90827831     1.04072426     1.18603629     1.34488818     1.51796111     1.70594354     1.90953096     2.12942573     2.36633693     2.62098018     2.89407749     3.18635714     3.49855352     3.99365758
     0.58151571     0.67932817     0.78803235     0.90827831     1.04072426     1.18603629     1.34488818     1.51796111     1.70594354     1.90953096     2.12942573     2.36633693     2.62098018     2.89407749     3.18635714     3.49855352     3.99365758
     0.58151571     0.67932817     0.78803235     0.90827831     1.04072426     1.18603629     1.34488818     1.51796111     1.70594354     1.90953096     2.12942573     2.36633693     2.62098018     2.89407749     3.18635714     3.49855352     3.99365758
     0.58151571     0.67932817     0.78803235     0.90827831     1.04072426     1.18603629     1.34488818     1.51796111     1.70594354     1.90953096     2.12942573     2.36633693     2.62098018     2.89407749     3.18635714     3.49855352     3.99365758
     0.58151571     0.67932817     0.78803235     0.90827831     1.04072426     1.18603629     1.34488818     1.51796111     1.70594354     1.90953096     2.12942573     2.36633693     2.62098018     2.89407749     3.18635714     3.49855352     3.99365758
     0.58151571     0.67932817     0.78803235     0.90827831     1.04072426     1.18603629     1.34488818     1.51796111     1.70594354     1.90953096     2.12942573     2.36633693     2.62098018     2.89407749     3.18635714     3.49855352     3.99365758
     0.58151571     0.67932817     0.78803235     0.90827831     1.04072426     1.18603629     1.34488818     1.51796111     1.70594354     1.90953096     2.12942573     2.36633693     2.62098018     2.89407749     3.18635714     3.49855352     3.99365758
     0.58151571     0.67932817     0.78803235     0.90827831     1.04072426     1.18603629     1.34488818     1.51796111     1.70594354     1.90953096     2.12942573     2.36633693     2.62098018     2.89407749     3.18635714     3.49855352     3.99365758
     0.58151571     0.67932817     0.78803235     0.90827831     1.04072426     1.18603629     1.34488818     1.51796111     1.70594354     1.90953096     2.12942573     2.36633693     2.62098018     2.89407749     3.18635714     3.49855352     3.99365758
     0.58151571     0.67932817     0.78803235     0.90827831     1.04072426     1.18603629     1.34488818     1.51796111     1.70594354     1.90953096     2.12942573     2.36633693     2.62098018     2.89407749     3.18635714     3.49855352     3.99365758
     0.58151571     0.67932817     0.78803235     0.90827831     1.04072426     1.18603629     1.34488818     1.51796111     1.70594354     1.90953096     2.12942573     2.36633693     2.62098018     2.89407749     3.18635714     3.49855352     3.99365758
     0.58151571     0.67932817     0.78803235     0.90827831     1.04072426     1.18603629     1.34488818     1.51796111     1.70594354     1.90953096     2.12942573     2.36633693     2.62098018     2.89407749     3.18635714     3.49855352     3.99365758
     0.58151571     0.67932817     0.78803235     0.90827831     1.04072426     1.18603629     1.34488818     1.51796111     1.70594354     1.90953096     2.12942573     2.36633693     2.62098018     2.89407749     3.18635714     3.49855352     3.99365758
     0.58151571     0.67932817     0.78803235     0.90827831     1.04072426     1.18603629     1.34488818     1.51796111     1.70594354     1.90953096     2.12942573     2.36633693     2.62098018     2.89407749     3.18635714     3.49855352     3.99365758
     0.58151571     0.67932817     0.78803235     0.90827831     1.04072426     1.18603629     1.34488818     1.51796111     1.70594354     1.90953096     2.12942573     2.36633693     2.62098018     2.89407749     3.18635714     3.49855352     3.99365758
     0.58151571     0.67932817     0.78803235     0.90827831     1.04072426     1.18603629     1.34488818     1.51796111     1.70594354     1.90953096     2.12942573     2.36633693     2.62098018     2.89407749     3.18635714     3.49855352     3.99365758
     0.58151571     0.67932817     0.78803235     0.90827831     1.04072426     1.18603629     1.34488818     1.51796111     1.70594354     1.90953096     2.12942573     2.36633693     2.62098018     2.89407749     3.18635714     3.49855352     3.99365758
     0.58151571     0.67932817     0.78803235     0.90827831     1.04072426     1.18603629     1.34488818     1.51796111     1.70594354     1.90953096     2.12942573     2.36633693     2.62098018     2.89407749     3.18635714     3.49855352     3.99365758
     0.58151571     0.67932817     0.78803235     0.90827831     1.04072426     1.18603629     1.34488818     1.51796111     1.70594354     1.90953096     2.12942573     2.36633693     2.62098018     2.89407749     3.18635714     3.49855352     3.99365758
     0.58151571     0.67932817     0.78803235     0.90827831     1.04072426     1.18603629     1.34488818     1.51796111     1.70594354     1.90953096     2.12942573     2.36633693     2.62098018     2.89407749     3.18635714     3.49855352     3.99365758
     0.58151571     0.67932817     0.78803235     0.90827831     1.04072426     1.18603629     1.34488818     1.51796111     1.70594354     1.90953096     2.12942573     2.36633693     2.62098018     2.89407749     3.18635714     3.49855352     3.99365758
     0.58151571     0.67932817     0.78803235     0.90827831     1.04072426     1.18603629     1.34488818     1.51796111     1.70594354     1.90953096     2.12942573     2.36633693     2.62098018     2.89407749     3.18635714     3.49855352     3.99365758
     0.58151571     0.67932817     0.78803235     0.90827831     1.04072426     1.18603629     1.34488818     1.51796111     1.70594354     1.90953096     2.12942573     2.36633693     2.62098018     2.89407749     3.18635714     3.49855352     3.99365758
     0.58151571     0.67932817     0.78803235     0.90827831     1.04072426     1.18603629     1.34488818     1.51796111     1.70594354     1.90953096     2.12942573     2.36633693     2.62098018     2.89407749     3.18635714     3.49855352     3.99365758
     0.58151571     0.67932817     0.78803235     0.90827831     1.04072426     1.18603629     1.34488818     1.51796111     1.70594354     1.90953096     2.12942573     2.36633693     2.62098018     2.89407749     3.18635714     3.49855352     3.99365758
     0.58151571     0.67932817     0.78803235     0.90827831     1.04072426     1.18603629     1.34488818     1.51796111     1.70594354     1.90953096     2.12942573     2.36633693     2.62098018     2.89407749     3.18635714     3.49855352     3.99365758
     0.58151571     0.67932817     0.78803235     0.90827831     1.04072426     1.18603629     1.34488818     1.51796111     1.70594354     1.90953096     2.12942573     2.36633693     2.62098018     2.89407749     3.18635714     3.49855352     3.99365758
     0.58151571     0.67932817     0.78803235     0.90827831     1.04072426     1.18603629     1.34488818     1.51796111     1.70594354     1.90953096     2.12942573     2.36633693     2.62098018     2.89407749     3.18635714     3.49855352     3.99365758
     0.58151571     0.67932817     0.78803235     0.90827831     1.04072426     1.18603629     1.34488818     1.51796111     1.70594354     1.90953096     2.12942573     2.36633693     2.62098018     2.89407749     3.18635714     3.49855352     3.99365758
     0.58151571     0.67932817     0.78803235     0.90827831     1.04072426     1.18603629     1.34488818     1.51796111     1.70594354     1.90953096     2.12942573     2.36633693     2.62098018     2.89407749     3.18635714     3.49855352     3.99365758
     0.58151571     0.67932817     0.78803235     0.90827831     1.04072426     1.18603629     1.34488818     1.51796111     1.70594354     1.90953096     2.12942573     2.36633693     2.62098018     2.89407749     3.18635714     3.49855352     3.99365758
     0.58151571     0.67932817     0.78803235     0.90827831     1.04072426     1.18603629     1.34488818     1.51796111     1.70594354     1.90953096     2.12942573     2.36633693     2.62098018     2.89407749     3.18635714     3.49855352     3.99365758
     0.58151571     0.67932817     0.78803235     0.90827831     1.04072426     1.18603629     1.34488818     1.51796111     1.70594354     1.90953096     2.12942573     2.36633693     2.62098018     2.89407749     3.18635714     3.49855352     3.99365758
     0.58151571     0.67932817     0.78803235     0.90827831     1.04072426     1.18603629     1.34488818     1.51796111     1.70594354     1.90953096     2.12942573     2.36633693     2.62098018     2.89407749     3.18635714     3.49855352     3.99365758
# Proportion mature by sex and size
 0.00000000 0.00000000 0.00000000 1.00000000 1.00000000 1.00000000 1.00000000 1.00000000 1.00000000 1.00000000 1.00000000 1.00000000 1.00000000 1.00000000 1.00000000 1.00000000 1.00000000
# Proportion legal by sex and size
 0.00000000 0.00000000 0.00000000 0.00000000 0.00000000 0.00000000 0.00000000 1.00000000 1.00000000 1.00000000 1.00000000 1.00000000 1.00000000 1.00000000 1.00000000 1.00000000 1.00000000

## ==================================================================================== ##
## GROWTH PARAMETER CONTROLS                                                            ##
## ==================================================================================== ##
## 
# Maximum number of size-classes to which recruitment must occur
 5
# Use functional maturity for terminally molting animals (0=no; 1=Yes)?
0
# Growth transition
##Type_1: Options for the growth matrix
#  1: Pre-specified growth transition matrix (requires molt probability)
#  2: Pre-specified size transition matrix (molt probability is ignored)
#  3: Growth increment is gamma distributed (requires molt probability)
#  4: Post-molt size is gamma distributed (requires molt probability)
#  5: Von Bert.: kappa varies among individuals (requires molt probability)
#  6: Von Bert.: Linf varies among individuals (requires molt probability)
#  7: Von Bert.: kappa and Linf varies among individuals (requires molt probability)
#  8: Growth increment is normally distributed (requires molt probability)
## Type_2: Options for the growth increment model matrix
#  1: Linear
#  2: Individual
#  3: Individual (Same as 2)
#  4: Power law for mean post-molt size
#  Block: Block number for time-varying growth   
## Type_1 Type_2  Block
        8      1      0 
# Molt probability
# Type: Options for the molt probability function
#  0: Pre-specified
#  1: Constant at 1
#  2: Logistic
#  3: Individual
#  Block: Block number for time-varying growth   
## Type Block
      2     0 

## General parameter specificiations 
##  Initial: Initial value for the parameter (must lie between lower and upper)
##  Lower & Upper: Range for the parameter
##  Prior type:
##   0: Uniform   - parameters are the range of the uniform prior
##   1: Normal    - parameters are the mean and sd
##   2: Lognormal - parameters are the mean and sd of the log
##   3: Beta      - parameters are the two beta parameters [see dbeta]
##   4: Gamma     - parameters are the two gamma parameters [see dgamma]
##  Phase: Set equal to a negative number not to estimate
##  Relative: 0: absolute; 1 relative 
##  Block: Block number for time-varying selectivity   
##  Block_fn: 0:absolute values; 1:exponential
##  Env_L: Environmental link - options are 0:none; 1:additive; 2:multiplicative; 3:exponential
##  EnvL_var: Environmental variable
##  RW: 0 for no random walk changes; 1 otherwise
##  RW_blk: Block number for random walks
##  Sigma_RW: Sigma used for the random walk

# Inputs for sex * type 1
# MAIN PARS: Initial  Lower_bound  Upper_bound Prior_type       Prior_1      Prior_2  Phase  Block Blk_fn  Env_L Env_vr     RW RW_Blk RW_Sigma
            22.709032    10.000000    50.000000          0     0.000000    20.000000      7      0      0      0      0      0      0   0.3000 # Alpha_base_male
             0.070494    -0.400000    20.000000          0     0.000000    10.000000      7      0      0      0      0      0      0   0.3000 # Beta_base_male
             3.658150     0.010000     5.000000          0     0.000000     3.000000      7      0      0      0      0      0      0   0.3000 # Gscale_base_male
# EXTRA PARS: Initial  Lower_bound  Upper_bound Prior_type      Prior_1      Prior_2  Phase Reltve 
# Inputs for sex * type 2
# MAIN PARS: Initial  Lower_bound  Upper_bound Prior_type       Prior_1      Prior_2  Phase  Block Blk_fn  Env_L Env_vr     RW RW_Blk RW_Sigma
           141.387494    65.000000   165.000000          0     0.000000   999.000000      7      0      0      0      0      0      0   0.3000 # Molt_probability_mu_base_male_period_1
             0.083565    -0.100000     2.000000          0     0.000000     2.000000      7      0      0      0      0      0      0   0.3000 # Molt_probability_CV_base_male_period_1
# EXTRA PARS: Initial  Lower_bound  Upper_bound Prior_type      Prior_1      Prior_2  Phase Reltve 


## ==================================================================================== ##
## NATURAL MORTALITY PARAMETER CONTROLS                                                 ##
## ==================================================================================== ##
## 
# Relative: 0 - absolute values; 1+ - based on another M-at-size vector (indexed by ig)
# Type: 0 for standard; 1: Spline
#  For spline: set extra to the number of knots, the parameters are the knots (phase -1) and the log-differences from base M
# Extra: control the number of knots for splines
# Brkpts: number of changes in M by size
# Mirror: Mirror M-at-size over to that for another partition (indexed by ig)
# Block: Block number for time-varying M-at-size
# Block_fn: 0:absolute values; 1:exponential
# Env_L: Environmental link - options are 0: none; 1:additive; 2:multiplicative; 3:exponential
# EnvL_var: Environmental variable
# RW: 0 for no random walk changes; 1 otherwise
# RW_blk: Block number for random walks
# Sigma_RW: Sigma for the random walk parameters
# Mirror_RW: Should time-varying aspects be mirrored (Indexed by ig)
## Relative?   Type   Extra  Brkpts  Mirror   Block  Blk_fn Env_L   EnvL_Vr      RW  RW_blk Sigma_RW Mirr_RW
          0       0       0       0       0       0       0       0       0       0       0   0.3000       0
 # sex*maturity state: male & 1

#      Initial    Lower_bound    Upper_bound  Prior_type        Prior_1        Prior_2  Phase 
    0.22000000     0.00000000     0.50000000           0     0.00000000     0.20000000     -1 # M_base_male_mature

## ==================================================================================== ##
## SELECTIVITY PARAMETERS CONTROLS                                                      ##
## ==================================================================================== ##
## 
# ## Selectivity parameter controls
# ## Selectivity (and retention) types
# ##  <0: Mirror selectivity
# ##   0: Nonparametric selectivity (one parameter per class)
# ##   1: Nonparametric selectivity (one parameter per class, constant from last specified class)
# ##   2: Logistic selectivity (inflection point and width (i.e. 1/slope))
# ##   3: Logistic selectivity (50% and 95% selection)
# ##   4: Double normal selectivity (3 parameters)
# ##   5: Flat equal to zero (1 parameter; phase must be negative)
# ##   6: Flat equal to one (1 parameter; phase must be negative)
# ##   7: Flat-topped double normal selectivity (4 parameters)
# ##   8: Declining logistic selectivity with initial values (50% and 95% selection plus extra)
# ##   9: Cubic-spline (specified with knots and values at knots)
# ##      Inputs: knots (in length units); values at knots (0-1) - at least one should have phase -1
# ##  10: One parameter logistic selectivity (inflection point and slope)
# ##  11: Pre-specified selectivity (matrix by year and class)
## Selectivity specifications --
# ## Extra (type 1): number of selectivity parameters to estimated
# #  Directed_Fishery GF_Bycatch
 0 0 # is selectivity sex=specific? (1=Yes; 0=No)
 2 5 # male selectivity type
 0 0 # selectivity within another gear
 0 0 # male extra parameters for each pattern
 1 1 # male: is maximum selectivity at size forced to equal 1 (1) or not (0)
 0 0 # size-class at which selectivity is forced to equal 1 (ignored if the previous input is 1)
## Retention specifications --
 0 0 # is retention sex=specific? (1=Yes; 0=No)
 2 6 # male retention type
 1 0 # male retention flag (0 = no, 1 = yes)
 0 0 # male extra parameters for each pattern
 0 0 # male - should maximum retention be estimated for males (1=Yes; 0=No)

## General parameter specificiations 
##  Initial: Initial value for the parameter (must lie between lower and upper)
##  Lower & Upper: Range for the parameter
##  Prior type:
##   0: Uniform   - parameters are the range of the uniform prior
##   1: Normal    - parameters are the mean and sd
##   2: Lognormal - parameters are the mean and sd of the log
##   3: Beta      - parameters are the two beta parameters [see dbeta]
##   4: Gamma     - parameters are the two gamma parameters [see dgamma]
##  Phase: Set equal to a negative number not to estimate
##  Relative: 0: absolute; 1 relative 
##  Block: Block number for time-varying selectivity   
##  Block_fn: 0:absolute values; 1:exponential
##  Env_L: Environmental link - options are 0:none; 1:additive; 2:multiplicative; 3:exponential
##  EnvL_var: Environmental variable
##  RW: 0 for no random walk changes; 1 otherwise
##  RW_blk: Block number for random walks
##  Sigma_RW: Sigma used for the random walk

# Inputs for type*sex*fleet: selectivity male Directed_Fishery
# MAIN PARS:  Initial  Lower_bound  Upper_bound Prior_type     Prior_1      Prior_2  Phase  Block Blk_fn  Env_L Env_vr     RW RW_Blk RW_Sigma
           120.027152   105.000000   180.000000          0   100.000000   190.000000      3      2      0      0      0      0      0   0.3000 # Sel_Directed_Fishery_male_base_Logistic_mean
            11.818495     0.010000    40.000000          0     0.100000    50.000000      3      2      0      0      0      0      0   0.3000 # Sel_Directed_Fishery_male_base_Logistic_cv
# EXTRA PARS: Initial  Lower_bound  Upper_bound Prior_type      Prior_1     Prior_2  Phase Reltve 
           136.422596   105.000000   180.000000          0   100.000000   190.000000      3      0 # Sel_Directed_Fishery_male_Logistic_mean_block_group_2_block_1
             7.124591     0.010000    20.000000          0     0.100000    50.000000      3      0 # Sel_Directed_Fishery_male_Logistic_cv_block_group_2_block_1

# Inputs for type*sex*fleet: retention male Directed_Fishery
# MAIN PARS:  Initial  Lower_bound  Upper_bound Prior_type     Prior_1      Prior_2  Phase  Block Blk_fn  Env_L Env_vr     RW RW_Blk RW_Sigma
           136.306465   105.000000   180.000000          0   100.000000   190.000000      3      0      0      0      0      0      0   0.3000 # Ret_Directed_Fishery_male_base_Logistic_mean
             2.103757     0.000100    20.000000          0     0.100000    50.000000      3      0      0      0      0      0      0   0.3000 # Ret_Directed_Fishery_male_base_Logistic_cv

# pre-specified selectivity/retention (ordered by type, sex, fleet and year)

## ==================================================================================== ##
## CATCHABILITY PARAMETER CONTROLS                                                      ##
## ==================================================================================== ##
## 
# Catchability (specifications)
# Analytic: should q be estimated analytically (1) or not (0)
# Lambda: the weight lambda
# Emphasis: the weighting emphasis
# Block: Block number for time-varying q
# Block_fn: 0:absolute values; 1:exponential
# Env_L: Environmental link - options are 0: none; 1:additive; 2:multiplicative; 3:exponential
# EnvL_var: Environmental variable
# RW: 0 for no random walk changes; 1 otherwise
# RW_blk: Block number for random walks
# Sigma_RW: Sigma for the random walk parameters
## Analytic  Lambda Emphass  Mirror   Block   Env_L EnvL_Vr      RW  RW_blk Sigma_RW
          0       1       1       0       0       0       0       0       0   0.3000 
          0       1       1       0       0       0       0       0       0   0.3000 
          0       1       1       0       0       0       0       0       0   0.3000 
# Catchability (parameters)
#      Initial    Lower_bound    Upper_bound  Prior_type        Prior_1        Prior_2  Phase 
    0.00043659     0.00000010     0.01000000           0     0.00000000     1.00000000      1 # Survey_q_parameter_1
    0.00032639     0.00000010     0.01000000           0     0.00000000     1.00000000      1 # Survey_q_parameter_2
    0.00054538     0.00000010     0.01000000           0     0.00000000     1.00000000      1 # Survey_q_parameter_3

## ==================================================================================== ##
## ADDITIONAL CV PARAMETER CONTROLS                                                     ##
## ==================================================================================== ##
## 
# Additiional CV controls (specifications)
# Mirror: should additional variance be mirrored (value > 1) or not (0)?
# Block: Block number for time-varying additional variance
# Block_fn: 0:absolute values; 1:exponential
# Env_L: Environmental link - options are 0: none; 1:additive; 2:multiplicative; 3:exponential
# EnvL_var: Environmental variable
# RW: 0 for no random walk changes; 1 otherwise
# RW_blk: Block number for random walks
# Sigma_RW: Sigma for the random walk parameters
##   Mirror   Block   Env_L EnvL_Vr     RW   RW_blk Sigma_RW
          0       0       0       0       0       0   0.3000 
          0       0       0       0       0       0   0.3000 
          0       0       0       0       0       0   0.3000 
## Mirror Block Env_L EnvL_Var  RW RW_blk Sigma_RW
# Additional variance (parameters)
#      Initial    Lower_bound    Upper_bound  Prior_type        Prior_1        Prior_2  Phase 
    0.23833168     0.00000010     0.50000000           0     0.50000000   100.00000000      6 # Add_cv_parameter_1
    0.10828327     0.00000010     0.50000000           0     0.50000000   100.00000000      6 # Add_cv_parameter_2
    0.20228459     0.00000010     0.50000000           0     0.50000000   100.00000000      6 # Add_cv_parameter_3

## ==================================================================================== ##
## CONTROLS ON F                                                                        ##
## ==================================================================================== ##
## 
# Controls on F
#   Initial_male_F Initial_fema_F   Pen_SD (mal)   Pen_SD (fem) Phz_mean_F_mal Phz_mean_F_fem   Lower_mean_F   Upper_mean_F Low_ann_male_F  Up_ann_male_F    Low_ann_f_F     Up_ann_f_F
          0.366136       0.000000       3.000000      15.000000       2.000000      -1.000000     -12.000000       4.000000     -10.000000      10.000000     -10.000000      10.000000  # Directed_Fishery
          0.000218       0.000000       4.000000      15.000000       2.000000      -1.000000     -12.000000       4.000000     -10.000000      10.000000     -10.000000      10.000000  # GF_Bycatch
#
 # Estimates related to fishing mortality
# Male fishing mortality by fleet
#Directed_Fishery GF_Bycatch 
# -1.12457456  -7.73039909 
# Male annual offset fishing mortality deviations by fleet
# Directed_Fishery         1981         1982         1983         1984         1985         1986         1987         1988         1989         1990         1991         1992         1993         1994         1995         1996         1997         1998         1999         2000         2001         2002         2003         2004         2005         2006         2007         2008         2009         2010         2011         2012         2013 
# Directed_Fishery  -1.80961087  -0.78769518  -0.48256280  -0.30437779   0.32074180   0.44889318   0.25987649   0.63332096   0.99377063   0.49645025   0.64399914   0.73732649   0.35093885   0.56835243   0.78907491   0.54939352   0.39536947   0.28051930   0.08309880  -0.04849271  -0.21048768  -0.43564167  -0.45949903  -0.53832693  -0.35074604  -0.28347627  -0.27900479  -0.23560815  -0.22845420  -0.25951013  -0.30721742  -0.27723936  -0.25308207 
# GF_Bycatch         1991         1992         1993         1994         1995         1996         1997         1998         1999         2000         2001         2002         2003         2004         2005         2006         2007         2008         2009         2010         2011         2012         2013 
# GF_Bycatch  -4.00039170  -2.92088733   0.33643460  -0.78753802  -0.92518230  -2.06298697  -3.09106083  -0.88481708   0.27755231  -0.40264339  -2.13709299   2.06195556   1.97652495  -1.33471665  -1.29999628   2.10355833   3.22449077   2.37484053   1.71463976   2.74870895   2.08526087   0.79503788   0.14828356 
#
 # Estimates related to recruitment
# Annual_deviations         1981         1982         1983         1984         1985         1986         1987         1988         1989         1990         1991         1992         1993         1994         1995         1996         1997         1998         1999         2000         2001         2002         2003         2004         2005         2006         2007         2008         2009         2010         2011         2012         2013 
# Annual_deviations   0.08194216  -0.32923934  -0.07678772  -0.15714139  -0.58554953   0.02383021   0.19417103  -0.15594675  -0.12054076  -0.19950940   0.21444768  -0.49427290   0.02293174  -0.07986938  -0.32310801  -0.00152577   0.06894275  -0.02523951   0.02294759  -0.19868307  -0.11375661  -0.30837714  -0.48355940   0.02281236  -0.24194673  -0.26440266   0.01567716   0.04958530  -0.18215501  -0.12965426  -0.11007591  -0.01489280  -0.12501489 

## ==================================================================================== ##
## SIZE COMPOSITIONS OPTIONS                                                            ##
## ==================================================================================== ##
## 
# Options when fitting size-composition data
## Likelihood types: 
##  1:Multinomial with estimated/fixed sample size
##  2:Robust approximation to multinomial
##  3:logistic normal
##  4:multivariate-t
##  5:Dirichlet

#  Directed_Fishery Directed_Fishery
#  male male
#  retained total
#  all_shell all_shell
#  immature+mature immature+mature
      1      1 # Type of likelihood
      0      0 # Auto tail compression
      0      0 # Auto tail compression (pmin)
      1      2 # Composition aggregator index
      1      1 # Set to 1 for catch-based predictions; 2 for survey or total catch predictions
 0.0850 0.0567 # Lambda for effective sample size
 1.0000 1.0000 # Lambda for overall likelihood
      0      0 # Survey to set Q for this comp

# The number of following parameters must match max(Composition Aggregator Index) 
#      Initial    Lower_bound    Upper_bound  Prior_type        Prior_1        Prior_2  Phase 
    1.00000000     0.10000000     5.00000000           0     0.00000000   999.00000000     -4 # Overdispersion_parameter_for_size_comp_1 (possibly extended)
    1.00000000     0.10000000     5.00000000           0     0.00000000   999.00000000     -4 # Overdispersion_parameter_for_size_comp_2 (possibly extended)

## ==================================================================================== ##
## EMPHASIS FACTORS                                                                     ##
## ==================================================================================== ##

1.0000 # Emphasis on tagging data

 1.0000 1.0000 1.0000 # Emphasis on Catch: (by catch dataframes)

# Weights for penalties 1, 11, and 12
#   Mean_M_fdevs | Mean_F_fdevs |  Ann_M_fdevs |  Ann_F_fdevs
          0.0000         0.0000         0.0010         0.0000 # Directed_Fishery
          0.0000         0.0000         0.0010         0.0000 # GF_Bycatch

## Emphasis Factors (Priors/Penalties: 13 values) ##
     0.0000	#--Penalty on log_fdev (male+combined; female) to ensure they sum to zero
     0.0000	#--Penalty on mean F by fleet to regularize the solution
     0.0000	#--Not used
     2.0000	#--Not used
     0.0000	#--Not used
     0.0000	#--Smoothness penalty on the recruitment devs
     0.0000	#--Penalty on the difference of the mean_sex_ratio from 0.5
     0.0000	#--Smoothness penalty on molting probability
     0.0000	#--Smoothness penalty on selectivity patterns with class-specific coefficients
     0.0000	#--Smoothness penalty on initial numbers at length
     1.0000	#--Penalty on annual F-devs for males by fleet
     0.0000	#--Penalty on annual F-devs for females by fleet
     0.0000	#--Penalty on deviation parameters

# eof_ctl
9999
