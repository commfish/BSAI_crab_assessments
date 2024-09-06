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
2021 # Last year of recruitment estimation
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
     9.08388187   -10.00000000    20.00000000     3          1   -10.00000000    20.00000000 # Log(Rinitial)
     8.00760527   -10.00000000    20.00000000     1          0   -10.00000000    20.00000000 # Log(Rbar)
   110.00000000   103.00000000   165.00000000    -2          1    72.50000000     7.25000000 # Recruitment_ra-males
     0.46926670     0.00100000    20.00000000     3          0     0.10000000     5.00000000 # Recruitment_rb-males
    -0.69314718   -10.00000000     0.75000000    -1          0   -10.00000000     0.75000000 # log(SigmaR)
     0.73000000     0.20000000     1.00000000    -2          3     3.00000000     2.00000000 # Steepness
     0.00100000     0.00000000     1.00000000    -3          3     1.01000000     1.01000000 # Rho
     0.09314189   -10.00000000     4.00000000     4          0    10.00000000    20.00000000 # Scaled_logN_for_male_mature_mature_newshell_class_1
     0.13996809   -10.00000000     4.00000000     4          0    10.00000000    20.00000000 # Scaled_logN_for_male_mature_mature_newshell_class_2
     0.17395084   -10.00000000     4.00000000     4          0    10.00000000    20.00000000 # Scaled_logN_for_male_mature_mature_newshell_class_3
     0.20183761   -10.00000000     4.00000000     4          0    10.00000000    20.00000000 # Scaled_logN_for_male_mature_mature_newshell_class_4
     0.22687217   -10.00000000     4.00000000     4          0    10.00000000    20.00000000 # Scaled_logN_for_male_mature_mature_newshell_class_5
     0.23855776   -10.00000000     4.00000000     4          0    10.00000000    20.00000000 # Scaled_logN_for_male_mature_mature_newshell_class_6
     0.22703333   -10.00000000     4.00000000     4          0    10.00000000    20.00000000 # Scaled_logN_for_male_mature_mature_newshell_class_7
     0.19694410   -10.00000000     4.00000000     4          0    10.00000000    20.00000000 # Scaled_logN_for_male_mature_mature_newshell_class_9
     0.13167266   -10.00000000     4.00000000     4          0    10.00000000    20.00000000 # Scaled_logN_for_male_mature_mature_newshell_class_10
     0.06847103   -10.00000000     4.00000000     4          0    10.00000000    20.00000000 # Scaled_logN_for_male_mature_mature_newshell_class_11
     0.03290263   -10.00000000     4.00000000     4          0    10.00000000    20.00000000 # Scaled_logN_for_male_mature_mature_newshell_class_12
    -0.09531972   -10.00000000     4.00000000     4          0    10.00000000    20.00000000 # Scaled_logN_for_male_mature_mature_newshell_class_13
    -0.26514931   -10.00000000     4.00000000     4          0    10.00000000    20.00000000 # Scaled_logN_for_male_mature_mature_newshell_class_14
    -0.35282551   -10.00000000     4.00000000     4          0    10.00000000    20.00000000 # Scaled_logN_for_male_mature_mature_newshell_class_15
    -0.58702519   -10.00000000     4.00000000     4          0    10.00000000    20.00000000 # Scaled_logN_for_male_mature_mature_newshell_class_16
    -0.59778683   -10.00000000     4.00000000     4          0    10.00000000    20.00000000 # Scaled_logN_for_male_mature_mature_newshell_class_17

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
            22.588813    10.000000    50.000000          0     0.000000    20.000000      7      0      0      0      0      0      0   0.3000 # Alpha_base_male
             0.069680    -0.400000    20.000000          0     0.000000    10.000000      7      0      0      0      0      0      0   0.3000 # Beta_base_male
             3.658796     0.010000     5.000000          0     0.000000     3.000000      7      0      0      0      0      0      0   0.3000 # Gscale_base_male
# EXTRA PARS: Initial  Lower_bound  Upper_bound Prior_type      Prior_1      Prior_2  Phase Reltve 
# Inputs for sex * type 2
# MAIN PARS: Initial  Lower_bound  Upper_bound Prior_type       Prior_1      Prior_2  Phase  Block Blk_fn  Env_L Env_vr     RW RW_Blk RW_Sigma
           141.550195    65.000000   165.000000          0     0.000000   999.000000      7      0      0      0      0      0      0   0.3000 # Molt_probability_mu_base_male_period_1
             0.087728    -0.100000     2.000000          0     0.000000     2.000000      7      0      0      0      0      0      0   0.3000 # Molt_probability_CV_base_male_period_1
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
           121.282510   105.000000   180.000000          0   100.000000   190.000000      3      2      0      0      0      0      0   0.3000 # Sel_Directed_Fishery_male_base_Logistic_mean
            11.886891     0.010000    40.000000          0     0.100000    50.000000      3      2      0      0      0      0      0   0.3000 # Sel_Directed_Fishery_male_base_Logistic_cv
# EXTRA PARS: Initial  Lower_bound  Upper_bound Prior_type      Prior_1     Prior_2  Phase Reltve 
           136.009973   105.000000   180.000000          0   100.000000   190.000000      3      0 # Sel_Directed_Fishery_male_Logistic_mean_block_group_2_block_1
             7.101025     0.010000    20.000000          0     0.100000    50.000000      3      0 # Sel_Directed_Fishery_male_Logistic_cv_block_group_2_block_1

# Inputs for type*sex*fleet: retention male Directed_Fishery
# MAIN PARS:  Initial  Lower_bound  Upper_bound Prior_type     Prior_1      Prior_2  Phase  Block Blk_fn  Env_L Env_vr     RW RW_Blk RW_Sigma
           136.519053   105.000000   180.000000          0   100.000000   190.000000      3      0      0      0      0      0      0   0.3000 # Ret_Directed_Fishery_male_base_Logistic_mean
             2.164751     0.000100    20.000000          0     0.100000    50.000000      3      0      0      0      0      0      0   0.3000 # Ret_Directed_Fishery_male_base_Logistic_cv

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
    0.00047261     0.00000010     0.01000000           0     0.00000000     1.00000000      1 # Survey_q_parameter_1
    0.00044082     0.00000010     0.01000000           0     0.00000000     1.00000000      1 # Survey_q_parameter_2
    0.00057521     0.00000010     0.01000000           0     0.00000000     1.00000000      1 # Survey_q_parameter_3

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
    0.22704045     0.00000010     0.50000000           0     0.50000000   100.00000000      6 # Add_cv_parameter_1
    0.25840106     0.00000010     0.50000000           0     0.50000000   100.00000000      6 # Add_cv_parameter_2
    0.21044460     0.00000010     0.50000000           0     0.50000000   100.00000000      6 # Add_cv_parameter_3

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
# -1.01776880  -7.03153767 
# Male annual offset fishing mortality deviations by fleet
# Directed_Fishery         1981         1982         1983         1984         1985         1986         1987         1988         1989         1990         1991         1992         1993         1994         1995         1996         1997         1998         1999         2000         2001         2002         2003         2004         2005         2006         2007         2008         2009         2010         2011         2012         2013         2014         2015         2016         2017         2018         2019         2020         2021 
# Directed_Fishery  -1.87306979  -0.85159356  -0.54392087  -0.36649563   0.25629856   0.38442265   0.19392302   0.56905215   0.93832491   0.44287429   0.58953117   0.68604812   0.29809961   0.50966123   0.73085521   0.49891068   0.35236362   0.24486089   0.05409969  -0.07353536  -0.23205584  -0.45483943  -0.47734562  -0.55362892  -0.38048387  -0.31191791  -0.30266178  -0.25555918  -0.24387756  -0.25417457  -0.25946975  -0.17938353  -0.09033319   0.00597155   0.12941804   0.18782326   0.16265391   0.25780793   0.29051831   0.02421566  -0.10338464 
# GF_Bycatch         1991         1992         1993         1994         1995         1996         1997         1998         1999         2000         2001         2002         2003         2004         2005         2006         2007         2008         2009         2010         2011         2012         2013         2014         2015         2016         2017         2018         2019         2020         2021 
# GF_Bycatch  -4.68972828  -3.61393598  -0.35452466  -1.47481154  -1.60651417  -2.73821794  -3.76058566  -1.54786803  -0.37910423  -1.05364077  -2.78374351   1.41704983   1.33485843  -1.97485390  -1.93657478   1.44883972   2.54123720   1.72820317   1.11867144   2.19383863   1.57642467   0.33744862  -0.23337382   0.56122456   1.64959766   3.14128784   2.25897071   1.46403027   1.05094336   3.18577201   1.13907671 
#
 # Estimates related to recruitment
# Annual_deviations         1981         1982         1983         1984         1985         1986         1987         1988         1989         1990         1991         1992         1993         1994         1995         1996         1997         1998         1999         2000         2001         2002         2003         2004         2005         2006         2007         2008         2009         2010         2011         2012         2013         2014         2015         2016         2017         2018         2019         2020         2021 
# Annual_deviations   0.13153845  -0.28900586  -0.02021615  -0.12216827  -0.55488467   0.07032608   0.23167124  -0.12487592  -0.08736006  -0.15578154   0.26382210  -0.47190567   0.04568376  -0.06432140  -0.31038463   0.01007190   0.07309068  -0.02630343   0.01772362  -0.20023160  -0.12227892  -0.32038226  -0.45062348   0.00019961  -0.24079852  -0.25155700  -0.07073419  -0.15219941  -0.35096932  -0.38634985  -0.43975971  -0.45619236  -0.20889457   0.02474028   0.04120459   0.18206400   0.28205465   0.08154990  -0.17645880  -0.28083385  -0.12528083 

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
