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
2023 # Last year of recruitment estimation
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
     9.08305701   -10.00000000    20.00000000     3          1   -10.00000000    20.00000000 # Log(Rinitial)
     8.16100000   -10.00000000    20.00000000   -99          0   -10.00000000    20.00000000 # Log(Rbar)
   110.00000000   103.00000000   165.00000000    -2          1    72.50000000     7.25000000 # Recruitment_ra-males
     0.45915850     0.00100000    20.00000000     3          0     0.10000000     5.00000000 # Recruitment_rb-males
    -0.69314718   -10.00000000     0.75000000    -1          0   -10.00000000     0.75000000 # log(SigmaR)
     0.73000000     0.20000000     1.00000000    -2          3     3.00000000     2.00000000 # Steepness
     0.00100000     0.00000000     1.00000000    -3          3     1.01000000     1.01000000 # Rho
     0.06211230   -10.00000000     4.00000000     4          0    10.00000000    20.00000000 # Scaled_logN_for_male_mature_mature_newshell_class_1
     0.11079064   -10.00000000     4.00000000     4          0    10.00000000    20.00000000 # Scaled_logN_for_male_mature_mature_newshell_class_2
     0.14739414   -10.00000000     4.00000000     4          0    10.00000000    20.00000000 # Scaled_logN_for_male_mature_mature_newshell_class_3
     0.17962566   -10.00000000     4.00000000     4          0    10.00000000    20.00000000 # Scaled_logN_for_male_mature_mature_newshell_class_4
     0.21156954   -10.00000000     4.00000000     4          0    10.00000000    20.00000000 # Scaled_logN_for_male_mature_mature_newshell_class_5
     0.22947149   -10.00000000     4.00000000     4          0    10.00000000    20.00000000 # Scaled_logN_for_male_mature_mature_newshell_class_6
     0.22408685   -10.00000000     4.00000000     4          0    10.00000000    20.00000000 # Scaled_logN_for_male_mature_mature_newshell_class_7
     0.21006010   -10.00000000     4.00000000     4          0    10.00000000    20.00000000 # Scaled_logN_for_male_mature_mature_newshell_class_9
     0.14719519   -10.00000000     4.00000000     4          0    10.00000000    20.00000000 # Scaled_logN_for_male_mature_mature_newshell_class_10
     0.08796452   -10.00000000     4.00000000     4          0    10.00000000    20.00000000 # Scaled_logN_for_male_mature_mature_newshell_class_11
     0.05701166   -10.00000000     4.00000000     4          0    10.00000000    20.00000000 # Scaled_logN_for_male_mature_mature_newshell_class_12
    -0.07443452   -10.00000000     4.00000000     4          0    10.00000000    20.00000000 # Scaled_logN_for_male_mature_mature_newshell_class_13
    -0.25060584   -10.00000000     4.00000000     4          0    10.00000000    20.00000000 # Scaled_logN_for_male_mature_mature_newshell_class_14
    -0.34207574   -10.00000000     4.00000000     4          0    10.00000000    20.00000000 # Scaled_logN_for_male_mature_mature_newshell_class_15
    -0.58132011   -10.00000000     4.00000000     4          0    10.00000000    20.00000000 # Scaled_logN_for_male_mature_mature_newshell_class_16
    -0.58958092   -10.00000000     4.00000000     4          0    10.00000000    20.00000000 # Scaled_logN_for_male_mature_mature_newshell_class_17

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
            22.464248    10.000000    50.000000          0     0.000000    20.000000      7      0      0      0      0      0      0   0.3000 # Alpha_base_male
             0.068912    -0.400000    20.000000          0     0.000000    10.000000      7      0      0      0      0      0      0   0.3000 # Beta_base_male
             3.658100     0.010000     5.000000          0     0.000000     3.000000      7      0      0      0      0      0      0   0.3000 # Gscale_base_male
# EXTRA PARS: Initial  Lower_bound  Upper_bound Prior_type      Prior_1      Prior_2  Phase Reltve 
# Inputs for sex * type 2
# MAIN PARS: Initial  Lower_bound  Upper_bound Prior_type       Prior_1      Prior_2  Phase  Block Blk_fn  Env_L Env_vr     RW RW_Blk RW_Sigma
           141.251760    65.000000   165.000000          0     0.000000   999.000000      7      0      0      0      0      0      0   0.3000 # Molt_probability_mu_base_male_period_1
             0.087603    -0.100000     2.000000          0     0.000000     2.000000      7      0      0      0      0      0      0   0.3000 # Molt_probability_CV_base_male_period_1
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
           121.625406   105.000000   180.000000          0   100.000000   190.000000      3      2      0      0      0      0      0   0.3000 # Sel_Directed_Fishery_male_base_Logistic_mean
            12.071387     0.010000    40.000000          0     0.100000    50.000000      3      2      0      0      0      0      0   0.3000 # Sel_Directed_Fishery_male_base_Logistic_cv
# EXTRA PARS: Initial  Lower_bound  Upper_bound Prior_type      Prior_1     Prior_2  Phase Reltve 
           135.404130   105.000000   180.000000          0   100.000000   190.000000      3      0 # Sel_Directed_Fishery_male_Logistic_mean_block_group_2_block_1
             6.781485     0.010000    20.000000          0     0.100000    50.000000      3      0 # Sel_Directed_Fishery_male_Logistic_cv_block_group_2_block_1

# Inputs for type*sex*fleet: retention male Directed_Fishery
# MAIN PARS:  Initial  Lower_bound  Upper_bound Prior_type     Prior_1      Prior_2  Phase  Block Blk_fn  Env_L Env_vr     RW RW_Blk RW_Sigma
           136.548610   105.000000   180.000000          0   100.000000   190.000000      3      0      0      0      0      0      0   0.3000 # Ret_Directed_Fishery_male_base_Logistic_mean
             2.172230     0.000100    20.000000          0     0.100000    50.000000      3      0      0      0      0      0      0   0.3000 # Ret_Directed_Fishery_male_base_Logistic_cv

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
    0.00046578     0.00000010     0.01000000           0     0.00000000     1.00000000      1 # Survey_q_parameter_1
    0.00041089     0.00000010     0.01000000           0     0.00000000     1.00000000      1 # Survey_q_parameter_2
    0.00057400     0.00000010     0.01000000           0     0.00000000     1.00000000      1 # Survey_q_parameter_3

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
    0.23424489     0.00000010     0.50000000           0     0.50000000   100.00000000      6 # Add_cv_parameter_1
    0.24228676     0.00000010     0.50000000           0     0.50000000   100.00000000      6 # Add_cv_parameter_2
    0.20827955     0.00000010     0.50000000           0     0.50000000   100.00000000      6 # Add_cv_parameter_3

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
# -1.06224385  -7.07964726 
# Male annual offset fishing mortality deviations by fleet
# Directed_Fishery         1981         1982         1983         1984         1985         1986         1987         1988         1989         1990         1991         1992         1993         1994         1995         1996         1997         1998         1999         2000         2001         2002         2003         2004         2005         2006         2007         2008         2009         2010         2011         2012         2013         2014         2015         2016         2017         2018         2019         2020         2021         2022         2023 
# Directed_Fishery  -1.84003856  -0.81214686  -0.49714794  -0.31954944   0.29998281   0.42728199   0.23843287   0.61138344   0.97913556   0.48667860   0.63403918   0.73030767   0.34376916   0.55736880   0.77383095   0.53984528   0.39116590   0.28034080   0.08662862  -0.04463219  -0.20665630  -0.43253423  -0.45835692  -0.53721134  -0.38798512  -0.31953376  -0.31102772  -0.26649139  -0.25648932  -0.26729060  -0.27260366  -0.19244678  -0.10363826  -0.00840155   0.11191002   0.16363740   0.13346863   0.22840581   0.26500966   0.00068978  -0.14562597  -0.33017073  -0.27332861 
# GF_Bycatch         1991         1992         1993         1994         1995         1996         1997         1998         1999         2000         2001         2002         2003         2004         2005         2006         2007         2008         2009         2010         2011         2012         2013         2014         2015         2016         2017         2018         2019         2020         2021         2022         2023 
# GF_Bycatch  -2.32987917  -1.79000014  -0.15867885  -0.71933006  -0.78602083  -1.35389201  -1.86776214  -0.76047633  -0.17708940  -0.51598844  -1.38438990   0.72160683   0.68326122  -0.98115711  -0.96076529   0.74336560   1.31291625   0.88703595   0.57627088   1.13347041   0.80883482   0.17951477  -0.10814108   0.29048690   0.83368700   1.55577098   1.13377779   0.74326992   0.53238743   1.55660649   0.51384093   0.16229004  -0.47482338 
#
 # Estimates related to recruitment
# Annual_deviations         1981         1982         1983         1984         1985         1986         1987         1988         1989         1990         1991         1992         1993         1994         1995         1996         1997         1998         1999         2000         2001         2002         2003         2004         2005         2006         2007         2008         2009         2010         2011         2012         2013         2014         2015         2016         2017         2018         2019         2020         2021         2022         2023 
# Annual_deviations   0.00394815  -0.41162714  -0.17688398  -0.28438531  -0.67677561  -0.07474808   0.07443723  -0.26900597  -0.23937524  -0.30341344   0.11038653  -0.59385737  -0.10595402  -0.20687162  -0.44871445  -0.12666931  -0.05670220  -0.15564286  -0.11359966  -0.32705133  -0.25052067  -0.46254581  -0.59012018  -0.14439601  -0.36854902  -0.38778584  -0.21387167  -0.29322606  -0.48782143  -0.52725693  -0.57800328  -0.58878737  -0.34529190  -0.11859521  -0.09960228   0.03495103   0.12899375   0.00051192   0.06590470  -0.11933925  -0.35171381  -0.30346744  -0.12530346 

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
          0.0000         0.0000         0.5000         0.0000 # GF_Bycatch

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
