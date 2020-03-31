# k.palof
# 11-12-19

# Attempts to get R2admb to run

library(R2admb)

# temp is the folder to run projections for model_1 for smbkc_19
# file path
setwd("C:/Users/kjpalof/Documents/BSAI_crab_assessments/SMBKC/smbkc_19/temp")

# run model 
run_admb("gmacs", verbose = TRUE)
# Running model in command line
# 1) Open command prompt ("C:/ADMB/ADMB Command Prompt (MinGW 64Bit)")
# 2) Navigate to smbkc_19
# > cd ../..
# make sure that the gmacs.exe is in this folder
# Run
# >gmacs

# run MCMC
run_admb("gmacs", extra.args="-mcmc 1000000 -mcsave 1000")


# for projections
run_admb("gmacs", extra.args="-mceval")
