# Model with higher weightings on the survies
From Andre with projection options available.
Similar to last year's model 5 but with updates to gmacs.tpl

# Data file
data file should be the same - sm18.dat 
NO changes to the data

# Ctl file
Starting at line # 180  ---- priors for catchability

##  LAMBDA: Arbitrary relative weights for each series, 0 = do not fit.
## SURVEYS/INDICES ONLY

Base model
 ival    lb       ub    phz   prior   p1       p2    Analytic?   LAMBDA Emphasis
   1.0     0.5      1.2   -4    0       0        9.0   0           1             1 # NMFS trawl
 0.003      0        5     3    0       0        9.0   0           1             1 # ADF&G pot


Model 5
 ival    lb       ub    phz   prior   p1       p2    Analytic?   Lambda   Emphasis
   0.4     0.2      2.0   -4    0       0        9.0   0           1.5        1     # NMFS trawl
4.11135867487e-4 0 5       3    0       0        9.0   0           2          1       # ADF&G pot


Base model has higher prior or fit for trawl survey but the model 5 downweights the pot survey ALOT - almost close to 0.


# Run the model
1) Using ADMB shell navigate to the folder for model_1
    ' cd Documents\BSAI_crab_assessments\SMBKC\smbkc_18a\model_5'
    
2) type 'gmacs'

3) code should run

4) Open 'Gmacsall.out' to view results


# Plot the results
1) look at plot_indiv_model to set up plot views for this model.




