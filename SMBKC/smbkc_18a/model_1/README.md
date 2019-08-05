# Base model - model 1 for 2018a
From Andre with projection options available.
This is last year's accepted model with updates to GMACS.tpl - most Andres'

# Run the model
1) Using ADMB shell navigate to the folder for model_1
    ' cd Documents\BSAI_crab_assessments\SMBKC\smbkc_18a\model_1'
    
2) type 'gmacs'

3) code should run

4) Open 'Gmacsall.out'
Compare this code to what I got for May 2019 CPT meeting - if looks good run mcmc
NOTE: identical likelihood to April/May results...looks good 


# Run mcmc for projections
Test mcmc run

'gmacs -mcmc 10 -mcsave 3' make sure .psv file is created

'gmacs -mcmc 1000000 -mcsave 1000' - started at 4:16pm 7-18-19, ended 5:35pm


# for projections
Need to run mcmc above to get .psv file for projections

1) update .prj file with the projection that you're running (see projections folder and readme with descriptions)
    a) change this file name to 'sm18.prj'
  ** navigate to the folder using ADMB shell - see above **
2) run projection
'gmacs -mceval'

3) save output files created from projection into folder - proj#/letter