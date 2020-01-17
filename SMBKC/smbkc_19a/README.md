udpated: 1-17-2020
# St. Matthews Blue King Crab 2019 - model options for 2020
2019 model version (accepted 19.0)
prep for model options for May 2020 meeting

Models housed in smbkc_19a 

## Alternatives

Model             | Model in Doc  | Description
---------- ---    | ------------- | -------------
model_1           |   19.0        | - Basecase (last year's selected model 1) - with no new data     
model_1b          |   19.2        | - add CV for ADF&G pot survey
model_1c          |   19.2a       | - add CV for both surveys

model_4           |               | - Apply VAST series - NOT done this year


Notice the use of a single .dat file for all of the different model runs. 
The .dat file sits in assessment root, 
each model has its own directory (i.e. model_1, ..., model_4), and in each model directory is a different control file.

## dat, ctl, prj files - FILE UPDATES ANNUALLY
.dat file lives in data folder in assessment root.  
Update annually - 
.ctl file updated for model_1 in that folder with new year ranges, etc. each model needs the .ctl file updated.

update gmacs.dat with correct file names for .dat, .ctl, .prj

# Run the model
1) Using ADMB shell navigate to the folder for model_1
    ' cd Documents\BSAI_crab_assessments\SMBKC\smbkc_19\model_1'
    
2) type 'gmacs'

3) code should run

4) Open 'Gmacsall.out'
Look at model output - did model converge? how are parameter estimates? check all this and open plot.R to graphically view output.

NOTE: check on ssb output. only has 41 years instead of 42 - crab year so 1978 to 2018 does not included projection for 2019/20

# Run mcmc for projections
Test mcmc run

'gmacs -mcmc 10 -mcsave 3' make sure .psv file is created

'gmacs -mcmc 1000000 -mcsave 1000' - started at 11:19am 8-19-19, ended 12:44 pm


# for projections
Need to run mcmc above to get .psv file for projections

1) update .prj file with the projection that you're running (see projections folder and readme with descriptions)
    a) change this file name to 'sm18.prj'
  ** navigate to the folder using ADMB shell - see above **
2) run projection
'gmacs -mceval'

3) save output files created from projection into folder - 'smbkc_18a/projections/proj#/letter'

4) See code in KJP_risk_profile_general.R to summarise output from projection run
5) Then see 'rebuilding_figures.R' for plotting


# rebuilding specific runs
look in smbkc_19/projections/rebuilding_brainstorm/

copy of model_1 from smbkc_19 lives here

