# St. Matthews Blue King Crab 2019
2019 model version with updates for rebuilding
prep for sept 2019 meeting

## Alternatives

Model         | Description
------------- | -------------
model_0       | - Basecase (last year's selected model 3) - with no new data     
model_1       | - new 2019 NOAA trawl survey data, new bycatch data
model_4       | - Apply VAST series
model_5       | - Fit surveys (lambda up on those)

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

'gmacs -mcmc 1000000 -mcsave 1000' - started at 4:16pm 7-18-19, ended 5:35pm


# for projections
Need to run mcmc above to get .psv file for projections

1) update .prj file with the projection that you're running (see projections folder and readme with descriptions)
    a) change this file name to 'sm18.prj'
  ** navigate to the folder using ADMB shell - see above **
2) run projection
'gmacs -mceval'

3) save output files created from projection into folder - 'smbkc_18a/projections/proj#/letter'