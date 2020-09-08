original 7-28-2020
# St. Matthews Blue King Crab 2020 - model options for Sept 2020
prep for model options for Sept 2020 meeting

Models housed in smbkc_20 

## Alternatives

Model             | Model in Doc  | Description
---------- ---    | ------------- | -------------
model_1           |   16.0        | - Basecase (last year's selected model 1) - with new gf bycatch data   
model_1_rfix      |   16.0b       | - 16.0, no rec dev estimated for 2019 (attempt to deal with large recruitment estimate                                           in the terminal year of the model). Edit to .ctl file
model_1_rfix2     |   16.0a      | - 16.0, no rec dev estimated in 2019, plus changes to .tpl file suggested by Jie to                                           use average recruitment for most recent year.

model_2           |   20.1        | - remove pot survey data from base model - sensitivity analysis


model_4           |   19.1        | - Apply VAST series - Jon Richar provided VAST estimates using just the trawl survey 
                  |               |    - April 2020


Notice the use of a single .dat file for all of the different model runs. 
The .dat file sits in assessment root, 
each model has its own directory (i.e. model_1, ..., model_4), and in each model directory is a different control file.

## dat, ctl, prj files - FILE UPDATES ANNUALLY
.dat file lives in data folder in assessment root.  

Update annually - 
.ctl file updated for model_1 in that folder with new year ranges, etc. each model needs the .ctl file updated.

update gmacs.dat with correct file names for .dat, .ctl, .prj

## data 
SMBKC_GE90_BIOMASS.csv - file from J. Richar using VAST to produce estimates from trawl survey

# Run the model
1) Using ADMB shell navigate to the folder for model_1
    ' cd Documents\BSAI_crab_assessments\SMBKC\smbkc_20\model_1'
    
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


# .prj file updates 
Prow and Initial eps?
- setting ProwR=0 and Initial epp = -999 - gives you same results as just running ricker SR (option 2)



### projections used for variablity in 2020 estimate
Projection 1d was used to determine the variability in the 2020 ssb estimate, although the projection used does not greatly impact the intial year variability

# retrospective pattern analysis 
Use model_1 for this. Manually adjust data and .ctl file for this 


## to do list: Tackle before Sept. 2020
address **FIX** tags in 'doc_figures_test.R' file and move things to figures_tables_create_SAFE.R' once they work.

gmr plotting issues:
- fishing mortality
- issues with time blocks
- fix gmr with my changes and make them general before pushing to github

Other models options?

Spatial analysis of the survey data
