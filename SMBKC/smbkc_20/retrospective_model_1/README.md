original 8-2-2020
# St. Matthews Blue King Crab 2020 - Retrospective model runs - done manually 
**fix** need to change this so GMACS can do it automatically - Jan modeling workshop??

Folders to run retrospective model runs using the current model, i.e. base model 2020 (model_1)

Each folder is labeled with a year to reflect the "end yr" in that particular retrospective run.

In each year, .dat, ctl, and .prj files need to be updated to reflect those of data years.

Models housed in smbkc_20/retrospective_model_1

Use model_1 for this. Manually adjust data and .ctl file for this 

## folder structure - 
Each folder is labeld with the "year" for that retrospective run (i.e. when the model ends)

Each folder labeled "year_wo_survey" has that same retrospective run without the terminal survey data.
i.e. for 2019 this includes all the same data as 2019 but does not have 2019 trawl survey data - abundance or length comps.


## dat, ctl, prj files - FILEs UPDATED FOR EACH YEAR
.dat files lives in data folder in assessment root.  "smbkc_20/retrospective_model_1/retro_data"
.ctl and .prj files live in each "year" designated folder

### retrospective pattern analysis 
#### .dat file: 
For each year in the past remove the corresponding "new" data - both survey and catch in the .dat file, also change model end date. 
Changes: 
line 15 - end year (last year of fishery data in the model, i.e. 2019 model, this is 2018)
catch data - remove catch data from year remove (2019 model, no catch in 2019)
abundance data - remove abundance data (i.e. 2018 model remove 2019 survey data)
- both catch and abundance need to adjust associated row numbers above these data inputs.

#### .ctl file
Lines 140 to 172, adjust end year here to reflect particular model year, remember catch data is the previous year (i.e. 2019 has 2018/2019 catch data labeled 2018), whereas survey data is same as model year (i.e. 2019)

Line 276 - adjust last rec_dev year


#### .prj file 
Update end year ranges and bycatch year ranges in the first section. Update meanrecruitement years (Line 26)


### Retro without terminal survey year
.ctl and .prj files do NOT have to be changed
.dat file changes are reflected in new data file name (make sure to update gmacs.dat in each particular folder)


BEFORE RUNNING MODEL: 
update gmacs.dat with correct file names for .dat, .ctl, .prj

# Run the model
1) Using ADMB shell navigate to the folder for model_1
    ' cd Documents\BSAI_crab_assessments\SMBKC\smbkc_20\model_1'
    
2) type 'gmacs'

3) code should run

4) Open 'Gmacsall.out'
Look at model output - did model converge? how are parameter estimates? check all this and open plot.R to graphically view output.

NOTE: check on ssb output. only has 41 years instead of 42 - crab year so 1978 to 2018 does not included projection for 2019/20


# Summary files / figures 
Summarized data is saved in teh combined_data folder (which really should be combined results **FIX**).

Files with results in one file are "ssb_all.csv" and "summary.csv"
See R code "summary_figures_analysis.R" for more details


# Model issues with fofl and OFL estimation --
Some of the runs were not producing fofl and OFL estimates that were appropriate.  It appears that adding in a small insignificant catch in the prior year to that model allowed the OFL estimation to occur.  Why does GMACS need a small catch in some years and not others?  Troubleshoot this - add to GMACS jan workshop.
See .dat files for the following years as examples:
2018 - both
2019 - wo T
2014 - both