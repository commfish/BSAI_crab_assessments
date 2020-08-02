original 8-2-2020
# St. Matthews Blue King Crab 2020 - Retrospective model runs - done manually 
**fix** need to change this so GMACS can do it automatically - Jan modeling workshop??

Folders to run retrospective model runs using the current model, i.e. base model 2020 (model_1)

Each folder is labeled with a year to reflect the "end yr" in that particular retrospective run.

In each year, .dat, ctl, and .prj files need to be updated to reflect those of data years.

Models housed in smbkc_20/retrospective_model_1


## dat, ctl, prj files - FILE UPDATES ANNUALLY
.dat file lives in data folder in assessment root.  "smbkc_20/retrospective_model_1/retro_data"

Update annually - 
.ctl file updated for model_1 in that folder with new year ranges, etc. each model needs the .ctl file updated.

update gmacs.dat with correct file names for .dat, .ctl, .prj


# Run the model
1) Using ADMB shell navigate to the folder for model_1
    ' cd Documents\BSAI_crab_assessments\SMBKC\smbkc_20\model_1'
    
2) type 'gmacs'

3) code should run

4) Open 'Gmacsall.out'
Look at model output - did model converge? how are parameter estimates? check all this and open plot.R to graphically view output.

NOTE: check on ssb output. only has 41 years instead of 42 - crab year so 1978 to 2018 does not included projection for 2019/20


# retrospective pattern analysis 
Use model_1 for this. Manually adjust data and .ctl file for this 



