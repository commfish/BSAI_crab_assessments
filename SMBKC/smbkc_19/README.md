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

## dat, ctl, prj files
.dat file lives in data folder in assessment root.  
Update annually - 
.ctl file updated for model_1 in that folder with new year ranges, etc. each model needs the .ctl file updated.

