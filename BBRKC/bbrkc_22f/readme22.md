# BBRKC Sept. 2022 Final SAFE 
katie.palof@alaska.gov


# CPT/SSC comments  -----


## Running model
1) Jie's 'gmacsbase.tpl' and 'personal.tpl' obtained 4-5-22
    add these to folder with other files needed to compile (see Jan modeling workshop files)
2) change the 'XX.prj' files to bbrkc.prj for that model run
3) update 'gmacs.dat' to reflect the model run files

## general model run -----
See instructions under SMBKC
Instructions:
gmacs.dat and gmacs.exe need to be in same folder. Navigate to this folder in CMD (I have to used the CMD linked to ADMB because I don't have ADMB installed via admin)
In the folder type - 'gmacs'
Model should run if input files are read in correctly.

'gmacs.dat' in each folder has files used for each model run and other instructions - jitter, retrospective, etc.



## Model options ------
Three models recommended from May 2022:

21.1b - updated "base" model, new bycatch data and updated GMACS version (2.01 E)
22.0 - "base" starting from 1985 - should have different control file
     - Does it have a different .dat file or .prj????
22.0a - "base" starting in 1985 + estimating M for males

** if I have time should I do 21.1b with estimated M????

## GMACS versions ----
Current models are using same version as May models - gmacs.exe taken from 'BBKRC/src_bbrkc_040622'

- this is version 2.01E 
  - it does NOT include updates made by Andre in April/May for AIGKC - therefore may not be correct in OFL calcs.
  - need to update GMACS and run with updated version - see GMACS folder under the BSAI project

## organization -----
Description of models: found in this readme.md
files used for model: found in the gmacs.dat file in each folder, also have a file labeled gmacsXXX.dat with XX being model number





## model runs - initial to compare to Jie's ------
### Model 21.1b - run 7-25-22; see gmacs.dat - no additional output/jitter/or anything, just basic model run
Time for run:
Compare results to Jie's - gmacsall.out 
Notes: 

### Model 22.0 



