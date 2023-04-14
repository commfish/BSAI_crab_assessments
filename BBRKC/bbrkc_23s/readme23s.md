# BBRKC May 2023 proposed models (4-12-23)
katie.palof@alaska.gov


# CPT/SSC comments  -----


## Running model
1) Update gmacs.exe with updated version via github. Make sure to update .dat and .ctl files to reflect changes in the program and input files
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
     - also has different .dat and .prj file (see folder in bbrkc_22f)
23.0 - "base" with estimated M for males, prior??? see Jie's 22.0a model
23.1 - "Q" work. relax prior on Q 
23.2 - explore contribution of retow data - run model excluding all retow data in years with retow.

## GMACS versions ----
Last Sept models used version from May 2022 models - gmacs.exe taken from 'BBKRC/src_bbrkc_040622'
- this is version 2.01E 
  - it does NOT include updates made by Andre in April/May for AIGKC - therefore may not be correct in OFL calcs.
  - need to update GMACS and run with updated version - see GMACS folder under the BSAI project

May 2023 models should be using version: 
- includes updates from Andre, Mattheiu and merged snow crab changes - see GitHub https://github.com/GMACS-project

## organization -----
Description of models: found in this readme.md
files used for model: found in the gmacs.dat file in each folder, also have a file labeled gmacsXXX.dat with XX being model number


# figures and tables ------
'figures_tables_create_SAFE.R' in bbrkc22f has figures created with 'gmr'
'Jie_figures_tables_create_SAFE.R' in bbrkc 22f has notes on each figure and what is used to create it, if it's Jie code see "Jie_cmn_files.R"



## model runs - initial to compare to Jie's ------
### Model 21.1b - run 7-25-22; see gmacs.dat - no additional output/jitter/or anything, just basic model run
Time for run: Runtime: 0 hours, 28 minutes, 23 seconds
Compare results to Jie's - gmacsall.out 
Notes: 
- first and last line of 'gmacsall.out' match - dig deeper later

### Model 22.0 
Time for run: Runtime: 0 hours, 25 minutes, 12 seconds
Compare results to Jie's - gmacsall.out 
Notes: 
- first and last line of 'gmacsall.out' match - dig deeper later

### Model 22.0a 
Time for run: Runtime: 0 hours, 25 minutes, 42 seconds
Compare results to Jie's - gmacsall.out 
Notes: 
- first and last line of 'gmacsall.out' match - dig deeper later



###Run mcmc for projections
Test mcmc run

'gmacs -mcmc 10 -mcsave 3' make sure .psv file is created (model 21.1b this takes - 42 mins.)

(SMBKC  ::: 'gmacs -mcmc 1000000 -mcsave 1000' - started at 11:19am 8-19-19, ended 12:44 pm)

BBRKC 21 :::: # Run mcmc for projections
Test mcmc run (see above)

gmacs -mcmc 500000 -mcsave 1000 -nox
(see below for time)

# for projections
Need to run mcmc above to get .psv file for projections
After completion, then run:

1) Update .prj file with the projection that you're running (see .prj file)
check Jie's prj file from 2021 - see what his parameters are here

2) 'gmacs -mceval'

There are five files generated from this run:
mcount.rep
mcountPROJ.rep
mcountREC.rep
mcountREF.rep
mcountSSB.rep
gmacs.hst


## mcmc model 21.1b ---
-------------------------------------------
--Start time: Mon Aug 22 01:28:13 2022

--Finish time: Thu Aug 25 19:55:46 2022

--Runtime: 90 hours, 27 minutes, 33 seconds
--Number of function evaluations: 506463
*******************************************