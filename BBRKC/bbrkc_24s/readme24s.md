# BBRKC May 2024 SAFE models (1-11-24,4-15-24 )
katie.palof@alaska.gov


# CPT/SSC comments  -----
see SAFE document and my notes in OneNote

## Running model
1) Update gmacs.exe with updated version via github. Make sure to update .dat and .ctl files to reflect changes in the program and input files - see "C:\Users\kjpalof\Documents\Rprojects\GMACS-project\GMACS_Assessment_code\GMACS_versions\Latest_Version" for latest version of GMACS. Make sure to "pull" this project first before following "katie" R script in R script file to compile
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
Model explorations from comments:

23.0a - "base" with estimated M for males, prior is tight - mean 0.18, CV 0.04 see Jie's 22.0a from May 2022
21.1b - updated "base" model, new bycatch data and updated GMACS version (see github)
21.1b_ph7 - base + ssb in season 7
23.0a_ph7 - base + ssb in season 7
24.0 - 23.0a_ph7 + selectivity informed by BSFRF (like snow crab)
24.0b - 21.1b_ph7 + selectivity informed by BSFRF (like snowc crab)

# Document updates ----------
SAFE document is in Rmarkdown format. Need to work on all tables, etc.
see "figures_tables_create_SAFE.R" and "Jie_figures_tables_create_SAFE.R" for creation of figures and tables. These also refer to some of Jie's code which is in folder 'BBRKC/code/Jie R code'.
Other code for figures or analysis is found in 'BBRKC/code'
Full markdown document was successful in 2023 but still some formatting with figures, etc. needed.

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
'figures_tables_create_SAFE.R' in bbrkc23f has figures created with 'gmr'
'Jie_figures_tables_create_SAFE.R' in bbrkc 23f has notes on each figure and what is used to create it, if it's Jie code see "Jie_cmn_files.R"

# retrospective run -------------
Thinking this should be done for a few models to compare with base?
Try with '-nohess' added to avoid error I'm getting in the resent version of GMACS.
Performed on all 3 models. Ran with "gmacs -nohess" to avoid errors - need to track these down.

### retrospective run code is found in folder (here bbrkc_23f) under each models name

## jitter ----
Tyler created functions to run jitter code in GMACS and save results, see file 'gmacs_functions.R'

## model runs - initial to compare to Jie's ------
### Model 21.1b - run 4-14-23; see gmacs.dat -
Runtime: 0 hours, 47 minutes, 59 seconds
Compare to Fall 2022 model 21.1b - no new data just new GMACS version - see if differences in output


### Model 22.0 
updated .dat and .ctl files to reflect new GMACS version. 

### Model 23.0a
updated .ctl, .dat file is the same as 21.1b due to same model just estimated M for males here.



###Run mcmc for projections
** add an estimate of catch in 2023/2024 for the forecast year for the projections to work**

Test mcmc run

'gmacs -mcmc 10 -mcsave 3' make sure .psv file is created (model 21.1b this takes - 42 mins.)
(model 22.0 - run on Siddeek laptop - time? )

(SMBKC  ::: 'gmacs -mcmc 1000000 -mcsave 1000' - started at 11:19am 8-19-19, ended 12:44 pm)

BBRKC 21 :::: # Run mcmc for projections
Test mcmc run (see above)

gmacs -mcmc 500000 -mcsave 1000 -nox (this means don't show vector and gradient values in function minimizer screen report)
(see below for time)

# for projections
Need to run mcmc above to get .psv file for projections
After completion, then run:

1) Update .prj file with the projection that you're running (see .prj file)
check Jie's prj file from 2021 - see what his parameters are here
- how many iterations to run? Jie had anywhere from 1,000 to 10,000
- try 10,000 - how long 21.1b 4:30pm

2) 'gmacs -mceval'

There are five files generated from this run:
mcount.rep
mcountPROJ.rep
mcountREC.rep
mcountREF.rep
mcountSSB.rep
gmacs.hst

#### projection summaries 
figures, etc. created in BBRKC/code/projection_summary_figures.R


These times are for 2022 but similar times for 2023 for all models.
## mcmc model 21.1b ---
-------------------------------------------
--Start time: Mon Aug 22 01:28:13 2022

--Finish time: Thu Aug 25 19:55:46 2022

--Runtime: 90 hours, 27 minutes, 33 seconds
--Number of function evaluations: 506463
*******************************************