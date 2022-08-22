# BSAI_crab_assessments
Bering Sea Aleutian Island crab assessments performed by ADF&G staff. 
As of Sept. 2022 the following stock assessments are here:
     
SMBKC - Tier 4 assessment using GMACS

PIGKC - Tier 5 assessment using average historical catch.

BBRKC - Tier 3 assessment using GMCAS

Goal would be for all BSAI_crab_asessments that use GMACS to live here on github 


# GMACS - read first!
 
Models here used GMACS .tpl file to run the model code therefore the most current GMACS .tpl file must be in this folder.

before beginning models download most recent GMACS .tpl file into the /GMACS folder. 

Currently up-to-date 18-07-2019
Instructions:
- go to https://github.com/seacode/gmacs/tree/develop/src
- download any updates to this folder (/src) - most likely this would be updates to gmacs.tpl or libraries
- make sure current versions are stored in /GMACS/src in this project folder
- navigate to GMACS/src folder with ADMB shell 'cd Documents\BSAI_crab_assessments\GMACS\src'
- type 'make.bat'

This should create libraries and gmacs.exe needed to run models.

13-11-2019
updates to .ctl file to run with newest versino of .tpl with Andre and Jie changes.  See folder smbkc_19a for updated .ctl and associated gmacs.exe
