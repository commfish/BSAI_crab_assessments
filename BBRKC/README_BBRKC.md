# Bristol Bay red king crab 
# 7-30-24/6-20-25

## Current assessment authors are:
  Katie Palof 
  
  Jie Zheng (historic)
  

## Folders here:
"f" final or fall assessment
"s" spring - May model runs

code: code for processing data etc. things that aren't related to specific model years. Maybe figure/tables code should live here?
annual code for figures and tables is found in each folder for that year 

- files:
    - "bbrkc_rema.R" - use this with survey data to get input for rema results
    - Survey data now is from seperate project -"EBSsurvey_analysis.R" - survey data and size comp figures. 
    - "bbrkc_sizecomp.R" - size comps for input into .dat
    - "bycatch_groundfish.R" - process groundfish bycatch
    - OLD see "Jie_figures_tables_create_SAFE.R" for where each figure is created...These need to all be in one file eventually
    - 'doc_figures_tables_gmacsr.R' for figure and table creation

data: data foldered by year. Updates from survey, observer data, catch data, groundfish bycatch
- survey data updated from - EBSsurvey_analysis.R
- size comps from 'bbrkc_sizecomps.R'


readme: files from Jie, background files, Excel summary files I inherited from Jie

LBA_state: length-based model for harvest strategy decisions

bbrkc_22: contains the May 2022 run for bbrkc

bbrkc_22f: final model runs for Sept. 2022

