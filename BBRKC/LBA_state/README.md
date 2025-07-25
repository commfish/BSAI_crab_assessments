K.Palof 7-13-2022/ 6-26-2023 / 7-16-2024 /7-23-25

LBA model for BBRKC used for the harvest strategy

Instructions from Jie in: BBRKC/readme/bbrkc-notes_kjp.docx
june/july update file to be ready for prelim data

## 2025 notes ----------
Prelim data from shannonn hennessy (sp?) - stored in folder see "prelim_data_survey_LBA.R"
open excel file - "LBA file_breakdown ....._2025.xlsx"

# prelim in rk25_prelim
prelim tabs in the above excel file
prelim_survey.dat tab - open "prelim_male_size_comp.csv" file - copy prelim size comps into column "BH"
see notes in file from Jie - 


## 2024 notes -----------
prelim data in BBRKC/data/survey/2024 but also instructions in "2023 folder" below

Prelim - data files in rk24_prelim
# 7-16-2024 prelim runs
how to get current year confidence intervals for females??? not working

- data prep in "LBA file_breakdown.....xlsx"
- prelim # also in excel file
- data file for prelim memo made from output using both files 
- How to deal with bootstrap for current year for females?



## 2023 notes --------

see C:\Users\kjpalof\Documents\Current projects\statewide shellfish\bbrkc\prelim LBA
for 2023 prelim data files
Program files are in rk23_prelim 
# 2023 notes
# catch.dat file?
- how to update this correctly - see excel fiel for what I did.

#2023 runs 
- all survey dat and catch updated.
- run males - got down to 0.323X04 for SSQ
- copied in estimated fishing mortality 

# runs 
		# v1 typical run for both males and females NO manipulation of recruits (mature female recruits is to low in last few years (3))
		# **USE** v2 JUST FEMALES -manipulation of female recruits towards the end of convergence (match 23_prelim v3)
		# DONT USE- v3 JUST FEMALES - adj R in last 3 years towards end - ended up with slightly higher SSQ - don't use
		# DONt USE -v4 JUST FEMALES - to high of recruitmentin 2023 way high estimate? not sure why

# document
		- just show v2 and survey data

# 7-11-23 prelim runs - have # individuals and raw size comps
# males run once - no removals included (need to update catch from bycatch and cost recovery)
# females run a couple times -
      # v1/v2 are similar using raw survey data and NO manupulation of recruitment in initial year
      # V3 uses average (last 3 years) size comps instead of survey raw
      # v4 has manipulation of recruitment initial estimate towards the end of convergence - these are to match what is likely as seen in V3
      # V5 reduces the raw # females caught by 141 to remove the large tow
# versions 3 and 4 are plotted in first draft of memo


GMACS model that's like the LBA???



## 2022 notes -----
Based input files on rk21n - assuming this is most updated values from 2021 *should confirm with Jie on this*

rk22_wrong (updated from rk21 which is NOT most recent)
rk22_run1_bound - wrong file updated in paramn and paramf files causing issues with final year recruitment hitting bounds - ARCHIVE this
rk22_run2_bound21 - issue with low recruitment estimate in 2021 so hitting bounds.

Run 3:
- recruitment estimate in year 50 - change in running of the file to a value similar to last years estimate (0.247), 
	- change paramf.dat line 67 second entry to 10 instead of 50
	

** FIX **
- - update catch.dat file with small directed fishery catch from 2021 catch data (2021/22 season)