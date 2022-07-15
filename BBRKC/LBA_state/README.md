K.Palof 7-13-2022

LBA model for BBRKC used for the harvest strategy

Instructions from Jie in: BBRKC/readme/bbrkc-notes_kjp.docx

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