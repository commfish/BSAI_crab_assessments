read me for gmacs version updating - Oct 2024 / Nov 2024 / Feb 2025

https://github.com/GMACS-project/GMACS_tpl-cpp_code

Version 16 issues:
- not working for sept cycle 
- Buck's emails:
  "Apparently Andre didn't initialize a counter for the number of deviations associated with environmental links or random walks--the counter didn't always start from 0. That's fixed in the code I have now (I haven't pushed it to github pending further some further work--see below). In the ctl file, the "options for size composition data" should look something like: "options for size composition"
  where I've changed the last 4 numbers in the last line (were 0's) to 1's and 2's to indicate the associated survey Q's to use were the first and second defined in the catchability section (not at all clear). The "corrected" v16 file is attached."
  
- update size comps in aggregate to not be 0 for females

Version 16 oct update
- used updated .ctl file (see above)
- used updated .dat file with size comp samples sizes being divided by two and entered for both males and females. 
- Results of v16 oct 24

Jan modeling workshop -----------------------
- began with version 17, which is similar to 16
- currently comparing using model "23.0a_ph7_24"

- version 18 current
    - Diff in version 18 document
    
- BBRKC large difference is how these versions treat effective sample size of size comps

Version 2.10.20 (2-10-2025)
- should have been corrected so effective sample sizes can be the same as version 2.10.14
- run and check (feb 10, 2025)
- version 20 of 23.0a 
    - LARGE differences - ran using .dat file similar to ver.14 and .ctl from 18, then changed .ctl to have all 0's in q pointers - still LARGE differences and no changes
    - version 20 with ver. 18 data file - trying now