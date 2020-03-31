# St. Matthews Blue King Crab
Modified from 2018 model version with updates for rebuilding


# Base model - model 1 
From Andre with projection options available. 
Model 1 from 2018a - see smbkc_18a/model_1
model for smbkc for 2018a model framework and setup are pulled from 'gmacs'

** update: Stock is considered rebuilt once the Bmsy is above the proxy (NOT 2 years like originally plotted)

# Current model 
from gmacs/examples/smbkc_18a/model_1 
was model 3 from 2018 assessment

# GMACS version
gmacs.tpl file from Jie on 4-8-19 copied gmacs.exe from folder 
(local only: C:\Users\kjpalof\Documents\SMBKC\breakpoint_SR\breakpoint_smbkc\model_1_2018a))
gmacs.tpl in projections folder, version with Jie's edits 4-5-19

# Projections 
projections
    1 mean recruitment 1979-2017
    2 ricker recruitment
    3 B-H recruitment
    4 mean recruitment 1996-2017
    5 mean recruitment 1996-2017, Bmsy from 1996-2017
    6 mean recruitment 1999-2008
    7 mean recruitment 1989-2017

a) No bycatch mortality, NO state harvest policy implemented
b) Bycatch mortality average last 5 years, NO state harvest policy implemented
c) No bycatch mortality, state harvest policy implemented
d) Bycatch mortality average last 5 years, state harvest policy implemented

## Rebuilding Alternatives 
All with random recruitment draws from the time period indicated.

Projection Model        | BMSY proxy years  | recruitment years | Description
------------- 			| -------------
projection 1      		| 1978-2017			|   1978-2017		| Using entire time frame for both BMSY proxy and recruitment draws		    
projection 5   			| 1996-2017			|	1996-2017		| Using current "regime" from breakpoint analysis for both 

(similar to projection version d in May document)
	- both have average bycatch from the last 5 years (2013-2017)
	- Use state harvest policy 

## Sensitivity to bycatch mortality

option  |	Description
--------|	--------------
aa    	| 	Groundfish bycatch max that it's every been - see Table 6 2018 SAFE document - 2007
bb	  	|	  other levels of bycatch???

-conclusions: increased bycatch to 2007 levels reduces the time to rebuilding over 10 years for both proj 1 and 5, the magnitude is greater for 5.


## May 2019 meeting notes ::
# Summary
Bycatch mortality does NOT influence projection results

1 Assumes mean recruitment from the entire time series (1978-2017)
Tmin - under no directed fishing - 7.5 years
        - F=0.18 not rebuilt
        - State harvest policy implemented - T min = 11.5 years

2 Assumes ricker stock recruit relationship 
Tmin - under no directed fishing - 16.5 years
        - F=0.18 not rebuilt
        - State harvest policy implemented - T min = 28.5 years

3 Assumes B-H stock recruit relationship
Tmin - under no directed fishing - 14.5 yeA ars
        - F=0.18 not rebuilt
        - State harvest policy implemented - T min = 23.5 years

4 Assumes mean recruitment from the recent break in recruitment 1996-2017
Not rebuilt in 50 years....no matter the F used. 
    e - 75 years from 4d
    f -  years from 4d - the 100 year projections aren't working...why?

5 Assumes mean recruitment from recent break 1996-2017, also uses this time frame for BMSY proxy (1996-2017)
Tmin - under no directed fishing - 10.5 years
        - F = 0.18 not rebuilt
        - State harvest policy implemented - Tmin = 10.5 years


# weighted combination


# To do / Issues
More projection models?
Weighted combinations?