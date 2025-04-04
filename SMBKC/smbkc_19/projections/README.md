# St. Matthews Blue King Crab Projections - for rebuilding analysis 2019
Using 2019 models - specifically model 1- the base model


# Base model - model 1 

Stock is considered rebuilt once the Bmsy is above the proxy (NOT 2 years like originally plotted)

## Rebuilding Alternatives 
All with random recruitment draws from the time period indicated.

Projection Model    | BMSY proxy years  | recruitment years | Description
------------- 			| -------------
projection 1      	| 1978-2018			    |   1978-2018		    | Using entire time frame for both BMSY proxy and recruitment draws		    
projection 5   			| 1996-2018			    | 	1996-2018		    | Using current "regime" from breakpoint analysis for both 

projection 2        | 1978 - 2018       |   1978-2018       | Uses Ricker stock-recruit for recruitment 
projection 4        | 1978 - 2018       |   1996-2018       | random draws from previous recruitment (current "regime") by BMSY                                                             |  entire time series                                    
(similar to projection version d in May document)
	- both have average bycatch from the last 5 years (2014-2018)
	- Use state harvest policy 

## Sensitivity to bycatch mortality

option  |	Description
--------|	--------------
aa    	| 	Groundfish bycatch max that it's every been - see Table 6 2018 SAFE document - 2007
d 	  	|	  (from May 2019) average bycatch last 5 years 2014-2018
b       |   average bycatch last 5 years AND F = M INSTEAD of State harvest policy (only run for proj 2)
abc     |   similar to b, instead of F=M, it's F=M*0.75 - roughly assuming a 25% buffer on OFL so ABC
SHP     |   changes the state harvest strategy to have a threshold of 75% of MMA before opening

-conclusions: increased bycatch to 2007 levels reduces the time to rebuilding over 10 years for both proj 1 and 5, the magnitude is greater for 5.


!! IMPORTANT !!
See readme.md under smbkc_19  for instructions on running projections. 

copy files from model_1 (current accepted model) into temp folder. Place in the correct .prj file and then run gmacs.exe here. Move these files into the correct projection folder (see other readme.md)