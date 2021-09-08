created 8-30-21
# bbrkc assist Jie with mcmc model runs 

## for VAST run
- see gmacs.dat for data, crl, prj files and jitter options

## runs for model 21.1
- see gmacs.dat for data, crl, prj files and jitter options

# Run mcmc for projections
Test mcmc run

gmacs -mcmc 500000 -mcsave 1000 -nox

After completion, then run:
gmacs -mceval

There are five files generated from this run:
mcount.rep
mcountPROJ.rep
mcountREC.rep
mcountREF.rep
mcountSSB.rep
gmacs.hst

VAST model
-------------------------------------------
--Start time: Mon Aug 30 08:47:10 2021

--Finish time: Fri Sep 03 00:42:44 2021

--Runtime: 87 hours, 55 minutes, 34 seconds
--Number of function evaluations: 505621
*******************************************


Model 21.1
-------------------------------------------
--Start time: Fri Sep 03 09:11:25 2021

--Finish time: Mon Sep 06 22:34:48 2021

--Runtime: 85 hours, 23 minutes, 23 seconds
--Number of function evaluations: 506067
*******************************************