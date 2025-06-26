# bbrkc harvest strategy CSA model RTMB v1
# .ctl file

# time block set up
# number of block groups
1
# block defintions (start year of each block)
1980 1985

# parameter inputs (not all are blockable)
# init: starting value
# lb: lower bound
# ub: upper bound
# block: block group
# map: 0 - estimated, 1 - fix at initial value

# thetas 
# init lb ub map
11.0 2.000  20  0 # log_initial_abundance
4.5  0.001  20  1 # rec_dist_alpha
0.8  0.001  20  1 # rec_dist_beta
10   0.001  100 0 # log_recruitment_1973
10   0.001  100 0 # log_recruitment_1974
10   0.001  100 0 # log_recruitment_1975
10   0.001  100 0 # log_recruitment_1976
10   0.001  100 0 # log_recruitment_1977
10   0.001  100 0 # log_recruitment_1978
10   0.001  100 0 # log_recruitment_1979
10   0.001  100 0 # log_recruitment_1980
10   0.001  100 0 # log_recruitment_1981
10   0.001  100 0 # log_recruitment_1982
10   0.001  100 0 # log_recruitment_1983
10   0.001  100 0 # log_recruitment_1984
10   0.001  100 0 # log_recruitment_1985
10   0.001  100 0 # log_recruitment_1986
10   0.001  100 0 # log_recruitment_1987
10   0.001  100 0 # log_recruitment_1988
10   0.001  100 0 # log_recruitment_1989
10   0.001  100 0 # log_recruitment_1990
10   0.001  100 0 # log_recruitment_1991
10   0.001  100 0 # log_recruitment_1992
10   0.001  100 0 # log_recruitment_1993
10   0.001  100 0 # log_recruitment_1994
10   0.001  100 0 # log_recruitment_1995
10   0.001  100 0 # log_recruitment_1996
10   0.001  100 0 # log_recruitment_1997
10   0.001  100 0 # log_recruitment_1998
10   0.001  100 0 # log_recruitment_1999
10   0.001  100 0 # log_recruitment_2000
10   0.001  100 0 # log_recruitment_2001
10   0.001  100 0 # log_recruitment_2002
10   0.001  100 0 # log_recruitment_2003
10   0.001  100 0 # log_recruitment_2004
10   0.001  100 0 # log_recruitment_2005
10   0.001  100 0 # log_recruitment_2006
10   0.001  100 0 # log_recruitment_2007
10   0.001  100 0 # log_recruitment_2008
10   0.001  100 0 # log_recruitment_2009
10   0.001  100 0 # log_recruitment_2010
10   0.001  100 0 # log_recruitment_2011
10   0.001  100 0 # log_recruitment_2012
10   0.001  100 0 # log_recruitment_2013
10   0.001  100 0 # log_recruitment_2014
10   0.001  100 0 # log_recruitment_2015
10   0.001  100 0 # log_recruitment_2016
10   0.001  100 0 # log_recruitment_2017
10   0.001  100 0 # log_recruitment_2018
10   0.001  100 0 # log_recruitment_2019
10   0.001  100 0 # log_recruitment_2020
10   0.001  100 0 # log_recruitment_2021
10   0.001  100 0 # log_recruitment_2022
10   0.001  100 0 # log_recruitment_2023
10   0.001  100 0 # log_recruitment_2024

# natural mortality 
# init lb ub block_grp map
0.227 0.01 2 1 1 # natural mortality base

# extra natural mortality pars
# init lb ub map
1.039 0.01 2 1
0.227 0.01 2 1

# molt probability
# init lb ub block_grp map
358930.1    0.01  1e6  1  1 # molt_alpha
     0.082  0.01    2  1  1 # molt_beta

# extra molt probability pars
# init lb ub map
 20584.94   0.01  1e6  1 # molt_alpha_blk_2
295159.60   0.01  1e6  1 # molt_alpha_blk_3
     0.077  0.01    2   1 # molt_beta_blk_2
     0.089  0.01    2   1 # molt_beta_blk_3

# growth
# can only have one transition matrix (i.e., no blocks)
# init lb ub map
0.228 0.001 10 1 # gscale

# molt increments
# init lb ub map
16.2 0.001 100 1 # molt_inc_1
16.1 0.001 100 1 # molt_inc_2
16.1 0.001 100 1 # molt_inc_3
16.0 0.001 100 1 # molt_inc_4
16.0 0.001 100 1 # molt_inc_5
15.9 0.001 100 1 # molt_inc_6
15.8 0.001 100 1 # molt_inc_7
15.8 0.001 100 1 # molt_inc_8
15.7 0.001 100 1 # molt_inc_9
15.7 0.001 100 1 # molt_inc_10
15.6 0.001 100 1 # molt_inc_11
15.6 0.001 100 1 # molt_inc_12
15.5 0.001 100 1 # molt_inc_13
15.5 0.001 100 1 # molt_inc_14