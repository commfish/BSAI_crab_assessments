# bbrkc 2022 May 
(Jie ran models, and Katie will present to CPT in May 2022)

## Running model
1) Jie's 'gmacsbase.tpl' and 'personal.tpl' obtained 4-5-22
    add these to folder with other files needed to compile (see Jan modeling workshop files)
2) change the 'XX.prj' files to bbrkc.prj for that model run
3) update 'gmacs.dat' to reflect the model run files


## Model options

### model 21.1
Base model 
### model 21.1a
removing BSFRF survey 
comment out BSFRF survey data, remove length comp indexes
remove BSFRF name for survey 


ctl file:
line 273 - comment out BSFRF and gear 6 throughout the matrices
M fixed 
Now higher M in later period
line 476
line 479 cannot be larger than the terminal year
line 489 phase -99 brings it back to fixed 0.18 value

### model 22.0
starts the model at 1985

model with M estimated between 2015 and 2019
- low recruitment, so higher natural on older crab 

Jie has models that estimate M and models that fix it at 0.18
- estimated at .225 
- 

ctl file:
starts from 1985 
change intial values - Jie had to play around with this
changes to selectivity and molting probability
changes to Natural mortality
