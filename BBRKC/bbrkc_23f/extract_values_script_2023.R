# k.palof   9-28-23

# code to extract values for other uses

# Code for plotting output of GMACS models taken from SMBKC
# Taken from Jim Ianellii https://github.com/seacode/gmacs/tree/develop/examples/smbkc_18a/model_1 but updated 

# Model or Model(s) plotted here: 
# Stock: BBRKC
# Year and timing: 2023  - models for Sept 2023
# Model: model 23.0a CPT recommended model sept 2023


# load ------------
#require(devtools)
#devtools::install_github("seacode/gmacs", subdir = "/gmr", ref = "develop") #, INSTALL_opts="--no-staged-install") #- only needs to be performed once, but needs to be 
# done again when updates are made to the package
# if gmr is updated and above doesn't work try:
# go to Build above - "configure build tools" - direct it to the gmr folder - press OK. 
# Over on right in the Build tab (upper right hand side) - 
# click "install and restart"

library(gmr)#require(gmr)
source("./SMBKC/code/functions.R") 
source("./SMBKC/code/helper.R") 
source("./SMBKC/code/gmr_functions2020.R") 

# Model 1 plots -------------------------
cur_yr <- 2023 # update annually 

mod_names <- c("model 23.0a")
.MODELDIR = c("./BBRKC/bbrkc_23f/model_23_0a/") # directory where the model results are
.THEME    = theme_bw(base_size = 12, base_family = "")
.OVERLAY  = TRUE
.SEX      = c("Aggregate","Male", "Female")
.FLEET    = c("Pot","Trawl bycatch", "Tanner bycatch", "Fixed bycatch","NMFS Trawl","BSFRF survey")
.TYPE     = c("Retained","Discarded", "Retained & Discarded")
.SHELL    = c("Aggregate","New", "Old")
.MATURITY = c("Aggregate")
.SEAS     = c("Annual")
.FIGS     = c("./BBRKC/bbrkc_23f/doc/figures/")
#.FILES    = c("./SMBKC/smbkc_20/retrospective_model_1/combined_data/")

fn       <- paste0(.MODELDIR, "gmacs")
M        <- lapply(fn, read_admb) #need .prj file to run gmacs and need .rep file here
names(M) <- mod_names

ww <- 6
hh <- 5

B <- read_rep("./BBRKC/bbrkc_23f/model_23_0a/gmacs.rep")

#--------------------------------------------------------------
# legal abundance ------------
f_temp <- B$N_males
head(f_temp)
# V1 to V20 represent size groups, legal is > 134, so V 15 to V20
# mature > 119
# 65 70 75 80 75 90 95	100	105	110	115	120	125	130	135	140	145	150	155	160

f_temp %>% 
  as.data.frame() %>% 
  mutate(total_mature = (V12 +V13 +V14 +V15 +V16 +V17 +V18 +V19 +V20)/1000000, 
         total_legal = (V15 +V16 +V17 +V18 +V19 +V20)/1000000) %>% 
  mutate(year = c(1975:2023)) %>% 
  select(year, total_mature, total_legal) -> est_number_males# abundance in millions of crab 

#estimated natural mortality on legals
est_number_males %>% 
  mutate(legal_y2 = total_legal*(exp(-0.23)), 
         legal_die = total_legal - legal_y2) #,
         #mature_harvest = total_mature*(1- exp(-.149)))


# recruitment ---------------
#raw_data <- data_out(mod_names[1], .MODELDIR[1])
#rec <- get_rec_out(mod_names[1:2], raw_data, M[1:2])
n1<-3
n2<-1
n3<-B$nyr-B$syr+1
#year<-c(1:n3) # number of years
year <- c(1976:2023) # does this start at 75 or 76? and go to 2021 or 2022??? check on GMACS here
#n4<-C$nyr-C$syr+1
rec<-(B$recruits[1,]+B$recruits[2,])/1000000 # in millions of crab - total both males and females

total_rec <- data.frame(year, rec) # total per year

# mean recruitment 
d1f<-mean(rec[9:(n3-1)])


# mature females ----
# model 21.1b mature females 
f_temp <- B$N_females_mature
head(f_temp)
f_temp %>% 
  as.data.frame() %>% 
  mutate(total_mature = rowSums(across(where(is.numeric)))/1000000, # this doesn't match what Jie has for mature females....
         totalGE90 = (V6+ V7 + V8 + V9 + V10 + V11 +V12 +V13 +V14 +V15 +V16)/1000000) # this doesn't match what Jie has for mature females....

# this is number greater than 90mm CL which is Jie's currency
# this is #'s greater than 90 mm CL
f_temp2 <- B$N_females
f_temp2 %>%    
  as.data.frame() %>% 
  mutate(totalGE90 = (V6+ V7 + V8 + V9 + V10 + V11 +V12 +V13 +V14 +V15 +V16)/1000000) -> f_temp2_sum
tail(f_temp2_sum)
f_year = c(1975:2023)

### combine rec and mature f --------
total_rec

bbrkc_abundance <- data.frame(year = f_year, mat_fem_GE90 = f_temp2_sum$totalGE90)
head(bbrkc_abundance)

bbrkc_abundance %>% 
  left_join(total_rec) -> combine_all # these are all in milions of individuals - abundance
write.csv(combine_all, "./BBRKC/bbrkc_23f/_23_0a_recruit_mfem_out.csv")
