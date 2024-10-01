# k.palof   9-28-23/ 8-27-24 / 9-30-24

# code to extract values for other uses

# Model or Model(s) plotted here: 
# Stock: BBRKC
# Year and timing: 2024  - models for Sept 2024
# Model: model 23.0a CPT recommended model sept 2023


# load ------------
#require(devtools)
#devtools::install_github("seacode/gmacs", subdir = "/gmr", ref = "develop") #, INSTALL_opts="--no-staged-install") #- only needs to be performed once, but needs to be 
# done again when updates are made to the package
# if gmr is updated and above doesn't work try:
# go to Build above - "configure build tools" - direct it to the gmr folder - press OK. 
# Over on right in the Build tab (upper right hand side) - 
# click "install and restart"

#library(gmr)#require(gmr)
source("./SMBKC/code/functions.R") 
source("./SMBKC/code/helper.R") 
source("./SMBKC/code/gmr_functions2020.R") 
source("./gmacsr/gmacsr.R")
# Model 1 plots -------------------------
cur_yr <- 2024 # update annually 
cur_yr <- 2024 # update annually 
folder <- "bbrkc_24f" # update annually 

.TABS     = c("./BBRKC/bbrkc_24f/doc/tables/")
.FIGS     = c("./BBRKC/bbrkc_24f/doc/figures/")

ww <- 6
hh <- 5


m230a_24 <- gmacs_read_allout(file = "./BBRKC/bbrkc_24f/model_23_0a_ph7_24/Gmacsall.out", model_name = "m23.0a", 
                              version = "2.20.14")

m24c <- gmacs_read_allout(file = "./BBRKC/bbrkc_24f/model_24_0c/Gmacsall.out", model_name = "m24.0c", 
                          version = "2.20.14")

B <- read_rep("./BBRKC/bbrkc_23f/model_23_0a/gmacs.rep")

#--------------------------------------------------------------
# recruitment ---------------
#
# get derived quantity summary
deriv.quant <- gmacs_get_derived_quantity_summary(all_out = list(m230a_24))

deriv.quant %>% 
  select(year, recruit_female, recruit_male) %>% 
  group_by(year) %>% 
  mutate(totalr = (recruit_female+recruit_male)/1000000) %>% # in millions of crab - total both males and females
  mutate(year = year+1) %>% # see Jie's notes on recruitment being the following year
  select(year, totalr) ->kemp

# mean recruitment 
#d1f<-mean(rec[9:(n3-1)])
# mature females ----
# model mature females model 23.0a
fem1 <- as.data.frame(m230a_24$n_matrix)
fem1 %>% 
  select(year, size, females) %>% 
  mutate(size = as.numeric(size)) %>% 
  filter(size >= 92.5, size < 147.5) %>% # just mature size classes 6 to 16 exclude first 5
  group_by(year) %>% 
  summarise(mat_total = sum(females)/1000000) %>% # millions of females
  mutate(model = "m230a") -> mat_fem

# this is number greater than 90mm CL which is Jie's currency
# this is #'s greater than 90 mm CL


### combine rec and mature f --------
mat_fem %>% 
  merge(kemp, all = T) -> bbrkc_abundance

#bbrkc_abundance <- data.frame(year = f_year, mat_fem_GE90 = f_temp2_sum$totalGE90)
#head(bbrkc_abundance)

#bbrkc_abundance %>% 
#  left_join(total_rec) -> combine_all # these are all in milions of individuals - abundance
write.csv(bbrkc_abundance, "./BBRKC/bbrkc_24f/_23_0a_recruit_mfem_out.csv")


# mature and legal abundance ------------
deriv.quant <- gmacs_get_derived_quantity_summary(all_out = list(m24c))

head(deriv.quant)

deriv.quant %>% 
  select(year, ssb, ssa) -> mmb_mma_24c

proj <- gmacs_get_ref_points(all_out = list(m24c))
N_all <- m24c$n_matrix
N_all %>% 
  select(year, size, males) %>% 
  mutate(size = as.numeric (size)) %>% 
  filter(size >= 120) %>% 
  group_by(year) %>% 
  summarise(mma = sum(males)) -> mma_N_matrix


# Jie's old method but doesn't currently work ------
# legal abundance
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


