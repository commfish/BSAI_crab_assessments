# Kpalof
# figures _tables_create SAFE using 'gmacsr'

# 4-12-24 / 6-13-24 / 7-10-24
# load ----------
#library(gmr) #require(gmr)
#source("./SMBKC/code/functions.R") 
#source("./SMBKC/code/helper.R") 
#source("./SMBKC/code/packages.R")
#source("./SMBKC/code/gmr_functions2020.R") 
#source("./BBRKC/code/bbrkc_functions.R")
source("./gmacsr/gmacsr.R")
# **************************************************************************************************
cur_yr <- 2024 # update annually 
folder <- "bbrkc_24f" # update annually 
# The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#D55E00", "#0072B2", "#CC79A7", "#F0E442")

#plot.dir <- paste0(here::here(), "/BBRKC/", folder, "/doc/figures/")
plot_save <- paste0(here::here(), "/BBRKC/", folder, "/doc/figures/")
#plot_save_sel <- paste0(here::here(), "/BBRKC/", folder, "/doc/figures/sel_models/")
#plot_save_molt <- paste0(here::here(), "/BBRKC/", folder, "/doc/figures/molt_models/")
#table directory 
.TABS     = c("./BBRKC/bbrkc_24f/doc/tables/")

## read in models
# for fall 2024 need:
##      - 23.0a_p7 as base (2023 and 2024 version)
##      - 24c base with less molting time period
##      - REMA model (as appendix look for in script "BBRKC/code/bbrkc_rema.R")

#m230a_p7 <- gmacs_read_allout(file = "./BBRKC/bbrkc_24s/ADJ_model_23_0a_ph7/Gmacsall.out", model_name = "m23.0a.p7", 
#                              version = "2.01.M.10")
m230a_23 <- gmacs_read_allout(file = "./BBRKC/bbrkc_24f/model_23_0a_ph7_23/Gmacsall.out", model_name = "m23.0a_23", 
                              version = "2.20.14")

m230a_24 <- gmacs_read_allout(file = "./BBRKC/bbrkc_24f/model_23_0a_ph7_24/Gmacsall.out", model_name = "m23.0a", 
                              version = "2.20.14")

m24c <- gmacs_read_allout(file = "./BBRKC/bbrkc_24f/model_24_0c/Gmacsall.out", model_name = "m24.0c", 
                          version = "2.20.14")


#m24.0 <- gmacs_read_allout(file = "./SMBKC/smbkc_24s/model_16_0_c/Gmacsall.out", model = "smbkc24.0")

## read std files -------------------

m230a_23_std <- gmacs_read_std(file = "./BBRKC/bbrkc_24f/model_23_0a_ph7_23/gmacs.std", model_name = "m23.0a_23")
m230a_24_std <- gmacs_read_std(file = "./BBRKC/bbrkc_24f/model_23_0a_ph7_24/gmacs.std", model_name = "m23.0a")
m24c_std <- gmacs_read_std(file = "./BBRKC/bbrkc_24f/model_24_0c/gmacs.std", model_name = "m24.0c")
#m24d_std <- gmacs_read_std(file = "./BBRKC/bbrkc_24s/model_24_0d/gmacs.std", model_name = "m24.0d")

# model groupings defined here for future plots ----------------
base_models <- list(m230a_23, m230a_24, m24c)
base_std <- list(m230a_23_std, m230a_24_std, m24c_std)

# Order for SAFE R markdown doc #############################
# data range ------
gmacs_plot_data_range(all_out = base_models, save_plot = T, plot_dir = plot_save)

#
# plot selectivity -------
gmacs_plot_slx(all_out = base_models, save_plot = F) #, plot_dir = plot_save)
#gmacs_plot_slx(all_out = base_models, save_plot = T, plot_dir = plot_save)

gmacs_get_slx(all_out = base_models) %>%
  mutate(capture_block = case_when(fleet %in% c("BSFRF", "Bairdi_Fishery_Bycatch", "Fixed_Gear") ~ "1975 - 2023",
                                   fleet == "NMFS_Trawl" & year %in% 1975:1981 ~ "1975 - 1981",
                                   fleet == "NMFS_Trawl" & year %in% 1982:2023 ~ "1982 - 2023",
                                   fleet == "Pot_Fishery" ~ "1975 - 2022",
                                   fleet == "Trawl_Bycatch" ~ "1975 - 2022")) %>%
  gmacs_plot_slx(data_summary = ., save_plot = T, plot_dir = plot_save)
# need retained and discarded for pot fishery --- **fix **

# molt and tagging data plots ------
gmacs_plot_molt_probability(all_out = base_models, save_plot = T, plot_dir = plot_save)

#gmacs_plot_molt_probability(all_out = list(m211b_p7, m230a_p7, m24, m24b, m24c, m24d), save_plot = T, plot_dir = plot_save)

# Molt and tagging plot **fix**--------


# plot indices ------------
#gmacs_plot_index(all_out = list(m211b, m211b_p7, m230a, m230a_p7), plot_dir = plot_save)
## ************!!!!!!!!!!!!!!! load one from "working doc_tables.R" here
gmacs_plot_index(all_out = base_models, plot_dir = plot_save)

temp <- gmacs_get_index_summary(all_out = list(m230a_p7, m230a_24, m24c))

#gmacs_plot_index(all_out = list(m230a_p7, m211b_p7, m24, m24b), plot_dir = plot_save_sel)

# plot size comps ----
gmacs_plot_sizecomp(all_out = base_models, save_plot = T, plot_dir = plot_save)
#gmacs_plot_sizecomp(all_out = sel_models, save_plot = T, plot_dir = plot_save_sel)
#gmacs_plot_sizecomp(all_out = molt_models, save_plot = T, plot_dir = plot_save_molt)


# plot mmb ------------
gmacs_plot_mmb(all_out = base_models, save_plot = T, plot_dir = plot_save, plot_ci = T, std_list = base_std)
#gmacs_plot_mmb(all_out = sel_models, plot_dir = plot_save_sel, plot_ci = T, std_list = sel_std)
#gmacs_plot_mmb(all_out = molt_models, plot_dir = plot_save_molt, plot_ci = T, std_list = molt_std)

# recruitment ------
gmacs_plot_recruitment(all_out = list(m230a_23, m230a_24, m24c), save_plot = T, plot_dir = plot_save)
#gmacs_plot_recruitment(all_out = list(m211b_p7, m230a_p7, m24, m24b), plot_dir = plot_save_sel)
#gmacs_plot_recruitment(all_out = list(m211b_p7, m230a_p7, m24c, m24d), plot_dir = plot_save_molt)

#gmacs_plot_recruitment(all_out = base_models, save_plot = T, plot_dir = plot_save, 
#                       data_summary = data_summary)

# fishing mortality and mmb ------
gmacs_plot_f_mmb(all_out = base_models, save_plot = T, plot_dir = plot_save)
# not the same as 2023 figures from Jie's code - look this over **fix**

# fishing mortality ------
gmacs_plot_f(all_out = base_models, save_plot = T, plot_dir = plot_save)
#gmacs_plot_f(all_out = sel_models, save_plot = T, plot_dir = plot_save_sel)
#gmacs_plot_f(all_out = molt_models, save_plot = T, plot_dir = plot_save_molt)
# fishing mortality panel ??? **fix**

# plot M nat mort -----------
gmacs_plot_m(all_out = base_models, save_plot = T, plot_dir = plot_save)
#gmacs_plot_m(all_out = sel_models, save_plot = T, plot_dir = plot_save_sel)
#gmacs_plot_m(all_out = molt_models, save_plot = T, plot_dir = plot_save_molt)
# still need to deal with M by sex.


# catch
gmacs_plot_catch(all_out = base_models, save_plot = T, plot_dir = plot_save)
gmacs_plot_catch(all_out = molt_models, save_plot = T, plot_dir = plot_save_molt)



# data extent -------
gmacs_plot_data_range(all_out = base_models, save_plot = T, plot_dir = plot_save)

## save output for future use? ------------------
# get derived quantity summary
deriv.quant <- gmacs_get_derived_quantity_summary(all_out = list(m211b, m211b_p7, m230a, m230a_p7))


## TABLES ====================================
# Tables 1 to 3 calcs -------
## table 1 
# model 21.1b
M <- m211b
#round(M[[rec_mod]]$spr_bmsy/1000 * 0.5, 2) -> msst_2223
round(M$bmsy/1000*0.5, 2) -> msst_2223
#round(M[[rec_mod]]$ssb[length(M[[rec_mod]]$ssb)]/1000, 2) -> mmb_2223
round(M$derived_quant_summary$ssb[length(M$derived_quant_summary$ssb)]/1000, 2) -> mmb_2223
#round(M[[rec_mod]]$spr_bmsy*M[[rec_mod]]$spr_depl/1000, 2) -> mmb_2324
round(M$mmb_curr/1000, 2) -> mmb_2324
#round(M[[rec_mod]]$spr_cofl/1000, 2) -> ofl_2324
round(M$ofl_tot/1000, 2) -> ofl_2324
#round(M[[rec_mod]]$spr_cofl/1000*0.80, 2) -> abc_2324
round(ofl_2324*.80, 2) -> abc_2324
table1specs_t <- c(msst_2223, mmb_2223, mmb_2324, ofl_2324, abc_2324)
table1specs_t

# use this as starting place for table 1 ----
refT1 <- gmacs_get_ref_points(all_out = list(m211b, m211b_p7, m24b, m24d, m230a, m230a_p7, m24, m24c))
refT1 %>% 
  as.data.frame() %>% 
  mutate(MMB = round(mmb/1000, 2), 
         b35 = round(b35/1000, 2), 
         f35 = round(f35,2), 
         fofl = round(fofl, 2), 
         OFL = round(ofl_tot/1000, 2), 
         male_rbar = round(male_rbar/1000000, 2), 
         b_b35 = round(b_b35, 2)) %>% 
  merge(M_tab1) %>% 
  mutate(maleM = round(base, 2)) %>% 
  select(model, MMB, b35, b_b35, f35, fofl, OFL, male_rbar, maleM) -> tab1_ref
write.csv(tab1_ref, paste0(.TABS, "specs_all_mods_detailed.csv"), row.names = FALSE)

# need to bring in M_tab1 from below for natural mortality 

# get reference points table
gmacs_get_ref_points(all_out = list(m211b, m211b_p7, m24b, m24d, m230a, m230a_p7, m24, m24c))
refT1 <- gmacs_get_ref_points(all_out = list(m211b, m211b_p7, m24b, m24d, m230a, m230a_p7, m24, m24c))
refT1 %>% 
  as.data.frame() %>% 
  mutate(MMB = round(mmb/1000, 2), 
         b35 = round(b35/1000, 2), 
         f35 = round(f35,2), 
         fofl = round(fofl, 2), 
         OFL = round(ofl_tot/1000, 2), 
         male_rbar = round(male_rbar/1000000, 2)) %>% 
  select(Model=model, MMB, b35, f35, fofl, OFL, male_rbar) -> ref_pt_table
write.csv(ref_pt_table, paste0(.TABS, "specs_all_mods.csv"), row.names = FALSE)




# table 2 -----------------------
# table 3 ---------------

# Table 7 nat mort----
#nat_mort <- .get_M_df_kjp(M[2:4]) # bbrkc_functions.R
nat_mort <- gmacs_get_m(all_out = list(m211b, m211b_p7, m24b, m24d, m230a, m230a_p7, m24, m24c))
#nat_mort <- m230a$M_by_class
nat_mort %>% 
  distinct(model, sex, M) %>% print(n =100)

nat_mort %>% 
  distinct(model, sex, M) %>% 
  mutate(year = c("base", "1980-84", "base", "1980-84", 
                  "base", "1980-84", "base", "1980-84", 
                  "base", "1980-84", "base", "1980-84",
                  "base", "1980-84", "base", "1980-84",
                  "base", "1980-84", "base", "1980-84",
                  "base", "1980-84", "base", "1980-84",
                  "base", "1980-84", "base", "1980-84",
                  "base", "1980-84", "base", "1980-84"))-> natural_mort_all
# want to seperate out the year ranges
natural_mort_all %>% 
  spread(year, M)  %>% 
  select(model, sex, base, `1980-84`) -> natural_mort_all2

natural_mort_all2 %>% 
  select(model, sex, base) %>% 
  filter(sex == "male") -> M_tab1

write.csv(natural_mort_all2, paste0(.TABS, "M_out.csv"), row.names = FALSE)


# get likelihood table --------------------
base_like <- gmacs_get_lik(all_out = list(m230a_p7, m24, m24c, m211b_p7, m24b, m24d))
# remove tagging and growth 

base_pen <- gmacs_get_lik_type_pen(all_out = list(m230a_p7, m24, m24c, m211b_p7, m24b, m24d))

all_like1 <- base_like[c(1:20), ]
all_like2 <- base_pen[c(10:11, 8, 7), ]
all_like3 <- base_like[c(26,25), ]

ref_all_like4 <- gmacs_get_ref_points(all_out = list(m211b_p7, m230a_p7, m24, m24b, m24c, m24d))
ref_all_like4 %>% 
  as.data.frame() %>% 
  select(model, b35, mmb, f35, fofl, ofl_tot) %>% 
  mutate(f35 = round(f35, 2), 
         fofl = round(fofl, 2), 
         ofl_tot = round(ofl_tot, 2)) %>% 
  select(b35, mmb, f35, fofl, ofl_tot) -> ref_all_like4a
  
row.names(ref_all_like4a) <- c("m21.1b.p7", "m23.0a.p7", "m24", "m24.0b", "m24.0c", "m24.0d")

ref_all_like4a %>% 
  rotate_df() %>% 
  mutate(process = c("b35", "mmb", "f35", "fofl", "ofl_tot")) -> temp2
  select()



# combine all of the above 
all_like1 %>% 
  rbind(all_like2) %>% 
  rbind(all_like3) %>% 
  rbind(temp2) -> df1
  print(n=100)

## NOTE: from 'BBRKC/code/Jie_cmn_files.R'
#d3f_names<-c("Pot-ret-catch","Pot-totM-catch","Pot-F-discC","Trawl-discC","Tanner-M-discC","Tanner-F-discC","Fixed-discC",
#             "Traw-suv-bio","BSFRF-sur-bio",
#             "Pot-ret-comp","Pot-totM-comp","Pot-discF-comp","Trawl-disc-comp","Tanner-disc-comp","Fixed-disc-comp","Trawl-sur-comp","BSFRF-sur-comp",
#             "Recruit-dev","Recruit-ini","Recruit-sex-R",
#             "Log_fdev_0","M-deviation","Sex-specific-R","PriorDensity","MMB35%","MMB-terminal","F35%","Fofl","OFL","ABC","Q-1982-now")
#
d3f_names<-c("Pot-ret-catch","Pot-totM-catch","Pot-F-discC","Trawl-discC","Tanner-M-discC","Tanner-F-discC","Fixed-discC",
             "Traw-suv-bio","BSFRF-sur-bio",
             "Pot-ret-comp","Pot-totM-comp","Pot-discF-comp","Trawl-disc-comp","Tanner-disc-comp","Fixed-disc-comp","Trawl-sur-comp","BSFRF-sur-comp",
             "Recruit-dev","Recruit-ini","Recruit-sex-R",
             "M-deviation","Sex-specific-R","Ini-size-struct", 
             "PriorDensity", "Tot-likelihood","Tot-parms",
             "MMB35","MMB-terminal","F35","$Fofl$","OFL")
#
like_all_out <- cbind(d3f_names, df1)
write.csv(like_all_out, paste0(.TABS, "likelihood.csv"), row.names = FALSE)

# parameter tables -------------
base_m230a_parm <- gmacs_get_pars(all_out = list(m230a_p7))
base_m230a_parm %>% 
  filter(standard_error != "NA") %>% # get only estimated parameters not the fixed ones
  select(model, parameter_count, parameter, estimate, standard_error) -> parm1

write.csv(parm1, paste0(.TABS, "para_model_23_0a.csv")) # use row names as index - don't need parameter count
# IMPORTANT - need to edit in excel right now to remove "_" - can't have "_" in names...
# 

# 21.1b.p7
base_211b_parm <- gmacs_get_pars(all_out = list(m211b_p7))
base_211b_parm %>% 
  filter(standard_error != "NA") %>% # get only estimated parameters not the fixed ones
  select(model, parameter_count, parameter, estimate, standard_error) -> parm1

write.csv(parm1, paste0(.TABS, "para_model_21_1b_p7.csv")) # use row names as index - don't need parameter count
# IMPORTANT - need to edit in excel right now to remove "_" - can't have "_" in names...
# 

# model 24.0
base_24_parm <- gmacs_get_pars(all_out = list(m24))
base_24_parm %>% 
  filter(standard_error != "NA") %>% # get only estimated parameters not the fixed ones
  select(model, parameter_count, parameter, estimate, standard_error) -> parm1

write.csv(parm1, paste0(.TABS, "para_model_24.csv")) # use row names as index - don't need parameter count
# IMPORTANT - need to edit in excel right now to remove "_" - can't have "_" in names...
#

# model 24.0c
base_24c_parm <- gmacs_get_pars(all_out = list(m24c))
base_24c_parm %>% 
  filter(standard_error != "NA") %>% # get only estimated parameters not the fixed ones
  select(model, parameter_count, parameter, estimate, standard_error) -> parm1

write.csv(parm1, paste0(.TABS, "para_model_24c.csv")) # use row names as index - don't need parameter count
# IMPORTANT - need to edit in excel right now to remove "_" - can't have "_" in names...
#

#sel_models_pars <- gmacs_get_pars(all_out = sel_models)
#sel_models_pars %>% 
#  as.data.frame() %>%
#  filter(parameter == c("Survey_q_survey_1", "Survey_q_survey_2"))
# would be in selectivity not q for those models. 

# tables of numbers ------------
#base_211b <- gmacs_get_n_matrix(all_out = list(m211b))

## Table of abundance per model -----------------------------------------------
# Jie's old code 
source("./BBRKC/code/bbrkc_functions.R")
# model 21.1b.p7
model <- "m211b_p7"
W <- m211b_p7 ### CHANGE HERE
Y <- m211b_p7_std ### change here 
A <- read_rep("./BBRKC/bbrkc_24s/ADJ_model_211b_ph7/gmacs.rep")

model <- "m230a_p7"
W <- m230a_p7 ### CHANGE HERE
Y <- m230a_p7_std ### change here 
A <- read_rep("./BBRKC/bbrkc_24s/ADJ_model_23_0a_ph7/gmacs.rep")

model <- "m24"
W <- m24 ### CHANGE HERE
Y <- m24_std ### change here 
A <- read_rep("./BBRKC/bbrkc_24s/ADJ_model_24_0/gmacs.rep")

model <- "m24c"
W <- m24c ### CHANGE HERE
Y <- m24c_std ### change here 
A <- read_rep("./BBRKC/bbrkc_24s/ADJ_model_24_0c/gmacs.rep")

# -- males - mature legal, females mature does NOT include projectino year!
temp <- W$n_matrix
temp %>% 
  select(year, size, males, females) %>% 
  mutate(size = as.numeric(size)) %>% 
  #filter(size >= 119) %>% 
  group_by(year) %>%  #  year
  summarise(mat_males = sum(males[size >= 119])/1000000, # mature males >119 mm
            leg_males = sum(males[size >= 134])/1000000, #legal males > 134 mm
            mat_fem = sum(females[size >= 90])/1000000) -> fmales1 #mature_females >90 mm
# -- MMB and sd and recruits - also does NOT include projection year!
temp2 <- W$derived_quant_summary
temp2 %>% 
  select(year, ssb, sd_log_ssb, recruit_male, recruit_female) %>% 
  group_by(year) %>% 
  mutate(recruits = sum(recruit_male, recruit_female)/1000000, # need to be moved down a year 
         MMB = ssb/1000, # mmb
         sd_mmb = MMB*(exp(sd_log_ssb^2)-1)^0.5) %>% # sd mmb
  select(year, MMB, sd_mmb) -> derived_m
# recruits needs to be moved down one year 
temp2 %>% 
  select(year, recruit_male, recruit_female) %>% 
  group_by(year) %>% 
  mutate(recruits = sum(recruit_male, recruit_female)/1000000) %>% 
  mutate(year = year +1) %>% 
  select(year, recruits) -> recruits

# survey obs and predicted - does include prj year
temp3 <- W$index_fit_summary
temp3 %>% 
  filter(fleet == "NMFS_Trawl") %>% 
  group_by(year) %>% 
  summarise(total_obs = round(sum(obs_index)/1000, 2), # total area swept
            total_pred = round(sum(pred_index)/1000, 2)) -> survey_est # total model est survey

# put them in the correct order for tables in doc -------
#abun_tab <- cbind(fmales1[ ,1:3], derived_m[ ,2:3], recruit_tab, survey_est[ ,c(3,2)])
survey_est %>% #[1:47, ] %>% 
  merge(derived_m, all = T) %>% 
  merge(fmales1, all = T) %>% 
  merge(recruits, all = T) %>% 
  filter(year <= 2022) -> abun_tab2
# doesn't have 2023 values for anything but the survey 
mat_fem1 <- as.data.frame(A$N_females[ , 6:16]) # need only mature females 
mat_fem1 %>% 
  rowwise() %>% 
  mutate(mat_fem = sum(V6+V7+V8+V9+V10+V11+V12+V13+V14+V15+V16)/1000000) -> mat_fem2
# values match so really just need 2023 value saved here 
#mat_fem2[49,12] # 2023 projected mature females
mat_mal <- as.data.frame(A$N_males[ , 12:20]) # need only mature males 
mat_mal %>% 
  rowwise() %>% 
  mutate(mat_males = sum(V12+V13+V14+V15+V16+V17+V18+V19+V20)/1000000, 
         leg_males = sum(V15+V16+V17+V18+V19+V20)/1000000) -> mat_mal2
# values match so really just need 2023 value saved here 
#mat_mal2[49,10:11] # 2023 projected mature females
#mmb_proj = W$bmsy * W$b_bmsy
Y %>% filter(par == "sd_last_ssb") %>% 
  mutate(MMB = est/1000, 
         sd_mmb = se/1000) %>% 
  select(MMB, sd_mmb) -> ref_prj

# 2023 vector 
vec1 <- cbind(year = 2023, survey_est[48, 2:3] , ref_prj, mat_mal2[49,10:11], 
              mat_fem2[49,12], recruits[48,2])

abun_tab2 %>% 
  merge(vec1, all = T) %>% 
  select(year, mat_males, leg_males, MMB, sd_mmb, mat_fem, recruits, total_pred, total_obs) -> abun_tab3

write.csv(abun_tab3, paste0(.TABS, "_", model, "gmacs_sum_abun.csv"), row.names = FALSE)

