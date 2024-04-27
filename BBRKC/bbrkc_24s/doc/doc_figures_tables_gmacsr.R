# Kpalof
# figures _tables_create SAFE using 'gmacsr'

# 4-12-24
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
folder <- "bbrkc_24s" # update annually 
# The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#D55E00", "#0072B2", "#CC79A7", "#F0E442")

#plot.dir <- paste0(here::here(), "/BBRKC/", folder, "/doc/figures/")
plot_save <- paste0(here::here(), "/BBRKC/", folder, "/doc/figures/")
plot_save_sel <- paste0(here::here(), "/BBRKC/", folder, "/doc/figures/sel_models/")
plot_save_molt <- paste0(here::here(), "/BBRKC/", folder, "/doc/figures/molt_models/")
#table directory 
.TABS     = c("./BBRKC/bbrkc_24s/doc/tables/")

## read in models
m211b <- gmacs_read_allout(file = "./BBRKC/bbrkc_24s/model_211b/Gmacsall.out", model = "m21.1b")
m211b_p7 <- gmacs_read_allout(file = "./BBRKC/bbrkc_24s/model_211b_ph7/Gmacsall.out", model = "m21.1b.p7")
m230a <- gmacs_read_allout(file = "./BBRKC/bbrkc_24s/model_23_0a/Gmacsall.out", model = "m23.0a")
m230a_p7 <- gmacs_read_allout(file = "./BBRKC/bbrkc_24s/model_23_0a_ph7/Gmacsall.out", model = "m23.0a.p7")
m24 <- gmacs_read_allout(file = "./BBRKC/bbrkc_24s/model_24_0/Gmacsall.out", model = "m24")
m24b <- gmacs_read_allout(file = "./BBRKC/bbrkc_24s/model_24_0b/Gmacsall.out", model = "m24.0b")
m24c <- gmacs_read_allout(file = "./BBRKC/bbrkc_24s/model_24_0c/Gmacsall.out", model = "m24.0c")
m24d <- gmacs_read_allout(file = "./BBRKC/bbrkc_24s/model_24_0d/Gmacsall.out", model = "m24.0d")

#m24.0 <- gmacs_read_allout(file = "./SMBKC/smbkc_24s/model_16_0_c/Gmacsall.out", model = "smbkc24.0")

## read std files -------------------
m211b_std <- gmacs_read_std(file = "./BBRKC/bbrkc_24s/model_211b/gmacs.std", model_name = "m21.1b")
m211b_p7_std <- gmacs_read_std(file = "./BBRKC/bbrkc_24s/model_211b_ph7/gmacs.std", model_name = "m21.1b.p7")
m230a_std <- gmacs_read_std(file = "./BBRKC/bbrkc_24s/model_23_0a/gmacs.std", model_name = "m23.0a")
m230a_p7_std <- gmacs_read_std(file = "./BBRKC/bbrkc_24s/model_23_0a_ph7/gmacs.std", model_name = "m23.0a.p7")
m24_std <- gmacs_read_std(file = "./BBRKC/bbrkc_24s/model_24_0/gmacs.std", model_name = "m24")
m24b_std <- gmacs_read_std(file = "./BBRKC/bbrkc_24s/model_24_0b/gmacs.std", model_name = "m24.0b")
m24c_std <- gmacs_read_std(file = "./BBRKC/bbrkc_24s/model_24_0c/gmacs.std", model_name = "m24.0c")
m24d_std <- gmacs_read_std(file = "./BBRKC/bbrkc_24s/model_24_0d/gmacs.std", model_name = "m24.0d")

# model groupings defined here for future plots ----------------
base_models <- list(m211b, m211b_p7, m230a, m230a_p7)
base_std <- list(m211b_std, m211b_p7_std, m230a_std, m230a_p7_std)

sel_models <- list(m230a_p7, m211b_p7, m24, m24b)
sel_std <- list(m230a_p7_std, m211b_p7_std, m24_std, m24b_std)

molt_models <-  list(m211b_p7, m230a_p7, m24c, m24d)
molt_std <- list(m211b_p7_std, m230a_p7_std, m24c_std, m24d_std)


# plot indices
#gmacs_plot_index(all_out = list(m211b, m211b_p7, m230a, m230a_p7), plot_dir = plot_save)
## ************!!!!!!!!!!!!!!! load one from "working doc_tables.R" here
gmacs_plot_index(all_out = base_models, plot_dir = plot_save)
gmacs_plot_index(all_out = sel_models, plot_dir = plot_save_sel)
gmacs_plot_index(all_out = molt_models, plot_dir = plot_save_molt)

temp <- gmacs_get_index_summary(all_out = list(m211b, m211b_p7, m230a, m230a_p7))

#gmacs_plot_index(all_out = list(m230a_p7, m211b_p7, m24, m24b), plot_dir = plot_save_sel)

# plot mmb
gmacs_plot_mmb(all_out = base_models, save_plot = T, plot_dir = plot_save, plot_ci = T, std_list = base_std)
gmacs_plot_mmb(all_out = sel_models, plot_dir = plot_save_sel, plot_ci = T, std_list = sel_std)
gmacs_plot_mmb(all_out = molt_models, plot_dir = plot_save_molt, plot_ci = T, std_list = molt_std)

# recruitment
gmacs_plot_recruitment(all_out = base_models, save_plot = T, plot_dir = plot_save)
gmacs_plot_recruitment(all_out = sel_models, plot_dir = plot_save_sel)
gmacs_plot_recruitment(all_out = molt_models, plot_dir = plot_save_molt)
# **fix** need units here 

# plot selectivity ---
gmacs_plot_slx(all_out = base_models, save_plot = T, plot_dir = plot_save)
gmacs_plot_slx(all_out = sel_models, save_plot = T, plot_dir = plot_save_sel)
gmacs_plot_slx(all_out = molt_models, save_plot = T, plot_dir = plot_save_molt)
# **fix** females issue with NMFS trawl

# molt and tagging data plots ---
gmacs_plot_molt_probability(all_out = base_models, save_plot = T, plot_dir = plot_save)
gmacs_plot_molt_probability(all_out = sel_models, save_plot = T, plot_dir = plot_save_sel)
gmacs_plot_molt_probability(all_out = molt_models, save_plot = T, plot_dir = plot_save_molt)
#

# plot M nat mort -----------
gmacs_plot_m(all_out = base_models, save_plot = T, plot_dir = plot_save)
gmacs_plot_m(all_out = sel_models, save_plot = T, plot_dir = plot_save_sel)
gmacs_plot_m(all_out = molt_models, save_plot = T, plot_dir = plot_save_molt)
# still need to deal with M by sex.

# plot size comps ----
gmacs_plot_sizecomp(all_out = base_models, save_plot = T, plot_dir = plot_save)
gmacs_plot_sizecomp(all_out = sel_models, save_plot = T, plot_dir = plot_save_sel)
gmacs_plot_sizecomp(all_out = molt_models, save_plot = T, plot_dir = plot_save_molt)

# catch
gmacs_plot_catch(all_out = base_models, save_plot = T, plot_dir = plot_save)

# fishing mortality --
gmacs_plot_f(all_out = base_models, save_plot = T, plot_dir = plot_save)
gmacs_plot_f(all_out = sel_models, save_plot = T, plot_dir = plot_save_sel)
gmacs_plot_f(all_out = molt_models, save_plot = T, plot_dir = plot_save_molt)


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
refT1 <- gmacs_get_ref_points(all_out = list(m211b, m211b_p7, m230a, m230a_p7, m24, m24b, m24c, m24d))
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
gmacs_get_ref_points(all_out = list(m211b, m211b_p7, m230a, m230a_p7, m24, m24b, m24c, m24d))
refT1 <- gmacs_get_ref_points(all_out = list(m211b, m211b_p7, m230a, m230a_p7, m24, m24b, m24c, m24d))
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
nat_mort <- gmacs_get_m(all_out = list(m211b, m211b_p7, m230a, m230a_p7, m24, m24b, m24c, m24d))
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
base_211b <- gmacs_get_n_matrix(all_out = list(m211b))

base_211b %>% 
  group_by(year) %>% 
  summarise(mat_male = sum(males_mature), mat_fem = sum(females_mature))

