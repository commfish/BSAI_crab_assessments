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
library(sjmisc)
source("./gmacsr/gmacsr.R")
# **************************************************************************************************
cur_yr <- 2024 # update annually 
folder <- "bbrkc_24f" # update annually 
# The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#D55E00", "#0072B2", "#CC79A7", "#F0E442")

#plot.dir <- paste0(here::here(), "/BBRKC/", folder, "/doc/figures/")
plot_save <- paste0(here::here(), "/BBRKC/", folder, "/doc/figures/")
plot_save_newD <- paste0(here::here(), "/BBRKC/", folder, "/doc/figures/newD")
#plot_save_sel <- paste0(here::here(), "/BBRKC/", folder, "/doc/figures/sel_models/")
#plot_save_molt <- paste0(here::here(), "/BBRKC/", folder, "/doc/figures/molt_models/")
#table directory 
.TABS     = c("./BBRKC/bbrkc_24f/doc/tables/")
.FIGS     = c("./BBRKC/bbrkc_24f/doc/figures/")
.THEME    = list(theme_bw(base_size = 12, base_family = ""), scale_fill_manual(values=cbPalette), 
                 scale_colour_manual(values=cbPalette), 
                 update_geom_defaults("line", list(size = 1.75)))
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
base_models <- list(m230a_24, m24c) # models for comparison 2024
base_std <- list(m230a_24_std, m24c_std)

newD_models <- list(m230a_23, m230a_24, m24c) # comparing new data
newD_std <- list(m230a_23_std, m230a_24_std, m24c_std)


#liklihood ------
gmacs_get_lik_type_pen(all_out = newD_models)
temp <- gmacs_get_lik(all_out = newD_models) 
print(temp, n = Inf)
gmacs_get_pars(all_out = newD_models)


# Order for SAFE R markdown doc #############################
# data range ------
gmacs_plot_data_range(all_out = base_models, save_plot = T, plot_dir = plot_save)

#
# plot selectivity ------- **FIx**
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

# molt  plots ------
gmacs_plot_molt_probability(all_out = base_models, save_plot = T, plot_dir = plot_save)

#gmacs_plot_molt_probability(all_out = list(m211b_p7, m230a_p7, m24, m24b, m24c, m24d), save_plot = T, plot_dir = plot_save)

# Molt and tagging plot --------
# molt with tag data base
mdf <- gmacs_get_molt_probability(all_out = base_models)
#mdf <- .get_molt_prob_df(M[rec_mod])

tag_molt <- read.csv(paste0(here::here(), '/BBRKC/data/tagging_data_molt_males.csv'))
tag_molt %>% 
  mutate(year = as.factor(year)) %>% 
  mutate(model = block) -> tag_molt

year_list <- c(1975, 2023)
mdf %>% 
  filter(sex == "male") %>% 
  mutate(year = as.factor(year)) %>% 
  filter(year %in% year_list) %>% 
  select(-sex) %>% 
  select(size, molt_probability, year, block, model)-> mdf_temp

mdf_temp %>% 
  rbind(tag_molt) -> molt_tag_data

molt_tag_data %>% 
  ggplot(aes(x = size, y = molt_probability)) + 
  expand_limits(y = c(0, 1)) + 
  labs(x = "Length(mm)", y = "Molting probabilities (males)") +
  geom_line(aes(linetype = model, col = year)) +
  geom_point(aes(linetype = model, col = year)) + .THEME +
  #scale_color_discrete(name = "Year Range", labels = c("1975-1979", "1980-2023", "1954-1961", "1966-1969"))+
  scale_color_manual(values = c("#999999", "#E69F00", "#56B4E9", "#009E73"), 
                     name = "Year Range", labels = c("1975-1979", "1980-2023", "1954-1961", "1966-1969"))-> p
# add in year range as labels. 
print(p)
ggsave(paste0(.FIGS, "molt_tagging_males_base.png"), width = 6*1.15, height = 1.25*5)

# plot indices ------------
#gmacs_plot_index(all_out = list(m211b, m211b_p7, m230a, m230a_p7), plot_dir = plot_save)
## ************!!!!!!!!!!!!!!! load one from "working doc_tables.R" here
gmacs_plot_index(all_out = base_models, plot_dir = plot_save)
gmacs_plot_index(all_out = newD_models, plot_dir = plot_save_newD)


temp <- gmacs_get_index_summary(all_out = list(m230a_p7, m230a_24, m24c))

#gmacs_plot_index(all_out = list(m230a_p7, m211b_p7, m24, m24b), plot_dir = plot_save_sel)

# plot size comps ----
# Tyler says this is broken. Need to fix this for what I need. **fix**
gmacs_plot_sizecomp(all_out = base_models, save_plot = T, plot_dir = plot_save)
#gmacs_plot_sizecomp(all_out = sel_models, save_plot = T, plot_dir = plot_save_sel)
#gmacs_plot_sizecomp(all_out = molt_models, save_plot = T, plot_dir = plot_save_molt)


# plot mmb ------------
gmacs_plot_mmb(all_out = base_models, save_plot = T, plot_dir = plot_save, plot_ci = T, std_list = base_std)
gmacs_plot_mmb(all_out = newD_models, plot_dir = plot_save_newD, plot_ci = T, std_list = newD_std)
#gmacs_plot_mmb(all_out = molt_models, plot_dir = plot_save_molt, plot_ci = T, std_list = molt_std)


# recruitment ------
gmacs_plot_recruitment(all_out = list(m230a_23, m230a_24, m24c), save_plot = T, plot_dir = plot_save_newD)
gmacs_plot_recruitment(all_out = list(m230a_24, m24c), save_plot = T, plot_dir = plot_save)
#gmacs_plot_recruitment(all_out = list(m211b_p7, m230a_p7, m24c, m24d), plot_dir = plot_save_molt)

#gmacs_plot_recruitment(all_out = base_models, save_plot = T, plot_dir = plot_save, 
#                       data_summary = data_summary)

# fishing mortality and mmb ------
gmacs_plot_f_mmb(all_out = list(m230a_24, m24c), save_plot = T, plot_dir = plot_save)
gmacs_plot_f_mmb_dir(all_out = list(m230a_24, m24c), save_plot = T, plot_dir = plot_save)
# load from "bbrkc_functions_gmacs.R" prior to running this 
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

# catch ----------------
gmacs_plot_catch(all_out = base_models, save_plot = T, plot_dir = plot_save)
gmacs_plot_catch_kjp(all_out = base_models, save_plot = T, plot_dir = plot_save) 
# load from "bbrkc_functions_gmacsr.R"
#gmacs_plot_catch(all_out = molt_models, save_plot = T, plot_dir = plot_save_molt)
## ** fix ** so that all directed catch is on the same scale and showing the entire time series - like old plot.

# data extent -------
#gmacs_plot_data_range(all_out = base_models, save_plot = T, plot_dir = plot_save)

#mature female abundance -----------
fem1 <- as.data.frame(m230a_24$n_matrix)
fem2 <- as.data.frame(m24c$n_matrix)

fem1 %>% 
  select(year, size, females) %>% 
  mutate(size = as.numeric(size)) %>% 
  filter(size >= 92.5, size < 147.5) %>% # just mature size classes 6 to 16 exclude first 5
  group_by(year) %>% 
  summarise(mat_total = sum(females)/1000000) %>% 
  mutate(model = "m230a") -> mat_fem

fem2 %>% 
  select(year, size, females) %>% 
  mutate(size = as.numeric(size)) %>% 
  filter(size >= 92.5, size < 147.5) %>% # just mature size classes 6 to 16 exclude first 5
  group_by(year) %>% 
  summarise(mat_total = sum(females)/1000000) %>% 
  mutate(model = "m24c") -> mat_fem2

mat_fem %>% 
  rbind(mat_fem2) -> mat_fem_abn

mat_fem_abn %>% 
  ggplot(aes(year, mat_total, col = model)) +
  geom_line(lwd =1) +
  expand_limits(y=0) +
  scale_y_continuous(expand = c(0,0)) +
  #geom_hline(data = Bmsy_options, aes(yintercept = Bmsy), color = c("blue", "red"), 
  #           lty = c("solid", "dashed"))+
  #geom_text(data = Bmsy_options, aes(x= 1980, y = Bmsy, label = label), 
  #          hjust = -0.45, vjust = 1.5, nudge_y = 0.05, size = 3.5) +
  ggtitle("Female Abundance") +
  ylab("Matue female abundance (million crab)") + xlab("Year") +
  .THEME
ggsave(paste0(.FIGS, "mature_female_abundance_mod_scen.png"), width = 6*1.3, height = 5*1.25)

#ggsave(paste0(.FIGS, "mature_female_abundance_m230a.png"), width = 6*1.3, height = 5*1.25)
  
## save output for future use? ------------------

# here here 
# get derived quantity summary
deriv.quant <- gmacs_get_derived_quantity_summary(all_out = list(m230a_23, m230a_24, m24c))


## TABLES ====================================
#executive summary stats ------------
# see EBSsurvey_analysis.R line 301

# Tables 1 to 3 calcs -------
## table 1 
# model 21.1b
M <- m230a_24
#round(M[[rec_mod]]$spr_bmsy/1000 * 0.5, 2) -> msst_2223
round(M$bmsy/1000*0.5, 2) -> msst_2324
#round(M[[rec_mod]]$ssb[length(M[[rec_mod]]$ssb)]/1000, 2) -> mmb_2223
round(M$derived_quant_summary$ssb[length(M$derived_quant_summary$ssb)]/1000, 2) -> mmb_2324
#round(M[[rec_mod]]$spr_bmsy*M[[rec_mod]]$spr_depl/1000, 2) -> mmb_2324
round(M$mmb_curr/1000, 2) -> mmb_2425
#round(M[[rec_mod]]$spr_cofl/1000, 2) -> ofl_2324
round(M$ofl_tot/1000, 2) -> ofl_2425
#round(M[[rec_mod]]$spr_cofl/1000*0.80, 2) -> abc_2324
round(ofl_2425*.80, 2) -> abc_2425
table1specs_t <- c(msst_2324, mmb_2324, mmb_2425, ofl_2425, abc_2425)
table1specs_t

# use this as starting place for table 1 ----
refT1 <- gmacs_get_ref_points(all_out = list(m230a_24, m24c))
refT1 %>% 
  as.data.frame() %>% 
  mutate(MMB = round(mmb/1000, 2), 
         b35 = round(b35/1000, 2), 
         f35 = round(f35,2), 
         fofl = round(fofl, 2), 
         OFL = round(ofl_tot/1000, 2), 
         male_rbar = round(male_rbar/1000000, 2), 
         b_b35 = round(b_b35, 2)) %>% 
  merge(M_tab1) %>% # see below for this data frame
  mutate(maleM = round(base, 2)) %>% 
  select(model, MMB, b35, b_b35, f35, fofl, OFL, male_rbar, maleM) -> tab1_ref
write.csv(tab1_ref, paste0(.TABS, "specs_all_mods_detailed.csv"), row.names = FALSE)

# need to bring in M_tab1 from below for natural mortality 

# get reference points table -------------
gmacs_get_ref_points(all_out = newD_models)
#refT1 <- gmacs_get_ref_points(all_out = list(m211b, m211b_p7, m24b, m24d, m230a, m230a_p7, m24, m24c))
#refT1 %>% 
#  as.data.frame() %>% 
#  mutate(MMB = round(mmb/1000, 2), 
#         b35 = round(b35/1000, 2), 
#         f35 = round(f35,2), 
#         fofl = round(fofl, 2), 
#         OFL = round(ofl_tot/1000, 2), 
#         male_rbar = round(male_rbar/1000000, 2)) %>% 
#  select(Model=model, MMB, b35, f35, fofl, OFL, male_rbar) -> ref_pt_table
#write.csv(ref_pt_table, paste0(.TABS, "specs_all_mods.csv"), row.names = FALSE)




# table 2 -----------------------
# table 3 ---------------

# Table 7 nat mort----
#nat_mort <- .get_M_df_kjp(M[2:4]) # bbrkc_functions.R
nat_mort <- gmacs_get_m(all_out = list(m230a_24, m24c))
#nat_mort <- m230a$M_by_class
nat_mort %>% 
  distinct(model, sex, M) %>% print(n =100)

nat_mort %>% 
  distinct(model, sex, M) %>% 
  mutate(year = c("base", "1980-84", "base", "1980-84", 
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
base_like <- gmacs_get_lik(all_out = list(m230a_24, m24c))
# remove tagging and growth 

base_pen <- gmacs_get_lik_type_pen(all_out = list(m230a_24, m24c))

all_like1 <- base_like[c(1:20), ]
all_like2 <- base_pen[c(10, 8, 7), ]
all_like3 <- base_like[c(26,25), ]

ref_all_like4 <- gmacs_get_ref_points(all_out = list(m230a_24, m24c))
ref_all_like4 %>% 
  as.data.frame() %>% 
  select(model, b35, mmb, f35, fofl, ofl_tot) %>% 
  mutate(f35 = round(f35, 2), 
         fofl = round(fofl, 2), 
         ofl_tot = round(ofl_tot, 2)) %>% 
  select(b35, mmb, f35, fofl, ofl_tot) -> ref_all_like4a
  
row.names(ref_all_like4a) <- c("m23.0a", "m24.0c")

ref_all_like4a %>% 
  rotate_df() %>% 
  mutate(process = c("b35", "mmb", "f35", "fofl", "ofl_tot")) -> temp2
  #select()



# combine all of the above 
all_like1 %>% 
  rbind(all_like2) %>% 
  rbind(all_like3) %>% 
  rbind(temp2) -> df1
  # %>% print(n=100)

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
             "Sex-specific-R","Ini-size-struct", 
             "PriorDensity", "Tot-likelihood","Tot-parms",
             "MMB35","MMB-terminal","F35","$Fofl$","OFL")
#
like_all_out <- cbind(d3f_names, df1)
write.csv(like_all_out, paste0(.TABS, "likelihood.csv"), row.names = FALSE)
# stop here

# parameter tables -------------
# model 23.0a for 2024 
base_m230a_parm <- gmacs_get_pars(all_out = list(m230a_24))
base_m230a_parm %>% 
  filter(standard_error != "NA") %>% # get only estimated parameters not the fixed ones
  select(model, parameter_count, parameter, estimate, standard_error) -> parm1

write.csv(parm1, paste0(.TABS, "para_model_23_0a.csv")) # use row names as index - don't need parameter count
# IMPORTANT - need to edit in excel right now to remove "_" - can't have "_" in names...
# 

# model 24.0
base_24_parm <- gmacs_get_pars(all_out = list(m24c))
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

model <- "m230a_24"
W <- m230a_24 ### CHANGE HERE
Y <- m230a_24_std ### change here 
A <- read_rep("./BBRKC/bbrkc_24f/model_23_0a_ph7_24/gmacs.rep")

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
# need to add projection here
Y %>% filter(par == "sd_last_ssb") %>% 
  mutate(MMB = est/1000, 
         sd_mmb = se/1000) %>%
  mutate(year = 2024) %>% 
  select(year, MMB, sd_mmb) -> ref_prj

derived_m %>% 
  merge(ref_prj, all = T) -> derived_m
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
  merge(recruits, all = T) -> abun_tab2

abun_tab2 %>% 
  #merge(vec1, all = T, by = year) %>% 
  select(year, mat_males, leg_males, MMB, sd_mmb, mat_fem, recruits, total_pred, total_obs) -> abun_tab3

write.csv(abun_tab3, paste0(.TABS, "_", model, "gmacs_sum_abun.csv"), row.names = FALSE)

