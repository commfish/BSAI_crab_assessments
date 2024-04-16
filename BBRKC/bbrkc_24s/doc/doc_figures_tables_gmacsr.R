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

plot.dir <- paste0(here::here(), "/BBRKC/", folder, "/doc/figures/")
plot_save <- paste0(here::here(), "/BBRKC/", folder, "/doc/figures/")

## read in models
m211b <- gmacs_read_allout(file = "./BBRKC/bbrkc_24s/model_211b/Gmacsall.out", model = "m21.1b")
m211b_p7 <- gmacs_read_allout(file = "./BBRKC/bbrkc_24s/model_211b_ph7/Gmacsall.out", model = "m21.1b_p7")
m230a <- gmacs_read_allout(file = "./BBRKC/bbrkc_24s/model_23_0a/Gmacsall.out", model = "m23.0a")
m230a_p7 <- gmacs_read_allout(file = "./BBRKC/bbrkc_24s/model_23_0a_ph7/Gmacsall.out", model = "m23.0a_p7")
#m24.0 <- gmacs_read_allout(file = "./SMBKC/smbkc_24s/model_16_0_c/Gmacsall.out", model = "smbkc24.0")

# plot indices
gmacs_plot_index(all_out = list(m211b, m211b_p7, m230a, m230a_p7), plot_dir = plot_save)
gmacs_get_index_summary(all_out = list(m211b, m211b_p7, m230a, m230a_p7))

# plot mmb
gmacs_plot_mmb(all_out = list(m211b, m211b_p7, m230a, m230a_p7), save_plot = T, plot_dir = plot_save)

# recruitment
gmacs_plot_recruitment(all_out = list(m211b, m211b_p7, m230a, m230a_p7), save_plot = T, plot_dir = plot_save)
# **fix** need units here 

# fishing mortality --
gmacs_plot_f(all_out = list(m211b, m211b_p7, m230a, m230a_p7), save_plot = T, plot_dir = plot_save)

# plot size comps
gmacs_plot_sizecomp(all_out = list(m211b, m211b_p7, m230a, m230a_p7), plot_dir = plot_save)

# plot selectivity ---
# **fix**

# molt and tagging data plots ---
# **fix**


## save output for future use? ------------------
# get derived quantity summary
deriv.quant <- gmacs_get_derived_quantity_summary(all_out = list(m211b, m211b_p7, m230a, m230a_p7))


## TABLES ====================================
# Tables 1 to 3 calcs -------
## table 1 ------
round(M[[rec_mod]]$spr_bmsy/1000 * 0.5, 2) -> msst_2223
round(M[[rec_mod]]$ssb[length(M[[rec_mod]]$ssb)]/1000, 2) -> mmb_2223
round(M[[rec_mod]]$spr_bmsy*M[[rec_mod]]$spr_depl/1000, 2) -> mmb_2324
round(M[[rec_mod]]$spr_cofl/1000, 2) -> ofl_2324
round(M[[rec_mod]]$spr_cofl/1000*0.80, 2) -> abc_2324
table1specs_t <- c(msst_2223, mmb_2223, mmb_2324, ofl_2324, abc_2324)
table1specs_t

# get reference points table
gmacs_get_ref_points(all_out = list(m211b, m211b_p7, m230a, m230a_p7))
# use this as starting place for table 1

# get likelihood table
gmacs_get_lik(all_out = list(m211b, m211b_p7, m230a, m230a_p7))
