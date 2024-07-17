# *******************************************************************************
# Produce SAFE tables 
# Date: March 2024
# Author: Caitlin Stern
# *******************************************************************************

# *******************************************************************************
# load
library(tidyverse)
source(paste0(here::here(), "/gmacsr/gmacsr.R"))

folder <- "smbkc_24s"

# *******************************************************************************
# bring in model info
m1 <- gmacs_read_allout(file = paste0(here::here(), "/SMBKC/smbkc_24s/model_16_0/Gmacsall.out"), model = "smbkc16.0")
m2 <- gmacs_read_allout(file = paste0(here::here(), "/SMBKC/smbkc_24s/model_16_0_a/Gmacsall.out"), model = "smbkc16.0a")
m3 <- gmacs_read_allout(file = paste0(here::here(), "/SMBKC/smbkc_24s/model_16_0_b/Gmacsall.out"), model = "smbkc16.0b")
m4 <- gmacs_read_allout(file = paste0(here::here(), "/SMBKC/smbkc_24s/model_16_0_c/Gmacsall.out"), model = "smbkc16.0c")
m5 <- gmacs_read_allout(file = paste0(here::here(), "/SMBKC/smbkc_24s/model_24_0/Gmacsall.out"), model = "smbkc24.0")

M <- list(m1, m2, m3, m4, m5)

# *******************************************************************************
# Management quantities table

manage.quant <- function(i) {
  m <- M[[i]]
  m.name <- gsub("smbkc", "Model ", m$model_name)
  Bcurr_BMSY <- m$reference_points %>% filter(parameter_name == "Bcurr/BMSY") %>% pull(estimate)
  BMSY <- m$reference_points %>% filter(parameter_name == "BMSY") %>% pull(estimate)
  MMB <- round(BMSY * Bcurr_BMSY, 3)
  Fofl <- m$reference_points %>% filter(parameter_name == "Fofl_1") %>% pull(estimate)
  OFL <- m$reference_points %>% filter(parameter_name == "OFL_tot") %>% pull(estimate)
  ABC <- OFL * 0.75
  mq.vec <- c(m.name, MMB, BMSY, Bcurr_BMSY, Fofl, OFL, ABC)
  assign(paste0("mq", i), mq.vec, 1)
}

mq.df1 <- data.frame(sapply(1:5, manage.quant))
names(mq.df1) <- mq.df1 %>% slice(1) %>% unlist()
mq.df2 <- mq.df1 %>% slice(-1)

Component <- c(paste0("$\\text{MMB}_{", cur_yr, "}$"), "$B_\\text{MSY}$", "$MMB/B_\\text{MSY}$", "$F_\\text{OFL}$", paste0("$\\text{OFL}_{", cur_yr, "}$"), paste0("$\\text{ABC}_{", cur_yr, "}$"))

mq.df <- cbind(Component, mq.df2)

write.csv(mq.df, paste0(here::here(), "/SMBKC/", folder, "/doc/safe_tables/manage_quant.csv"), row.names = FALSE)

# *******************************************************************************
# Data weighting table

data.wt <- function(i) {
  m <- M[[i]]
  m.name <- gsub("smbkc", "Model ", m$model_name)
  m.nmfs.wt <- 1.00
  m.adfg.wt <- 1.00
  m.pot.wt.lf <- 1.00
  m.nmfs.wt.lf <- 1.00
  m.adfg.wt.lf <- 1.00
  m.nmfs.sdnr <- m$sdnr_MAR_cpue[1,1]
  m.adfg.sdnr <- m$sdnr_MAR_cpue[2,1]
  m.pot.sdnr.lf <- m$sdnr_MAR_lf[1,1]
  m.nmfs.sdnr.lf <- m$sdnr_MAR_lf[2,1]
  m.adfg.sdnr.lf <- m$sdnr_MAR_lf[3,1]
  m.nmfs.mar <- m$sdnr_MAR_cpue[1,2]
  m.adfg.mar <- m$sdnr_MAR_cpue[2,2]
  m.pot.mar.lf <- m$sdnr_MAR_lf[1,2]
  m.nmfs.mar.lf <- m$sdnr_MAR_lf[2,2]
  m.adfg.mar.lf <- m$sdnr_MAR_lf[3,2]
  wt.vec <- c(m.name, m.nmfs.wt, m.adfg.wt, m.pot.wt.lf, m.nmfs.wt.lf, m.adfg.wt.lf, m.nmfs.sdnr, m.adfg.sdnr, m.pot.sdnr.lf, m.nmfs.sdnr.lf, m.adfg.sdnr.lf, m.nmfs.mar, m.adfg.mar, m.pot.mar.lf, m.nmfs.mar.lf, m.adfg.mar.lf)
  assign(paste0("wt", i), wt.vec, 1)
}

df1.wt <- data.frame(sapply(1:5, data.wt))
names(df1.wt) <- df1.wt %>% slice(1) %>% unlist()
df2.wt <- df1.wt %>% slice(-1)

Component.wt <- c("NMFS trawl survey weight", "ADF&G pot survey weight", "Directed pot LF weight", "NMFS trawl survey LF weight", "ADF&G pot survey LF weight", "SDNR NMFS trawl survey", "SDNR ADF&G pot survey", "SDNR directed pot LF", "SDNR NMFS trawl survey LF", "SDNR ADF&G pot survey LF", "MAR NMFS trawl survey", "MAR ADF&G pot survey", "MAR directed pot LF", "MAR NMFS trawl survey LF", "MAR ADF&G pot survey LF")

df.wt <- cbind(Component.wt, df2.wt)

write.csv(df.wt, paste0(here::here(), "/SMBKC/", folder, "/doc/safe_tables/data_weighting.csv"), row.names = FALSE)

# *******************************************************************************
# Likelihood components table

liks <- function(i) {
  m <- M[[i]]
  m.name <- gsub("smbkc", "Model ", m$model_name)
  pot.ret <- m$likelihoods_by_type_and_fleet %>% filter(process == "catch_1") %>% pull(net_lik)
  pot.dis <- m$likelihoods_by_type_and_fleet %>% filter(process == "catch_2") %>% pull(net_lik)
  tby.dis <- m$likelihoods_by_type_and_fleet %>% filter(process == "catch_3") %>% pull(net_lik)
  fby.dis <- m$likelihoods_by_type_and_fleet %>% filter(process == "catch_4") %>% pull(net_lik)
  ind.nmfs <- m$likelihoods_by_type_and_fleet %>% filter(process == "index_1") %>% pull(net_lik)
  ind.adfg <- m$likelihoods_by_type_and_fleet %>% filter(process == "index_2") %>% pull(net_lik)
  pot.lf <- m$likelihoods_by_type_and_fleet %>% filter(process == "size_comp_1") %>% pull(net_lik)
  nmfs.lf <- m$likelihoods_by_type_and_fleet %>% filter(process == "size_comp_2") %>% pull(net_lik)
  adfg.lf <- m$likelihoods_by_type_and_fleet %>% filter(process == "size_comp_3") %>% pull(net_lik)
  rec.dev <- m$likelihoods_by_type_and_fleet %>% filter(process == "rec_pen_1") %>% pull(net_lik)
  f.pen <- m$penalties %>% filter(penalty == "Mean_Fdev") %>% pull(raw_lik)
  m.pen <- m$penalties %>% filter(penalty == "Mdevs") %>% pull(net_lik)
  prior <- m$likelihoods_by_type %>% filter(process == "priors") %>% pull(net_lik)
  tot.lik <- m$likelihoods_by_type %>% filter(process == "total") %>% pull(net_lik)
  est.param <- m$parameters %>% filter(phase > 0) %>% count() %>% unname() %>% unlist()
  lik.vec <- c(m.name, pot.ret, pot.dis, tby.dis, fby.dis, ind.nmfs, ind.adfg, pot.lf, nmfs.lf, adfg.lf, rec.dev, f.pen, m.pen, prior, tot.lik, est.param)
  assign(paste0("lik", i), lik.vec, 1)
}

lik.df1 <- data.frame(sapply(1:5, liks))
names(lik.df1) <- lik.df1 %>% slice(1) %>% unlist()
lik.df2 <- lik.df1 %>% slice(-1)

Component <- c("Pot retained catch", "Pot discarded catch", "Trawl bycatch discarded catch", "Fixed bycatch discarded catch", "NMFS trawl survey", "ADF&G pot survey CPUE", "Directed pot LF", "NMFS trawl LF", "ADF&G pot LF", "Recruitment deviations", "F penalty", "M penalty", "Prior", "Total", "Total estimated parameters")

lik.df <- cbind(Component, lik.df2)

write.csv(lik.df, paste0(here::here(), "/SMBKC/", folder, "/doc/safe_tables/likelihood_components.csv"), row.names = FALSE)
