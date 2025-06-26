# BBRKC state harvest strategy CSA model RTMB
## tyler jackson
## 6/25/2025

library(tidyverse)
library(RTMB)

read_dat <- function(file) {
  
  # Suppress the NA message in the coercion to double
  options(warn = -1)
  
  # read text file
  tmp <- read.delim(file, sep = "", header = F, col.names = c(1:1000), fill = T, na.strings = "", colClasses = "character")
  # create out object
  data <- list()
  # get only data that rows start with a number
  tmp_alpha <- filter(tmp, grepl("^[A-Za-z]", tmp[,1]))
  tmp <- filter(tmp, !is.na(as.numeric(tmp[,1])))
  
  # sex
  data$sex <- as.numeric(tmp[1, 1]); last <- 2
  
  # start and end year
  data$start_yr <- as.numeric(tmp[last, 1]); last <- 3
  data$end_yr <- as.numeric(tmp[last, 1]); last <- 4
  
  # bins 
  data$nbin <- as.numeric(tmp[last, 1]); last <- 5
  data$bin_brks <- as.numeric(tmp[last, 1:(data$nbin+1)]); last <- last + 1
  
  # weight at length
  data$wal <- as.numeric(tmp[last, 1:(data$nbin)]); last <- last + 1
  
  # number of survey rows
  data$survey_rows <- as.numeric(tmp[last, 1:2]); last <- last + 1
  
  # survey abundance
  # new shell
  data$obs_ns <- matrix(nrow = data$survey_rows[1], ncol = data$nbin + 2)
  for(i in 1:data$survey_rows[1]){
    data$obs_ns[i,] <- as.numeric(tmp[last, 1:ncol(data$obs_ns)])
    last <- last + 1
  }
  
  # old shell
  data$obs_os <- matrix(nrow = data$survey_rows[2], ncol = data$nbin + 2)
  for(i in 1:data$survey_rows[1]){
    data$obs_os[i,] <- as.numeric(tmp[last, 1:ncol(data$obs_os)])
    last <- last + 1
  }
  
  # tau 
  data$tau <- matrix(nrow = length(data$start_yr:data$end_yr), ncol = 2)
  for(i in 1:nrow(data$tau)){
    data$tau[i,] <- as.numeric(tmp[last, 1:2])
    last <- last + 1
  }
  
  # number of catch rows
  data$catch_rows <- as.numeric(tmp[last, 1:2]); last <- last + 1
  
  # retained catch
  data$catch <- matrix(nrow = data$catch_rows[1], ncol = data$nbin + 1)
  for(i in 1:data$catch_rows[1]){
    data$catch[i,] <- as.numeric(tmp[last, 1:ncol(data$catch)])
    last <- last + 1
  }
  
  # bycatch mortality
  data$bycatch <- matrix(nrow = data$catch_rows[2], ncol = data$nbin + 1)
  for(i in 1:data$catch_rows[2]){
    data$bycatch[i,] <- as.numeric(tmp[last, 1:ncol(data$bycatch)])
    last <- last + 1
  }
  
  # return list
  return(data)
  
}
read_ctl  <- function(file, data) {
  
  # Suppress the NA message in the coercion to double
  options(warn = -1)
  
  # read text file ----
  tmp <- read.delim(file, sep = "", header = F, col.names = c(1:1000), fill = T, na.strings = "", colClasses = "character")
  # create out objects
  pars <- list()
  brks <- list()
  lower_bounds <- list()
  upper_bounds <- list()
  map <- list() 
  # get only data that rows start with a number
  tmp_alpha <- filter(tmp, grepl("^[A-Za-z]", tmp[,1]))
  tmp <- filter(tmp, !is.na(as.numeric(tmp[,1])))
  
  # block setup ----
  n_blk_grps <-  as.numeric(tmp[1, 1]); last <- 2
  # read blocks
  blocks <- list()
  for (i in 1:n_blk_grps) {
    blocks[[i]] <-  as.numeric(na.omit(as.numeric(tmp[last,]))); last <- last + 1
  }
  
  
  # initial abundance -----
  ln_n_init <- as.numeric(tmp[last, 1:4]); last <- last + 1
  pars$ln_n_init <- ln_n_init[1]
  lower_bounds$ln_n_init <- ln_n_init[2]
  upper_bounds$ln_n_init <- ln_n_init[3]
  if(ln_n_init[4] == 1){map$ln_n_init <- factor(NA)}
  
  
  # recruitment distribution ----
  # alpha
  ln_ra <- as.numeric(tmp[last, 1:4]); last <- last + 1
  pars$ln_ra <- ln_ra[1]
  lower_bounds$ln_ra <- ln_ra[2]
  upper_bounds$ln_ra <- ln_ra[3]
  if(ln_ra[4] == 1){map$ln_ra <- factor(NA)}
  # beta
  ln_rbeta <- as.numeric(tmp[last, 1:4]); last <- last + 1
  pars$ln_rbeta <- ln_rbeta[1]
  lower_bounds$ln_rbeta <- ln_rbeta[2]
  upper_bounds$ln_rbeta <- ln_rbeta[3]
  if(ln_rbeta[4] == 1){map$ln_rbeta <- factor(NA)}
  
  # annual recruitment ----
  ln_recruits <- matrix(nrow = length((data$start_yr+1):data$end_yr), ncol = 4)
  for(i in 1:length((data$start_yr+1):data$end_yr)) {
    ln_recruits[i, 1:4] <- as.numeric(tmp[last, 1:4]); last <- last + 1
  }
  pars$ln_recruits <- ln_recruits[,1]
  lower_bounds$ln_recruits <- ln_recruits[,2]
  upper_bounds$ln_recruits <- ln_recruits[,3]
  for (i in 1:nrow(ln_recruits)){if(ln_recruits[i,4] == 1) {map$ln_recruits[i] = factor(NA)} }
  
  # natural mortality ----
  natm_base <- as.numeric(tmp[last, 1:5]); last <- last + 1
  
  if(as.numeric(natm_base[4]) > 0){
    n_extra_pars <- length(blocks[[as.numeric(natm_base[4])]])
    # read extra pars
    natm_extra <- matrix(nrow = n_extra_pars, ncol = 4)
    for (i in 1:n_extra_pars) {
      natm_extra[i, 1:4] <- as.numeric(tmp[last, 1:4]); last <- last + 1
    }
  } else {natm_extra <- NULL}
  
  # combine base and extra pars
  natm <- rbind(natm_base[c(1:3, 5)], natm_extra)
  pars$natm <- natm[,1]
  brks$natm <- blocks[[as.numeric(natm_base[4])]]
  lower_bounds$natm <- natm[,2]
  upper_bounds$natm <- natm[,3]
  map$natm <- NULL
  for (i in 1:nrow(natm)){if(natm[i,4] == 1) {map$natm[i] = factor(NA)}}
  
  # molt probability ----
  
  molt_a_base <- as.numeric(tmp[last, 1:5]); last <- last + 1
  molt_b_base <- as.numeric(tmp[last, 1:5]); last <- last + 1
  
  if(as.numeric(molt_a_base[4]) > 0){
    n_extra_pars <- length(blocks[[as.numeric(molt_a_base[4])]])
    # read extra pars
    molt_a_extra <- matrix(nrow = n_extra_pars, ncol = 4)
    for (i in 1:n_extra_pars) {
      molt_a_extra[i, 1:4] <- as.numeric(tmp[last, 1:4]); last <- last + 1
    }
  } else {nmolt_a_extra <- NULL}
  
  # combine base and extra pars
  molt_a <- rbind(molt_a_base[c(1:3, 5)], molt_a_extra)
  pars$molt_a <- molt_a[,1]
  brks$molt_a <- blocks[[as.numeric(molt_a_base[4])]]
  lower_bounds$molt_a <- molt_a[,2]
  upper_bounds$molt_a <- molt_a[,3]
  map$molt_a <- NULL
  for (i in 1:nrow(molt_a)){if(molt_a[i,4] == 1) {map$molt_a[i] = factor(NA)}}
  
  if(as.numeric(molt_b_base[4]) > 0){
    n_extra_pars <- length(blocks[[as.numeric(molt_b_base[4])]])
    # read extra pars
    molt_b_extra <- matrix(nrow = n_extra_pars, ncol = 4)
    for (i in 1:n_extra_pars) {
    molt_b_extra[i, 1:4] <- as.numeric(tmp[last, 1:4]); last <- last + 1
    }
  } else {molt_b_extra <- NULL}
  
  # combine base and extra pars
  molt_b <- rbind(molt_b_base[c(1:3, 5)], molt_b_extra)
  pars$molt_b <- molt_b[,1]
  brks$molt_b <- blocks[[as.numeric(molt_b_base[4])]]
  lower_bounds$molt_b <- molt_b[,2]
  upper_bounds$molt_b <- molt_b[,3]
  map$molt_b <- NULL
  for (i in 1:nrow(molt_b)){if(molt_b[i,4] == 1) {map$molt_b[i] = factor(NA)}}
  
  # growth ----
  
  # gamma scale
  gscale <- as.numeric(tmp[last, 1:4]); last <- last + 1
  pars$gscale <- gscale[1]
  lower_bounds$gscale <- gscale[2]
  upper_bounds$gscale <- gscale[3]
  if(gscale[4] == 1){map$gscale <- factor(NA)}
  
  # molt increments
  molt_inc <- matrix(nrow = data$nbin, ncol = 4)
  for(i in 1:data$nbin) {
    molt_inc[i, 1:4] <- as.numeric(tmp[last, 1:4]); last <- last + 1
  }
  pars$molt_inc <- molt_inc[,1]
  lower_bounds$molt_inc <- molt_inc[,2]
  upper_bounds$molt_inc <- molt_inc[,3]
  map$molt_inc <- molt_inc[,4]
  for (i in 1:nrow(molt_inc)){if(molt_inc[i,4] == 1) {map$molt_inc[i] = factor(NA)} }
  
  
  
  # output ----
  return(list(pars = pars, lower_bounds = lower_bounds, upper_bounds = upper_bounds, map = map))
}
hs_csa <- function(pars) {
  
  # get parameters and data
  RTMB::getAll(pars, dat_in)
  
  # parameters ----
  # time block function and block pars
  f_tblocks <- function(break_years, param_blocks){
    breaks <- c(min(yrs), break_years, max(yrs) + 1)
    par_out <- rep(0, nyr)
    for(i in 1:length(yrs)){
      for(b in 2:length(breaks)){
        if(yrs[i] < breaks[b] & yrs[i] >= breaks[b-1]) {par_out[i] <- param_blocks[b-1] }
      }
    }

    return(par_out)
  }
  
  ## molt probability
  molt_a_tb <- f_tblocks(ctl$brks$molt_a, molt_a)
  molt_b_tb <- f_tblocks(ctl$brks$molt_b, molt_b)
  # natural mortality
  natm_tb <- f_tblocks(ctl$brks$natm, natm)
  # initial n
  n_init <- exp(ln_n_init)
  # recruitment
  recruits <- exp(ln_recruits)
  ra <- exp(ln_ra)
  rbeta <- exp(ln_rbeta)
  # sigmas
  sigma_survey <- exp(ln_sigma_survey)
  

  # setup growth ----
  
  ## molt probability
  molt_prob = array(0, dim = c(nyr, nbin))
  for(y in 1:nyr){
    for(l in 1:nbin){
      molt_prob[y, l] = 1 - (1 / (1 + molt_a_tb[y] * exp(-molt_b_tb[y] * mid_bins[l])))
    }
  }
  
  ## growth transition
  ### gamma distributed molt increment with specified molt increment parameters (from GMACS)
  gtrans = array(0, dim = c(nbin, nbin))
  for (l in 1:(nbin - 1)) {
    mean_size_after_molt <- molt_inc[l] / gscale
    accum <- 0
    for (ll in l:(nbin - 1)) {
      upp_inc <- (bin_brks[ll + 1] -  mid_bins[l]) / gscale
      cumul_inc <- pgamma(upp_inc, mean_size_after_molt)
      gtrans[l, ll] <- cumul_inc - accum
      accum <- cumul_inc
    }
    gtrans[l, nbin] <- 1.0 - accum
  }
  gtrans[nbin, nbin] <- 1.0
  # transpose
  for (i in 1:nrow(gtrans)) {
    for (j in 1:ncol(gtrans)) {
      gtrans[j, i] <- gtrans[i, j]
    }
  }

  
  # setup recruitment ----
  
  ### recruitment distribution (from GMACS)
  ralpha = ra / rbeta
  z = pgamma(bin_brks / rbeta, ralpha)
  rec_sdd = diff(z)
  rec_sdd[(nbin + 1):length(rec_sdd)] = 0
  recdist = c(rec_sdd / sum(rec_sdd, na.rm = T), 0)
  
  ### fill recruitment matrix
  r =  array(data = 0, dim = c(nyr-1, nbin))
  for(y in 1:(nyr-1)){
    for(l in 1:nbin){
      r[y, l] = recruits[y] * recdist[l]
    }
  }
  

  # do predictions ----
  
  ## initialize matrices for predicted abundance
  pre_ns = array(0, dim = c(nyr, nbin)) # new shell n matrix
  pre_os = array(0, dim = c(nyr, nbin)) # old shell n matrix
  
  ## initial conditions
  ### assume initial proportions at length are true
  obs_init_n <- 0
  for(l in 1:nbin){
    obs_init_n <- obs_init_n  + obs_ns[1,l+2] + obs_os[1,l+2]
    
  }
  obs_lfq_ns = obs_ns[1,-1:-2] / obs_init_n # observed length frequency of new shell
  obs_lfq_os = obs_os[1,-1:-2] / obs_init_n # observed length frequency of old shell
  pre_ns[1,] = obs_lfq_ns * n_init # predicted yr 1 new shell
  pre_os[1,] = obs_lfq_os * n_init # predicted yr 1 old shell
  
  ## first length group
  for (y in 2:nyr) {
    # recruitment only
    pre_ns[y, 1] <- r[y-1, 1]
    # crab in previous yrs that survived and did not molt
    pre_os[y, 1] <- (pre_ns[y-1, 1] + pre_os[y-1, 1]) * exp(-natm_tb[y]) * (1.0 - molt_prob[y - 1, 1])
  }
  
  # project population forward
  for (l in 1:(nbin - 1)) {
    for (y in 1:(nyr - 1)) {
      
      # new shell
      tmp <- 0
      for (j in 1:(l + 1)) {
        tmp <- tmp + gtrans[l + 1, j] * ((pre_ns[y, j] + pre_os[y, j]) * exp(-natm_tb[y]) - (catch[y, j] + bycatch[y, j]) / wal[j] * exp((tau[y] - 1) * natm_tb[y])) * molt_prob[y, j]
      }
      pre_ns[y + 1, l + 1] <- max(tmp, 1e-6) + r[y, l + 1]
      
      # old shell
      pre_os[y + 1, l + 1] <- ((pre_ns[y, l + 1] + pre_os[y, l + 1]) * exp(-natm_tb[y]) - (catch[y, l + 1] + bycatch[y, l + 1])  / wal[l + 1] * exp((tau[y] - 1) * natm_tb[y])) * (1.0 - molt_prob[y, l + 1])
      pre_os[y + 1, l + 1] <- max(pre_os[y + 1, l + 1],  1e-6)
      
    }
  }

  # likelihood ----
  
  ns_lik <- 0
  # new shell
  pre_ns_na.rm <- matrix(nrow = nrow(obs_ns), ncol = nbin)
  for(l in 1:nbin){
    for(y in 1:nrow(obs_ns)){ 
      y_row <- which(yrs == obs_ns[y, 1]) #index the nrow of the pred matrix that year y of survey data corresponds to
      pre_ns_na.rm[y,l] <- pre_ns[y_row,l] # vector of predictions for stage h in only years that we have data
      ns_lik <- ns_lik + -dnorm(obs_ns[y, l+2], pre_ns_na.rm[y,l], sigma_survey[1], TRUE)
      #ns_lik <- ns_lik + (obs_ns[y, l+2] - pre_ns_na.rm[y,l])^2
    }
  }
  os_lik <- 0
  # os shell
  pre_os_na.rm <- matrix(nrow = nrow(obs_os), ncol = nbin)
  for(l in 1:nbin){
    for(y in 1:nrow(obs_os)){
      y_row <- which(yrs == obs_os[y, 1]) #index the nrow of the pred matrix that year y of survey data corresponds to
      pre_os_na.rm[y,l] <- pre_os[y_row,l] # vector of predictions for stage h in only years that we have data
      os_lik <- os_lik + -dnorm(obs_os[y, l+2], pre_os_na.rm[y,l], sigma_survey[2], TRUE)
      #os_lik <- os_lik + (obs_os[y, l+2] - pre_os_na.rm[y,l])^2
    }
  }
  # objective function
  obj_func <- ns_lik + os_lik
  
  
  # report ----
  RTMB::REPORT(pre_ns)
  RTMB::REPORT(pre_os)
  RTMB::REPORT(obs_ns)
  RTMB::REPORT(obs_os)
  RTMB::REPORT(ns_lik)
  RTMB::REPORT(os_lik)
  RTMB::REPORT(obj_func)
  RTMB::REPORT(gtrans)
  RTMB::REPORT(molt_prob)
  
  # return ----
  return(obj_func)
  
}
run_hs_csa <- function(dat_file, ctl_file, out_dir = getwd()) {
  
  # read inputs ----
  
  # read data file
  dat <- read_dat(file = dat_file)
  
  # read ctl file 
  ctl <- read_ctl(file = ctl_file, data = dat)
  ctl$pars$ln_sigma_survey <- rep(log(0.1), 2)
  #ctl$map$ln_recruits <- rep(NA, 52)
  
  # organize input data ----
  
  # duplicate dat
  dat_in <- list()
  # model dim 
  dat_in$yrs <- dat$start_yr:dat$end_yr
  dat_in$nyr <- length(dat_in$yrs)
  dat_in$nbin <- dat$nbin
  dat_in$bin_brks <- dat$bin_brks
  dat_in$mid_bins <- dat_in$bin_brks[-(dat_in$nbin+1)]+2.5
  # catch data
  dat_in$catch <- dat$catch %>%
    as_tibble() %>%
    right_join(tibble(V1 = dat_in$yrs), by = join_by(V1)) %>% 
    replace(is.na(.), 0) %>%
    dplyr::select(-1) %>%
    as.matrix()
  # bycatch data
  dat_in$bycatch <- dat$bycatch %>%
    as_tibble() %>%
    right_join(tibble(V1 = dat_in$yrs), by = join_by(V1)) %>% 
    replace(is.na(.), 0) %>%
    dplyr::select(-1) %>%
    as.matrix()
  # survey data
  dat_in$obs_ns <- dat$obs_ns
  dat_in$obs_os <- dat$obs_os
  # tau
  dat_in$tau <- dat$tau[,2]
  # weight at length
  dat_in$wal <- dat$wal
  
  
  # run model ----
  
  # factorize map
  for(i in 1:length(ctl$map)) {ctl$map[[i]] <- factor(ctl$map[[i]])}
  # build model
  mod <- RTMB::MakeADFun(hs_csa, parameters = ctl$pars, map = ctl$map)
  # optimize
  opt <- stats::nlminb(mod$par, mod$fn, mod$gr,
                       control = list(iter.max = 1e5, eval.max = 1e5, rel.tol = 1e-15))
  
  # file outputs ----
  
  wd <- getwd() # save current wd
  setwd(out_dir) # move to file out directory
  
  # par out
  sink("par.out")  # Start redirecting output
  print(summary(sdreport(mod)))  # Or any other RTMB output
  sink() 
  
  # report 
  sink("rep.out")  # Start redirecting output
  print(mod$report())  # Or any other RTMB output
  sink() 
  
  # return
  setwd(wd)
  
  # r outputs ----
  
  rep <- mod$report()
  
  #numbers and biomass at size
  rep$pre_ns %>%
    as_tibble() %>%
    bind_cols(tibble(year = dat_in$yrs, shell = "New"), .) %>%
    rename_at(3:ncol(.), ~as.character(dat_in$mid_bins)) %>%
    pivot_longer(3:ncol(.), names_to = "size", values_to = "pre_n") %>%
    mutate(size = as.numeric(size)) %>%
    left_join(tibble(size = dat_in$mid_bins, wal = dat_in$wal), by = join_by(size)) %>%
    mutate(pre_t = pre_n * wal) %>% dplyr::select(-wal) %>%
    
    bind_rows(  rep$pre_os %>%
                  as_tibble() %>%
                  bind_cols(tibble(year = dat_in$yrs, shell = "Old"), .) %>%
                  rename_at(3:ncol(.), ~as.character(dat_in$mid_bins)) %>%
                  pivot_longer(3:ncol(.), names_to = "size", values_to = "pre_n") %>%
                  mutate(size = as.numeric(size)) %>%
                  left_join(tibble(size = dat_in$mid_bins, wal = dat_in$wal), by = join_by(size)) %>%
                  mutate(pre_t = pre_n * wal) %>% dplyr::select(-wal) ) %>%
    left_join( rep$obs_ns %>%
                 as_tibble() %>%
                 rename_all(~c("year", "shell", as.character(dat_in$mid_bins))) %>%
                 pivot_longer(3:ncol(.), names_to = "size", values_to = "obs_n") %>%
                 mutate(size = as.numeric(size),
                        shell = case_when(shell == 1 ~ "New",
                                          shell == 2 ~ "Old")) %>%
                 left_join(tibble(size = dat_in$mid_bins, wal = dat_in$wal), by = join_by(size)) %>%
                 mutate(obs_t = obs_n * wal) %>% dplyr::select(-wal) %>%
                 
                 bind_rows( rep$obs_os %>%
                              as_tibble() %>%
                              rename_all(~c("year", "shell", as.character(dat_in$mid_bins))) %>%
                              pivot_longer(3:ncol(.), names_to = "size", values_to = "obs_n") %>%
                              mutate(size = as.numeric(size),
                                     shell = case_when(shell == 1 ~ "New",
                                                       shell == 2 ~ "Old")) %>%
                              left_join(tibble(size = dat_in$mid_bins, wal = dat_in$wal), by = join_by(size)) %>%
                              mutate(obs_t = obs_n * wal) %>% dplyr::select(-wal) ), ., by = join_by(year, shell, size) ) -> est_at_size
  
  # total numbers by year
  est_at_size %>%
    group_by(year, shell) %>% 
    summarise(obs_n = sum(obs_n), obs_t = sum(obs_t),
              pre_n = sum(pre_n), pre_t = sum(pre_t)) %>%
    ungroup -> est_year
  
  
  rep_out <- list(nll = rep$obj_func,
                  lik_comp = c(rep$ns_lik, rep$os_lik),
                  est_at_size = est_at_size,
                  est_year = est_year,
                  gtrans = rep$gtrans,
                  molt_prob = rep$molt_prob,
                  sdrepor = sdreport(mod),
                  opt = opt)
                    
  return(rep_out)
                  
  

  }



run_hs_csa(dat_file, ctl_file, "BBRKC/tyler_hs_csa")

