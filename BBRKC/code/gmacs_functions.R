# functions for processing gmacs models
# tyler jackson
# 5/1/2023

# load ----
library(gmr)
library(ggpmisc)
library(ggridges)
library(tidyverse)

source("./AIGKC/code/aigkc_functions.R")

# ggplot theme and yr axis ----

theme_sleek <- function(base_size = 12, base_family = "Times") {
  
  windowsFonts(Times=windowsFont("TT Times New Roman"))
  
  half_line <- base_size/2
  
  theme_light(base_size = base_size, base_family = base_family) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.ticks.length = unit(half_line / 2.2, "pt"),
      strip.background = element_rect(fill = NA, colour = NA),
      strip.text.x = element_text(colour = "black"),
      strip.text.y = element_text(colour = "black"),
      #axis.text = element_text(colour = "grey30"),
      #axis.title = element_text(colour = "grey30"),
      #legend.title = element_text(colour = "grey30"),#, size = rel(0.9)
      panel.border = element_rect(fill = NA),#, colour = "grey70", size = 1),
      legend.key.size = unit(0.9, "lines"),
      #legend.text = element_text(size = rel(0.7)),#, colour = "grey30"),
      legend.key = element_rect(colour = NA, fill = NA),
      legend.background = element_rect(colour = NA, fill = NA)#,
      #plot.title = element_text(colour = "grey30"),#, size = rel(1)
      #plot.subtitle = element_text(colour = "grey30")#, size = rel(.85)
    )
  
}
tickr <- function(data, var, to) {
  
  VAR <- enquo(var) # makes VAR a dynamic variable
  
  data %>%
    dplyr::filter(!is.na(!!VAR)) %>%
    distinct(!!VAR) %>%
    mutate(labels = ifelse(!!VAR %in% seq(to * round(min(!!VAR) / to), max(!!VAR), to),
                           !!VAR, "")) %>%
    dplyr::select(breaks = UQ(VAR), labels)
}

theme_set(theme_sleek())
yraxis <- tickr(tibble(yr = 1950:2100), yr, 5)

### custom color/fill pallete
cb_palette <- c("#009E73", "#0072B2", "#E69F00", "#56B4E9", 
                "#F0E442", "#D55E00", "#CC79A7")
scale_colour_discrete <- function(...) {scale_colour_manual(..., values = cb_palette)}
scale_fill_discrete <- function(...) {scale_fill_manual(..., values = cb_palette)}
ggplot <- function(...) ggplot2::ggplot(...) + scale_colour_discrete() + scale_fill_discrete()

# f_gmacs_francis_reweight() ----
## args: dir = directory to model
#        ctl = name of control file
#        convergence_threshold = level of precision for convergence of wts, default = 0.01
#        max_iter = maximum number of calls to gmacs, default = 1000
f_gmacs_francis_reweight <- function(dir, ctl, convergence_threshold = 0.001, max_iter = 1000) {
  
  # get current wd
  current_dir <- getwd()
  # set wd for gmacs to run in 
  setwd(dir)
  # get file paths and line number with wts
  ctl_path <- file.path(getwd(), ctl)
  rep_path <- file.path(dir, "gmacs.rep")
  ctl_wts_line <- grep("Francis wts multiplier", readLines(ctl_path))
  ctl_ref_line <- grep("Calculate reference points", readLines(ctl_path))
  
  # turn off reference point calculation
  ctl_file <- readLines(ctl_path)
  ctl_file[ctl_ref_line] <- "0 # Calculate reference points (0=no)"
  writeLines(ctl_file, ctl_path)
  
  # check pin file
  if(file.path(dir, "gmacs.pin") %in% list.files(dir)) {
    file.copy(file.path(dir, "gmacs.par"), file.path(dir, "gmacs.pin"))
    }
  
  for (i in 1:max_iter) {
    # extract adjusted weights from report file
    rep_wts <- f_read_rep(file.path(getwd(), "gmacs.rep"))$Francis_weights
    
    # read weights from report file
    ctl_wts <- scan(ctl_path, 
                    skip = grep("Francis wts multiplier", readLines(ctl_path))-1, 
                    n = 2, 
                    comment.char = "#", 
                    quiet = T)
    
    # test for difference
    if(sum(abs(rep_wts - ctl_wts) <= convergence_threshold) == length(rep_wts)) {break} 
    if(sum(abs(rep_wts - ctl_wts) <= convergence_threshold) != length(rep_wts)) {
      # re-write wts in ctl file
      ctl_file <- readLines(ctl_path)
      ctl_file[ctl_wts_line] <- paste(c(as.character(rep_wts), "# Francis wts multiplier"), collapse = " ")
      writeLines(ctl_file, ctl_path)
      # re-run gmacs
      system("gmacs.exe")
      # update pin
      file.copy(file.path(dir, "gmacs.par"), file.path(dir, "gmacs.pin"))
    }
    
  }
  print(paste0("Done. Francis wts converged within ", convergence_threshold))
  
  # rerun with reference point calculation
  ctl_file <- readLines(ctl_path)
  ctl_file[ctl_ref_line] <- "1 # Calculate reference points (0=no)"
  writeLines(ctl_file, ctl_path)
  # re-run gmacs
  system("gmacs.exe")
  setwd(current_dir)
  
  
}

# f_run_jitter() ----
## args: dir = directory containing gmacs files for the model you want to do jitter analysis on
#        jitter_factor = jitter sd
#        iterations = number of iterations to run
#        ref_points = calculate bioloigcal reference points or not, default = T
f_run_jitter <- function(dir, jitter_factor, iterations, ref_points = T) {
  
  # get current wd
  current_dir <- getwd()
  # set wd for gmacs to run in 
  setwd(dir)
  
  # create subdirectory for jitter run files
  dir.create("./jitter")
  
  # do jitter runs
  out <- tibble(iteration = 1:iterations,
                obj_fun = NA,
                max_gradient = NA,
                catch_lik = NA,
                survey_lik = NA,
                size_lik = NA,
                mmb = NA,
                b35 = NA)
  for (i in 1:iterations) {
    
    # run 1
    if(i == 1) {
      dir.create("./jitter/run_1")
      file.copy("./gmacs.dat", "./jitter/run_1")
      setwd("jitter/run_1")
      # copy over files to run gmacs
      file.copy(paste0("../../", scan("./gmacs.dat", skip = 1, nlines = 1, what = "character")), ".") # dat
      file.copy(paste0("../../", scan("./gmacs.dat", skip = 3, nlines = 1, what = "character")), ".") # ctl
      file.copy(paste0("../../", scan("./gmacs.dat", skip = 5, nlines = 1, what = "character")), ".") # proj
      file.copy("../../gmacs.exe", ".") # exe
      # turn off reference point calculation
      if(ref_points == F) {
        ctl_file <- readLines(paste0("./", scan("./gmacs.dat", skip = 3, nlines = 1, what = "character")))
        ctl_ref_line <- grep("Calculate reference points", ctl_file)
        ctl_file[ctl_ref_line] <- "0 # Calculate reference points (0=no)"
        writeLines(ctl_file, paste0("./", scan("./gmacs.dat", skip = 3, nlines = 1, what = "character")))
      }
      # turn on jittering to specifications 
      gmacs_dat <- readLines("./gmacs.dat")
      jitter_line <- grep("Jitter", gmacs_dat) + 1
      gmacs_dat[jitter_line] <- paste0("1 ", jitter_factor)
      writeLines(gmacs_dat, "./gmacs.dat")
      # get a list of files to copy for future iterations
      files_to_copy <- list.files(getwd(), full.names = T)
    }
    # run 2:iterations
    if(i > 1){
      dir.create(paste0("./run_", i))
      setwd(paste0("./run_", i))
      file.copy(files_to_copy, getwd())
      
    }
    # do gmacs run
    system("./gmacs.exe")
    # save output stats
    rep <- f_read_rep("./gmacs.rep")
    par <- scan("./gmacs.par", nlines = 1, what = "character")
    out$obj_fun[i] <- as.numeric(par[11])
    out$max_gradient[i] <- as.numeric(par[16])
    out$catch_lik[i] <- sum(rep$Catches_like)
    out$survey_lik[i] <- sum(rep$Index_like)
    out$size_lik[i] <- sum(rep$Size_comp_like)
    if(ref_points == T) {
      out$mmb[i] <- rep$spr_bmsy * rep$spr_depl
      out$b35[i] <- rep$spr_bmsy
    }
    # return to ./jitter
    setwd("..")
  }
  
  # return to original working directory
  setwd(current_dir)
  # save output
  write.csv(out, paste0(dir, "./jitter/jitter_results_", jitter_factor, ".csv"))
  
  # plots
  par <- scan(file.path(dir, "./gmacs.par"), nlines = 1, what = "character")
  mle_obj <- as.numeric(par[11])
  mle_rep <- f_read_rep(file.path(dir, "gmacs.rep"))
  # obj fxn
  ggplot()+
    geom_histogram(data = out, aes(x = obj_fun), color = 1, fill = "grey80", 
                   width = 1)+
    geom_vline(xintercept = mle_obj, linetype = 2, color = 2)+
    labs(x = "Objective Function", y = "Jitter Runs") -> p_obj
  ggsave(filename = paste0(dir, "/jitter/jitter_obj_fxn_", jitter_factor, ".png"),
         plot = p_obj,
         height = 3, width = 5, units = "in")
  # mmb
  ggplot()+
    geom_histogram(data = out, aes(x = mmb), color = 1, fill = "grey80", 
                   width = 1)+
    geom_vline(xintercept = mle_rep$spr_bmsy * mle_rep$spr_depl, linetype = 2, color = 2)+
    scale_x_continuous(labels = scales::comma)+
    labs(x = "MMB (1000 t)", y = "Jitter Runs") -> p_mmb
  ggsave(paste0(dir, "/jitter/jitter_mmb_", jitter_factor, ".png"),
         plot = p_mmb,
         height = 3, width = 5, units = "in")
  # b35
  ggplot()+
    geom_histogram(data = out, aes(x = b35), color = 1, fill = "grey80", 
                   width = 1)+
    geom_vline(xintercept = mle_rep$spr_bmsy, linetype = 2, color = 2)+
    scale_x_continuous(labels = scales::comma)+
    labs(x = "B35% (1000 t)", y = "Jitter Runs") -> p_b35
  ggsave(filename = paste0(dir, "/jitter/jitter_b35_", jitter_factor, ".png"),
         plot = p_b35,
         height = 3, width = 5, units = "in") 
  # done
  return(out)
  
}


# f_read_rep() ----
## args: fn = file path to report file from GMACS (from Jie)
f_read_rep <- function(fn) {
  options(warn = -1) # Suppress the NA message in the coercion to double
  repfile <- scan(fn, what = "character", flush = TRUE, blank.lines.skip = FALSE, quiet = TRUE, na.strings = c("nan","-nan"))
  #repfile <- scan(fn, what = "character", flush = TRUE, blank.lines.skip = FALSE, quiet = TRUE)
  inan <- which(is.na(repfile)) # Identify any nan entries so that they are not picked up as objects
  idx <- sapply(as.double(repfile), is.na)
  idx[inan] <- FALSE
  vnam <- repfile[idx] # list names
  nv <- length(vnam) # number of objects
  A <- list()
  ir <- 0
  for (i in 1:nv)
  {
    ir <- match(vnam[i], repfile)
    if (i != nv)
    {
      irr <- match(vnam[i+1], repfile)
    } else {
      irr <- length(repfile) + 1 # next row
    }
    dum <- NA
    if (irr-ir == 2)
    {
      dum <- as.double(scan(fn, skip = ir, nlines = 1, quiet = TRUE, what = ""))
    }
    if (irr-ir > 2)
    {
      # ncols <- 0
      # irows <- ir:irr-1
      # for(j in irows)
      # {
      #       tmp=as.double(scan(fn,skip=j,nlines=1,quiet=TRUE,what=""))
      #       if(length(tmp)>ncols) ncols <- length(tmp)
      #       #print(paste(1:ncols))
      # }
      # cname <- paste(1:ncols)
      # dum=as.matrix(read.table(fn,skip=ir,nrow=irr-ir-1,fill=TRUE,col.names=cname))
      # cat("\n ir ",ir," irr ",irr)
      dum <- as.matrix(read.table(fn, skip = ir, nrow = irr-ir-1, fill = TRUE, row.names = NULL))
    }
    if (is.numeric(dum)) # Logical test to ensure dealing with numbers
    {
      A[[vnam[i]]] <- dum
    }
  }
  
  options(warn = 0)
  return(A)
}


# f_plot_catch() ----
## args: dirs = directory to rep files
#        models = names of model scenarios
#        file_path = file path to save plots
#        height = height in inches of plot
#        width = width in inches of plot

f_plot_catch <- function(dirs, models, file_path, height = 4, width = 6) {
  
  ## load all rep files
  biglist <- lapply(paste0(dirs, "/gmacs.rep"), f_read_rep)
  ## extract catch data
  tibble(model = models,
         rep = biglist) %>%
    mutate(catch = purrr::map(rep, function(rep) {
      
      # plot catch 
      rep$dCatchData %>%
        as_tibble() %>%
        rename_all(~c("year", "seas", "fleet", "sex", "obs", "cv", "type",
                      "units", "mult", "effort", "discard_mortality")) %>%
        # add predicted catch
        mutate(pred = as.numeric(na.omit(as.vector(t(rep$pre_catch)))))
      
    })) %>%
    transmute(model, catch) %>%
    unnest(catch) -> gmacs_catch 
  
  # retained
  gmacs_catch %>% 
    filter(fleet == 1, type == 1) %>%
    # fill in any missing years with NA
    right_join(expand_grid(model = models,
                           tibble(year = min(.$year):max(.$year)))) %>%
    # convert numbers to t for plotting
    mutate(obs = ifelse(units ==2, obs * 1000 * mean(len_bins$mean_t[len_bins$mid_cl >= 136]), obs),
           pred = ifelse(units ==2, pred * 1000 * mean(len_bins$mean_t[len_bins$mid_cl >= 136]), pred)) %>%
    ggplot()+
    geom_point(aes(x = year, y = obs), color = "grey40")+
    geom_errorbar(aes(x = year, ymin = obs + -1.96*cv*obs, ymax = obs + 1.96*cv*obs), 
                  width = 0, color = "grey40")+
    geom_line(aes(x = year, y = pred, color = model))+
    scale_x_continuous(labels = yraxis$labels, breaks = yraxis$breaks)+
    scale_y_continuous(labels = scales::comma)+
    labs(x = NULL, y = "Retained Catch (t)", color = NULL) -> p_ret
  ggsave(plot = p_ret, filename = paste0(file_path, "retained_catch_fit.png"), 
         height = height, width = width, units = "in")
  
  # total
  gmacs_catch %>% 
    filter(fleet == 1, type == 0) %>%
    # fill in any missing years with NA
    right_join(expand_grid(model = models,
                           tibble(year = min(.$year):max(.$year)))) %>%
    ggplot()+
    geom_point(aes(x = year, y = obs), color = "grey40")+
    geom_errorbar(aes(x = year, ymin = obs + -1.96*cv*obs, ymax = obs + 1.96*cv*obs), 
                  width = 0, color = "grey40")+
    geom_line(aes(x = year, y = pred, color = model))+
    scale_x_continuous(labels = yraxis$labels, breaks = yraxis$breaks)+
    scale_y_continuous(labels = scales::comma)+
    labs(x = NULL, y = "Total Catch (t)", color = NULL) -> p_tot
  ggsave(plot = p_tot, filename = paste0(file_path, "total_catch_fit.png"), 
         height = height, width = width, units = "in")
  
  # gf bycatch
  gmacs_catch %>% 
    filter(fleet == 2, type == 2) %>%
    # fill in any missing years with NA
    right_join(expand_grid(model = models,
                           tibble(year = min(.$year):max(.$year)))) %>%
    ggplot()+
    geom_point(aes(x = year, y = obs), color = "grey40")+
    geom_errorbar(aes(x = year, ymin = obs + -1.96*cv*obs, ymax = obs + 1.96*cv*obs), 
                  width = 0, color = "grey40")+
    geom_line(aes(x = year, y = pred, color = model))+
    scale_x_continuous(labels = yraxis$labels, breaks = yraxis$breaks)+
    scale_y_continuous(labels = scales::comma)+
    labs(x = NULL, y = "Groundfish Bycatch (t)", color = NULL) -> p_byc
  ggsave(plot = p_byc, filename = paste0(file_path, "gf_bycatch_catch_fit.png"), 
         height = height, width = width, units = "in")
  
  # save plot of all stacked
  ggsave(plot = p_ret/p_tot | p_byc/plot_spacer(), 
         filename = paste0(file_path, "catch_fit.png"), 
         height = 7, width = 11, units = "in")
  
}

# f_plot_index() ----
## args: dirs = directory to rep files
#        models = names of model scenarios
#        file_path = file path to save plots
#        height = height in inches of plot
#        width = width in inches of plot
f_plot_index <- function(dirs, models, file_path, height = 4, width = 6) {
  
  biglist <- lapply(paste0(dirs, "/gmacs.rep"), f_read_rep)
  ## extract catch data
  tibble(model = models,
         rep = biglist) %>%
    mutate(index = purrr::map(rep, function(rep) {
      
      # plot catch 
      rep$dSurveyData %>%
        as_tibble() %>%
        rename_all(~c("index", "year", "seas", "fleet", "sex", "maturity", "obs", "cv",
                      "units", "timing")) %>%
        # add predicted catch
        mutate(pred = as.numeric(na.omit(as.vector(t(rep$pre_cpue)))))
      
    })) %>%
    transmute(model, index) %>%
    unnest(index) -> gmacs_index
  
  # fishery cpue
  gmacs_index %>%
    filter(index %in% 1:2) %>%
    ggplot()+
    geom_point(aes(x = year, y = obs), color = "grey40")+
    geom_errorbar(aes(x = year, ymin = obs + -1.96*cv*obs, ymax = obs + 1.96*cv*obs), 
                  width = 0, color = "grey40")+
    geom_line(data = function(x) filter(x, index == 1), aes(x = year, y = pred, color = model))+
    geom_line(data = function(x) filter(x, index == 2), aes(x = year, y = pred, color = model))+
    scale_x_continuous(labels = yraxis$labels, breaks = yraxis$breaks)+
    scale_y_continuous(labels = scales::comma)+
    labs(x = NULL, y = "Observer CPUE", color = NULL) -> p_obs
  ggsave(plot = p_obs, filename = paste0(file_path, "obs_index_fit.png"), 
         height = height, width = width, units = "in")
  
  # fish ticket cpue
  gmacs_index %>%
    filter(index %in% 3) %>%
    ggplot()+
    geom_point(aes(x = year, y = obs), color = "grey40")+
    geom_errorbar(aes(x = year, ymin = obs + -1.96*cv*obs, ymax = obs + 1.96*cv*obs), 
                  width = 0, color = "grey40")+
    geom_line(aes(x = year, y = pred, color = model))+
    scale_x_continuous(labels = yraxis$labels, breaks = yraxis$breaks)+
    scale_y_continuous(labels = scales::comma)+
    labs(x = NULL, y = "Fish Ticket CPUE", color = NULL) -> p_ft
  ggsave(plot = p_ft, filename = paste0(file_path, "ft_index_fit.png"), 
         height = height, width = width, units = "in")

}

# f_plot_comp() ----
## args: dirs = directory to rep files
#        models = names of model scenarios
#        file_path = file path to save plots
#        height = height in inches of plot
#        width = width in inches of plot

f_plot_comp <- function(dirs, models, file_path, height = 8, width = 5) {
  
  biglist <- lapply(paste0(dirs, "/gmacs.rep"), f_read_rep)
  ## extract catch data
  tibble(model = models,
         rep = biglist) %>%
    mutate(comp = purrr::map(rep, function(rep) {
      
      # plot catch 
      rep$d3_SizeComps_in %>%
        as_tibble() %>%
        dplyr::select(c(1, 3, 5, 8, 9:ncol(.))) %>%
        # change col names to bins
        rename_all(~c("year", "fleet", "type", "nsamp", as.character(rep$mid_points))) %>%
        # pivot longer
        pivot_longer(5:ncol(.), names_to = "bin", values_to = "obs") %>%
        # add predicted comp
        bind_cols(as_tibble(rep$d3_pre_size_comps_out) %>%
                    rename_all(~as.character(rep$mid_points)) %>% 
                    pivot_longer(1:ncol(.), names_to = "bin", values_to = "pred") %>%
                    dplyr::select(-bin)) %>%
        mutate(bin = as.numeric(bin) - 3) 
      
    })) %>%
    transmute(model, comp) %>%
    unnest(comp) %>%
    mutate(annotation = paste0("N = ", nsamp)) -> gmacs_comp
  
  # retained
  gmacs_comp %>%
    bind_rows(., gmacs_comp %>%
                mutate(bin = bin + 5 - 1e-10,
                       edge = T)) %>%
    filter(fleet == 1, type == 1) %>%
    
    ggplot()+
    geom_area(aes(x = bin, y = obs, fill = model), 
              position = position_dodge(width = 0), alpha = 0.5, show.legend = F)+
    geom_line(data = function(x) filter(x, is.na(edge)) %>% mutate(bin = bin + 3), 
              aes(x = bin, y = pred, color = model))+
    geom_text_npc(aes(npcx = "left", npcy = 0.6, label = year),
                  check_overlap = T, size = 3)+
    geom_text_npc(aes(npcx = "right", npcy = 0.6, label = annotation),
                  check_overlap = T, size = 3)+
    #scale_x_continuous(breaks = seq(0, 200, by = 20))+
    facet_wrap(~year, ncol = 3, dir = "v", )+
    labs(x = "\nCarapace Length (mm)", y = NULL, fill = NULL, color = NULL) +
    scale_fill_manual(values = rep("grey70", length(models)))+
    theme(panel.border= element_blank(),
          panel.spacing.y = unit(-0.5, "lines"),
          panel.spacing.x = unit(1, "lines"),
          strip.background = element_blank(),
          strip.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.line.y = element_blank(),
          axis.line.x = element_line(size = 0.1, color = "grey70"),
          axis.ticks.y = element_blank(),
          panel.background = element_blank()) -> p_ret
  ggsave(plot = p_ret, filename = paste0(file_path, "retained_comp_fit.png"), 
         height = height, width = width, units = "in")
  
  
  # total
  gmacs_comp %>%
    bind_rows(., gmacs_comp %>%
                mutate(bin = bin + 5 - 1e-10,
                       edge = T)) %>%
    filter(fleet == 1, type == 0) %>%
    
    ggplot()+
    geom_area(aes(x = bin, y = obs, fill = model), 
              position = position_dodge(width = 0), alpha = 0.5, show.legend = F)+
    geom_line(data = function(x) filter(x, is.na(edge)) %>% mutate(bin = bin + 3), 
              aes(x = bin, y = pred, color = model))+
    geom_text_npc(aes(npcx = "left", npcy = 0.6, label = year),
                  check_overlap = T, size = 3)+
    geom_text_npc(aes(npcx = "right", npcy = 0.6, label = annotation),
                  check_overlap = T, size = 3)+
    facet_wrap(~year, ncol = 3, dir = "v", )+
    labs(x = "\nCarapace Length (mm)", y = NULL, fill = NULL, color = NULL) +
    scale_fill_manual(values = rep("grey70", length(models)))+
    theme(panel.border= element_blank(),
          panel.spacing.y = unit(-0.5, "lines"),
          panel.spacing.x = unit(1, "lines"),
          strip.background = element_blank(),
          strip.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.line.y = element_blank(),
          axis.line.x = element_line(size = 0.1, color = "grey70"),
          axis.ticks.y = element_blank(),
          panel.background = element_blank()) -> p_tot
  ggsave(plot = p_tot, filename = paste0(file_path, "total_comp_fit.png"), 
         height = height, width = width, units = "in")
  
  #   filter(fleet == 1, type == 1) %>%
  #   # three panels
  #   mutate(panel = case_when(as.numeric(factor(year)) < (mean(as.numeric(factor(year))) * (2/3)) ~ 1,
  #                            as.numeric(factor(year)) >= (mean(as.numeric(factor(year))) * (2/3)) & as.numeric(factor(year)) < (mean(as.numeric(factor(year)))) * (4/3) ~ 2,
  #                            as.numeric(factor(year)) >= (mean(as.numeric(factor(year))) * (4/3)) ~ 3)) %>%
  #   ggplot()+
  #   geom_density_ridges(aes(x = bin, y = factor(year), height = obs, fill = model),
  #                       color = NA, stat = "identity", alpha = 0.4, show.legend = F,)+
  #   geom_density_ridges(aes(x = bin, y = factor(year), height = pred, color = model),
  #                       fill = NA, stat = "identity")+
  #   geom_text(aes(x = 108, y = as.numeric(factor(year)) + 0.4, label = annotation), 
  #                 check_overlap = T, size = 3)+
  #   scale_fill_manual(values = rep("grey70", length(models)))+
  #   scale_y_discrete(limits = rev)+
  #   labs(y = NULL, x = "Carapace Length (mm)", fill = NULL, color = NULL)+
  #   facet_wrap(~panel, scales = "free")+
  #   theme(strip.text.x = element_blank(),
  #         strip.background = element_blank(),
  #         legend.position = "top") 
  
  
  }




# f_plot_compdot() ----
## args: dirs = directory to rep files
#        models = names of model scenarios
#        file_path = file path to save plots
#        height = height in inches of plot
#        width = width in inches of plot
f_plot_compdot <- function(dirs, models, file_path, height = 5, width = 7) {
  
  biglist <- lapply(paste0(dirs, "/gmacs.rep"), f_read_rep)
  ## extract catch data
  tibble(model = models,
         rep = biglist) %>%
    mutate(comp = purrr::map(rep, function(rep) {
      
      # plot catch 
      rep$d3_SizeComps_in %>%
        as_tibble() %>%
        dplyr::select(c(1, 3, 5, 9:ncol(.))) %>%
        # change col names to bins
        rename_all(~c("year", "fleet", "type", as.character(rep$mid_points))) %>%
        # pivot longer
        pivot_longer(4:ncol(.), names_to = "bin", values_to = "obs") %>%
        # add predicted comp
        bind_cols(as_tibble(rep$d3_pre_size_comps_out) %>%
                    rename_all(~as.character(rep$mid_points)) %>% 
                    pivot_longer(1:ncol(.), names_to = "bin", values_to = "pred") %>%
                    dplyr::select(-bin)) %>%
        mutate(bin = as.numeric(bin)) 
      
    })) %>%
    transmute(model, comp) %>%
    unnest(comp) %>%
    # add residual
    mutate(res = ifelse(round(obs - pred == 0, 3), NA, obs - pred)) -> gmacs_comp
  
  # retained
  gmacs_comp %>%
    filter(fleet == 1, type == 1,
           !is.na(res)) %>%
    # compute residual
    mutate(pos = case_when(res > 0 ~ "> 0", 
                           res < 0 ~ "< 0")) %>%
    ggplot()+
    geom_point(aes(x = year, y = bin, size = abs(res), fill = pos), 
               shape = 21, alpha = 0.5)+
    scale_fill_manual(values = c("black", "white", NA))+
    labs(x = NULL, y = "Carapce Length (mm)", size = NULL, fill = NULL)+
    theme(legend.position = "top") -> p_ret
  ggsave(plot = p_ret, filename = paste0(file_path, "retained_compdot.png"), 
         height = height, width = width, units = "in")
  
  # total
  gmacs_comp %>%
    filter(fleet == 1, type == 0,
           !is.na(res)) %>%
    # compute residual
    mutate(pos = case_when(res > 0 ~ "> 0", 
                           res < 0 ~ "< 0")) %>%
    ggplot()+
    geom_point(aes(x = year, y = bin, size = abs(res), fill = pos), 
               shape = 21, alpha = 0.5)+
    scale_fill_manual(values = c("black", "white", NA))+
    labs(x = NULL, y = "Carapce Length (mm)", size = NULL, fill = NULL)+
    theme(legend.position = "top") -> p_tot
  ggsave(plot = p_tot, filename = paste0(file_path, "total_compdot.png"), 
         height = height, width = width, units = "in")
  
}


# f_plot_ssb() ----
## args: dirs = directory to rep files
#        models = names of model scenarios
#        file_path = file path to save plots
#        height = height in inches of plot
#        width = width in inches of plot
f_plot_ssb <- function(dirs, models, file_path, height = 4, width = 6) {
  
  biglist <- lapply(paste0(dirs, "/gmacs.rep"), f_read_rep)
  ## extract catch data
  tibble(model = models,
         rep = biglist) %>%
    mutate(ssb = purrr::map(rep, function(rep) {
      
      # plot catch 
      tibble(year = rep$syr:rep$nyr,
             ssb = rep$ssb) %>%
        add_row(year = max(.$year) + 1,
                ssb = rep$spr_bmsy * rep$spr_depl) 
      })) %>%
    transmute(model, ssb) %>%
    unnest(ssb) %>%
    
    ggplot()+
    geom_line(aes(x = year, y = ssb, color = model))+
    geom_point(data = function(x) filter(x, year == max(year)),
               aes(x = year, y = ssb, color = model))+
    scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
    scale_y_continuous(labels = scales::comma)+
    labs(x = NULL, y = "MMB (t)", color = NULL)+
    theme(legend.position = c(1, 1),
          legend.justification = c(1, 1)) -> p
  ggsave(plot = p, filename = paste0(file_path, "ssb.png"), 
         height = height, width = width, units = "in")
  
}

# f_plot_selex() ----
## args: dirs = directory to rep files
#        models = names of model scenarios
#        file_path = file path to save plots
#        height = height in inches of plot
#        width = width in inches of plot
f_plot_selex <- function(dirs, models, file_path, height = 4, width = 6) {
  
  biglist <- lapply(paste0(dirs, "/gmacs.rep"), f_read_rep)
  ## extract selectivity data
  tibble(model = models,
         rep = biglist) %>%
    mutate(sel = purrr::map(rep, function(rep) {
      
      # plot catch 
      rep$selectivity %>%
        as_tibble() %>%
        rename_all(~c("year", "sex", "fleet", len_bins$mid_cl)) %>%
        pivot_longer(4:ncol(.), names_to = "bin", values_to = "sel")
      
    })) %>%
    transmute(model, sel) %>%
    unnest(sel) %>%
    
    # remove groundfish bycatch fleet
    filter(fleet != 2) %>%
    # change year to period
    # add fleet name
    mutate(period = case_when(year == min(year) ~ "1960 - 2004",
                              year == max(year) ~ paste0("2005 - ", max(year))),
           fleet_name = case_when(fleet == 1 ~ "Directed Fishery",
                                  fleet == 2 ~ "Groundfish Fisheries",
                                  fleet == 3 ~ "Co-op Suvrey"),
           bin = as.numeric(bin)) %>%
    ggplot()+
    geom_line(aes(x = bin, y = sel, color = model))+
    facet_grid(rows = vars(fleet_name), cols = vars(period))+
    labs(x = "Carapace Length (mm)", y = "Selectivity", color = NULL)-> p_sel
  ggsave(plot = psel, filename = paste0(file_path, "selectivity.png"), 
         height = height, width = width, units = "in")
   
}

# f_plot_retention() ----
## args: dirs = directory to rep files
#        models = names of model scenarios
#        file_path = file path to save plots
#        height = height in inches of plot
#        width = width in inches of plot
f_plot_retention <- function(dirs, models, file_path, height = 4, width = 6) {
  
  biglist <- lapply(paste0(dirs, "/gmacs.rep"), f_read_rep)
  ## extract selectivity data
  tibble(model = models,
         rep = biglist) %>%
    mutate(sel = purrr::map(rep, function(rep) {
      
      # plot catch 
      rep$retained %>%
        as_tibble() %>%
        rename_all(~c("year", "sex", "fleet", len_bins$mid_cl)) %>%
        pivot_longer(4:ncol(.), names_to = "bin", values_to = "sel")
      
    })) %>%
    transmute(model, sel) %>%
    unnest(sel) %>%
    
    # remove groundfish bycatch fleet
    filter(fleet != 2) %>%
    # change year to period
    # add fleet name
    mutate(period = case_when(year == min(year) ~ "1960 - 2004",
                              year == max(year) ~ paste0("2005 - ", max(year))),
           fleet_name = case_when(fleet == 1 ~ "Directed Fishery",
                                  fleet == 2 ~ "Groundfish Fisheries",
                                  fleet == 3 ~ "Co-op Suvrey"),
           bin = as.numeric(bin)) %>%
    ggplot()+
    geom_line(aes(x = bin, y = sel, color = model))+
    facet_grid(rows = vars(fleet_name), cols = vars(period))+
    labs(x = "Carapace Length (mm)", y = "Retention", color = NULL)-> p_ret
  ggsave(plot = psel, filename = paste0(file_path, "Retention.png"), 
         height = height, width = width, units = "in")
  
}



# f_plot_recruitment() ----
f_plot_recruitment <- function(dirs, models, file_path, height = 3, width = 7) {
  
  biglist <- lapply(paste0(dirs, "/gmacs.rep"), f_read_rep)
  ## extract catch data
  tibble(model = models,
         rep = biglist) %>%
    mutate(rec = purrr::map(rep, function(rep) {
      
      # plot catch 
      tibble(year = rep$syr:rep$nyr,
             rec = rep$recruits,
             rec_dev = rep$rec_dev)
      
    })) %>%
    transmute(model, rec) %>%
    unnest(rec) -> gmacs_rec
  
  # recruits
  gmacs_rec %>%
    ggplot()+
    geom_line(aes(x = year, y = rec, color = model))+
    scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
    scale_y_continuous(labels = scales::comma)+
    labs(x = NULL, y = "Recruitment (1,000s)", color = NULL)+
    theme(legend.position = c(0, 1),
          legend.justification = c(0, 1)) -> p_rec
  ggsave(plot = p_rec, filename = paste0(file_path, "recruits.png"), 
         height = height, width = width, units = "in")
  
  
  # recruits
  gmacs_rec %>%
    ggplot()+
    geom_vline(xintercept = 0, linetype = 2)+
    geom_point(aes(x = rec_dev, y = year, color = model), alpha = 0.5)+
    scale_y_reverse(breaks = yraxis$breaks, labels = yraxis$labels)+
    labs(x = NULL, y = "Recruitment (1,000s)", color = NULL)+
    theme(legend.position = c(1, 1),
          legend.justification = c(1, 1)) -> p_devs
  ggsave(plot = p_devs, filename = paste0(file_path, "rec_devs.png"), 
         height = width, width = height, units = "in")
  
}

# f_plot_F() ----
dirs <- paste0("./AIGKC/models/2024/jan/EAG/", c("22.1e2_upd", "23.0"))
models = c("22.1e2", "23.0")
file_path = "./AIGKC/figures/models/2024/jan/EAG_"
rep = f_read_rep("./AIGKC/models/2024/jan/EAG/23.0/gmacs.rep")
width = 7
height = 3

f_plot_f <- function(dirs, models, file_path, height = 3, width = 7) {
  
  biglist <- lapply(paste0(dirs, "/gmacs.rep"), f_read_rep)
  ## extract catch data
  tibble(model = models,
         rep = biglist) %>%
    mutate(f = purrr::map(rep, function(rep) {
      
      # plot catch 
      tibble(year = rep(rep$mod_yrs, rep$nfleet),
             fleet = rep(1:rep$nfleet, each = length(rep$mod_yrs)),
             fleet_name = case_when(fleet == 1 ~ "Directed Fishery",
                                    fleet == 2 ~ "Groundfish Fisheries"),
             f = rowSums(rep$ft))
      
    })) %>%
    transmute(model, f) %>%
    unnest(f) %>%
    
    ggplot()+
    geom_line(aes(x = year, y = f, color = model))+
    scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
    scale_y_continuous(labels = scales::comma)+
    coord_cartesian(xlim = c(biglist[[1]]$dCatchData[1,1], NA))+
    labs(x = NULL, y = "F", color = NULL)+
    facet_wrap(~fleet_name, scales = "free", ncol = 1)+
    theme(legend.position = c(1, 1),
          legend.justification = c(1, 1)) -> p
  
  ggsave(plot = p, filename = paste0(file_path, "f_by_fleet.png"), 
         height = height, width = width, units = "in")
  
}




# # f_plot_retlencomp() ----
# ## args: dirs = directory to rep files
# #        models = names of model scenarios
# #        file_path = file path to save plots
# #        height = height in inhes of plot
# #        width = width in inches of plot
# f_plot_retlencomp <- function(dirs, models, file_path, height = 7, width = 7) {
#   
#   ## load all rep files
#   biglist <- lapply(dirs, f_read_gmacs_rep)
#   
#   ## prep data
#   tibble(model = 1:length(biglist),
#          replist = biglist) %>%
#     ## extract comps
#     mutate(obs = purrr::map(replist, function(x){return(x$obs_retained_comp %>% as_tibble)}),
#            exp = purrr::map(replist, function(x){return(x$pred_retained_comp %>% as_tibble)}),
#            scenario = models[model]) %>%
#     transmute(scenario, obs, exp) %>%
#     pivot_longer(2:3, names_to = "obsexp", values_to = "size_comp") %>%
#     unnest(size_comp) %>%
#     # reformat for ggplot
#     pivot_longer(11:ncol(.), names_to = "bin", values_to = "prop") %>% 
#     mutate(bin = as.numeric(bin)) %>% 
#     pivot_wider(names_from = obsexp, values_from = prop) %>%
#     mutate(annotation = paste0("N eff = ", round(nsamp))) -> plot_dat
#     
#   ggplot(data = plot_dat)+
#     geom_bar(aes(x = bin, y = obs), stat = "identity", position = "identity", color = "grey80", fill = "grey80", width=plot_dat$bin[2]-plot_dat$bin[1])+
#     geom_line(aes(x = bin, y = exp, color = scenario, linetype = scenario))+
#     geom_text_npc(aes(npcx = "left", npcy = "top", label = year), check_overlap = T)+
#     geom_text_npc(aes(npcx = "right", npcy = "top", label = annotation), check_overlap = T, size = 3)+
#     labs(x = "CL (mm)", y = "Proportion", color = NULL, linetype = NULL)+
#     scale_y_continuous(expand = expansion(mult = c(0, 0.5)))+
#     facet_wrap(~year, ncol = 5, dir = "v")+
#     theme(panel.spacing = unit(0, "lines"),
#           strip.text.x = element_blank(),
#           strip.background = element_blank()) -> x
#   
#   # write plot to directory
#   ggsave(file_path, plot = x,  height = height, width = width, units = "in")
#   
#   return(x)
# }
# 
# # f_plot_totlencomp() ----
# ## args: dirs = directory to rep files
# #        models = names of model scenarios
# #        file_path = file path to save plots
# #        height = height in inhes of plot
# #        width = width in inches of plot
# f_plot_totlencomp <- function(dirs, models, file_path, height = 7, width = 7) {
#   
#   ## load all rep files
#   biglist <- lapply(dirs, f_read_gmacs_rep)
#   
#   ## prep data
#   tibble(model = 1:length(biglist),
#          replist = biglist) %>%
#     ## extract comps
#     mutate(obs = purrr::map(replist, function(x){return(x$obs_total_comp %>% as_tibble)}),
#            exp = purrr::map(replist, function(x){return(x$pred_total_comp %>% as_tibble)}),
#            scenario = models[model]) %>%
#     transmute(scenario, obs, exp) %>%
#     pivot_longer(2:3, names_to = "obsexp", values_to = "size_comp") %>%
#     unnest(size_comp) %>%
#     # reformat for ggplot
#     pivot_longer(11:ncol(.), names_to = "bin", values_to = "prop") %>% 
#     mutate(bin = as.numeric(bin)) %>% 
#     pivot_wider(names_from = obsexp, values_from = prop) %>%
#     mutate(annotation = paste0("N eff = ", round(nsamp))) -> plot_dat
#   
#   ggplot(data = plot_dat)+
#     geom_bar(aes(x = bin, y = obs), stat = "identity", position = "identity", color = "grey80", fill = "grey80", width=plot_dat$bin[2]-plot_dat$bin[1])+
#     geom_line(aes(x = bin, y = exp, color = scenario, linetype = scenario))+
#     geom_text_npc(aes(npcx = "left", npcy = "top", label = year), check_overlap = T)+
#     geom_text_npc(aes(npcx = "right", npcy = "top", label = annotation), check_overlap = T, size = 3)+
#     labs(x = "CL (mm)", y = "Proportion", color = NULL, linetype = NULL)+
#     scale_y_continuous(expand = expansion(mult = c(0, 0.5)))+
#     facet_wrap(~year, ncol = 5, dir = "v")+
#     theme(panel.spacing = unit(0, "lines"),
#           strip.text.x = element_blank(),
#           strip.background = element_blank()) -> x
#   
#   # write plot to directory
#   ggsave(file_path, plot = x,  height = height, width = width, units = "in")
#   
#   return(x)
# }
# 

