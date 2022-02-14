# k palof
# 04-09-2019 / 1-14-22
# user created functions for smbkc projections

proj_biomass_out <- function(location, folder, recruit, output){
  proj_file <- read.table(paste0(here::here(), '/SMBKC/smbkc_21/', location, '/', folder, '/mcoutPROJ.rep'), 
                          header = TRUE)[,-c(4,5,6,7,8)]
  Nyear <- length(proj_file[1,])-4
  Nline <- length(proj_file[,1])
  print(Nyear)
  print(Nline)
  
  raw <- proj_file
  
  raw %>% 
    gather(year, mmb, -Draw, -Replicate, -F_val, -BMSY) %>% 
    mutate(year = as.numeric(as.factor(year)), 
           FishMort = ifelse(F_val == 1, "F = 0", "F = 0.18")) -> raw_all 
  
  raw_all %>% 
    summarise(Bmsy = mean(BMSY)) -> Bmsy
  
  raw_all %>% 
    group_by(year, FishMort) %>% 
    summarise(q0.05 = quantile(mmb, prob = 0.05), 
              q0.25 = quantile(mmb, prob = 0.25),
              q0.50 = quantile(mmb, prob = 0.50),
              q0.75 = quantile(mmb, prob = 0.75),
              q0.95 = quantile(mmb, prob = 0.95), 
              Bmsy = mean(BMSY)) -> typical_var
  write.csv(typical_var,paste0(here::here(), '/SMBKC/smbkc_21/', location, '/', folder, '/proj_variability.csv'))
  raw_all %>% 
    group_by(year, FishMort) %>% 
    summarise(q0.05 = quantile(mmb, prob = 0.05), 
              q0.25 = quantile(mmb, prob = 0.25),
              q0.50 = quantile(mmb, prob = 0.50),
              q0.75 = quantile(mmb, prob = 0.75),
              q0.95 = quantile(mmb, prob = 0.95), 
              Bmsy = mean(BMSY)) %>% 
    ggplot(aes(year, q0.50, colour = FishMort)) +
    geom_line(lwd = 1) +
    scale_color_manual(name = "", values = c(cbPalette[4], cbPalette[1]))+
    geom_ribbon(aes(ymin = q0.05, ymax = q0.95, x = year, fill = FishMort), alpha = 0.17) +
    scale_fill_manual(name = "", values = c(cbPalette[4], cbPalette[1]))+
    geom_hline(yintercept = Bmsy[1,], lwd = 0.75, color = "darkgoldenrod4", linetype = "dashed") +
    geom_text(aes(0, 3300, label = "Bmsy proxy",
                  vjust = -1, hjust = 0.05)) +
    ylab ("MMB (tons)") +
    xlab ("Projection Year") +
    ggtitle(recruit) -> plotA
  
  ggsave(paste0(here::here(), '/SMBKC/smbkc_21/', location, '/', output,'.png'), plotA, dpi = 800,
         width = 7.5, height = 3.75)
}




f_sum <- function(data, col1, col2, div){
  col1 = enquo(col1)
  col2 = enquo(col2)
  div = enquo(div)
  
  data %>% 
    dplyr::select(!!col1:!!col2) %>% 
    apply(1, sum, na.rm=T) %>% 
    as.data.frame %>% 
    bind_cols(data) %>% 
    transmute(temp = . / !!div * 100) %>% 
    .$temp
}  


f_test <- function(x){
  as.numeric(x > 100)
}


# function to output .csv and .png ------------------------

write_rec_prob <- function(n_prob_yr, model, version) {
  TheD <- read.table(paste0(here::here(), "/SMBKC/smbkc_18a/projections/", model, "/", version, "/mcoutPROJ.rep"))[,-c(4,5,6,7,8)]
  
  Nyear <- length(TheD[1,])-4
  Nline <- length(TheD[,1])
  print(Nyear)
  print(Nline)
  
  # prob of recovery 
  for (Iline in 1:Nline)
    TheD[Iline,5:(4 + Nyear)] <- TheD[Iline, 5:(4 + Nyear)] / TheD[Iline, 4] * 100 
  
  
  TheD %>% 
    #mutate(ratio = f_sum(., V10, V59, V9)) %>% 
    mutate_at(vars(-V1, -V2, -V3, -V9), f_test) %>% 
    mutate(id = 1:n()) %>% 
    gather(year, value, -V1, -V2, -V3, -V9, -id) %>% 
    group_by(id) %>% 
    mutate(test = as.numeric(lead(value) + value == n_prob_yr),
           year = as.numeric(as.factor(year))) %>%  #this doesn't work if you get above V99
    group_by(year, V3) %>% 
    summarise(recovery = sum(test, na.rm = TRUE) / n() * 100)  %>% 
    mutate(FishMort = ifelse(V3 == 1, "F = 0", "F = 0.18")) -> output
  #write_csv("test.csv")
  write_csv(output, paste0('./projections/', model, '/', version, '/rec_prob_out_', model, version, '.csv'))
  
  year1 <- output[1:2, ]
  year1 %>% 
    mutate(year2 = year) -> year1
  
  output %>% 
    mutate(year2 = year + 1) %>% 
    bind_rows(year1) %>% 
    filter(year2 <= Nyear) %>% 
    ggplot(aes(year2, recovery, group = V3)) + 
    geom_line() +
    geom_point(aes(group = V3, shape = FishMort), size = 2) +
    scale_shape_manual(name = "", values = c(16, 22)) +
    geom_hline(yintercept = 50, color = "red", lty = "dashed", lwd = 1.5) +
    geom_vline(xintercept = 10, color = "blue", lty = 2, lwd = 1.5) +
    ggtitle(paste0(model, version)) +
    ylab("Probability of recovery") +
    xlab("Year") +
    ylim(0,100) +
    theme(plot.title = element_text(hjust = 0.5)) -> plotA
  ggsave(paste0('./projections/figures/', model, '_', version, '_rec_prob.png'), plotA, dpi = 800,
         width = 7.5, height = 3.75)
  
}

# 1 year above Bmsy proxy -- function to output .csv and .png ------------------------
#model_yr <- "smbkc_19"
#proj <- "proj_1" # projection set up - involve recruitment years
#version <- "d" # projection version - see readme
#label <- "19.0 (ref)" # model label - used for SAFE
#model <- "model_1" # model name in files
#write_rec_prob1(1, "smbkc_19", "proj_1", "d", "19.0 (ref)", "model_1")

write_rec_prob1 <- function(n_prob_yr, model_yr, proj, version, label, model ) {
  TheD <- read.table(paste0(here::here(), "/SMBKC/", model_yr, '/', model, "/projections/", proj, "/", version, "/mcoutPROJ.rep"))[,-c(4,5,6,7,8)]
  
  Nyear <- length(TheD[1,])-4
  Nline <- length(TheD[,1])
  print(Nyear)
  print(Nline)
  
  # prob of recovery 
  for (Iline in 1:Nline)
    TheD[Iline,5:(4 + Nyear)] <- TheD[Iline, 5:(4 + Nyear)] / TheD[Iline, 4] * 100 
  
  TheD %>% 
    #mutate(ratio = f_sum(., V10, V59, V9)) %>% 
    mutate_at(vars(-V1, -V2, -V3, -V9), f_test) %>% 
    mutate(id = 1:n()) %>% 
    gather(year, value, -V1, -V2, -V3, -V9, -id) %>% 
    group_by(id) %>% 
    mutate(year = as.numeric(as.factor(year))) %>%  #this doesn't work if you get above V99
    group_by(year, V3) %>% 
    summarise(recovery = sum(value, na.rm = TRUE) / n() * 100)  %>% 
    mutate(FishMort = ifelse(V3 == 1, "F = 0", "F = 0.18")) -> output
  #write_csv("test.csv")
  write_csv(output, paste0(here::here(), '/SMBKC/', model_yr, '/', model, '/projections/', proj, '/', version,
                           '/rec_1yr_prob_out_', proj, version, '.csv'))
  
  year1 <- output[1:2, ]
  year1 %>% 
    mutate(year2 = year) -> year1
  
  output %>% 
    mutate(year2 = year + 1) %>% 
    bind_rows(year1) %>% 
    filter(year2 <= Nyear) %>% 
    ggplot(aes(year2, recovery, group = V3)) + 
    geom_line() +
    geom_point(aes(group = V3, shape = FishMort), size = 2) +
    scale_shape_manual(name = "", values = c(16, 22)) +
    geom_hline(yintercept = 50, color = "red", lty = "dashed", lwd = 1.5) +
    geom_vline(xintercept = 10, color = "blue", lty = 2, lwd = 1.5) +
    ggtitle(paste0(model, '_', proj, '_', version)) +
    ylab("Probability of recovery") +
    xlab("Year") +
    ylim(0,100) +
    theme(plot.title = element_text(hjust = 0.5)) -> plotA
  ggsave(paste0(here::here(), '/SMBKC/', model_yr, '/', model, '/projections/', proj, '/', version, '/_rec_1yr_prob.png'), plotA, dpi = 800,
         width = 7.5, height = 3.75)
  
}




## uncertainty in just 2019 projection -------
# un_cur_yr("smbkc_19", "proj_1", "d", "model_1", "19.0 (ref)")
#model_yr <- "smbkc_19"
#proj <- "proj_1" # projection set up - involve recruitment years
#version <- "d" # projection version - see readme
#label <- "19.0 (ref)" # model label - used for SAFE
#model <- "model_1" # model name in files

un_cur_yr <- function(model_yr, proj, version, model, label) {
  TheD <- read.table(paste0(here::here(), "/SMBKC/", model_yr ,"/", model,"/projections/", proj, "/", version, "/mcoutPROJ.rep"))[,-c(4,5,6,7,8)]
  
  Nyear <- length(TheD[1,])-4 #each year is its own column
  Nline <- length(TheD[,1]) #1000 for each scenario direct F=0, F=0.18
  print(Nyear)
  print(Nline)
  TheD %>% 
    filter(V3 == 1) %>% 
    select(V1, V2, V3, Bmsy = V9, curyr = V10) %>% 
    mutate(diff_sq = ((curyr - mean(curyr))^2)) %>% 
    summarise(lci = quantile(curyr, 0.025),
              uci = quantile(curyr, 0.975), 
              l05 = quantile(curyr, 0.05), 
              u95 = quantile(curyr, 0.95),
              median_ssb = median(curyr), 
              se_ssb = sqrt((1/((Nline/2)-1))*sum(diff_sq)), 
              CV_ssb = se_ssb/median_ssb) %>% 
    mutate(Model = label) -> uncertain_current
  #write_csv(uncertain_current, paste0(here::here(), 
   #                                   '/SMBKC/', model_yr,'/', model,'/projections/', proj, '/', 
  #                                    version, '/uncertainty_ssb_', model, "_", proj, '_', version, '_', cur_yr, '.csv'))
  write_csv(uncertain_current, paste0(here::here(), 
                                  '/SMBKC/', model_yr,'/', model,'/projections/', proj, '/', 
                                  version, '/uncertainty_ssb_', cur_yr, '.csv'))
}



## size comps for me since gmr version has ylim ??? -----
#' Plot fits to size composition data
#' 
#' Get observed and predicted size composition values
#'
#' @param M List object(s) created by read_admb function
#' @param which_plots the size composition fits that you want to plot
#' @param xlab the x-axis label for the plot
#' @param ylab the y-axis label for the plot
#' @param slab the sex label for the plot that appears above the key
#' @param mlab the model label for the plot that appears above the key
#' @param tlab the fleet label for the plot that appears above the key
#' @param res boolean if residual or observed and predicted
#' @return Plots of observed and predicted size composition values
#' @export
#'
plot_size_comps <- function(M, which_plots = "all", xlab = "Mid-point of size-class (mm)", ylab = "Proportion",
                            slab = "Sex", mlab = "Model", tlab = "Fleet", res = FALSE,legend_loc=c(1,1))
{
  ylab <- paste0(ylab, "\n")
  
  mdf <- .get_sizeComps_df(M)
  ix <- pretty(1:length(M[[1]]$mid_points))
  
  p <- ggplot(data = mdf[[1]])
  
  if (res)
  {
    xlab <- paste0(xlab, "\n")
    p <- p + geom_point(aes(factor(year), variable, col = factor(sign(resd)), size = abs(resd)), alpha = 0.6)
    p <- p + scale_size_area(max_size = 10)
    p <- p + labs(x = "\nYear", y = xlab, col = "Sign", size = "Residual")
    #p <- p + scale_x_discrete(breaks = pretty(mdf[[1]]$mod_yrs))
    #p <- p + scale_y_discrete(breaks = pretty(mdf[[1]]$mid_points))
    if (length(unique(do.call(rbind.data.frame, mdf)$model)) != 1)
    {
      p <- p + facet_wrap(~model)
    }
    p <- p + .THEME + theme(axis.text.x = element_text(angle = 45, vjust = 0.5))
  } else {
    xlab <- paste0("\n", xlab)
    p <- p + geom_bar(aes(variable, value), stat = "identity", position = "dodge", alpha = 0.5, fill = "grey")
    if (length(unique(do.call(rbind.data.frame, mdf)$model)) == 1)
    {
      p <- p + geom_line(aes(as.numeric(variable), pred), alpha = 0.85)
    } else {
      p <- p + geom_line(aes(as.numeric(variable), pred, col = model), alpha = 0.85)
    }
    p <- p + scale_x_discrete(breaks=M[[1]]$mid_points[ix]) 
    p <- p + labs(x = xlab, y = ylab, col = mlab, fill = slab, linetype = tlab)
    p <- p + ggtitle("title")
    p <- p + facet_wrap(~year) + .THEME #+ ylim(0,0.3)
    p <- p + theme(axis.text.x = element_text(angle = 45, vjust = 0.5, size = 10),
                   strip.text.x = element_text(margin= margin(1,0,1,0)),
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   legend.position=legend_loc,
                   panel.border = element_blank(),
                   strip.background = element_rect(color="white",fill="white"))
    p <- p + geom_text(aes(label = paste0("N = ", nsamp)), x = -Inf, y = Inf, hjust = -0.2, vjust = 1.5)
  }
  #print(p)
  fun <- function(x, p)
  {
    if (length(unique(do.call(rbind.data.frame, mdf)$fleet)) == 1 && length(unique(do.call(rbind.data.frame, mdf)$sex)) == 1 && length(unique(do.call(rbind.data.frame, mdf)$seas)) == 1)
    {
      p$labels$title <- ""
    } else if (length(unique(do.call(rbind.data.frame, mdf)$fleet)) != 1 && length(unique(do.call(rbind.data.frame, mdf)$sex)) == 1 && length(unique(do.call(rbind.data.frame, mdf)$seas)) == 1) {
      p$labels$title <- paste("Gear =", unique(x$fleet))
    } else if (length(unique(do.call(rbind.data.frame, mdf)$fleet)) == 1 && length(unique(do.call(rbind.data.frame, mdf)$sex)) != 1 && length(unique(do.call(rbind.data.frame, mdf)$seas)) == 1) {
      p$labels$title <- paste("Sex =", unique(x$sex))
    } else if (length(unique(do.call(rbind.data.frame, mdf)$fleet)) == 1 && length(unique(do.call(rbind.data.frame, mdf)$sex)) == 1 && length(unique(do.call(rbind.data.frame, mdf)$seas)) != 1) {
      p$labels$title <- paste("Season =", unique(x$seas))
    } else if (length(unique(do.call(rbind.data.frame, mdf)$fleet)) != 1 && length(unique(do.call(rbind.data.frame, mdf)$sex)) != 1 && length(unique(do.call(rbind.data.frame, mdf)$seas)) == 1) {
      p$labels$title <- paste("Gear =", unique(x$fleet), ", Sex =", unique(x$sex))
    } else if (length(unique(do.call(rbind.data.frame, mdf)$fleet)) != 1 && length(unique(do.call(rbind.data.frame, mdf)$sex)) == 1 && length(unique(do.call(rbind.data.frame, mdf)$seas)) != 1) {
      p$labels$title <- paste("Gear =", unique(x$fleet), ", Season =", unique(x$seas))
    } else if (length(unique(do.call(rbind.data.frame, mdf)$fleet)) == 1 && length(unique(do.call(rbind.data.frame, mdf)$sex)) != 1 && length(unique(do.call(rbind.data.frame, mdf)$seas)) != 1) {
      p$labels$title <- paste("Sex =", unique(x$sex), ", Season =", unique(x$seas))
    } else {
      p$labels$title <- paste("Gear =", unique(x$fleet), ", Sex =", unique(x$sex), ", Season =", unique(x$seas))
    }
    p %+% x
  }
  
  plist <- lapply(mdf, fun, p = p)
  
  if (which_plots == "all")
  {
    print(plist)
  } else {
    print(plist[[which_plots]])
  }
}






