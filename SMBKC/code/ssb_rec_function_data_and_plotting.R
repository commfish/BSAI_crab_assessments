# k.palof
# ssb and recruitment from gmacsall.out

# plotting ssb and recruitment with variability from gmacsall.out file

# function to get data 
.MODELDIR = c(paste0(here::here(), "/SMBKC/smbkc_22/model_1_base20/"),
              paste0(here::here(), "/SMBKC/smbkc_22/model_1_22/"), 
              paste0(here::here(), "/SMBKC/smbkc_22/model_2a/"),
              paste0(here::here(), "/SMBKC/smbkc_22/model_2b/"))
mod_names <- c("model 16.0 (2020)", "model 16.0 (2022)", "model 22.0a (M=0.21)", "model 22.0b (M=0.26)")

#df <- read.csv(paste0(here::here(), "/SMBKC/smbkc_22/model_1_22/", "ssb_rec_out.csv"), header = TRUE)

raw_data <- data_out(mod_names[1:2], .MODELDIR[1:2])

ssb1 <- get_ssb_out(mod_names[1:2], raw_data)
ssb_last <- get_ssb_last(M[1:2]) %>% select(-par, -sd)

ssb1 %>% 
  select(Model, ssb, year, lb, ub) %>% 
  rbind(ssb_last) -> ssb

# Figures -----
## reference with last year ------
ssb %>% 
  ggplot(aes(year, ssb, col = Model)) +
  geom_line() +
  geom_ribbon(aes(x=year, ymax = ub, ymin = lb, fill = Model, col = NULL), alpha = 0.2) +
  expand_limits(y=0) +
  ylim(0, max(ssb$ub)+ 100)+
  #ylab = "SSB (tonnes)" +
  #scale_y_continuous(expand = c(0,0)) +
  #geom_hline(data = Bmsy_options, aes(yintercept = Bmsy), color = c("blue", "red"), 
  #           lty = c("solid", "dashed"))+
  #geom_text(data = Bmsy_options, aes(x= 1980, y = Bmsy, label = label), 
  #          hjust = -0.45, vjust = 1.5, nudge_y = 0.05, size = 3.5) +
  #ggtitle("Base model - model 1 (Model 3 2018)") +
  ylab("Mature male biomass (tons) on 15 February") + xlab("Year") +
  .THEME
ggsave(paste0(.FIGS, "lastyr_reference_ssb_wprojected_yr.png"), width = ww*1.18, height = hh)
ggsave(paste0(.FIGS, "PRESENTATION_lastyr_reference_ssb_wprojected_yr.png"), width = ww*1.5, height = hh)



# functions ---------------
data_out <- function(model_names, direct)
{
  n <- length(model_names)
  mdf <- NULL
  for (i in 1:n)
  {
    out <- read.csv(paste0(direct[i], "ssb_rec_out.csv"), header = TRUE)
    df <- data.frame(Model = model_names[i], 
                     par = out$Parameter_name, 
                     log_par = out$Estimate, 
                     log_sd = out$Standard_error, 
                     year = out$Year)
    mdf     <- rbind(mdf, df)
  }
  return(mdf)
}

get_ssb_out <- function(model_names, raw_data)
{
  n <- length(model_names)
  mdf <- NULL
  for (i in 1:n)
  {
    model <- model_names[i]
    df <- subset(raw_data, Model == model_names[i])
    df <- subset(df, par == "Log(ssb)")
    #df$year <- A$mod_yrs
    df$ssb  <- exp(df$log_par)
    df$lb   <- exp(df$log_par - 1.96*df$log_sd)
    df$ub   <- exp(df$log_par + 1.96*df$log_sd)
    mdf     <- rbind(mdf, df)
  }
  return(mdf)
}

get_ssb_last <-function(M)
{
  n <- length(M)
  mdf <- NULL
  for (i in 1:n)
  {
    A <- M[[i]]
    df <- data.frame(Model = names(M)[i],
                     par = A$fit$names,
                     ssb = A$fit$est,
                     sd = A$fit$std)
    df      <- subset(df, par == "sd_last_ssb")
    df$year <- max(A$mod_yrs)+1
    df$lb   <- df$ssb - 1.96*df$sd
    df$ub   <- df$ssb + 1.96*df$sd
    mdf     <- rbind(mdf, df)
  }
  return(mdf)
}

