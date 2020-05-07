# Troubleshooting gmr issues with udpate GMACS code in Jan 2020


# load ------------
#require(devtools)
#devtools::install_github("seacode/gmacs", subdir = "/gmr", ref = "develop") - only needs to be performed once.
require(gmr)
#setwd("./smbkc_19/model_1")

# All model plots  -------------------------
# still reference 2018 models since I'm currently runing 2019 **FIX**
cur_yr <- 2019 # update annually 

mod_names <- c("model 16.0", "model 16.0 (ref)", "model 19.1 (VAST))", "model 19.2 (add CV pot)", "model 19.3 (add CV both)") 
.MODELDIR = c(paste0(here::here(), "/SMBKC/smbkc_19/model_1/"), 
              paste0(here::here(), "/SMBKC/smbkc_19a/model_1/"),
              paste0(here::here(), "/SMBKC/smbkc_19a/model_4/"), 
              paste0(here::here(), "/SMBKC/smbkc_19a/model_1a/"), 
              paste0(here::here(), "/SMBKC/smbkc_19a/model_1b/")) #need to update these model options
.THEME    = theme_bw(base_size = 12, base_family = "")
.OVERLAY  = TRUE
.SEX      = c("Aggregate","Male")
.FLEET    = c("Pot","Trawl bycatch","Fixed bycatch","NMFS Trawl","ADF&G Pot")
.TYPE     = c("Retained","Discarded", "Retained & Discarded")
.SHELL    = c("Aggregate")
.MATURITY = c("Aggregate")
.SEAS     = c("1","2","3","4","5")
# Read report file and create gmacs report object (a list):
fn       <- paste0(.MODELDIR, "gmacs")
M        <- lapply(fn, read_admb)
names(M) <- mod_names
#jj <- 1 # The position in the list that Jies model outputs sit

nmult_1 <- 1e+06
nmult_2 <- 0.0004535923 * 1e+6
#fn <- paste0(.MODELDIR[1], "gmacs")
#Mmatch <- lapply(fn, read_admb)
#names(Mmatch) <- c("SMBKC")

fn <- paste0(.MODELDIR[2], "gmacs")
Mbase <- lapply(fn, read_admb)
names(Mbase) <- c("SMBKC")

rinline <- function(code){
  html <- '<code  class="r">``` `CODE` ```</code>'
  sub("CODE", code, html)
}

#alt_mod <- 5 # alt reference time frame
ref_mod <- 1 # base
rec_mod <- 2 # base
mod_scen<- 2:5 #scenarios you want graphed together

ww <- 6
hh <- 5

# setup for testing functions -----------

K <- M[rec_mod]
L <- M[ref_mod]
B <- L[[1]]

# size comps -------
# error in names(x)

.get_sizeComps_df
function(M)
{
  n <- length(M)
  ldf <- list()
  mdf <- mpf <- mrf <- NULL
  for (i in 1:n)
  {
    A <- M[[1]]
    df <- data.frame(Model = names(M)[1], cbind(A$d3_SizeComps[,1:8], A$d3_obs_size_comps_out))
    pf <- data.frame(Model = names(M)[1], cbind(A$d3_SizeComps[,1:8], A$d3_pre_size_comps_out))
    rf <- data.frame(Model = names(M)[1], cbind(A$d3_SizeComps[,1:8], A$d3_res_size_comps_out))
    
    colnames(df) <- tolower(c("Model", "Year", "Seas", "Fleet", "Sex", "Type", "Shell", "Maturity", "Nsamp", as.character(A$mid_points)))
    colnames(pf) <- colnames(rf) <- colnames(df)
    
    df$fleet    <- pf$fleet    <- rf$fleet    <- .FLEET[df$fleet]
    df$sex      <- pf$sex      <- rf$sex      <- .SEX[df$sex+1]
    df$shell    <- pf$shell    <- rf$shell    <- .SHELL[df$shell+1]
    df$maturity <- pf$maturity <- rf$maturity <- .MATURITY[df$maturity+1]
    df$type     <- pf$type     <- rf$type     <- .TYPE[df$type+1]
    df$seas     <- pf$seas     <- rf$seas     <- .SEAS[df$seas]
    
    mdf <- rbind(mdf, df)
    mpf <- rbind(mpf, pf)
    mrf <- rbind(mrf, rf)
  }
  
  mdf <- reshape2::melt(mdf, id.var = 1:9)
  mpf <- reshape2::melt(mpf, id.var = 1:9)
  mrf <- reshape2::melt(mrf, id.var = 1:9)
  
  for(i in 1:n)
  {
    j  <- 1
    for(k in unique(df$fleet))
    {
      for(h in unique(df$sex))
      {
        for(t in unique(df$type))
        {
          for(s in unique(df$shell))
          {
            tdf <- mdf %>% filter(fleet==k) %>% filter(sex==h) %>% filter(type==t) %>% filter(shell==s)
            tpf <- mpf %>% filter(fleet==k) %>% filter(sex==h) %>% filter(type==t) %>% filter(shell==s)
            trf <- mrf %>% filter(fleet==k) %>% filter(sex==h) %>% filter(type==t) %>% filter(shell==s)
            if(dim(tdf)[1]!=0)
            {
              # deterimin row & column.
              # fyr = unique(tdf$year)
              # syr = min(fyr); nyr = max(fyr)
              # nn  = (nyr-syr+1)
              # nc  = ceiling(nn/sqrt(nn))
              # irow = icol = rep(1,length=nn)
              
              # ii = ic = ir = 1
              # for(iyr in fyr)
              # {
              # 	icol[ii] <- ic
              # 	irow[ii] <- ir
              
              # 	ic = ic + 1
              # 	ii = ii + 1
              
              # 	if(ic > nc)
              # 	{
              # 		ic = 1
              # 		ir = ir + 1	
              # 	} 
              # }
              # tdf$irow = irow[tdf$year-syr+1]
              # tdf$icol = icol[tdf$year-syr+1]
              # cat(" n = ",nn,"\n")
              # print(tdf$year - syr + 1)
              ldf[[j]] <- cbind(tdf, pred = tpf$value, resd = trf$value)
              j <- j + 1
            }
          }
        }
      }
    }
  }   
  return(ldf)
}



plot_size_comps
function(M, which_plots = "all", xlab = "Mid-point of size-class (mm)", ylab = "Proportion",
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


# natural mortality -------------
plot_natural_mortality2(M[rec_mod])

## data extent -----------
plot_datarange(M[ref_mod])
plot_datarange(M[rec_mod]) # not working...output change?
plot_datarangeSM(M[rec_mod])

# selectivity -------------
plot_selectivity(M[mod_scen]) 
plot_selectivity(M[2]) 


plot_selectivity
function(M,
         xlab = "Mid-point of size class (mm)",
         ylab = "Selectivity",
         tlab = "Type", ilab = "Period year",
         nrow = NULL, ncol = NULL, legend_loc=c(1.05,.05))
{
  xlab <- paste0("\n", xlab)
  ylab <- paste0(ylab, "\n")
  
  mdf <- .get_selectivity_df(M)
  ncol <-length(unique(mdf$fleet))
  nrow <-length(unique(mdf$Model))
  nrow_sex <-length(unique(mdf$sex))
  p <- ggplot(mdf) + expand_limits(y = c(0,1))
  if (.OVERLAY)
  {
    p <- p + geom_line(aes(variable, value, col = factor(year), linetype = type))
    if (length(M) == 1 && length(unique(mdf$sex)) == 1)
    {
      p <- p + facet_wrap(~fleet, nrow = nrow, ncol = ncol)
    } else if (length(M) != 1 && length(unique(mdf$sex)) == 1) {
      p <- p + facet_wrap(~Model + fleet, nrow = nrow, ncol = ncol)
    } else if (length(M) == 1 && length(unique(mdf$sex)) != 1) {
      p <- p + facet_wrap(~fleet + sex, ncol = nrow_sex, nrow = ncol)
    } else {
      p <- p + facet_wrap(~Model + fleet + sex, nrow = nrow_sex, ncol = ncol)
    }
  } else {
    p <- p + geom_line(aes(variable, value, col = factor(year), linetype = sex), alpha = 0.5)
    p <- p + facet_wrap(~Model + fleet + type, nrow = nrow, ncol = ncol)
  }
  p <- p + labs(y = ylab, x = xlab, col = ilab, linetype = tlab) +
    scale_linetype_manual(values = c("solid", "dashed", "dotted")) + 
    .THEME
  p <- p + theme(strip.text.x = element_text(margin= margin(1,0,1,0)),
                 panel.grid.major = element_blank(), 
                 panel.grid.minor = element_blank(),
                 panel.border = element_blank(),
                 panel.background = element_blank(),
                 strip.background = element_rect(color="white",fill="white"))
  
  print(p )
}