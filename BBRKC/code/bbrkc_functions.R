# k.palof
# Functions created for BBRKC figures see Jie_cmn_files.R for more context

r## bubbleplot of residualts ----------
## !!!! old inherited base code ---
# see bubleplot-m.r  
bubbleplot_resid <- function(letter_m, sex, ltitle = "Model")
{
  if(sex == "male"){
    d1<-letter_m$d3_res_size_comps_7[,1:20]
    ny<-20
    nx<-letter_m$nyr-letter_m$syr+1
    y <-c(67.5,72.5,77.5,82.5,87.5,92.5,97.5,102.5,107.5,112.5,117.5,122.5,127.5,132.5,137.5,142.5,147.5,152.5,157.5,162.5)
  }
  
  if(sex == "female"){
    d1<-letter_m$d3_res_size_comps_7[,21:36]
    ny<-16
    nx<-letter_m$nyr-letter_m$syr+1
    y <-c(67.5,72.5,77.5,82.5,87.5,92.5,97.5,102.5,107.5,112.5,117.5,122.5,127.5,132.5,137.5,142.5)
  }
  
  x<-c(letter_m$syr:(letter_m$nyr-2),(letter_m$nyr:(letter_m$nyr+1)))
  tin<-ltitle
  Year<-c(rep(x,ny))
  Length<-c(rep(y,nx))
  Residual<-c(rep(0.0,nx*ny))
  dfr<-data.frame(Year,Length,Residual)
  for (i in 1:nx)
  {
    for (j in 1:ny)
    {
      ii = (i-1)*ny+j;
      if (d1[i,j] < -5.0) d1[i,j] <- -5.0;
      if (d1[i,j] > 5.0) d1[i,j] <- 5.0;
      dfr[ii,3] <- d1[i,j];
      dfr[ii,1] <- x[i];
    }
  }
  dfr$clr<-ifelse(dfr[["Residual"]]>0.0,">0","<0");
  
  dfr[["Residual"]]<-abs(dfr[["Residual"]]);
  
  p <- ggplot(aes_string(x="Year",y="Length",size="Residual",colour="clr"),data=dfr);
  p <- p + scale_size_area(max_size=6);
  p <- p + geom_point(alpha=0.5);
  p <- p + theme(panel.background=element_rect(fill="white",color="black"));
  p <- p + scale_color_manual(values=c("red","green"));
  p <- p + ggtitle(tin);
  p <- p + theme(axis.text=element_text(color="black"));
  p <- p + theme(legend.position="top", legend.background=element_rect(fill="white",color="black"));
  # p <- p + theme(legend.position="top");
  print(p);
}

# jie's read function for GMACS output ---------------
read_rep <- function(fn)
{
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



# size comp plotting to include season as a divider ------------------
plot_size_comps_kjp <- 
  function(M, which_plots = "all", xlab = "Mid-point of size-class (mm)", ylab = "Proportion",
           slab = "Sex", mlab = "Model", tlab = "Fleet", res = FALSE,legend_loc=c(1,1))
  {
    ylab <- paste0(ylab, "\n")
    
    mdf <- .get_sizeComps_df_kjp(M)
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

#  get size comps --------------
.get_sizeComps_df_kjp <- function(M)  
{
  n <- length(M)
  ldf <- list()
  mdf <- mpf <- mrf <- NULL
  for (i in 1:n)
  {
    A <- M[[i]]
    df <- data.frame(Model = names(M)[i], cbind(A$d3_SizeComps[,1:8], A$d3_obs_size_comps_out))
    pf <- data.frame(Model = names(M)[i], cbind(A$d3_SizeComps[,1:8], A$d3_pre_size_comps_out))
    rf <- data.frame(Model = names(M)[i], cbind(A$d3_SizeComps[,1:8], A$d3_res_size_comps_out))
    
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
            for(u in unique(df$seas))
            {
              tdf <- mdf %>% filter(fleet==k) %>% filter(sex==h) %>% filter(type==t) %>% filter(shell==s) %>% filter(seas==u)
              tpf <- mpf %>% filter(fleet==k) %>% filter(sex==h) %>% filter(type==t) %>% filter(shell==s) %>% filter(seas==u)
              trf <- mrf %>% filter(fleet==k) %>% filter(sex==h) %>% filter(type==t) %>% filter(shell==s) %>% filter(seas==u)
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
  }  
  return(ldf)
}

# natural mortality output ------------
.get_M_df_kjp <- function(M)
{
  n <- length(M)
  ldf <- list()
  mdf <- NULL
  for (i in 1:n)
  {
    A <- M[[i]]
    nrow <- nrow(A$M)
    nsex <- nrow / length(A$mod_yrs)
    A$sex <- rep(1, length = nrow / nsex)
    if (nsex > 1) A$sex <- c(A$sex, rep(2, length = nrow / nsex))
    df <- data.frame(Model=names(M)[i], (cbind(as.numeric(A$mod_yrs), .SEX[A$sex+1], as.numeric(M[[i]]$M[,1])) ), stringsAsFactors = FALSE)
    colnames(df) <- c("Model", "Year", "Sex", "M")
    df$M <- as.numeric(df$M)
    df$Year <- as.numeric(df$Year)
    if (nsex == 2)
    {
      ss <- split(df, df$Sex)
      if (all(ss[[1]]$M == ss[[2]]$M)) df$Sex <- "Male"
    }
    #if(A$nmature==2)
    #{
    #  df$maturity<-rep(c("Mature","Immature"),each=nrow(df)/2)  
    #}
    mdf <- rbind(mdf, df)
  }
  return(mdf)
}


### recruitment -------------
plot_rec_bb_kjp <- function (M, xlab = "Year", ylab = "Recruitment (millions of individuals)") 
{
  xlab <- paste0("\n", xlab)
  ylab <- paste0(ylab, "\n")
  mdf <- .get_recruitment_df(M) 
  p <- ggplot(mdf)
  if (length(M) == 1 && length(unique(mdf$sex)) == 1) {
    p <- p + geom_line(aes(x = year, y = exp(log_rec)))
  }
  else if (length(M) != 1 && length(unique(mdf$sex)) == 1) {
    p <- p + geom_line(aes(x = year, y = exp(log_rec), col = Model))
  }
  else if (length(M) == 1 && length(unique(mdf$sex)) != 1) {
    p <- p + geom_line(aes(x = year, y = exp(log_rec), col = sex))
  }
  else {
  }
  p <- p + labs(x = xlab, y = ylab)
  if (!.OVERLAY) 
    p <- p + facet_wrap(~Model)
  if (length(unique(mdf$sex)) > 1) 
    p <- p + geom_line(aes(x = year, y = exp(log_rec), col = Model))
    p <- p + facet_wrap(~sex, ncol = 1)
  print(p + .THEME)
}

# trawl survey fit seperate out males and females ------
plot_cpue_kjp <- function(M, subsetby = "", psex = "", xlab = "Year", ylab = "CPUE", slab = "Sex", ShowEstErr = FALSE, logy = FALSE)
{
  mdf <- .get_cpue_df(M)
  if (subsetby != "") mdf <- subset(mdf, fleet == subsetby)
  if (psex != "") mdf <- subset(mdf, sex == psex)
  
  if (logy) {
    mdf$cpue <- log(mdf$cpue)
    mdf$lb <- log(mdf$lb)
    mdf$ub <- log(mdf$ub)
    mdf$lbe <- log(mdf$lbe)
    mdf$ube <- log(mdf$ube)
    mdf$pred <- log(mdf$pred)
    ylab <- paste0("log(", ylab, ")")
  }
  
  xlab <- paste0("\n", xlab)
  ylab <- paste0(ylab, "\n")
  
  p  <- ggplot(mdf, aes(year, cpue)) +
    expand_limits(y = 0) +
    geom_pointrange(aes(year, cpue, ymax = ub, ymin = lb), col = "black")
  
  if (ShowEstErr) {
    if (length(M) == 1 && length(unique(mdf$sex)) == 1) {
      p  <- p + geom_pointrange(aes(year, cpue, ymax = ube, ymin = lbe), color = "red", shape = 1, linetype = "dotted", position = position_dodge(width = 1))
    } else if (length(M) != 1 && length(unique(mdf$sex)) == 1) {
      p  <- p + geom_pointrange(aes(year, cpue, ymax = ube, ymin = lbe, col = Model), shape = 1, linetype = "dotted", position = position_dodge(width = 1))
    } else if (length(M) == 1 && length(unique(mdf$sex)) != 1) {
      p  <- p + geom_pointrange(aes(year, cpue, ymax = ube, ymin = lbe, col = sex), shape = 1, linetype = "dotted", position = position_dodge(width = 1))
    } else {
      p  <- p + geom_pointrange(aes(year, cpue, ymax = ube, ymin = lbe, col = Model), shape = 1, linetype = "dotted", position = position_dodge(width = 1))
    }
  }
  
  if (.OVERLAY) {
    if (length(M) == 1 && length(unique(mdf$sex)) == 1) {
      p <- p + geom_line(data = mdf, aes(year, pred)) +
        facet_wrap(~fleet, scales = "free_y")
    } else if (length(M) != 1 && length(unique(mdf$sex)) == 1) {
      p <- p + geom_line(data = mdf, aes(year, pred, color = Model, linetype = Model)) +
        facet_wrap(~fleet, scales = "free_y")
    } else if (length(M) == 1 && length(unique(mdf$sex)) != 1) {
      p <- p + geom_line(data = mdf, aes(year, pred, color = sex)) + labs(col = slab) +
        facet_wrap(~fleet + sex, scales = "free_y")
    } else {
      p <- p + geom_line(data = mdf, aes(year, pred, color = Model, linetype = Model)) +
        facet_wrap(~fleet + sex, scales = "free_y")
    }
  } else {
    p  <- p + geom_line(data = mdf, aes(year, pred))
    p  <- p + facet_wrap(~fleet + sex + Model, scales = "free_y")
  }
  
  p  <- p + labs(x = xlab, y = ylab)
  print(p + .THEME + theme(legend.position=c(.7,.85)))
}

# selectivity separate out fleets ------
plot_selectivity_kjp <- function (M, subsetby = "", ctype = "", xlab = "Mid-point of size class (mm)", ylab = "Selectivity", 
                                   tlab = "Type", ilab = "Period year", nrow = NULL, 
          ncol = NULL, legend_loc = c(1.05, 0.05)) 
{
  xlab <- paste0("\n", xlab)
  ylab <- paste0(ylab, "\n")
  mdf <- .get_selectivity_df(M)
  mdf <- mdf[!mdf$sex %in% "Aggregate", ]
  if (subsetby != "") mdf <- subset(mdf, fleet == subsetby)
  if (ctype != "") mdf <- subset(mdf, type == subsetby)
  
  ncol <- length(unique(mdf$fleet))
  nrow <- length(unique(mdf$Model))
  nrow_sex <- length(unique(mdf$sex))
  p <- ggplot(mdf) + expand_limits(y = c(0, 1))
  if (.OVERLAY) {
    if(ctype != "") {
      p <- p + geom_line(aes(variable, value, col = factor(year), 
                             linetype = Model))
    }
    else {
      p <- p + geom_line(aes(variable, value, col = factor(year), 
                           linetype = type))
    }
    if (length(M) == 1 && length(unique(mdf$sex)) == 1) {
      p <- p + facet_wrap(~fleet, nrow = nrow, ncol = ncol)
    }
    else if (length(M) != 1 && length(unique(mdf$sex)) == 
             1) {
      p <- p + facet_wrap(~Model + fleet, nrow = nrow, 
                          ncol = ncol)
    }
    else if (length(M) == 1 && length(unique(mdf$sex)) != 
             1) {
      p <- p + facet_wrap(~fleet + sex, ncol = nrow_sex, 
                          nrow = ncol)
    }
    else if (ctype != ""){
      p <- p + facet_grid(sex ~ fleet, margins = FALSE)
    }
    else { 
      #if (ctype == 'Capture'){
     #   p <- p + facet_grid(sex ~ fleet, margins = FALSE)
     # }
     # else {
      p <- p + facet_grid(sex + fleet ~ Model, margins = FALSE)
     # }
    }
  }
  else {
    p <- p + geom_line(aes(variable, value, col = factor(year), 
                           linetype = sex), alpha = 0.5)
    p <- p + facet_wrap(~Model + fleet + type, nrow = nrow, 
                        ncol = ncol)
  }
  p <- p + labs(y = ylab, x = xlab, col = ilab, linetype = tlab) + 
    scale_linetype_manual(values = c("solid", "dashed", 
                                     "dotted")) + .THEME
  p <- p + theme(strip.text.x = element_text(margin = margin(1, 
                                                             0, 1, 0)), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                 panel.border = element_blank(), panel.background = element_blank(), 
                 strip.background = element_rect(color = "white", 
                                                 fill = "white"))
  print(p)
}

# size composition residuals by fleet -------------------
plot_size_comps_res_kjp <- function (M, subsetby = "", ncol = 1, xlab = "Year", ylab = "Mid-point of size-class (mm)") 
{
  seas <- NULL
  xlab <- paste0(xlab, "\n")
  ylab <- paste0(ylab, "\n")
  mdf <- .get_sizeComps_df(M) %>% reshape2::melt(id = c("model", 
                                                        "year", "seas", "fleet", "sex", 
                                                        "type", "shell", "maturity", "nsamp", 
                                                        "variable", "value", "pred", "resd")) %>% 
    dplyr::mutate(seas = paste("Season", seas))
  if (subsetby != "") mdf <- subset(mdf, fleet == subsetby)
  
  p <- ggplot(data = mdf) + geom_point(aes(factor(year), variable, 
                                           col = factor(sign(resd)), size = abs(resd)), alpha = 0.6) + 
    scale_size_area(max_size = 10) + labs(x = xlab, y = xlab, 
                                          col = "Sign", size = "Residual") + facet_wrap(fleet ~ 
                                                                                          seas, scales = "free_x", ncol = ncol) +
                                              facet_wrap(sex ~ fleet, scales = "free_x", ncol = ncol) + .THEME + 
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5))
  print(p)
}

# selectivity separate out fleets ------
plot_selectivity_kjp <- function (M, subsetby = "", ctype = "", xlab = "Mid-point of size class (mm)", ylab = "Selectivity", 
                                   tlab = "Type", ilab = "Period year", nrow = NULL, 
          ncol = NULL, legend_loc = c(1.05, 0.05)) 
{
  xlab <- paste0("\n", xlab)
  ylab <- paste0(ylab, "\n")
  mdf <- .get_selectivity_df(M)
  mdf <- mdf[!mdf$sex %in% "Aggregate", ]
  if (subsetby != "") mdf <- subset(mdf, fleet == subsetby)
  if (ctype != "") mdf <- subset(mdf, type == subsetby)
  
  ncol <- length(unique(mdf$fleet))
  nrow <- length(unique(mdf$Model))
  nrow_sex <- length(unique(mdf$sex))
  p <- ggplot(mdf) + expand_limits(y = c(0, 1))
  if (.OVERLAY) {
    if(ctype != "") {
      p <- p + geom_line(aes(variable, value, col = factor(year), 
                             linetype = Model))
    }
    else {
      p <- p + geom_line(aes(variable, value, col = factor(year), 
                           linetype = type))
    }
    if (length(M) == 1 && length(unique(mdf$sex)) == 1) {
      p <- p + facet_wrap(~fleet, nrow = nrow, ncol = ncol)
    }
    else if (length(M) != 1 && length(unique(mdf$sex)) == 
             1) {
      p <- p + facet_wrap(~Model + fleet, nrow = nrow, 
                          ncol = ncol)
    }
    else if (length(M) == 1 && length(unique(mdf$sex)) != 
             1) {
      p <- p + facet_wrap(~fleet + sex, ncol = nrow_sex, 
                          nrow = ncol)
    }
    else if (ctype != ""){
      p <- p + facet_grid(sex ~ fleet, margins = FALSE)
    }
    else { 
      #if (ctype == 'Capture'){
     #   p <- p + facet_grid(sex ~ fleet, margins = FALSE)
     # }
     # else {
      p <- p + facet_grid(sex + fleet ~ Model, margins = FALSE)
     # }
    }
  }
  else {
    p <- p + geom_line(aes(variable, value, col = factor(year), 
                           linetype = sex), alpha = 0.5)
    p <- p + facet_wrap(~Model + fleet + type, nrow = nrow, 
                        ncol = ncol)
  }
  p <- p + labs(y = ylab, x = xlab, col = ilab, linetype = tlab) + 
    scale_linetype_manual(values = c("solid", "dashed", 
                                     "dotted")) + .THEME
  p <- p + theme(strip.text.x = element_text(margin = margin(1, 
                                                             0, 1, 0)), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                 panel.border = element_blank(), panel.background = element_blank(), 
                 strip.background = element_rect(color = "white", 
                                                 fill = "white"))
  print(p)
}

## selectivity just capture and by fleet ----
plot_selectivity_kjp_capture <- function (M, subsetby = "", ctype = "Capture", xlab = "Mid-point of size class (mm)", ylab = "Selectivity", 
                                  tlab = "Model", ilab = "Period year", nrow = NULL, 
                                  ncol = NULL, legend_loc = c(1.05, 0.05)) 
{
  xlab <- paste0("\n", xlab)
  ylab <- paste0(ylab, "\n")
  mdf <- .get_selectivity_df(M)
  mdf <- mdf[!mdf$sex %in% "Aggregate", ]
  if (subsetby != "") mdf <- subset(mdf, fleet == subsetby)
  if (ctype != "") mdf <- subset(mdf, type == subsetby)
  
  ncol <- length(unique(mdf$fleet))
  #nrow <- length(unique(mdf$Model))
  nrow_sex <- length(unique(mdf$sex))
  p <- ggplot(mdf) + expand_limits(y = c(0, 1))
  
    
  p <- p + geom_line(aes(variable, value, col = factor(year), 
                             linetype = Model))
  p <- p + facet_grid(fleet ~ sex, margins = FALSE)
    
  p <- p + labs(y = ylab, x = xlab, col = ilab, linetype = tlab) + .THEME
    #scale_linetype_manual(values = c("solid", "dashed", 
                                     #"dotted")) + .THEME
  p <- p + theme(strip.text.x = element_text(margin = margin(1, 
                                                             0, 1, 0)), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                 panel.border = element_blank(), panel.background = element_blank(), 
                 strip.background = element_rect(color = "white", 
                                                 fill = "white"))
  print(p)
}


ggplot(mdf) + expand_limits(y = c(0, 1)) +
  geom_line(aes(variable, value, col = factor(year), linetype = Model))

# size composition residuals by fleet -------------------
plot_size_comps_res_kjp <- function (M, subsetby = "", ncol = 1, xlab = "Year", ylab = "Mid-point of size-class (mm)") 
{
  seas <- NULL
  xlab <- paste0(xlab, "\n")
  ylab <- paste0(ylab, "\n")
  mdf <- .get_sizeComps_df(M) %>% reshape2::melt(id = c("model", 
                                                        "year", "seas", "fleet", "sex", 
                                                        "type", "shell", "maturity", "nsamp", 
                                                        "variable", "value", "pred", "resd")) %>% 
    dplyr::mutate(seas = paste("Season", seas))
  if (subsetby != "") mdf <- subset(mdf, fleet == subsetby)
  
  p <- ggplot(data = mdf) + geom_point(aes(factor(year), variable, 
                                           col = factor(sign(resd)), size = abs(resd)), alpha = 0.6) + 
    scale_size_area(max_size = 10) + labs(x = xlab, y = xlab, 
                                          col = "Sign", size = "Residual") + facet_wrap(fleet ~ 
                                                                                          seas, scales = "free_x", ncol = ncol) +
                                              facet_wrap(sex ~ fleet, scales = "free_x", ncol = ncol) + .THEME + 
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5))
  print(p)
}

# molt prob male only -------------
plot_molt_prob_sex <- function (M, subsetby = "", xlab = "Mid-point of size class (mm)", ylab = "Probability of molting") 
{
  xlab <- paste0("\n", xlab)
  ylab <- paste0(ylab, "\n")
  mdf <- .get_molt_prob_df(M)
  if (subsetby != "") mdf <- subset(mdf, Sex == subsetby)
  
  p <- ggplot(mdf, aes(x = Length, y = MP)) + expand_limits(y = c(0, 
                                                                  1)) + labs(x = xlab, y = ylab)
  #if (length(M) == 1 && length(unique(mdf$Sex)) == 1) {
  #  p <- p + geom_line() + geom_point()
  #}
  #else if (length(M) != 1 && length(unique(mdf$Sex)) == 1) {
  #  p <- p + geom_line(aes(col = Model, linetype = Year)) + 
  #    geom_point(aes(col = Model, shape = Year))
  #}
  #else if (length(M) == 1 && length(unique(mdf$Sex)) != 1) {
  #  p <- p + geom_line(aes(linetype = Sex, col = Year)) + 
  #    geom_point(aes(col = Year, shape = Sex))
  #}
  #else {
    p <- p + geom_line(aes(linetype = Model, col = Year)) + 
      geom_point(aes(linetype = Model, col = Year)) + facet_wrap(~Sex)
  #}
  print(p + .THEME)
}


# save speces for each model -----------------
save_specs_out <- function (M){
  n <- length(M)
  mdf <- NULL
  for (i in 1:n)
  {
    A <- M[[i]]
    round(A$spr_bmsy*A$spr_depl/1000, 2) -> mmb_2223
    round(A$spr_bmsy/1000, 2) -> b_35 # also B35%
    round(A$sd_fmsy[1], 2) -> f_35 # F35%
    round(A$sd_fofl[1], 2) -> f_ofl # Fofl
    round(A$spr_cofl/1000, 2) -> ofl_2223
    round(A$spr_cofl/1000*0.80, 2) -> abc_2223
    round((A$spr_rbar[1] + A$spr_rbar[2])/1000000, 2) -> avg_rec
    specs <- c(mmb_2223, b_35, f_35, f_ofl, ofl_2223, avg_rec)
    cnames <- c("MMB", "B35%", "F35%", "Fofl", "OFL", "avg_rec")
    df <- data.frame(Model=names(M)[i], cnames, specs)
    mdf <- rbind(mdf, df)
  }
  return(mdf)
}
  
# Detailed save specs for each model -----------------
save_specs_out_more <- function (M){
  n <- length(M)
  mdf <- NULL
  for (i in 1:n)
  {
    A <- M[[i]]
    round(A$spr_bmsy*A$spr_depl/1000, 2) -> mmb_2223
    round(A$spr_bmsy/1000, 2) -> b_35 # also B35%
    round(A$spr_bmsy*A$spr_depl/1000, 2) -> mmb_2223
    round(A$spr_depl, 2) -> b_bmsy # also B over Bmsy
    round(A$sd_fmsy[1], 2) -> f_35 # F35%
    round(A$sd_fofl[1], 2) -> f_ofl # Fofl
    round(A$spr_cofl/1000, 2) -> ofl_2223
    round(A$spr_cofl/1000*0.80, 2) -> abc_2223
    round((A$spr_rbar[1] + A$spr_rbar[2])/1000000, 2) -> avg_rec
    round((A$M[1]), 2) -> maleM
    specs <- c(mmb_2223, b_35, b_bmsy, f_35, f_ofl, ofl_2223, avg_rec, maleM)
    cnames <- c("MMB", "B35%", "B/Bmsy","F35%", "Fofl", "OFL", "avg_rec", "maleM")
    df <- data.frame(Model=names(M)[i], cnames, specs)
    mdf <- rbind(mdf, df)
  }
  return(mdf)
}
  
  
  