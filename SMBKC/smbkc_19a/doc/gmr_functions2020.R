# May 2020
# SMBKC gmr functions.  Need to load these prior to figure creation
# also need to generalize these so that I can update gmr.

plot_datarangeSM <- function(M, verbose = FALSE)
{
  n <- length(M)
  mdf <- NULL
  for (i in 1:n)
  {
    A <- M[[i]]
    #repfile   <- paste(deparse(substitute(M)),".rep",sep="")
    repfile   <- A$run_name
    if (verbose) print(repfile)
    narepfile <- strsplit(scan(repfile,what="character",flush=TRUE,blank.lines.skip=FALSE,quiet=TRUE)[1:6],':')
    
    startyr       <- A$mod_yrs[1]
    endyr         <- A$mod_yrs[length(A$mod_yrs)]
    nfleets       <- length(narepfile[[2]]) + length(narepfile[[3]]) + 
      length(narepfile[[4]]) + length(narepfile[[5]]) + length(narepfile[[6]])
    nfishfleets   <- length(narepfile[[2]]) + length(narepfile[[3]]) + length(narepfile[[4]]) 
    fleetnames    <- c(narepfile[[2]], narepfile[[3]], narepfile[[4]],narepfile[[5]],narepfile[[6]])
    
    df <- as.data.frame(A$dCatchData)
    colnames(df)<- c("year","seas","fleet","sex","obs","cv","type","units","mult","effort","discard_mort")
    
    retainedcatch <- df[df$type==1,]
    discards <- df[df$type==2,]
    cpue <- as.data.frame(A$dSurveyData)
    colnames(cpue)<- c("index", "year","seas","fleet","sex","zero", "obs","cv","units")
    size <- as.data.frame(A$d3_SizeComps)
    colnames(size)<- c("year","seas","fleet","sex","type","shell","maturity","Nsamp",as.character(A$mid_points))
    
    typetable <- matrix(c("retainedcatch", "Retained Catch",
                          "discards",      "Discards",
                          "cpue",          "Abundance indices",
                          "size",          "Size compositions"), ncol = 2, byrow = TRUE)
    
    typenames <- typetable[,1]
    typelabels <- typetable[,2]
    
    # loop over types to make a database of years with comp data
    ntypes <- 0
    # replace typetable object with empty table
    typetable <- NULL
    # now loop over typenames looking for presence of this data type
    for(itype in 1:length(typenames))
    {
      dat <- get(typenames[itype])
      typename <- typenames[itype]
      if(!is.null(dat) && !is.na(dat) && nrow(dat)>0)
      {
        ntypes <- ntypes+1
        for(ifleet in 1:nfleets)
        {
          allyrs <- NULL
          # identify years from different data types
          #if(typename=="catch" & ifleet<=nfishfleets) allyrs <- dat$Yr[dat[,ifleet]>0]
          if(typename %in% c("retainedcatch","discards") & ifleet<=nfishfleets) 
          {
            allyrs <- dat$year[dat$fleet==ifleet]
          }
          if(typename %in% "cpue") allyrs <- dat$year[dat$fleet==ifleet]
          if(typename %in% "size") allyrs <- dat$year[dat$fleet==ifleet]
          # expand table of years with data
          if(!is.null(allyrs) & length(allyrs)>0)
          {
            yrs <- sort(unique(floor(allyrs)))
            typetable <- rbind(typetable,
                               data.frame(yr=yrs,fleet=ifleet,
                                          itype=ntypes,typename=typename,
                                          stringsAsFactors=FALSE))
          }
        }
      }
    }
    ntypes <- max(typetable$itype)
    fleets <- sort(unique(typetable$fleet))
  }
  
  plotdata <- function()
  {
    margins=c(5.1,2.1,4.1,8.1)  
    par(mar=margins) # multi-panel plot
    xlim <- c(-1,1)+range(typetable$yr,na.rm=TRUE)
    yval <- 0
    # count number of unique combinations of fleet and data type
    ymax <- sum(as.data.frame(table(typetable$fleet,typetable$itype))$Freq>0)
    plot(0,xlim=xlim,ylim=c(0,ymax+ntypes+.5),axes=FALSE,xaxs='i',yaxs='i',
         type="n",xlab="Year",ylab="",main="Data by type and year",cex.main=1.5)
    xticks <- 5*round(xlim[1]:xlim[2]/5)
    abline(v=xticks,col='grey',lty=3)
    axistable <- data.frame(fleet=rep(NA,ymax),yval=NA)
    itick <- 1
    for(itype in rev(unique(typetable$itype)))
    {
      typename <- unique(typetable$typename[typetable$itype==itype])
      #fleets <- sort(unique(typetable2$fleet[typetable2$itype==itype]))
      for(ifleet in rev(fleets))
      {
        yrs <- typetable$yr[typetable$fleet==ifleet & typetable$itype==itype]
        if(length(yrs)>0)
        {
          yval <- yval+1
          x <- min(yrs):max(yrs)
          n <- length(x)
          y <- rep(yval,n)
          y[!x%in%yrs] <- NA
          # identify solo points (no data from adjacent years)
          solo <- rep(FALSE,n)
          if(n==1) solo <- 1
          if(n==2 & yrs[2]!=yrs[1]+1) solo <- rep(TRUE,2)
          if(n>=3)
          {
            for(i in 2:(n-1)) if(is.na(y[i-1]) & is.na(y[i+1])) solo[i] <- TRUE
            if(is.na(y[2])) solo[1] <- TRUE
            if(is.na(y[n-1])) solo[n] <- TRUE
          }
          # add points and lines
          points(x[solo], y[solo], pch=16, cex=2,col=rainbow(nfleets)[ifleet])
          lines(x, y, lwd=12,col=rainbow(nfleets)[ifleet])
          axistable[itick,] <- c(ifleet,yval)
          itick <- itick+1
        }
      }
      yval <- yval+1
      if(itype!=1) abline(h=yval,col='grey',lty=3)
      text(mean(xlim),yval-.3,typelabels[typenames==typename],font=2)
    }        
    axis(4,at=axistable$yval,labels=fleetnames[axistable$fleet],las=1)
    box()
    axis(1,at=xticks)        
  }
  pdatarange <- plotdata()
  if (verbose) return(pdatarange)
}

.get_M_df2 <- function(M)
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

plot_natural_mortality2 <- 
  function(M, plt_knots = TRUE, knots = c(1976, 1980, 1985, 1994),
           slab = "Knot")
  {
    mdf <- .get_M_df2(M)
    if (length(M) == 1)
    {
      p <- ggplot(mdf, aes(x = Year, y = M))
    } else {
      p <- ggplot(mdf, aes(x = Year, y = M, colour = Model))
    }
    
    if (length(unique(mdf$Sex)) == 1)
    {
      p <- p + geom_line()
    } else {
      p <- p + geom_line(aes(linetype = Sex))
    }
    
    #if (length(unique(mdf$maturity)) == 1)
    #{
    #  p <- p + geom_line()
    #} else {
    #  p <- p + facet_wrap(~maturity)
    #}
    
    
    if (plt_knots)
    {
      mdf$Knot <- NA
      mdf$Knot[mdf$Year %in% knots] <- mdf$M[mdf$Year %in% knots]
      p <- p + geom_point(data = mdf, aes(x = Year, y = Knot, colour = Model)) +
        labs(col = slab)
    }
    p <- p + expand_limits(y = 0) + labs(x = "\nYear", y = "Natural mortality (M)\n")
    print(p + .THEME)
  }




# under development -----------------
plot_F2 <- function (M, scales = "free_y", xlab = "Year", ylab = "F", 
                     mlab = "Model") 
{
  xlab <- paste0("\n", xlab)
  ylab <- paste0(ylab, "\n")
  m <- .get_F_df(M)
  mdf <- m$F
  mdf$season <- paste0("Season: ", mdf$season)
  fbar <- m$fbar
  p <- ggplot(data = mdf) + geom_hline(data = fbar, aes(yintercept = fbar, 
                                                        color = Model), linetype = "dashed", alpha = 0.5) + 
    geom_line(aes(year, F, col = model, group = 1)) + facet_grid(fleet ~ 
                                                                   season, scales = scales) + labs(x = xlab, y = ylab)
  print(p + .THEME + theme(legend.position = c(0.2, 0.9)))
}

.get_F_df2 <- function(M)
{
  n <- length(M)
  fdf <- NULL
  fbar <- NULL
  for ( i in 1:n )
  {
    A <- M[[i]]
    nyear <- length(A$mod_yrs)
    nseas <- nseason <- A$nseason
    nclass <- length(A$mid_points)
    nfleet <- A$nfleet
    df <- data.frame(A$ft, Model = names(M)[i])
    colnames(df) <- c(1:nseason, "model")
    df$year <- rep(A$mod_yrs, by = nfleet)
    df$fleet <- rep(.FLEET, each = nyear*A$nsex)
    df$sex <- rep("Sex",nrow(df))
    if(A$nsex==2)
      df$sex <- rep(rep(c("Male","Female"),each = nyear),nfleet)
    del <- NULL
    for ( j in 1:nseason )
    {
      if (all(df[,j] == 0))
      {
        del <- c(del, j)
        nseas <- nseas - 1
      }
    }
    df <- df[,-del]
    df <- tidyr::gather(df, "season", "F", 1:nseas)
    for ( j in unique(df$model) )
    {
      for ( k in unique(df$season) )
      {
        for ( l in unique(df$fleet) )
        {
          if (all(df[df$model %in% j & df$season %in% k & df$fleet %in% l,]$F == 0)) df <- df[-which(df$model %in% j & df$season %in% k & df$fleet %in% l),]
        }
      }
    }
    fdf <- rbind(fdf, df)
    
    df <- data.frame(Model = names(M)[i], fbar = exp(A$log_fbar))
    df$fleet <- .FLEET
    df <- df[which(df$fleet %in% unique(fdf$fleet)),]
    fbar <- rbind(fbar, df)
  }
  fdf$year <- as.integer(fdf$year)
  fdf$fleet <- factor(fdf$fleet, levels = .FLEET)
  fbar$fleet <- factor(fbar$fleet, levels = .FLEET)
  return(list(F = fdf, fbar = fbar))
}