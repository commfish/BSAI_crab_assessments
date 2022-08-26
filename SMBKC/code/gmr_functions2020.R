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

#A <- M[mod_scen]

plot_cpue2 <- 
function(M, subsetby = "", xlab = "Year", ylab = "CPUE", slab = "Sex", 
         ShowEstErr = FALSE, logy = FALSE, vastdata = FALSE, vastm = NULL)
{
  mdf <- .get_cpue_df(M)
  if (subsetby != "") mdf <- subset(mdf, fleet == subsetby)
  
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
  
  if (vastdata == TRUE){
    mdf %>% 
      mutate(dataseries = as.factor(ifelse(Model == vastm, "vast", "area"))) -> mdf
    p  <- ggplot(mdf, aes(year, cpue, shape = dataseries)) +
      expand_limits(y = 0) +
      geom_pointrange(aes(year, cpue, ymax = ub, ymin = lb), position = position_dodge(width = 0.85 )) 
      
  }
  else if (vastdata == FALSE) {
    p  <- ggplot(mdf, aes(year, cpue)) +
      expand_limits(y = 0) +
      geom_pointrange(aes(year, cpue, ymax = ub, ymin = lb), col = "black")
  }
  
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
      p <- p + geom_line(data = mdf, aes(year, pred, color = Model, linetype = Model)) +
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
  print(p + .THEME + theme(legend.position = "right",
                           legend.box = "vertical"))
}

plot_cpue_q <- 
  function(M, subsetby = "", xlab = "Year", ylab = "CPUE", slab = "Sex", 
           ShowEstErr = FALSE, logy = FALSE, qblock = FALSE)
  {
    mdf <- .get_cpue_df(M)
    
    if (qblock == TRUE) {
      mdf %>% 
        mutate(fleet = ifelse(fleet == "ADF&G Pot2", "ADF&G Pot", fleet)) -> mdf
    }
    if (subsetby != "") mdf <- subset(mdf, fleet == subsetby)
    
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
    print(p + .THEME + theme(legend.position=c(.8,.85)))
  }

# dealing with VAST data plotting
plot_cpue3 <- 
  function(M, subsetby = "", xlab = "Year", ylab = "CPUE", slab = "Sex", 
           ShowEstErr = FALSE, logy = FALSE, vastdata = FALSE, vastm = NULL)
  {
    mdf <- .get_cpue_df(M)
    if (subsetby != "") mdf <- subset(mdf, fleet == subsetby)
    
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
    
    if (vastdata == TRUE){
      mdf %>% 
        mutate(dataseries = as.factor(ifelse(Model == vastm, "vast", "area"))) -> mdf
      p  <- ggplot(mdf, aes(year, cpue, shape = dataseries, fill = dataseries)) +
        expand_limits(y = 0) +
        scale_shape(guide = FALSE) +
        #geom_line(aes(year, cpue), lwd = 1.25) +
        #geom_ribbon(aes(x=year, ymax = ub, ymin = lb), alpha = 0.25) 
        geom_pointrange(aes(year, cpue, ymax = ub, ymin = lb), 
                        position = position_dodge(width = 0.85 ), 
                        show.legend = FALSE) 
      
    }
    else if (vastdata == FALSE) {
      p  <- ggplot(mdf, aes(year, cpue)) +
        expand_limits(y = 0) +
        geom_pointrange(aes(year, cpue, ymax = ub, ymin = lb), col = "black")
    }
    
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
      p  <- p + scale_shape (guides = FALSE)
    }
    
    p  <- p + labs(x = xlab, y = ylab)
    print(p + .THEME + theme(legend.position=c(.38,.79)) +
            guides(shape = FALSE))
  }

plot_cpue_VAST <- 
  function(M, subsetby = "", xlab = "Year", ylab = "CPUE", slab = "Sex", 
           ShowEstErr = FALSE, logy = FALSE, vastdata = FALSE, vastm = NULL, 
           options = FALSE)
  {
    mdf <- .get_cpue_df(M)
    if (subsetby != "") mdf <- subset(mdf, fleet == subsetby)
    
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
    
    if (vastdata == TRUE){
      mdf %>% 
        mutate(dataseries = as.factor(ifelse(Model == vastm, "vast", "area_swept"))) -> mdf
      mdf %>% 
        select(year, fleet, dataseries, cpue, lb, ub) -> mdf2 #lbe, ube, pred) -> mdf2
      mdf2 %>% 
        melt(id.vars = c("year", "fleet", "dataseries")) %>% 
        unite("dataVar", dataseries:variable, remove = FALSE) %>% 
        dcast(year + fleet ~ dataVar, value.var = "value") -> mdf3
      
      p  <- ggplot(mdf3, aes(year, area_swept_cpue))+
        expand_limits(y = 0) +
        geom_pointrange(aes(year, area_swept_cpue, ymax = area_swept_ub, 
                            ymin = area_swept_lb)) +
        geom_line(aes(year, vast_cpue), col = "gold4", lwd = 1) +
        geom_ribbon(aes(x=year, ymax = vast_ub, ymin = vast_lb), alpha = 0.25, 
                    fill = "gold4")
      
    }
    else if (vastdata == FALSE) {
      p  <- ggplot(mdf, aes(year, cpue)) +
        expand_limits(y = 0) +
        geom_pointrange(aes(year, cpue, ymax = ub, ymin = lb), col = "black")
    }
    
    #if (ShowEstErr) {
    #  if (length(M) == 1 && length(unique(mdf$sex)) == 1) {
    #    p  <- p + geom_pointrange(aes(year, cpue, ymax = ube, ymin = lbe), color = "red", shape = 1, linetype = "dotted", position = position_dodge(width = 1))
    #  } else if (length(M) != 1 && length(unique(mdf$sex)) == 1) {
    #    p  <- p + geom_pointrange(aes(year, cpue, ymax = ube, ymin = lbe, col = Model), shape = 1, linetype = "dotted", position = position_dodge(width = 1))
    #  } else if (length(M) == 1 && length(unique(mdf$sex)) != 1) {
    #    p  <- p + geom_pointrange(aes(year, cpue, ymax = ube, ymin = lbe, col = sex), shape = 1, linetype = "dotted", position = position_dodge(width = 1))
    #  } else {
    #    p  <- p + geom_pointrange(aes(year, cpue, ymax = ube, ymin = lbe, col = Model), shape = 1, linetype = "dotted", position = position_dodge(width = 1))
    #  }
    #}
    
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
    #  p  <- p + scale_shape (guides = FALSE)
    }
    
    p  <- p + labs(x = xlab, y = ylab)
    print(p + .THEME + theme(legend.position=c(.38,.79)) +
            guides(shape = FALSE))
  }

# ssb without using .std file
.get_ssb_dfKP <- function(M)
{
  n <- length(M)
  mdf <- NULL
  for (i in 1:n)
  {
    A <- M[[i]]
    df <- data.frame(Model = names(M)[i],
                     ssb = c(A$ssb, (A$spr_bmsy *A$spr_depl)))
    df$year <- c(A$mod_yrs, (max(A$mod_yrs)+1))
    mdf     <- rbind(mdf, df)
  }
  return(mdf)
}

.get_ssb_dfKP_retro <- function(M)
{
  n <- length(M)
  mdf <- NULL
  for (i in 1:n)
  {
    A <- M[[i]]
    df <- data.frame(Model = names(M)[i],
                     ssb = A$ssb)
    df$year <- c(A$mod_yrs)
    mdf     <- rbind(mdf, df)
  }
  return(mdf)
}


.get_ssb_dfKP_2 <-function(M)
{
  n <- length(M)
  mdf <- NULL
  for (i in 1:n)
  {
    A <- M[[i]]
    df <- data.frame(Model = names(M)[i],
                     par = A$fit$names,
                     log_ssb = A$fit$est,
                     log_sd = A$fit$std)
    df      <- subset(df, par == c("sd_log_ssb", "sd_last_ssb"))
    df$year <- c(A$mod_yrs, (max(A$mod_yrs)+1))
    df$ssb  <- exp(df$log_ssb)
    df$lb   <- exp(df$log_ssb - 1.96*df$log_sd)
    df$ub   <- exp(df$log_ssb + 1.96*df$log_sd)
    mdf     <- rbind(mdf, df)
  }
  return(mdf)
}

.get_recruits_retro_kjp <- function(M)
{
  n <- length(M)
  mdf <- NULL
  for (i in 1:n)
  {
    A <- M[[i]]
    temp <- as.data.frame(A$recruits)
    df <- data.frame(Model = names(M)[i],
                     recruit_m = temp[1,], 
                     recruit_f = temp[2,])
    df$year <- c(A$mod_yrs)
    mdf     <- rbind(mdf, df)
  }
  return(mdf)
}

#.get_recruits_retro_kjp(M[[2]])

## ssb and rec functions ---------------
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

get_rec_out <- function(model_names, raw_data, M)
{
  n <- length(model_names)
  mdf <- NULL
  for (i in 1:n)
  {
    model <- model_names[i]
    df <- subset(raw_data, Model == model_names[i])
    df <- subset(df, par == "Log(rec)")
    #df$year <- A$mod_yrs
    df$rec  <- exp(df$log_par)
    df$lb   <- exp(df$log_par - 1.96*df$log_sd)
    df$ub   <- exp(df$log_par + 1.96*df$log_sd)
    j <- which(M[[i]]$fit$names %in% c("theta[4]"))
    #rstd <- M[[i]]$fit$std[j]
    if (length(j) > 0)
    {
      df$rbar = exp(M[[i]]$fit$est[j])
    } else {
      df$rbar = NA
    }
    mdf     <- rbind(mdf, df)
  }
  return(mdf)
}

get_Db0_out <- function(model_names, raw_data, M)
{
  n <- length(model_names)
  mdf <- NULL
  for (i in 1:n)
  {
    model <- model_names[i]
    df <- subset(raw_data, Model == model_names[i])
    df <- subset(df, par == "Db0(f)")
    #df$year <- A$mod_yrs[-1]
    df$ssb  <- exp(df$log_par)
    df$lb   <- exp(df$log_par - 1.96*df$log_sd)
    df$ub   <- exp(df$log_par + 1.96*df$log_sd)
    mdf     <- rbind(mdf, df)
  }
  return(mdf)
}
# under development ------------

# Fishing mortality plot adjusted-----------------
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


## size comps ----------
plot_size_comps2 <- function(M, which_plots = "all", xlab = "Mid-point of size-class (mm)", ylab = "Proportion",
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






