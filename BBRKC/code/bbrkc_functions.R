# k.palof
# Functions created for BBRKC figures see Jie_cmn_files.R for more context

## bubbleplot of residualts ----------
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

# natural mortality output
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

