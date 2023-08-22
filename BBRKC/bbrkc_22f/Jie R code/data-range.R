#data#' Plot data range by fleet and year
#'
#' @param A a list of lists object created by the read_admb function

  margins=c(3.5,1.5,3.1,7.1)
  par(mgp=c(2.2,0.8,0),xpd=FALSE)
  par(mar=margins) # multi-panel plot
  xlim <- c(B$syr-1,B$nyr+2)
  # number of data groups: 1. Size Compositions, 2. Abundance Indies, 3. Discard Catches, 4. Retained Catch
  np <- 4
  # Number of datasets for each groups: 1. 7, 2. 2, 3. 4, 4. 1.
  nn <- c(7, 2, 4, 1)

 # number of years for data: retained, total male, trawl, Tanner, fixed, tawl survey, bsfrf
# nn1 <- c(                       43,       29,      44,      11,    25,    46,          6)
 nn1 <- c(nrow(B$d3_obs_size_comps_1),nrow(B$d3_obs_size_comps_2),nrow(B$d3_obs_size_comps_4),nrow(B$d3_obs_size_comps_5),nrow(B$d3_obs_size_comps_6),nrow(B$d3_obs_size_comps_7),nrow(B$d3_obs_size_comps_8))
 nn2 <- c(nn1[1],(nn1[1]+nn1[2]),(nn1[1]+2*nn1[2]+nn1[3]),(nn1[1]+2*nn1[2]+2*nn1[3]+nn1[4]),(nn1[1]+2*nn1[2]+2*nn1[3]+2*nn1[4]+nn1[5]),(nn1[1]+2*nn1[2]+2*nn1[3]+2*nn1[4]+2*nn1[5]+nn1[6]),(nn1[1]+2*nn1[2]+2*nn1[3]+2*nn1[4]+2*nn1[5]+2*nn1[6]+nn1[7]))
  yrs_fish <- B$d3_SizeComps_in[1:nn2[1],1]
  yrs_fish_disc <- B$d3_SizeComps_in[(nn2[1]+1):nn2[2],1]
  yrs_trawl <- B$d3_SizeComps_in[(nn2[2]+nn1[2]+1):nn2[3],1]
  yrs_tc <- B$d3_SizeComps_in[(nn2[3]+nn1[3]+1):nn2[4],1]
  yrs_fix <- B$d3_SizeComps_in[(nn2[4]+nn1[4]+1):nn2[5],1]
  yrs_srv1 <- B$d3_SizeComps_in[(nn2[5]+nn1[5]+1):nn2[6],1]
  yrs_bsfrf <- B$d3_SizeComps_in[(nn2[6]+nn1[6]+1):nn2[7],1]

  yval <- 0
  # count number of unique combinations of fleet and data type
  ymax <- 4+7+2+4+1
  coi <- c(1,2,6,3,3,4,5,4,5,1,2,6,3,3)
  # A list for years
  yrl <- list(yrs_tc,yrs_trawl,yrs_fix,yrs_fish_disc,yrs_fish,yrs_bsfrf,yrs_srv1,yrs_bsfrf,yrs_srv1,yrs_tc,yrs_trawl,yrs_fix,yrs_fish_disc,yrs_fish)
  nal <- list("TC_bycatch","Trawl_bycatch","Fix_bycatch","Pot_discard","Retained","BSFRF_trawl","NMFS_trawl","BSFRF_trawl","NMFS_trawl","TC_bycatch","Trawl_bycatch","Fix_bycatch","Pot_discard","Retained")
  typena <- c("Size Compositions","Survey Biomass Indices","Discarded Catches","Retained Catch")
  plot(0,xlim=xlim,ylim=c(0,ymax+0.7),axes=FALSE,xaxs='i',yaxs='i',
       type="n",xlab="Year",ylab="",main="Data by type and year",cex.main=1.5)
  xticks <- 5*round(xlim[1]:xlim[2]/5)
  abline(v=xticks,col='grey',lty=3)
  ii = 0
  for(i in 1:np)
  {
      for(j in 1:nn[i])
      {
          ii <- ii + 1
          yrs <- yrl[[ii]]
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
                  for(k in 2:(n-1)) if(is.na(y[k-1]) & is.na(y[k+1])) solo[k] <- TRUE
                  if(is.na(y[2])) solo[1] <- TRUE
                  if(is.na(y[n-1])) solo[n] <- TRUE
              }
              # add points and lines
              points(x[solo], y[solo], pch=16, cex=2,col=coi[ii])
              lines(x, y, lwd=12,col=coi[ii])
  #            axistable[itick,] <- c(j,yval)
  #            itick <- itick+1
          }
          axis(4,at=yval,labels=nal[ii],las=1)
      }
      yval <- yval+1
      text(mean(xlim),yval-.3,typena[i],font=2)
  }
  box()
  axis(1,at=xticks)
