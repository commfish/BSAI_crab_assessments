
### mmb 5 ------------
n1<-1
n2<-1
n3<-A$nyr-A$syr+2
d0m0<-A$ssb/1000.0
#d1m0<-I$ssb/1000.0
#d2m0<-A$ssb/1000.0
#d3m0<-J$ssb/1000.0
#d4m0<-K$ssb/1000.0
#d5m0<-F$ssb/1000.0
#d4bm0<-G$ssb/1000.0
#d5m0<-H$ssb/1000.0
d0m<-c(d0m0,A$spr_bmsy*A$spr_depl/1000.0)
#d1m<-c(d1m0,B$spr_bmsy*B$spr_depl/1000.0)
#d2m<-c(d2m0,C$spr_bmsy*C$spr_depl/1000.0)
#d3m<-c(d3m0,D$spr_bmsy*D$spr_depl/1000.0)
#d4m<-c(d4m0,E$spr_bmsy*E$spr_depl/1000.0)
#d5m<-c(d5m0,F$spr_bmsy*F$spr_depl/1000.0)
#d4bm<-c(d4bm0,G$spr_bmsy*G$spr_depl/1000.0)
#d5m<-c(d5m0,H$spr_bmsy*H$spr_depl/1000.0)
d6m<-c(rep(0,n3))
d6f<-c(rep(0,n3))

par(oma=c(3.50,4.0,1.0,1.0),tck=-0.01,xaxs="i",yaxs="i",font=1,lwd=0.75)
par(mgp=c(1.2,1.0,0),mar=c(0,0,0,0),mfcol=c(n1,n2),xpd=FALSE)
xat<-c(1975,1978,1981,1984,1987,1990,1993,1996,1999,2002,2005,2008,2011,2014,2017,2020)
yat<-c(0,10,20,30,40,50,60,70,80,90,100,110,120,130)
xm<-A$syr-1
xx<-A$nyr+2
ym<-0
up<-0.87
year<-c(A$syr:(A$nyr+1))

yx<-130.0
plot(year,d0m[1:n3],axes=FALSE,ylim=c(ym,yx),xlim=c(xm,xx),type="n",lwd=1)
lines(year,d0m[1:n3],lty=1,lwd=2.5,col=1)
lines(year,d1m[1:n3],lty=2,lwd=2.5,col=2)
lines(year,d2m[1:n3],lty=3,lwd=2.5,col=3)
lines(year,d3m[1:n3],lty=4,lwd=2.5,col=4)
lines(year,d4m[1:n3],lty=5,lwd=2.5,col=5)
#   lines(year,d5m[1:n3],lty=6,lwd=2.5,col=6)
#   lines(year,d4bm[1:n3],lty=7,lwd=2.5,col=7)
#   lines(year,d5m[1:n3],lty=8,lwd=2.5,col=8)
legend("topright",inset=0.02,c("21.1","21.1a","21.1b","22.1","22.1a"),lwd=c(2.5,2.5,2.5,2.5,2.5),lty=c(1,2,3,4,5),col=c(1,2,3,4,5),cex=c(0.8,0.8,source("0.8,0.8,0.8")))

#   legend("topright",inset=0.02,c("19.3d","19.3e","19.3g","21.0"),lwd=c(2.5,2.5,2.5,2.5),lty=c(1,2,3,4),col=c(1,2,3,4),cex=c(0.8,0.8,0.8,0.8))
#   legend("topright",inset=0.02,c("19.3d","19.3e","19.3f","19.3g","19.3i","19.6"),lwd=c(2.5,2.5,2.5,2.5,2.5,2.5),lty=c(1,2,3,4,5,6),col=c(1,2,3,4,5,6),cex=c(0.8,0.8,0.8,0.8,0.8,0.8))
#  legend("topright",inset=0.02,c("19.0a","19.3","19.3a"),lwd=c(2.5,2.5,2.5),lty=c(1,2,3),col=c(1,2,3),cex=c(0.8,0.8,0.8))
   axis(2,at=yat,labels=yat,outer=T,cex=1.5)
   par(mgp=c(3.0,0.75,0))
   axis(1,at=xat,labels=xat,outer=T,cex=1.5)
   box()
yt<-c('Estimated mature male biomass (1000 t)')
mtext(yt,2,2.8,outer=T,cex=1.0)
mtext('Year',1,2.10,outer=T,cex=1.0)
par(mfrow=c(1,1))




### sel-nmfs-one -----
n3<-20
#tt<-0.930
tt1<-0.961 # not sure what these are? 
tt2<-0.964
d1m<-B$selectivity[5,4:23]*tt1 # base model for 2022
d2m<-B$selectivity[17,4:23]*tt1
d2f<-C$selectivity[5,4:23]*tt2
d2g<-D$selectivity[5,4:23]*tt2
#d2f<-J$selectivity[15,4:23]*tt2
xat<-c(70,80,90,100,110,120,130,140,150,160)
yat<-c(0.1,0.3,0.5,0.7,0.9)
yatv<-c("0.1","0.3","0.5","0.7","0.9")
xm<-67
xx<-163
ym<-0
up<-0.9
par(oma=c(3.50,4.0,1.0,1.0),tck=-0.01,xaxs="i",yaxs="i",font=1,lwd=0.75)
par(mgp=c(1.2,1.0,0),mar=c(0,0,0,0),mfcol=c(1,1),xpd=FALSE)
# par(mgp=c(5.0,0.7,0))
yx = 1.05
plot(A$mid_points,d1f[1:n3],ylim=c(ym,yx),xlim=c(xm,xx),type="n",lwd=1)
#   title(xlab="Length (mm)",ylab="MFS survey selectivities")
lines(A$mid_points,d1m[1:n3],lty=1,lwd=2.5,col=1)
lines(A$mid_points,d2m[1:n3],lty=3,lwd=2.5,col=3)
lines(A$mid_points,d2f[1:n3],lty=2,lwd=2.5,col=2)
lines(A$mid_points,d2g[1:n3],lty=4,lwd=2.5,col=4)
legend("bottomright",inset=0.01,cex = 0.7, c("Model 21.1b 1975-81","Model 21.1b 1982-2022","Model 22.0 1985-2022","Model 22.0a 1985-2022"),lwd=c(2.5,2.5,2.5,2.5),lty=c(1,2,3,4),col=c(1,2,3,4))
axis(2,at=yat,labels=yatv,outer=T,cex=1.0)
#  axis(1,at=xat,labels=xat,outer=T,cex=1.0,las=3)
axis(1,at=xat,labels=xat,outer=T,cex=1.0)
yt<-c('NMFS survey selectivities')
mtext(yt,2,2.3,outer=T,cex=1.1)
mtext('Length (mm)',1,2.0,outer=T,cex=1.1)
box()
par(mfrow=c(1,1))




#### sel-directedpot (model 21.1b)-------
n1<-2
n2<-1
n3<-20
# B is 2022 base model 21.1b
d1m<-B$selectivity[1,4:23] # fleet #1 is pot fishery 
d3m<-B$retained[1,4:23] # pot fishery retained before 2005
d4m<-B$retained[2,4:23] # pot fishery retained after 2004
d1f<-B$selectivity[2,4:23] # trawl bycatch
d3f<-B$selectivity[4,4:23] # fixed gear
par(oma=c(3.50,4.0,1.0,1.0),tck=-0.01,xaxs="i",yaxs="i",font=1,lwd=0.75)
par(mgp=c(1.2,1.0,0),mar=c(0,0,0,0),mfcol=c(n1,n2),xpd=FALSE)
nc<-ncol(d1m)
nr<-nrow(d1m)
xat<-c(70,80,90,100,110,120,130,140,150,160)
yat<-c(0.1,0.3,0.5,0.7,0.9)
yatv<-c("0.1","0.3","0.5","0.7","0.9")
#yatv<-c(paste(t2),paste(t3))
xm<-67
xx<-163
ym<-0
up<-0.45 #0.63

yx<-1.05
plot(B$mid_points,d1m[1:n3],axes=FALSE,ylim=c(ym,yx),xlim=c(xm,xx),type="n",lwd=1)
#  par(new=T,xaxs="i",yaxs="i")
lines(B$mid_points,d1m[1:n3],lty=1,lwd=2.5,col=1)
lines(B$mid_points,d3m[1:n3],lty=2,lwd=2.5,col=2)
lines(B$mid_points,d4m[1:n3],lty=3,lwd=2.5,col=3)
legend("topleft",inset=0.02,cex = 0.5, c("Total","Retained, before 2005","Retained, after 2004"),lwd=c(2.5,2.5,2.5),lty=c(1,2,3),col=c(1,2,3))
text(0.58*xx,yx*up,"Pot total selectivity & retained proportions",cex=0.9)
axis(2,at=yat,labels=yatv,outer=T,cex=1.5)
box()
yx = 1.05
up<-0.55 #0.70
#yat<-c(10,40,70,100,130)
#   yat<-c(0.5,1.0,1.5,2.0)
#   yatv<-c("0.5","1.0","1.5","2.0")
plot(B$mid_points,d1f[1:n3],axes=FALSE,ylim=c(ym,yx),xlim=c(xm,xx),type="n",lwd=1)
lines(B$mid_points,d1f[1:n3],lty=1,lwd=2.5,col=1)
lines(B$mid_points,d3f[1:n3],lty=2,lwd=2.5,col=2)
legend("topleft",inset=0.02, cex = 0.5, c("Trawl","Fixed gear"),lwd=c(2.5,2.5),lty=c(1,2),col=c(1,2))
text(0.58*xx,yx*up,"Trawl & fixed gear bycatch selectivities",cex=0.9)
axis(2,at=yat,labels=yatv,outer=T,cex=1.5)
par(mgp=c(3.0,0.75,0))
#  axis(1,at=xat,labels=xat,outer=T,cex=1.0,las=3)
axis(1,at=xat,labels=xat,outer=T,cex=1.5)
box()
#yt<-c('NMFS survey selectivities')
#mtext(yt,2,2.8,outer=T,cex=1.0)
mtext('Length (mm)',1,2.10,outer=T,cex=1.0)
par(mfrow=c(1,1))

#### sel-directedpot (model 22.0)-------
n1<-2
n2<-1
n3<-20
# C is model 22.0 for 2022
d1m<-C$selectivity[1,4:23] # fleet #1 is pot fishery 
d3m<-C$retained[1,4:23] # pot fishery retained before 2005
d4m<-C$retained[2,4:23] # pot fishery retained after 2004
d1f<-C$selectivity[2,4:23] # trawl bycatch
d3f<-C$selectivity[4,4:23] # fixed gear
par(oma=c(3.50,4.0,1.0,1.0),tck=-0.01,xaxs="i",yaxs="i",font=1,lwd=0.75)
par(mgp=c(1.2,1.0,0),mar=c(0,0,0,0),mfcol=c(n1,n2),xpd=FALSE)
nc<-ncol(d1m)
nr<-nrow(d1m)
xat<-c(70,80,90,100,110,120,130,140,150,160)
yat<-c(0.1,0.3,0.5,0.7,0.9)
yatv<-c("0.1","0.3","0.5","0.7","0.9")
#yatv<-c(paste(t2),paste(t3))
xm<-67
xx<-163
ym<-0
up<-0.45 #0.63

yx<-1.05
plot(B$mid_points,d1m[1:n3],axes=FALSE,ylim=c(ym,yx),xlim=c(xm,xx),type="n",lwd=1)
#  par(new=T,xaxs="i",yaxs="i")
lines(B$mid_points,d1m[1:n3],lty=1,lwd=2.5,col=1)
lines(B$mid_points,d3m[1:n3],lty=2,lwd=2.5,col=2)
lines(B$mid_points,d4m[1:n3],lty=3,lwd=2.5,col=3)
legend("topleft",inset=0.02,cex = 0.6, c("Total","Retained, before 2005","Retained, after 2004"),lwd=c(2.5,2.5,2.5),lty=c(1,2,3),col=c(1,2,3))
text(0.58*xx,yx*up,"Pot total selectivity & retained proportions",cex=0.9)
axis(2,at=yat,labels=yatv,outer=T,cex=1.5)
box()
yx = 1.05
up<-0.55 #0.70
#yat<-c(10,40,70,100,130)
#   yat<-c(0.5,1.0,1.5,2.0)
#   yatv<-c("0.5","1.0","1.5","2.0")
plot(B$mid_points,d1f[1:n3],axes=FALSE,ylim=c(ym,yx),xlim=c(xm,xx),type="n",lwd=1)
lines(B$mid_points,d1f[1:n3],lty=1,lwd=2.5,col=1)
lines(B$mid_points,d3f[1:n3],lty=2,lwd=2.5,col=2)
legend("topleft",inset=0.02, cex = 0.6, c("Trawl","Fixed gear"),lwd=c(2.5,2.5),lty=c(1,2),col=c(1,2))
text(0.58*xx,yx*up,"Trawl & fixed gear bycatch selectivities",cex=0.9)
axis(2,at=yat,labels=yatv,outer=T,cex=1.5)
par(mgp=c(3.0,0.75,0))
#  axis(1,at=xat,labels=xat,outer=T,cex=1.0,las=3)
axis(1,at=xat,labels=xat,outer=T,cex=1.5)
box()
#yt<-c('NMFS survey selectivities')
#mtext(yt,2,2.8,outer=T,cex=1.0)
mtext('Length (mm)',1,2.10,outer=T,cex=1.0)
par(mfrow=c(1,1))

#### sel-directedpot (model 22.0a)-------

#### molt-prob (model 21.1b) --------------
n3<-20
d1m<-B$molt_probability[1,] # B is model 21.1b for 2022
d2m<-B$molt_probability[10,]
d3m<-c(1.0,1.0,1.0,1.0,1.0,0.993,0.990,0.987,0.983,0.979,0.976,0.97,0.96,0.95,0.94,0.92,0.89,0.86,0.81,0.74)
d4m<-c(1.0,1.0,1.0,1.0,0.99,0.98,0.970,0.960,0.940,0.920,0.880,0.83,0.77,0.69,0.61,0.53,0.44,0.36,0.29,0.23)
xat<-c(70,80,90,100,110,120,130,140,150,160)
yat<-c(0.1,0.3,0.5,0.7,0.9)
yatv<-c("0.1","0.3","0.5","0.7","0.9")
xm<-67
xx<-163
ym<-0
up<-0.9

par(mgp=c(5.0,0.7,0))
yx = 1.05
plot(A$mid_points,d1m[1:n3],ylim=c(ym,yx),xlim=c(xm,xx),xlab="Length (mm)",type="n",lwd=1)
lines(A$mid_points,d1m[1:n3],lty=1,lwd=2.5,col=1)
lines(A$mid_points,d2m[1:n3],lty=2,lwd=2.5,col=2)
lines(A$mid_points,d3m[1:n3],lty=3,lwd=2.5,col=3)
lines(A$mid_points,d4m[1:n3],lty=4,lwd=2.5,col=4)
legend("bottomleft",inset=0.02,cex = 0.7, c("Model 1975-1979","Model 1980-2022","Tagging 1954-1961","Tagging 1966-1969"),lwd=c(2.5,2.5,2.5,2.5),lty=c(1,2,3,4),col=c(1,2,3,4))
axis(2,at=yat,labels=yatv,outer=T,cex=1.5)
#  axis(1,at=xat,labels=xat,outer=T,cex=1.0,las=3)
axis(1,at=xat,labels=xat,outer=T,cex=1.5)
box()
yt<-c('Molting probabilities')
mtext(yt,2,2.3,outer=T,cex=1.2)
mtext('Length (mm)',1,2.0,outer=T,cex=1.2)
par(mfrow=c(1,1))

#### molt-prob (model 22.0/22.0a) --------------
n3<-20
d1m<-C$molt_probability[1,] # C is model 22.0 for 2022
d2m<-D$molt_probability[1,] # D is model 22.0a 
d3m<-c(1.0,1.0,1.0,1.0,1.0,0.993,0.990,0.987,0.983,0.979,0.976,0.97,0.96,0.95,0.94,0.92,0.89,0.86,0.81,0.74)
d4m<-c(1.0,1.0,1.0,1.0,0.99,0.98,0.970,0.960,0.940,0.920,0.880,0.83,0.77,0.69,0.61,0.53,0.44,0.36,0.29,0.23)
xat<-c(70,80,90,100,110,120,130,140,150,160)
yat<-c(0.1,0.3,0.5,0.7,0.9)
yatv<-c("0.1","0.3","0.5","0.7","0.9")
xm<-67
xx<-163
ym<-0
up<-0.9

par(mgp=c(5.0,0.7,0))
yx = 1.05
plot(B$mid_points,d1m[1:n3],ylim=c(ym,yx),xlim=c(xm,xx),xlab="Length (mm)",type="n",lwd=1)
lines(B$mid_points,d1m[1:n3],lty=1,lwd=2.5,col=1)
lines(B$mid_points,d2m[1:n3],lty=2,lwd=2.5,col=2)
lines(B$mid_points,d3m[1:n3],lty=3,lwd=2.5,col=3)
lines(B$mid_points,d4m[1:n3],lty=4,lwd=2.5,col=4)
legend("bottomleft",inset=0.02,cex = 0.7, c("Model 22.0, 1985-2022", "Model 22.0a, 1985-2022","Tagging 1954-1961","Tagging 1966-1969"),lwd=c(2.5,2.5,2.5,2.5),lty=c(1,2,3,4),col=c(1,2,3,4))
axis(2,at=yat,labels=yatv,outer=T,cex=1.5)
#  axis(1,at=xat,labels=xat,outer=T,cex=1.0,las=3)
axis(1,at=xat,labels=xat,outer=T,cex=1.5)
box()
yt<-c('Molting probabilities')
mtext(yt,2,2.3,outer=T,cex=1.2)
mtext('Length (mm)',1,2.0,outer=T,cex=1.2)
par(mfrow=c(1,1))



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


# BSFRF selectivity ----------
n3<-20
tt1<-1
tt2<-1
tt3<-1
#tt4<-1
d1m<-B$selectivity[6,4:23]*tt1 
d2m<-C$selectivity[6,4:23]*tt2
d3m<-D$selectivity[6,4:23]*tt3
#d4m<-D$selectivity[6,4:23]*tt4
xat<-c(70,80,90,100,110,120,130,140,150,160)
yat<-c(0.1,0.3,0.5,0.7,0.9)
yatv<-c("0.1","0.3","0.5","0.7","0.9")
xm<-67
xx<-163
ym<-0
up<-0.9

par(mgp=c(5.0,0.7,0))
yx = 1.1
plot(A$mid_points,d1m[1:n3],ylim=c(ym,yx),xlim=c(xm,xx),xlab="Length (mm)",type="n",lwd=1)
lines(A$mid_points,d1m[1:n3],lty=1,lwd=2.5,col=1)
lines(A$mid_points,d2m[1:n3],lty=2,lwd=2.5,col=2)
lines(A$mid_points,d3m[1:n3],lty=3,lwd=2.5,col=3)
#lines(A$mid_points,d4m[1:n3],lty=4,lwd=2.5,col=4)
legend("bottomright",inset=0.02,c("21.1b","22.0","22.0a"),lwd=c(2.5,2.5,2.5),lty=c(1,2,3),col=c(1,2,3),cex=c(0.6,0.6,0.6)) 
axis(2,at=yat,labels=yatv,outer=T,cex=1.5)
axis(1,at=xat,labels=xat,outer=T,cex=1.5)
box()
yt<-c('BSFRF survey selectivities')
mtext(yt,2,2.3,outer=T,cex=1.2)
mtext('Length (mm)',1,2.0,outer=T,cex=1.2)
par(mfrow=c(1,1))


## rec 3 ----------------
n1<-3
n2<-1
n3<-B$nyr-B$syr+1
n4<-C$nyr-C$syr+1
d1m<-(B$recruits[1,]+B$recruits[2,])/1000000
d2m<-(C$recruits[1,]+C$recruits[2,])/1000000
d3m<-(D$recruits[1,]+D$recruits[2,])/1000000
d1f<-mean(d1m[9:(n3-1)])
d2f<-mean(d2m[1:(n4-1)])
d3f<-mean(d3m[1:(n4-1)])

par(oma=c(3.50,4.0,1.0,1.0),tck=-0.01,xaxs="i",yaxs="i",font=1,lwd=0.75)
par(mgp=c(1.2,0.7,0),mar=c(0,0,0,0),mfcol=c(n1,n2),xpd=FALSE)
xat<-c(2,7,12,17,22,27,32,37,42,47)-0.5
xatv<-c("1977","1982","1987","1992","1997","2002","2007","2012","2017","2022")
yat<-c(10,40,70,100,130,160,190,220)
xm<-0
xx<-n3
ym<-0
up<-0.87
year<-c(1:n3)

yx<-255
plot(year,d1m[1:n3],axes=FALSE,ylim=c(ym,yx),xlim=c(xm,xx),type="n",lwd=1)
par(new=T,xaxs="i",yaxs="i")
barplot(d1m[1:n3],space=0,density=-1,col="red",axes=FALSE,ylim=c(ym,yx),xlim=c(xm,xx),names.arg=" ")
lines(c(9,(n3-1))-0.5,c(d1f,d1f),lty=1,lwd=2.5,col=1)
# legend("topright",inset=0.02,c("Recruits","1984-2018 mean"),lwd=c(2.5,2.5),lty=c(1,2),col=c(1,2))
text(0.4*xx,yx*up,"Model 21.1b",cex=1.5)
axis(2,at=yat,labels=yat,outer=T,cex=1.5)
box()

#plot(year,d3m[1:n3],axes=FALSE,ylim=c(ym,yx),xlim=c(xm,xx),type="n",lwd=1)
#par(new=T,xaxs="i",yaxs="i")
#barplot(d3m[1:n3],space=0,density=-1,col="red",axes=FALSE,ylim=c(ym,yx),xlim=c(xm,xx),names.arg=" ")
#lines(c(9,(n3-1))-0.5,c(d3f,d3f),lty=1,lwd=2.5,col=1)
# legend("topright",inset=0.02,c("Recruits","1984-2018 mean"),lwd=c(2.5,2.5),lty=c(1,2),col=c(1,2))
#text(0.4*xx,yx*up,"Model 22.1",cex=1.5)
#axis(2,at=yat,labels=yat,outer=T,cex=1.5)
#box()
#yx = 75.0
#yat<-c(30,60,90,120,150)
plot(year,d2m[1:n3],axes=FALSE,ylim=c(ym,yx),xlim=c(xm,xx),type="n",lwd=1)
par(new=T,xaxs="i",yaxs="i")
barplot(c(0,0,0,0,0,0,0,0,0,0,d2m[1:n4]),space=0,density=-1,col="red",axes=FALSE,ylim=c(ym,yx),xlim=c(xm,xx),names.arg=" ")
lines(c(9,(n3-1))-0.5,c(d2f,d2f),lty=1,lwd=2.5,col=1)
# legend("topright",inset=0.02,c("Recruits","1984-2018 mean"),lwd=c(2.5,2.5),lty=c(1,2),col=c(1,2))
text(0.4*xx,yx*up,"Model 22.0",cex=1.5)
axis(2,at=yat,labels=yat,outer=T,cex=1.5)
box()

plot(year,d3m[1:n3],axes=FALSE,ylim=c(ym,yx),xlim=c(xm,xx),type="n",lwd=1)
par(new=T,xaxs="i",yaxs="i")
barplot(c(0,0,0,0,0,0,0,0,0,0,d2m[1:n4]),space=0,density=-1,col="red",axes=FALSE,ylim=c(ym,yx),xlim=c(xm,xx),names.arg=" ")
lines(c(9,(n3-1))-0.5,c(d3f,d3f),lty=1,lwd=2.5,col=1)
# legend("topright",inset=0.02,c("Recruits","1984-2018 mean"),lwd=c(2.5,2.5),lty=c(1,2),col=c(1,2))
text(0.4*xx,yx*up,"Model 22.0a",cex=1.5)
axis(2,at=yat,labels=yat,outer=T,cex=1.5)
par(mgp=c(2.5,0.65,0))
axis(1,at=xat,labels=xatv,outer=T,cex=1.5)
box()
yt<-c('Estimated total recruits (million crab)')
mtext(yt,2,2.5,outer=T,cex=1.0)
mtext('Year',1,1.8,outer=T,cex=1.0)
par(mfrow=c(1,1))


## r-range -------
# recruitment length distributions
n1<-3
n2<-1
d1m<-B$rec_sdd[1,1:8] #B is model 21.1b
d3m<-B$rec_sdd[2,1:8]
d4m<-C$rec_sdd[1,1:8] # C is model 22.0
d5m<-C$rec_sdd[2,1:8]
d6m<-D$rec_sdd[1,1:8] # D is model 22.0a
d7m<-D$rec_sdd[2,1:8]
d1m<-c(0,d1m)
d3m<-c(0,d3m)
d4m<-c(0,d4m)
d5m<-c(0,d5m)
d6m<-c(0,d6m)
d7m<-c(0,d7m)

par(oma=c(3.50,4.0,1.0,1.0),tck=-0.01,xaxs="i",yaxs="i",font=1,lwd=0.75)
par(mgp=c(1.2,1.0,0),mar=c(0,0,0,0),mfcol=c(n1,n2),xpd=FALSE)
xat<-c(1:9)
xatv=c("62.5","67.5","72.5","77.5","82.5","87.5","92.5","97.5","102.5")
yat<-c(0.1,0.2,0.3,0.4)
yatv<-c("0.1","0.2","0.3","0.4")
xm<-1
xx<-9
ym<-0

yx<-0.45
plot(xat,d3m,axes=FALSE,ylim=c(ym,yx),xlim=c(xm,xx),type="n",lwd=1)
#  par(new=T,xaxs="i",yaxs="i")
lines(xat,d1m,lty=1,lwd=2.5,col=1)
lines(xat,d3m,lty=2,lwd=2.5,col=2)
legend("topright",inset=0.02,cex = 0.8, c("Model 21.1b: males","Model 21.1b: females"),lwd=c(2.5,2.5),lty=c(1,2),col=c(1,2))
axis(2,at=yat,labels=yatv,outer=T,cex=1.4)
box()
plot(xat,d5m,axes=FALSE,ylim=c(ym,yx),xlim=c(xm,xx),type="n",lwd=1)
lines(xat,d4m,lty=1,lwd=2.5,col=1)
lines(xat,d5m,lty=2,lwd=2.5,col=2)
legend("topright",inset=0.02,cex = 0.8, c("Model 22.0: males","Model 22.0: females"),lwd=c(2.5,2.5),lty=c(1,2),col=c(1,2))
axis(2,at=yat,labels=yatv,outer=T,cex=1.4)
par(mgp=c(3.0,0.75,0))
axis(1,at=xat,labels=xatv,outer=T,cex=1.5)
box()
plot(xat,d7m,axes=FALSE,ylim=c(ym,yx),xlim=c(xm,xx),type="n",lwd=1)
lines(xat,d6m,lty=1,lwd=2.5,col=1)
lines(xat,d7m,lty=2,lwd=2.5,col=2)
legend("topright",inset=0.02,cex = 0.8, c("Model 22.0a: males","Model 22.0a: females"),lwd=c(2.5,2.5),lty=c(1,2),col=c(1,2))
axis(2,at=yat,labels=yatv,outer=T,cex=1.4)
par(mgp=c(3.0,0.75,0))
axis(1,at=xat,labels=xatv,outer=T,cex=1.5)
box()
yt<-c('Recruitment length distribution (proportion)')
mtext(yt,2,2.8,outer=T,cex=1.0)
mtext('Length (mm)',1,2.10,outer=T,cex=1.0)
par(mfrow=c(1,1))


## rkf -------------
# Figure 13a
plot.new()
d11<-B$ssb/1000.0 # model 21.1b 2022
dtot<-B$ft
b35 <- B$spr_bmsy/1000.0
f35<- B$sd_fmsy[1]
k <- 0
n<-length(d11)
d1<-d11[1:n]
d2<-dtot[(1:n),3]
d3<-cbind(c(0,0.25*b35, 0.25*b35, b35, 100), c(0,0,0.15*f35/0.9,f35,f35))
par(oma=c(3.5,3.5,1.0,1.0),tck=-0.01,xaxs="i",yaxs="i",font=1,lwd=0.75)
par(mgp=c(1.0,0.5,0),mar=c(0,0,0,0),xpd=FALSE)
tem<-c('75','76','77','78','79','80','81','82','83','84','85','86','87','88','89','90','91','92','93','94','95','96','97','98','99','00','01','02','03','04','05','06','07',
       '08','09','10','11','12','13','14','15','16','17','18','19','20', ' ')
tem1<-c('Mature male biomass (1000 t) on Feb. 15')
plot(d3[,1],d3[,2],xlim=c(0,125),ylim=c(-0.03,0.8),xlab=tem1,ylab='Fishing mortality at fishing time',axes=FALSE,type="n",las=1,cex=1.0)
text(d1,d2,tem,cex=0.9)
text(d1[n],d2[n],'21',cex=1.2,font=2, col=2)
lines(d3[,1],d3[,2],lty=1,lwd=2.5)
text(107,f35,'F35%',cex=1.0)
#text(105,0.25,'F40%',cex=1.0)
xat<-c(0,15,30,45,60,75,90,105,120)
yat<-c(0.00,0.1,0.20,0.3,0.4,0.5,0.60,0.7,0.8,0.9,1.0,1.1,1.2, 1.3,1.4,1.5)
yln<-c('0.0','0.1','0.2','0.3','0.4','0.5','0.6','0.7','0.8','0.9','1.0','1.1','1.2','1.3','1.4','1.5')
axis(2,at=yat,labels=yln,outer=T,cex=1.0)
par(mgp=c(2.9,0.4,0))
axis(1,at=xat,labels=xat,outer=T,cex=1.0)
box()
mtext(tem1,1,1.7,outer=T,cex=1.0)
mtext('Fishing mortality at fishing time',2,2.0,outer=T,cex=1.0)

### rkf85 ---------
# Figure 13b
d11<-D$ssb/1000.0 # model 22.0a 2022
dtot<-D$ft
b35 <- D$spr_bmsy/1000.0
f35<- D$sd_fmsy[1]
k <- 0
n<-length(d11)
d1<-d11[1:n]
d2<-dtot[(1:n),3]
d3<-cbind(c(0,0.25*b35, 0.25*b35, b35, 100), c(0,0,0.15*f35/0.9,f35,f35))
par(oma=c(3.5,3.5,1.0,1.0),tck=-0.01,xaxs="i",yaxs="i",font=1,lwd=0.75)
par(mgp=c(1.0,0.5,0),mar=c(0,0,0,0),xpd=FALSE)
tem<-c('85','86','87','88','89','90','91','92','93','94','95','96','97','98','99','00','01','02','03','04','05','06','07','08','09','10','11',
       '12','13','14','15','16','17','18','19','20',' ') #update a year here
tem1<-c('Mature male biomass (1000 t) on Feb. 15')
plot(d3[,1],d3[,2],xlim=c(0,41),ylim=c(-0.03,0.7),xlab=tem1,ylab='Fishing mortality at fishing time',axes=FALSE,type="n",las=1,cex=1.0)
text(d1,d2,tem,cex=0.9)
text(d1[n],d2[n],'21',cex=1.2,font=2, col=2) # update year to highlight here - last years's fishery
lines(d3[,1],d3[,2],lty=1,lwd=2.5)
text(107,f35,'F35%',cex=1.0)
#text(105,0.25,'F40%',cex=1.0)
xat<-c(0,5,10,15,20,25,30,35,40)
yat<-c(0.00,0.1,0.20,0.3,0.4,0.5,0.60,0.7,0.8,0.9,1.0,1.1,1.2, 1.3,1.4,1.5)
yln<-c('0.0','0.1','0.2','0.3','0.4','0.5','0.6','0.7','0.8','0.9','1.0','1.1','1.2','1.3','1.4','1.5')
axis(2,at=yat,labels=yln,outer=T,cex=1.0)
par(mgp=c(2.9,0.4,0))
axis(1,at=xat,labels=xat,outer=T,cex=1.0)
box()
mtext(tem1,1,1.7,outer=T,cex=1.0)
mtext('Fishing mortality at fishing time',2,2.0,outer=T,cex=1.0)

# Figure 13c
d11<-C$ssb/1000.0 # model 22.0 2022
dtot<-C$ft
b35 <- C$spr_bmsy/1000.0
f35<- C$sd_fmsy[1]
k <- 0
n<-length(d11)
d1<-d11[1:n]
d2<-dtot[(1:n),3]
d3<-cbind(c(0,0.25*b35, 0.25*b35, b35, 100), c(0,0,0.15*f35/0.9,f35,f35))
par(oma=c(3.5,3.5,1.0,1.0),tck=-0.01,xaxs="i",yaxs="i",font=1,lwd=0.75)
par(mgp=c(1.0,0.5,0),mar=c(0,0,0,0),xpd=FALSE)
tem<-c('85','86','87','88','89','90','91','92','93','94','95','96','97','98','99','00','01','02','03','04','05','06','07','08','09','10','11',
       '12','13','14','15','16','17','18','19','20',' ') #update a year here
tem1<-c('Mature male biomass (1000 t) on Feb. 15')
plot(d3[,1],d3[,2],xlim=c(0,41),ylim=c(-0.03,0.7),xlab=tem1,ylab='Fishing mortality at fishing time',axes=FALSE,type="n",las=1,cex=1.0)
text(d1,d2,tem,cex=0.9)
text(d1[n],d2[n],'21',cex=1.2,font=2, col=2) # update year to highlight here - last years's fishery
lines(d3[,1],d3[,2],lty=1,lwd=2.5)
text(107,f35,'F35%',cex=1.0)
#text(105,0.25,'F40%',cex=1.0)
xat<-c(0,5,10,15,20,25,30,35,40)
yat<-c(0.00,0.1,0.20,0.3,0.4,0.5,0.60,0.7,0.8,0.9,1.0,1.1,1.2, 1.3,1.4,1.5)
yln<-c('0.0','0.1','0.2','0.3','0.4','0.5','0.6','0.7','0.8','0.9','1.0','1.1','1.2','1.3','1.4','1.5')
axis(2,at=yat,labels=yln,outer=T,cex=1.0)
par(mgp=c(2.9,0.4,0))
axis(1,at=xat,labels=xat,outer=T,cex=1.0)
box()
mtext(tem1,1,1.7,outer=T,cex=1.0)
mtext('Fishing mortality at fishing time',2,2.0,outer=T,cex=1.0)

#### mortality  -----
n1<-2
n2<-1
n3<-B$nyr-B$syr+1 # model 21.1b
d1m<-B$M[1:n3,1]
d3m<-B$M[(n3+1):(n3+n3),1]
d3f0<-B$ft

n4<-C$nyr-C$syr+1 # model 22.0
d4m<-C$M[1:n4,1]
d5m<-C$M[(n4+1):(n4+n4),1]
d4f0<-C$ft

d8m<-D$M[1:n4,1] #model 22.0a
d9m<-D$M[(n4+1):(n4+n4),1]
d6f0<-D$ft
#d6m<-K$M[1:n3,1]
#d7m<-K$M[(n3+1):(n3+n3),1]
#d5f0<-K$ft
d3f<-c(1:n3)
d4f<-c(1:n4)
#d5f<-c(1:n4)
d6f<-c(1:n4)
d3f<-d3f0[(1:n3),3]
d4f<-d4f0[(1:n4),3]
#d5f<-d5f0[(1:n3),3]
d6f<-d6f0[(1:n4),3]
par(oma=c(3.50,4.0,1.0,1.0),tck=-0.01,xaxs="i",yaxs="i",font=1,lwd=0.75)
par(mgp=c(1.2,1.0,0),mar=c(0,0,0,0),mfcol=c(n1,n2),xpd=FALSE)
xat<-c(1975:(A$nyr))
yat<-c(0.1,0.3,0.5,0.7,0.9,1.1)
yatv<-c("0.1","0.3","0.5","0.7","0.9","1.1")
#yatv<-c(paste(t2),paste(t3))
xm<-B$syr
xx<-B$nyr
ym<-0
up<-0.40
year<-c(B$syr:(B$nyr))

yx<-1.3
#  plot(year,d2m[1:n3],axes=F,ylim=c(ym,yx),xlim=c(xm,xx),type="n",lwd=1)
plot(year,d3m[1:n3],axes=FALSE,ylim=c(ym,yx),xlim=c(xm,xx),type="n",lwd=1)
#  par(new=T,xaxs="i",yaxs="i")
lines(year,d1m[1:n3],lty=1,lwd=2.5,col=1)
lines(year,d3m[1:n3],lty=2,lwd=2.5,col=2)
lines(year[11:n3],d4m[1:n4],lty=3,lwd=2.5,col=3)
lines(year[11:n3],d5m[1:n4],lty=4,lwd=2.5,col=4)
lines(year[11:n3],d8m[1:n4],lty=5,lwd=2.5,col=5)
lines(year[11:n3],d9m[1:n4],lty=6,lwd=2.5,col=6)
#lines(year,d6m[1:n3],lty=7,lwd=2.5,col=7)
#lines(year,d7m[1:n3],lty=8,lwd=2.5,col=8)
legend("topright",inset=0.02,cex = 0.55, c("Model 21.1b: males","Model 21.1b: females","Model 22.0: males","Model 22.0: females","Model 22.0a: males","Model 22.0a: females"),lwd=c(2.5,2.5,2.5,2.5,2.5,2.5),lty=c(1,2,3,4,5,6),col=c(1,2,3,4,5,6))
text(1995,yx*up,"Natural mortality",cex=1.3)
axis(2,at=yat,labels=yatv,outer=T,cex=1.4)
box()
yx = 2.5
yat<-c(0.1,0.5,0.9,1.3,1.7,2.1)
yatv<-c("0.1","0.5","0.9","1.3","1.7","2.1")
plot(year,d3f[1:n3],axes=FALSE,ylim=c(ym,yx),xlim=c(xm,xx),type="n",lwd=1)
lines(year,d3f[1:n3],lty=1,lwd=2.5,col=1)
lines(year[11:n3],d4f[1:n4],lty=3,lwd=2.5,col=3)
lines(year[11:n3],d6f[1:n4],lty=5,lwd=2.5,col=5)
#lines(year,d5f[1:n3],lty=7,lwd=2.5,col=7)
legend("topright",inset=0.02,cex = 0.6, c("Model 21.1b","Model 22.0","Model 22.0a"),lwd=c(2.5,2.5,2.5),lty=c(1,3,5),col=c(1,3,5))
text(2000,yx*up,"Directed pot fishing mortality",cex=1.3)
axis(2,at=yat,labels=yatv,outer=T,cex=1.4)
par(mgp=c(3.0,0.75,0))
#  axis(1,at=xat,labels=xat,outer=T,cex=1.0,las=3)
axis(1,at=xat,labels=xat,outer=T,cex=1.5)
box()
yt<-c('Mortality')
mtext(yt,2,2.8,outer=T,cex=1.0)
mtext('Year',1,2.10,outer=T,cex=1.0)
par(mfrow=c(1,1))

## rksr -----
# Figure 14a
plot.new()
c1<-B$ssb/1000.0 # model 21.1b 2022
n<-length(c1)
c2<-(B$recruits[1,]+B$recruits[2,])/1000000
b35<- B$spr_bmsy/1000.0
nn<- n-6#40 # n -6 

par(oma=c(3.2,3.5,0.7,1.0),tck=-0.01,xaxs="i",yaxs="i",font=1,lwd=1.00)
par(mgp=c(1.0,0.5,0),mar=c(0,0,0,0),xpd=FALSE)
tem<-c(0,2,4,6,8,10,12,14,17,20,23,26,29,32,35,38,41,44,48,52,57,62,68,74,79,80)
tem1<-c('Mature male biomass on 2/15 (1000 t)')
tem2<-c('75','76','77','78','79','80','81','82','83','84','85','86','87','88','89','90','91','92','93','94','95','96','97','98','99','00','01','02','03','04','05','06','07','08','09','10','11','12','13','14','15')
plot(c1[1:nn],c2[6:(n-1)],xlim=c(0,125),ylim=c(-2.0,260),xlab=tem1,ylab='Total Recruits (millions)',axes=FALSE,type="n",las=1,cex=1.0)
text(c1[1:3],c2[6:8],tem2[1:3],cex=1.0)
text(c1[4:(n-6)],c2[9:(n-6)],tem2[4:(n-5)],cex=1.0, col=2)
#lines(tem,tem**2.1595*exp(-2.1297-0.0581*tem),lty=1,lwd=1.5,col=3)
#lines(tem,tem*exp(0.3010-0.0212*tem),lty=4,lwd=1.5,col=2)
lines(c(b35, b35),c(0.0,160),lty=2,lwd=3.0,col=1)
text(b35,168,'B35%',cex=1.0)
#text(52.882,205,'Bmsy?',cex=1.0)
#text(134.108,205,'Bmsy?',cex=1.0)
xat<-c(0,15,30,45,60,75,90,105,130,145,160,175,190,205,220,235)
yat<-c(0,20,40,60,80,100,120,140,160,180,200,220,240,260,280,300)
axis(2,at=yat,labels=yat,outer=T,cex=1.0)
par(mgp=c(2.9,0.4,0))
axis(1,at=xat,labels=xat,outer=T,cex=1.0)
box()
mtext(tem1,1,1.6,outer=T,cex=1.0)
mtext('Total Recruits (millions)',2,1.7,outer=T,cex=1.0)

#dev.off()
## rksrp ----------------
# Figure 14b
plot.new()
n<-length(B$ssb) # model 21.1b 2022
c1<-matrix(rep(0,(n-5-1)*2),ncol=2)
c1[,1]<-B$ssb[1:(n-5-1)]/1000.0
c1[,2]<-(B$recruits[1,(6:(n-1))]+B$recruits[2,(6:(n-1))])/1000000/c1[,1]
c2<-c1[4:(n-5-1),]
mmb<-c1[4:(n-5-1),1]
rmmb<-c1[4:(n-5-1),2]
c2<-data.frame(mmb,rmmb)
c3<-lm(rmmb~mmb,data=c2)
par(oma=c(3.2,3.5,0.7,1.0),tck=-0.01,xaxs="i",yaxs="i",font=1,lwd=1.00)
par(mgp=c(1.0,0.5,0),mar=c(0,0,0,0),xpd=FALSE)
tem<-c(0,2,4,6,8,10,12,14,17,20,23,26,29,32,35,38,41,44,48,52,57,62,68,74,79,80)
tem1<-c('Mature male biomass on 2/15 (1000 t)')
tem2<-c('75','76','77','78','79','80','81','82','83','84','85','86','87','88','89','90','91','92','93','94','95','96','97','98','99','00','01','02','03','04','05','06','07','08','09','10','11','12','13','14','15')
plot(c1[,1],c1[,2],xlim=c(0,130),ylim=c(-1.0,4.2),xlab=tem1,ylab=' ',axes=FALSE,type="n",las=1,cex=1.0)
text(c1[1:3,1],c1[1:3,2],tem2[1:3],cex=1.0,col=1)
text(c2[,1],c2[,2],tem2[4:(n-5-1)],cex=1.0,col=2)
abline(c3,lty=1,lwd=3.0,col=2)
#lines(tem,tem**2.1595*exp(-2.1297-0.0581*tem),lty=1,lwd=1.5,col=3)
#lines(tem,tem*exp(0.3010-0.0212*tem),lty=4,lwd=1.5,col=2)
# lines(c(5.0,98.0),c(-0.579509,-0.889338),lty=1,lwd=3.0,col=3)
xat<-c(0,10,20,30,40,50,60,70,80,90,100,110,120)
yat<-c('-0.5','0.0','0.5','1.0','1.5','2.0','2.5','3.0','3.5','4.0')
yatt<-c(-0.5,0.0,0.5,1.0,1.5,2.0,2.5,3,3.5,4)
axis(2,at=yatt,labels=yat,outer=T,cex=1.0)
par(mgp=c(2.9,0.4,0))
axis(1,at=xat,labels=xat,outer=T,cex=1.0)
box()
mtext(tem1,1,1.6,outer=T,cex=1.0)
mtext('Log(Total Recruits/MMB)',2,1.7,outer=T,cex=1.0)

## c-direct -----
n1<-3
n2<-1
n3<-B$nyr-B$syr + 1 # base model 21.1b
n4<-B$nyr-1990 + 1
n5<-C$nyr-C$syr + 1
d1d<-B$pre_catch[1,]/1000.0
d1m<-B$pre_catch[2,]/1000.0
d1f<-B$pre_catch[3,]/1000.0
d3d<-C$pre_catch[1,]/1000.0
d3m<-C$pre_catch[2,]/1000.0
d3f<-C$pre_catch[3,]/1000.0
d4d<-D$pre_catch[1,]/1000.0
d4m<-C$pre_catch[2,]/1000.0
d4f<-C$pre_catch[3,]/1000.0
#d5d<-G$pre_catch[1,]/1000.0
#d5m<-G$pre_catch[2,]/1000.0
#d5f<-G$pre_catch[3,]/1000.0
#d6d<-J$pre_catch[1,]/1000.0
#d6m<-J$pre_catch[2,]/1000.0
#d6f<-J$pre_catch[3,]/1000.0
#d7d<-K$pre_catch[1,]/1000.0
#d7m<-K$pre_catch[2,]/1000.0
#d7f<-K$pre_catch[3,]/1000.0
d2d<-B$obs_catch[1,]/1000.0
d2m<-B$obs_catch[2,]/1000.0
d2f<-B$obs_catch[3,]/1000.0
par(oma=c(3.50,4.0,1.0,1.0),tck=-0.01,xaxs="i",yaxs="i",font=1,lwd=1.0)
par(mgp=c(1.2,1.0,0),mar=c(0,0,0,0),mfcol=c(n1,n2),xpd=FALSE)
nc<-ncol(d1m)
nr<-nrow(d1m)
xat<-c(1975,1980,1985,1990,1995,2000,2005,2010,2015,2020,2025)
yat<-c(10,25,40,55)
#yatv<-c(paste(t2),paste(t3))
xm<-B$syr-1
xx<-B$nyr+1
ym<-0
up<-0.87
year<-c(B$syr:B$nyr)

yx<-61.0
#  plot(xat,d2f[i,1:nc],axes=F,ylim=c(ym,yx),xlim=c(xm,xx),type="n",lwd=1)
plot(year,d2d[1:n3],axes=FALSE,ylim=c(ym,yx),xlim=c(xm,xx),lwd=1.5, pch=19,cex=2)
#  par(new=T,xaxs="i",yaxs="i")
lines(year,d1d[1:n3],lty=1,lwd=2.5,col=1)
lines(year[11:n3],d3d[1:n5],lty=2,lwd=2.5,col=2)
lines(year[11:n3],d4d[1:n5],lty=3,lwd=2.5,col=3)
#lines(year[11:n3],d5d[1:n5],lty=4,lwd=2.5,col=4)
#lines(year,d6d[1:n3],lty=5,lwd=2.5,col=5)
#lines(year,d7d[1:n3],lty=6,lwd=2.5,col=6)
legend("topright",inset=0.02,c("Model 21.1b","Model 22.0","Model 22.0a"),lwd=c(2.5,2.5,2.5),lty=c(1,2,3),col=c(1,2,3),cex=c(0.8,0.8,0.8))
#   legend("topright",inset=0.02,c("19.3d","19.3e","19.3g","21.0"),lwd=c(2.5,2.5,2.5,2.5),lty=c(1,2,3,4),col=c(1,2,3,4))
# legend("topright",inset=0.02,c("19.3","19.3d"),lwd=c(2.5,2.5),lty=c(1,2),col=c(1,2))
text(0.99*xx,yx*up,"Pot retained catch",cex=1.5)
axis(2,at=yat,labels=yat,outer=T,cex=1.5)
box()
year1<-c(1990:(B$nyr))
yx <- 61.0
#  yat<-c(10,25,40,55,70)
#   yatv<-c("1.0","2.5","4.0","5.5")
plot(year1,d2m[1:n4],axes=FALSE,ylim=c(ym,yx),xlim=c(xm,xx),lwd=1.5, pch=19,cex=2)
#   par(new=T,xaxs="i",yaxs="i")
lines(year1,d1m[1:n4],lty=1,lwd=2.5,col=1)
lines(year1,d3m[1:n4],lty=2,lwd=2.5,col=2)
lines(year1,d4m[1:n4],lty=3,lwd=2.5,col=3)
#lines(year1,d5m[1:n4],lty=4,lwd=2.5,col=4)
#lines(year1,d6m[1:n4],lty=5,lwd=2.5,col=5)
#lines(year1,d7m[1:n4],lty=6,lwd=2.5,col=6)
legend("topright",inset=0.02,c("Model 21.1b","Model 22.0","Model 22.0a"),lwd=c(2.5,2.5,2.5),lty=c(1,2,3),col=c(1,2,3),cex=c(0.8,0.8,0.8))   
#  legend("topright",inset=0.02,c("19.3d","19.3e","19.3g","21.0"),lwd=c(2.5,2.5,2.5,2.5),lty=c(1,2,3,4),col=c(1,2,3,4))
#  legend("topright",inset=0.02,c("19.0a","19.4","19.4a"),lwd=c(2.5,2.5,2.5),lty=c(1,2,3),col=c(1,2,3))
# legend("topright",inset=0.02,c("19.3","19.3d"),lwd=c(2.5,2.5),lty=c(1,2),col=c(1,2))
text(0.99*xx,yx*up,"Pot total male",cex=1.5)
axis(2,at=yat,labels=yat,outer=T,cex=1.5)
box()
yx <- 4.2
yat<-c(1,2,3,4)
#  yatv<-c("0.5","1.0","1.5","2.0","2.5","3.0")
plot(year1,d2f[1:n4],axes=FALSE,ylim=c(ym,yx),xlim=c(xm,xx),lwd=1.5, pch=19,cex=2)
#  par(new=T,xaxs="i",yaxs="i")
lines(year1,d1f[1:n4],lty=1,lwd=2.5,col=1)
lines(year1,d3f[1:n4],lty=2,lwd=2.5,col=2)
lines(year1,d4f[1:n4],lty=3,lwd=2.5,col=3)
#lines(year1,d5f[1:n4],lty=4,lwd=2.5,col=4)
#lines(year1,d6f[1:n4],lty=5,lwd=2.5,col=5)
#lines(year1,d7f[1:n4],lty=6,lwd=2.5,col=6)
legend("topright",inset=0.02,c("Model 21.1b","Model 22.0","Model 22.0a"),lwd=c(2.5,2.5,2.5),lty=c(1,2,3),col=c(1,2,3),cex=c(0.8,0.8,0.8)) 
#  legend("topright",inset=0.02,c("19.3d","19.3e","19.3g","21.0"),lwd=c(2.5,2.5,2.5,2.5),lty=c(1,2,3,4),col=c(1,2,3,4))
#   legend("topright",inset=0.02,c("19.0a","19.4","19.4a"),lwd=c(2.5,2.5,2.5),lty=c(1,2,3),col=c(1,2,3))
# legend("topright",inset=0.02,c("19.3","19.3d"),lwd=c(2.5,2.5),lty=c(1,2),col=c(1,2))
text(0.99*xx,yx*up,"Pot disc. female",cex=1.5)
axis(2,at=yat,labels=yat,outer=T,cex=1.5)
par(mgp=c(3.0,0.75,0))
#  axis(1,at=xat,labels=xat,outer=T,cex=1.0,las=3)
axis(1,at=xat,labels=xat,outer=T,cex=1.5)
box()
yt<-c('Directed fishery retained, total and discarded catch biomass (1000 t)')
mtext(yt,2,2.8,outer=T,cex=1.0)
mtext('Year',1,2.10,outer=T,cex=1.0)
par(mfrow=c(1,1))

### c-discard -----
n1<-3
n2<-1
n3<-B$nyr-B$syr + 1
n4<-B$nyr-1996 + 1
n5<-C$nyr-C$syr + 1
d1d<-B$pre_catch[4,]/1000.0
d1m<-B$pre_catch[7,]/1000.0
d1f0<-(B$pre_catch[5,]/1000.0+B$pre_catch[6,]/1000.0)
d1f<-c(d1f0[1:14],0,0,d1f0[15:18],0,0,0,0,0,0,0,0,0,0,0,d1f0[19:22],0,0,0,d1f0[23:25],0,0,0,0,0,0)
d3d<-C$pre_catch[4,]/1000.0
d3m<-C$pre_catch[7,]/1000.0
d3f0<-(C$pre_catch[5,]/1000.0+C$pre_catch[6,]/1000.0)
d3f<-c(d3f0[1:4],0,0,d3f0[5:8],0,0,0,0,0,0,0,0,0,0,0,d3f0[9:12],0,0,0,d3f0[13:15],0,0,0,0,0,0)
d4d<-D$pre_catch[4,]/1000.0
d4m<-D$pre_catch[7,]/1000.0
d4f0<-(D$pre_catch[5,]/1000.0+D$pre_catch[6,]/1000.0)
d4f<-c(d4f0[1:4],0,0,d4f0[5:8],0,0,0,0,0,0,0,0,0,0,0,d4f0[9:12],0,0,0,d4f0[13:15],0,0,0,0,0,0)
#d5d<-G$pre_catch[4,]/1000.0
#d5m<-G$pre_catch[7,]/1000.0
#d5f0<-(G$pre_catch[5,]/1000.0+G$pre_catch[6,]/1000.0)
#d5f<-c(d5f0[1:4],0,0,d5f0[5:8],0,0,0,0,0,0,0,0,0,0,0,d5f0[9:12],0,0,0,d5f0[13:15],0,0,0,0)
#d6d<-J$pre_catch[4,]/1000.0
#d6m<-J$pre_catch[7,]/1000.0
#d6f0<-(J$pre_catch[5,]/1000.0+J$pre_catch[6,]/1000.0)
#d6f<-c(d6f0[1:14],0,0,d6f0[15:18],0,0,0,0,0,0,0,0,0,0,0,d6f0[19:22],0,0,0,d6f0[23:25],0,0,0,0)
#d7d<-K$pre_catch[4,]/1000.0
#d7m<-K$pre_catch[7,]/1000.0
#d7f0<-(K$pre_catch[5,]/1000.0+K$pre_catch[6,]/1000.0)
#d7f<-c(d7f0[1:14],0,0,d7f0[15:18],0,0,0,0,0,0,0,0,0,0,0,d7f0[19:22],0,0,0,d7f0[23:25],0,0,0,0)
d2d<-B$obs_catch[4,]/1000.0
d2m<-B$obs_catch[7,]/1000.0
d2f<-(B$obs_catch[5,]/1000.0+B$obs_catch[6,]/1000.0)
d2f<-c(d2f[1:14],0,0,d2f[15:18],0,0,0,0,0,0,0,0,0,0,0,d2f[19:22],0,0,0,d2f[23:25],0,0,0,0)
par(oma=c(3.50,4.0,1.0,1.0),tck=-0.01,xaxs="i",yaxs="i",font=1,lwd=0.75)
par(mgp=c(1.2,1.0,0),mar=c(0,0,0,0),mfcol=c(n1,n2),xpd=FALSE)
xat<-c(1975,1980,1985,1990,1995,2000,2005,2010,2015,2020)
yat<-c(0.2,0.7,1.2,1.7)
yatv<-c("0.2","0.7","1.2","1.7")
xm<-B$syr-1
xx<-B$nyr + 1
ym<-0
up<-0.87
year<-c(B$syr:(B$nyr))
year2<-c((B$syr+1):(B$nyr))

yx<-1.8
plot(year,c(0,d2d[1:(n3-1)]),axes=FALSE,ylim=c(ym,yx),xlim=c(xm,xx),lwd=1.5, pch=19,cex=2)
#  par(new=T,xaxs="i",yaxs="i")
lines(year,c(0,d1d[1:(n3-1)]),lty=1,lwd=2.5,col=1)
lines(year[11:n3],d3d[1:n5],lty=2,lwd=2.5,col=2)
lines(year[11:n3],d4d[1:n5],lty=3,lwd=2.5,col=3)
#lines(year[11:n3],d5d[1:n5],lty=4,lwd=2.5,col=4)
#lines(year,c(0,d6d[1:(n3-1)]),lty=5,lwd=2.5,col=5)
#lines(year,c(0,d7d[1:(n3-1)]),lty=6,lwd=2.5,col=6)
legend("topright",inset=0.02,c("Model 21.1b","Model 22.0","Model 22.0a"),lwd=c(2.5,2.5,2.5),lty=c(1,2,3),col=c(1,2,3),cex=c(0.8,0.8,0.8))   
# legend("topright",inset=0.02,c("19.3d","19.3e","19.3g","21.0"),lwd=c(2.5,2.5,2.5,2.5),lty=c(1,2,3,4),col=c(1,2,3,4))
# legend("topright",inset=0.02,c("19.0a","19.4","19.4a"),lwd=c(2.5,2.5,2.5),lty=c(1,2,3),col=c(1,2,3))
# legend("topright",inset=0.02,c("19.3","19.3d"),lwd=c(2.5,2.5),lty=c(1,2),col=c(1,2))
text(0.99*xx,yx*up,"GF trawl bycatch",cex=1.5)
axis(2,at=yat,labels=yat,outer=T,cex=1.5)
box()
year1<-c(1996:(B$nyr))
yx = 0.5
yat<-c(0.1,0.2,0.3,0.4)
yatv<-c("0.1","0.2","0.3","0.4")
plot(year1,d2m[1:n4],axes=FALSE,ylim=c(ym,yx),xlim=c(xm,xx),lwd=1.5, pch=19,cex=2)
#   par(new=T,xaxs="i",yaxs="i")
lines(year1,d1m[1:n4],lty=1,lwd=2.5,col=1)
lines(year1,d3m[1:n4],lty=2,lwd=2.5,col=2)
lines(year1,d4m[1:n4],lty=3,lwd=2.5,col=3)
#lines(year1,d5m[1:n4],lty=4,lwd=2.5,col=4)
#lines(year1,d6m[1:n4],lty=5,lwd=2.5,col=5)
#lines(year1,d7m[1:n4],lty=6,lwd=2.5,col=6)
legend("topleft",inset=0.02,c("Model 21.1b","Model 22.0","Model 22.0a"),lwd=c(2.5,2.5,2.5),lty=c(1,2,3),col=c(1,2,3),cex=c(0.8,0.8,0.8))    
# legend("topright",inset=0.02,c("19.3d","19.3e","19.3g","21.0"),lwd=c(2.5,2.5,2.5,2.5),lty=c(1,2,3,4),col=c(1,2,3,4))
#  legend("topright",inset=0.02,c("19.0a","19.4","19.4a"),lwd=c(2.5,2.5,2.5),lty=c(1,2,3),col=c(1,2,3))
# legend("topright",inset=0.02,c("19.3","19.3d"),lwd=c(2.5,2.5),lty=c(1,2),col=c(1,2))
text(0.99*xx,yx*up,"GF fixed gear bycatch",cex=1.5)
axis(2,at=yat,labels=yatv,outer=T,cex=1.5)
box()
#  year1<-c(1991:(A$nyr))
year2 <-c(1991:(B$nyr))
#  n3 <-27
yx = 9.0
yat<-c(2.0,4.0,6.0,8.0)
yatv<-c("2.0","4.0","6.0","8.0")
plot(year2,d2f[17:n3],axes=FALSE,ylim=c(ym,yx),xlim=c(xm,xx),lwd=1.5, pch=19,cex=2)
#  plot(year[16:n3],d2f[16:n3],axes=FALSE,ylim=c(ym,yx),xlim=c(xm,xx),lwd=1.5, pch=19,cex=2)
#  par(new=T,xaxs="i",yaxs="i")
lines(year,d1f[1:n3],lty=1,lwd=2.5,col=1)
lines(year[11:n3],d3f[1:n5],lty=2,lwd=2.5,col=2)
lines(year[11:n3],d4f[1:n5],lty=3,lwd=2.5,col=3)
#lines(year[11:n3],d5f[1:n5],lty=4,lwd=2.5,col=4)
#lines(year,d6f[1:n3],lty=5,lwd=2.5,col=5)
#lines(year,d7f[1:n3],lty=6,lwd=2.5,col=6)
legend("topright",inset=0.02,c("Model 21.1b","Model 22.0","Model 22.0a"),lwd=c(2.5,2.5,2.5),lty=c(1,2,3),col=c(1,2,3),cex=c(0.8,0.8,0.8))    
# legend("topright",inset=0.02,c("19.3d","19.3e","19.3g","21.0"),lwd=c(2.5,2.5,2.5,2.5),lty=c(1,2,3,4),col=c(1,2,3,4))
#  legend("topright",inset=0.02,c("19.0a","19.4","19.4a"),lwd=c(2.5,2.5,2.5),lty=c(1,2,3),col=c(1,2,3))
# legend("topright",inset=0.02,c("19.3","19.3d"),lwd=c(2.5,2.5),lty=c(1,2),col=c(1,2))
text(0.99*xx,yx*up,"Tanner fishery bycatch",cex=1.5)
axis(2,at=yat,labels=yatv,outer=T,cex=1.5)
par(mgp=c(3.0,0.75,0))
#  axis(1,at=xat,labels=xat,outer=T,cex=1.0,las=3)
axis(1,at=xat,labels=xat,outer=T,cex=1.5)
box()
yt<-c('Trawl, fixed gear and Tanner fisheries discarded catch biomass (1000 t)')
mtext(yt,2,2.8,outer=T,cex=1.0)
mtext('Year',1,2.10,outer=T,cex=1.0)
par(mfrow=c(1,1))

### length comps -----
#lfplot1<-function(t1,t2,t3,n1,n2,d1,d2) {
t1<-0
t2<-0.05
t3<-0.10
n1<-12
n2<-4
d1<-B$d3_pre_size_comps_7[,1:20] # model 21.1b 
d2<-B$d3_obs_size_comps_7[,1:20]
d3<-C$d3_pre_size_comps_7[,1:20]
d4<-D$d3_pre_size_comps_7[,1:20]
#d5<-J$d3_pre_size_comps_7[,1:20]
par(oma=c(4.40,4.0,1.0,1.0),tck=-0.01,xaxs="i",yaxs="i",font=1,lwd=0.75)
par(mgp=c(1.5,0.4,0),mar=c(0,0,0,0),mfcol=c(n1,n2),xpd=FALSE)
yat<-c(t2,t3)
yatv<-c(paste(t2),paste(t3))
nc<-ncol(d1)
nr<-nrow(d1)
xat<-c(1:nc)-0.5
#xatv<-c('67.5','72.5','77.5','82.5','87.5','92.5','97.5','102.5','107.5','112.5','117.5','122.5', '127.5','132.5','137.5','142.5','147.5','152.5','157.5','162.5+')
xatv<-c('67.5','','77.5','','87.5','','97.5','','107.5','','117.5','', '127.5','','137.5','','147.5','','157.5','')
xm<-0.0
xx<-nc
ym<-0.0
up<-0.87
year<-c(c(1975:2019),2021,2022)
yx<-0.12
for (i in 1:(n1-1))
{
  j <- i-10
  plot(xat,d1[i,1:nc],axes=FALSE,ylim=c(ym,yx),xlim=c(xm,xx),type="n",lwd=1)
  par(new=T,xaxs="i",yaxs="i")
  barplot(d2[i,1:nc],space=0,density=-1,col="white",ylim=c(ym,yx),xlim=c(xm,xx),axes=FALSE,names.arg=" ")
  lines(xat,d1[i,1:nc],lty=1,lwd=1.5)
  if (i>10) 
  {
    lines(xat,d3[j,1:nc],lty=2,lwd=1.5,col=2)
    lines(xat,d4[j,1:nc],lty=3,lwd=1.5,col=3)
  } 
  #lines(xat,d5[i,1:nc],lty=4,lwd=1.5,col=4)
  text(0.8*xx,yx*up,paste(year[i]),cex=1.0)
  axis(2,at=yat,labels=yatv,outer=T,cex=0.7,las=2)
  box()
  i1<-i
}
plot(xat,d1[i1+1,1:nc],axes=FALSE,ylim=c(ym,yx),xlim=c(xm,xx),type="n",lwd=1)
par(new=T,xaxs="i",yaxs="i")
barplot(d2[i1+1,1:nc],space=0,density=-1,col="white",ylim=c(ym,yx),xlim=c(xm,xx),axes=FALSE,names.arg=" ")
lines(xat,d1[i1+1,1:nc],lty=1,lwd=1.5)
lines(xat,d3[i1-9,1:nc],lty=2,lwd=1.5,col=2)
lines(xat,d4[i1-9,1:nc],lty=3,lwd=1.5,col=3)
#lines(xat,d5[i1+1,1:nc],lty=4,lwd=1.5,col=4)
text(0.8*xx,yx*up,paste(year[i1+1]),cex=1.0)
axis(2,at=yat,labels=yatv,outer=T,cex=0.7,las=2)

plot(xat,d1[i1+2,1:nc],axes=FALSE,ylim=c(ym,yx),xlim=c(xm,xx),type="n",lwd=1)
par(new=T,xaxs="i",yaxs="i")
barplot(d2[i1+2,1:nc],space=0,density=-1,col="white",ylim=c(ym,yx),xlim=c(xm,xx),axes=FALSE,names.arg=" ")
lines(xat,d1[i1+2,1:nc],lty=1,lwd=1.5)
lines(xat,d3[i1-8,1:nc],lty=2,lwd=1.5,col=2)
lines(xat,d4[i1-8,1:nc],lty=3,lwd=1.5,col=3)
#lines(xat,d5[i1+1,1:nc],lty=4,lwd=1.5,col=4)
text(0.8*xx,yx*up,paste(year[i1+2]),cex=1.0)
axis(2,at=yat,labels=yatv,outer=T,cex=0.7,las=2)

par(mgp=c(3.0,0.75,0))
axis(1,at=xat,labels=xatv,outer=T,cex=0.7,las=3)
box()
for (i in (n1+1):(n1+n1-1))
{
  j <- i-10
  plot(xat,d1[i,1:nc],axes=FALSE,ylim=c(ym,yx),xlim=c(xm,xx),type="n",lwd=1)
  par(new=T,xaxs="i",yaxs="i")
  barplot(d2[i,1:nc],space=0,density=-1,col="white",ylim=c(ym,yx),xlim=c(xm,xx),axes=FALSE,names.arg=" ")
  lines(xat,d1[i,1:nc],lty=1,lwd=1.5)
  lines(xat,d3[j,1:nc],lty=2,lwd=1.5,col=2)
  lines(xat,d4[j,1:nc],lty=3,lwd=1.5,col=3)
  #lines(xat,d5[i,1:nc],lty=4,lwd=1.5,col=4)
  text(0.8*xx,yx*up,paste(year[i]),cex=1.0)
  box()
  i1<-i
}
{
  plot(xat,d1[i1+1,1:nc],axes=FALSE,ylim=c(ym,yx),xlim=c(xm,xx),type="n",lwd=1)
  par(new=T,xaxs="i",yaxs="i")
  barplot(d2[i1+1,1:nc],space=0,density=-1,col="white",ylim=c(ym,yx),xlim=c(xm,xx),axes=FALSE,names.arg=" ")
  lines(xat,d1[i1+1,1:nc],lty=1,lwd=1.5)
  lines(xat,d3[i1-9,1:nc],lty=2,lwd=1.5,col=2)
  lines(xat,d4[i1-9,1:nc],lty=3,lwd=1.5,col=3)
  #lines(xat,d5[i1+1,1:nc],lty=4,lwd=1.5,col=4)
  text(0.8*xx,yx*up,paste(year[i1+1]),cex=1.0)
  axis(1,at=xat,labels=xatv,outer=T,cex=0.7,las=3)
  box()
}
for (i in (n1+n1+1):(n1+n1+n1-1))
{
  j <- i-10
  plot(xat,d1[i,1:nc],axes=FALSE,ylim=c(ym,yx),xlim=c(xm,xx),type="n",lwd=1)
  par(new=T,xaxs="i",yaxs="i")
  barplot(d2[i,1:nc],space=0,density=-1,col="white",ylim=c(ym,yx),xlim=c(xm,xx),axes=FALSE,names.arg=" ")
  lines(xat,d1[i,1:nc],lty=1,lwd=1.5)
  lines(xat,d3[j,1:nc],lty=2,lwd=1.5,col=2)
  lines(xat,d4[j,1:nc],lty=3,lwd=1.5,col=3)
  #lines(xat,d5[i,1:nc],lty=4,lwd=1.5,col=4)
  text(0.8*xx,yx*up,paste(year[i]),cex=1.0)
  box()
  i1<-i
}
plot(xat,d1[i1+1,1:nc],axes=FALSE,ylim=c(ym,yx),xlim=c(xm,xx),type="n",lwd=1)
par(new=T,xaxs="i",yaxs="i")
barplot(d2[i1+1,1:nc],space=0,density=-1,col="white",ylim=c(ym,yx),xlim=c(xm,xx),axes=FALSE,names.arg=" ")
lines(xat,d1[i1+1,1:nc],lty=1,lwd=1.5)
lines(xat,d3[i1-9,1:nc],lty=2,lwd=1.5,col=2)
lines(xat,d4[i1-9,1:nc],lty=3,lwd=1.5,col=3)
#lines(xat,d5[i1+1,1:nc],lty=4,lwd=1.5,col=4)
text(0.8*xx,yx*up,paste(year[i1+1]),cex=1.0)
axis(1,at=xat,labels=xatv,outer=T,cex=0.7,las=3)
box()
for (i in (n1+n1+n1+1):(n1+n1+n1+n1-3))
{
  j <- i-10 
  plot(xat,d1[i,1:nc],axes=FALSE,ylim=c(ym,yx),xlim=c(xm,xx),type="n",lwd=1)
  par(new=T,xaxs="i",yaxs="i")
  barplot(d2[i,1:nc],space=0,density=-1,col="white",ylim=c(ym,yx),xlim=c(xm,xx),axes=FALSE,names.arg=" ")
  lines(xat,d1[i,1:nc],lty=1,lwd=1.5)
  lines(xat,d3[j,1:nc],lty=2,lwd=1.5,col=2)
  lines(xat,d4[j,1:nc],lty=3,lwd=1.5,col=3)
  #lines(xat,d5[i,1:nc],lty=4,lwd=1.5,col=4)
  text(0.8*xx,yx*up,paste(year[i]),cex=1.0)
  box()
  i1<-i
}
plot(xat,d1[i1+1,1:nc],axes=FALSE,ylim=c(ym,yx),xlim=c(xm,xx),type="n",lwd=1)
par(new=T,xaxs="i",yaxs="i")
barplot(d2[i1+1,1:nc],space=0,density=-1,col="white",ylim=c(ym,yx),xlim=c(xm,xx),axes=FALSE,names.arg=" ")
lines(xat,d1[i1+1,1:nc],lty=1,lwd=1.5)
lines(xat,d3[i1-9,1:nc],lty=2,lwd=1.5,col=2)
lines(xat,d4[i1-9,1:nc],lty=3,lwd=1.5,col=3)
#lines(xat,d5[i1+1,1:nc],lty=4,lwd=1.5,col=4)
text(0.8*xx,yx*up,paste(year[i1+1]),cex=1.0)
legend("topleft",inset=0.02,c("21.1b","22.0","22.0a"),lwd=c(1.5,1.5,1.5),lty=c(1,2,3),col=c(1,2,3),cex=c(0.7,0.7,0.7))
# legend("topleft",inset=0.010,c("19.3d","19.3e","19.3g","21.0"),lwd=c(1.5,1.5,1.5,1.5),lty=c(1,2,3,4),col=c(1,2,3,4),cex=c(0.5,0.5,0.5,0.5))
#  legend("topleft",inset=0.010,c("19.3","19.3c","19.3d"),lwd=c(1.5,1.5,1.5),lty=c(1,2,3),col=c(1,2,3),cex=c(0.5,0.5,0.5))
axis(1,at=xat,labels=xatv,outer=T,cex=0.7,las=3)
box()
yt<-c('Length compositions of male red king crab')
mtext(yt,2,2.50,outer=T,cex=1.0)
mtext('Carapace length group (mm)',1,3.0,outer=T,cex=1.0)
par(mfrow=c(1,1))

### next ------
