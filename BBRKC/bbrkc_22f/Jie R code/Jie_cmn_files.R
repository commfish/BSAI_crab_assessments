## rec 3 ----------------
n1<-3
n2<-1
n3<-A$nyr-A$syr+1
n4<-B$nyr-B$syr+1
d1m<-(A$recruits[1,]+A$recruits[2,])/1000000
#d2m<-(B$recruits[1,]+B$recruits[2,])/1000000
#d3m<-(J$recruits[1,]+J$recruits[2,])/1000000
d1f<-mean(d1m[9:(n3-1)])
#d2f<-mean(d2m[1:(n4-1)])
#d3f<-mean(d3m[9:(n3-1)])

par(oma=c(3.50,4.0,1.0,1.0),tck=-0.01,xaxs="i",yaxs="i",font=1,lwd=0.75)
par(mgp=c(1.2,0.7,0),mar=c(0,0,0,0),mfcol=c(n1,n2),xpd=FALSE)
xat<-c(2,7,12,17,22,27,32,37,42,47)-0.5
xatv<-c("1977","1982","1987","1992","1997","2002","2007","2012","2017","2020")
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

plot(year,d3m[1:n3],axes=FALSE,ylim=c(ym,yx),xlim=c(xm,xx),type="n",lwd=1)
par(new=T,xaxs="i",yaxs="i")
barplot(d3m[1:n3],space=0,density=-1,col="red",axes=FALSE,ylim=c(ym,yx),xlim=c(xm,xx),names.arg=" ")
lines(c(9,(n3-1))-0.5,c(d3f,d3f),lty=1,lwd=2.5,col=1)
# legend("topright",inset=0.02,c("Recruits","1984-2018 mean"),lwd=c(2.5,2.5),lty=c(1,2),col=c(1,2))
text(0.4*xx,yx*up,"Model 22.1",cex=1.5)
axis(2,at=yat,labels=yat,outer=T,cex=1.5)
box()
#yx = 75.0
#yat<-c(30,60,90,120,150)
plot(year,d2m[1:n3],axes=FALSE,ylim=c(ym,yx),xlim=c(xm,xx),type="n",lwd=1)
par(new=T,xaxs="i",yaxs="i")
barplot(c(0,0,0,0,0,0,0,0,0,0,d2m[1:n4]),space=0,density=-1,col="red",axes=FALSE,ylim=c(ym,yx),xlim=c(xm,xx),names.arg=" ")
lines(c(9,(n3-1))-0.5,c(d2f,d2f),lty=1,lwd=2.5,col=1)
# legend("topright",inset=0.02,c("Recruits","1984-2018 mean"),lwd=c(2.5,2.5),lty=c(1,2),col=c(1,2))
text(0.4*xx,yx*up,"Model 22.0",cex=1.5)
axis(2,at=yat,labels=yat,outer=T,cex=1.5)
par(mgp=c(2.5,0.65,0))
axis(1,at=xat,labels=xatv,outer=T,cex=1.5)
box()
yt<-c('Estimated total recruits (million crab)')
mtext(yt,2,2.5,outer=T,cex=1.0)
mtext('Year',1,1.8,outer=T,cex=1.0)
par(mfrow=c(1,1))

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
