library(ggplot2);

d1<-C$d3_res_size_comps_7[,21:36]
ny<-16
nx<-B$nyr-B$syr+1
y <-c(67.5,72.5,77.5,82.5,87.5,92.5,97.5,102.5,107.5,112.5,117.5,122.5,127.5,132.5,137.5,142.5)
x<-c(B$syr:(B$nyr-1),(B$nyr+1))
tin<-"Model 22.0a, Survey Females"
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
    #     xlab("Year") +
    #     ylab("Carapace length (mm)") +
    #     guides(size=guide_legend(title=units,override.aes=list(alpha=1.0),order=1)) +
