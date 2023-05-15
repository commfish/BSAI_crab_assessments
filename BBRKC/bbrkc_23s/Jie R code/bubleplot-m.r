library(ggplot2);
# load function below 
.FIGS     = c("./BBRKC/bbrkc_23s/doc/figures/")
# function in bbrkc_functions.R
# need to run intro lines of "Jie_cmn_files.R' to get the letters for each model.

bubbleplot_resid(B, sex = "male", ltitle = "Model 21.1b(update), Survey Males")
ggsave(paste0(.FIGS, "length_resid_211b_males.png"), width = 7, height = 8)
bubbleplot_resid(B, sex = "female", ltitle = "Model 21.1b(update), Survey Females")
ggsave(paste0(.FIGS, "length_resid_211b_females.png"), width = 7, height = 8)

# 2023 spring models - see "Jie_cmn_files.R' for models 
# G model 22.0
bubbleplot_resid(G, sex = "male", ltitle = "Model 22.0, Survey Males")
ggsave(paste0(.FIGS, "length_resid_22_males.png"), width = 7, height = 8)
bubbleplot_resid(G, sex = "female", ltitle = "Model 22.0, Survey Females")
ggsave(paste0(.FIGS, "length_resid_22_females.png"), width = 7, height = 8)

# H model 23.3
bubbleplot_resid(H, sex = "male", ltitle = "Model 23.3, Survey Males")
ggsave(paste0(.FIGS, "length_resid_23_3_males.png"), width = 7, height = 8)
bubbleplot_resid(H, sex = "female", ltitle = "Model 23.3, Survey Females")
ggsave(paste0(.FIGS, "length_resid_23_3_females.png"), width = 7, height = 8)

#E  model 23.0b
bubbleplot_resid(E, sex = "male", ltitle = "Model 23.0b, Survey Males")
ggsave(paste0(.FIGS, "length_resid_23_0b_males.png"), width = 7, height = 8)
bubbleplot_resid(E, sex = "female", ltitle = "Model 23.0b, Survey Females")
ggsave(paste0(.FIGS, "length_resid_23_0b_females.png"), width = 7, height = 8)


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
    #     xlab("Year") +
    #     ylab("Carapace length (mm)") +
    #     guides(size=guide_legend(title=units,override.aes=list(alpha=1.0),order=1)) +




## old bubble plot without function ------------------
d1<-B$d3_res_size_comps_7[,1:20]
ny<-20
nx<-B$nyr-B$syr+1
y <-c(67.5,72.5,77.5,82.5,87.5,92.5,97.5,102.5,107.5,112.5,117.5,122.5,127.5,132.5,137.5,142.5,147.5,152.5,157.5,162.5)
x<-c(B$syr:(B$nyr-2),(B$nyr:(B$nyr+1)))
tin<-"Model 21.1b, Survey Males"
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