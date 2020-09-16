# k.palof
# crab plan team 
# obj: box plot figure 

# load --
library(ggplot2)
library(viridis)
ofl7=read.csv("C:/Users/kjpalof/Documents/Current projects/crab plan team/2020/sept_2020/uncertainty/PropDiffOFL_crab.csv",header=TRUE) #see attached

ggplot(ofl7,aes(x=OFL,y=spp,fill=spp)) +
  geom_boxplot() + 
  geom_vline(xintercept=0) +
  scale_fill_viridis(name="",discrete=TRUE,alpha=0.75) +
  xlab("Proportional difference in OFL (no Survey - survey)/survey") +
  theme_bw() +
  theme(strip.text.x=element_text(size=12,color="black",face="bold.italic"),strip.background=element_rect(fill="white"))+
  theme(axis.title.x = element_text(size = 12),axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),axis.title.y = element_text(size = 12),
        legend.text=element_text(size=12),legend.position="none")

ggsave(("ofl_boxplot_terminalyearsurvey.png"), width = 1.15*6, height = 5)
