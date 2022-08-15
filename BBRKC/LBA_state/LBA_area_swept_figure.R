# k.palof
# Figure to compare area-swept from survey with model output from LBA model

# created 7-13-22


# load ----
library(tidyverse)
loadfonts(device="win")
windowsFonts(Times=windowsFont("TT Times New Roman"))

theme_set(theme_bw(base_size=12,base_family='Times New Roman')+ 
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank()))

# data -----
# read in area swept from lba data file "survey.dat" and surveyf.dat

#out <- read.table("./BBRKC/LBA/rk22/pop.out", header = FALSE, sep = "", nrows = 28)
                  #col.names = c(1972:2022))
#years <- as.character(c(1972:2022))
#out[nrow(out)+1, ] <- years

lba_out <- read.csv("./BBRKC/LBA_state/rk22/rk22_r_input.csv")
head(lba_out)

## male figure ------------
lba_out %>% 
  select(Year, survey.m, model.mm) %>% 
  gather(type, number, survey.m:model.mm) %>% 
  ggplot(aes(Year, number/1000, group = type)) +
    geom_point(aes(shape = type), size = 3) +
    geom_line(aes(group = type, linetype = type), lwd = 1) +
  scale_shape_manual(name = "", values = c(32,19), 
                     labels = c("Model", "Survey")) + 
  scale_linetype_manual(name = "", values = c("solid", "blank"), 
                        labels = c("Model", "Survey")) +
  scale_x_continuous(breaks = seq(min(1972),max(max(lba_out$Year) + 1), by = 2)) +
  ggtitle("Mature males") + 
  ylab("Millions of crab") +
  xlab("Year") +
  theme(legend.position = c(0.8,0.8), 
        axis.text = element_text(size = 12), 
        axis.title=element_text(size=14,face="bold"), 
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.title = element_text(hjust =0.5)) -> males 
ggsave('./BBRKC/LBA/rk22/mature_males.png', males, dpi = 800, width = 7.5, height = 5.5)
  


# female figure ----------
lba_out %>% 
  select(Year, survey.f, model.mf) %>% 
  gather(type, number, survey.f:model.mf) %>% 
  ggplot(aes(Year, number/1000, group = type)) +
  geom_point(aes(shape = type), size = 3) +
  geom_line(aes(group = type, linetype = type), lwd = 1) +
  scale_shape_manual(name = "", values = c(32,19), 
                     labels = c("Model", "Survey")) + 
  scale_linetype_manual(name = "", values = c("solid", "blank"), 
                        labels = c("Model", "Survey")) +
  scale_x_continuous(breaks = seq(min(1972),max(max(lba_out$Year) + 1), by = 2)) +
  ggtitle("Mature females") + 
  ylab("Millions of crab") +
  xlab("Year") +
  theme(legend.position = c(0.8,0.8), 
        axis.text = element_text(size = 12), 
        axis.title=element_text(size=14,face="bold"), 
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.title = element_text(hjust =0.5)) -> females 
  ggsave('./BBRKC/LBA/rk22/mature_females.png', females, dpi = 800, width = 7.5, height = 5.5)
