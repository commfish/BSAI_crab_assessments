# from Jie's lba.cmn file 
# should help to convert GMACS to be similar to LBA output??? According to Jie's notes

W<-B        #A is the read-in rep file from a model. B is model 21.1b from 'Jie_cmn_files.R'
Q<- 0.96799679
#0.953749007847 last value Jie had
#Q<-0.960885171330     #model 21.1b, 2021.  #NMFS trawl survey catchability. Need to enter for each model from the par file.
#Q<- 0.963728075484     #model 22.1, 2021
#Q<-0.925114642139       #model 22.0c, 2021
#Q<-0.939295765650      #model 22.0d, 2021
#Q<-0.932190505921       #model 22.0e, 2021
ra<-c(1.0,1.2,1.4,1.6,1.8,2.1,2.4,2.7,3.0)   #mating ratios by size
wf<-c(0.56382,0.63418,0.70913,0.78874,0.87305,0.96211,1.05597,1.15469,1.25829,1.36684,1.48036)  #old weight by size for mature females
# wf<-c(0.62711,0.7216, 0.82452,0.93615,1.05678,1.18669,1.32613,1.47539,1.63473,1.80441,2.18315) #current weight	
n1<-15      #size 12 is length group 122.5 mm, male mature size
n2<-9       #size 6 is length group 92.5 mm, female mature size
if (W$syr == "1975" & W$selectivity[6,3] == 6) n4 = 17            #Terminal year of NMFS selectivity when starting in 1975
if (W$syr == "1975" & W$selectivity[6,3] == 1) n4 = 15
if (W$syr == "1985") n4 = 5                                       #Terminal year of NMFS selectivity when starting in 1985
n3<-W$nyr-W$syr+2
d1m<-W$N_males/1000000.0
d1f<-W$N_females/1000000.0
d2m<-d1m[n3,(n1-3):20]*W$selectivity[n4,n1:23]*Q                   #total mature male abundance by size
d2l<-sum(d1m[n3,n1:20]*W$selectivity[n4,(n1+3):23]*Q)      #total legal male abundance 
d2f<-d1f[n3,(n2-3):16]*W$selectivity[n4,n2:19]*Q                   #total mature female abundance by size
d3m<-sum(d2m)
d3f<-sum(d2f)
# male reproductive potential
tm<-sum(d2m*ra)
r<-tm/d3f
if (r>1.0) r <- 1.0
#effective spawning biomass
epb <- sum(d2f*wf*r)
t1<-epb*2.0/0.90718474
#print out model-survey abundances of matue males, legal males, mature females, epb(1000t), epb(millions of lbs), and ratio
print(c(d3m, d2l, d3f, epb, t1, r))
results <- c(d3m, d2l, d3f, epb, t1, r)
namesC <- c("MatureM", "LegalM", "MatureF", "ESB_1000t", "ESB_millionlbs", "ratio")
data.frame(namesC, results)
write.csv(data.frame(namesC, results), "./BBRKC/bbrkc_23f/doc/tables/lba_cmn.csv", row.names = FALSE)


# sum of mature females by year???----
fem2 <- d1f[ ,(n2-3):16]*W$selectivity[n4,n2:19]*Q  
glimpse(fem2)
            