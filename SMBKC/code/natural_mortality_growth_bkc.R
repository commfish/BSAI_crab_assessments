# determine range of natural mortality for STMATT bkc
# katie.palof@alaska.gov
# 4-14-22

# 
library(TropFishR)
?M_empirical

M_empirical(tmax = 10, method = c("Hoenig", "Then_tmax"))
# seems low and unrealistically high M - don't use 10
M_empirical(tmax = 20, method = c("Hoenig", "Then_tmax"))
#                                   M
#Hoenig (1983) - Joint Equation 0.223
#Hoenig (1983) - Fish Equation  0.209
#Then (2015) - tmax             0.315
M_empirical(tmax = 25, method = c("Hoenig", "Then_tmax"))
#Hoenig (1983) - Joint Equation 0.179
#Hoenig (1983) - Fish Equation  0.167
#Then (2015) - tmax             0.257
M_empirical(tmax = 30, method = c("Hoenig", "Then_tmax"))
#Hoenig (1983) - Joint Equation 0.150
#Hoenig (1983) - Fish Equation  0.139
#Then (2015) - tmax             0.217


# Run models using 0.26, and 0.21