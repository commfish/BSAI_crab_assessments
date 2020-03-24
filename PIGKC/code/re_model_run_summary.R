# notes ----
## Run PIGKC Random Effects Model
## author: Tyler Jackson (model executable file -> Jim Ianelli)
## tyler.jackson@alaska.gov
## last updated: 3/23/2020

# load ----

## source mature male biomass estimation if needed
#source(nmfs_slope_biomass_est.R)

## global option
YEAR <- 2020

# run model ----

## copy model executable file to annual directory if needed
if(!("re.exe" %in% list.files(paste0("./PIGKC/model/", YEAR)))){
  file.copy(from = "./PIGKC/model/re.exe",
            to = paste0("./PIGKC/model/", YEAR, "/re.exe"))
}

## run model
setwd(paste0("./PIGKC/model/", YEAR))
system("./re.exe")
setwd("../../..")


# summarize results ----

To Be Continued....