### Example script to run MCMC using R package adnuts for a stock
### synthesis assessment model. Started Feb 2021 by Cole Monnahan
### (cole.monnahan@noaa.gov | AFSC)

library(adnuts)
packageVersion('adnuts')        # 1.1.2
library(shinystan)

### ------------------------------------------------------------
### Task 0: Set up and test model for running. This requires
### pointing to a folder and executable. The folder needs to
### contain all sufficient input files and assumes optimization
### has occurred and produced all necessary outputs. Temporary
### copies will be made in the working directory during execution

## Define the path name
p <- "C:\\Research\\gmacs\\Unified\\build\\SMBKC\\"
## Assumes current working directory is where this R script is

### Optimize model from R. Want to optimize w/ -mcmc flag b/c of
### bias adjustment.
setwd(p);

system("gmacs -nox");

#AAA

system("gmacs -hbf -nox -mcmc 10"); 


# Run specs
print(Sys.time())
chains <- 5
thin <- 1
iter <- 1000*thin
Seeds <- c(190,205,1980,539,1201)
fit <- sample_nuts(model='lecta', path=p, iter=1000*5+200, warmup=200, thin=5,
                   chains=chains, seeds = Seeds,
                   control=list(adapt_delta=0.9,metric='mle'))

print(fit$cmd)
print(mean(fit$time.total)/60)
print(Sys.time())

## Good idea to save the output, I recommend RDS format.
cat("saving",paste0('fits.RDS'),"\n")
saveRDS(fit, file=paste0('fits.RDS'))

## Key information from run. Including the two recommended
## convergence diagnostics:
summary(fit)

## Interactive tools (must close out browser to regain console)
launch_shinyadmb(fit)

