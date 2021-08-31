created 8-30-21
# bbrkc assist Jie with mcmc model runs for VAST run

- see gmacs.dat for data, crl, prj files and jitter options


# Run mcmc for projections
Test mcmc run

gmacs -mcmc 500000 -mcsave 1000 -nox

After completion, then run:
gmacs -mceval

There are five files generated from this run:
mcount.rep
mcountPROJ.rep
mcountREC.rep
mcountREF.rep
mcountSSB.rep



