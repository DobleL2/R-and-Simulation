
# Installing SIMRES package from github
install.packages('https://github.com/rubenfcasal/simres/releases/download/v0.1/simres_0.1.3.tar.gz', repos = NULL, type = "source")


# Vector with all the packages needed to create simulation models
pkgs <- c('tictoc', 'boot', 'randtoolbox', 'MASS', 'DEoptim', 'nortest', 'geoR', 'copula',
          'sm', 'car', 'tseries', 'forecast', 'plot3D', 'rgl', 'rngWELL', 'randtoolbox')

# Installing packages
install.packages(setdiff(pkgs, installed.packages()[,"Package"]), 
                 dependencies = TRUE)


