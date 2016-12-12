## @knitr singleObjectVersion
rm(list=ls())

# Adjustable settings ##########################################################
times <- seq(from=0, to=2*365, by=1)                         # Times of interest
pars <- c(kWat=0.1, kSed=0.02, kDif=1e-9*86400, hDif=0.05,   # Parameters
  por=0.9, uSet=0.5, zWat=5, zSed=0.1, vol=10e6, s_x=1/106)
vars <- c(xWat=0, xSed=0, sWat=0, sSed=0)                    # Initial values
# End of settings ##############################################################

# Load packages
library("rodeo")
library("deSolve")

# Initialize rodeo object
rd <- function(f, ...) {read.table(file=paste0("singleObject/",f),
  sep="\t", header=TRUE, ...)}
model <- rodeo$new(
  vars=rd("vars.txt"), pars=rd("pars.txt"), funs=rd("funs.txt"), pros=rd("pros.txt"),
  stoi=as.matrix(rd("stoi.txt", row.names="process")), asMatrix=TRUE, dim=1)

# Assign initial values and parameters
model$setVars(vars)
model$setPars(pars)

# Generate code, compile into shared library, load library
model$compile("functions.f95")              

# Integrate
out <- ode(y=model$getVars(), times=times, func=model$libFunc(), parms=model$getPars(),
  dllname=model$libName(), nout=model$lenPros(), outnames=model$namesPros())

# Plot method for deSolve objects
plot(out)

