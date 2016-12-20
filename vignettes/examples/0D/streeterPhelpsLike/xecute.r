## @knitr streeterPhelpsLike
rm(list=ls())

# Adjustable settings ##########################################################
pars <- c(kd=1, ka=0.5, s=2.76, temp=20)  # parameters
vars <- c(OM=1, DO=9.02)                  # initial values
times <- seq(from=0, to=10, by=1/24)      # times of interest
# End of settings ##############################################################

# Load required packages
library("deSolve")
library("rodeo")

# Initialize rodeo object
rd <- function(f, ...) {read.table(file=f,
  header=TRUE, sep="\t", stringsAsFactors=FALSE, ...) }
model <- rodeo$new(vars=rd("vars.txt"), pars=rd("pars.txt"), funs=rd("funs.txt"),
  pros=rd("pros.txt"), stoi=as.matrix(rd("stoi.txt", row.names="process")),
  asMatrix=TRUE, dim=c(1))

# Assign initial values and parameters
model$setVars(vars)
model$setPars(pars)

# Implement required functions
DOsat <- function(t) {
  14.652 - 0.41022*t + 7.991e-3*(t**2) - 7.7774e-5*(t**3)
}

# Generate R code
model$compile(fortran=FALSE)

# Integrate
out <- model$dynamics(times=times, fortran=FALSE)

# Plot, using the method for objects of class deSolve
plot(out)   

