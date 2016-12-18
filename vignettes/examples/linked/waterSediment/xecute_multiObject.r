## @knitr multiObjectVersion
rm(list=ls())

# Adjustable settings ##########################################################
internal <- TRUE                       # Use internal solver instead of deSolve?
times <- seq(from=0, to=365*2, by=1)   # Times of interest
objects <- c("wat", "sed")             # Object names
pars <- list(                          # Fixed parameters
  wat= c(kWat=0.1, uSet=0.5, zWat=5, vol=10e6, s_x=1/106),
  sed= c(kSed=0.02, kDif=1e-9*86400, hDif=0.05, por=0.9, zSed=0.1, s_x=1/106)
)
vars <- list(                          # Initial values
  wat= c(xWat=0, sWat=0),
  sed= c(xSed=0, sSed=0)
)

# Parameters used for model coupling; these need to be initialized
pars$wat["flux_s"] <- 0
pars$sed["flux_x"] <- 0
pars$sed["sWat"] <- vars$w["sWat"]

# Definition of links between objects
# The value for a parameter in a target object (needs data) is provided by a
# source object (supplier). Supplied is either a state variable or process rate.
links <- rbind(
  link1= c(tarObj="wat", tarPar="flux_s", srcObj="sed", srcItem="diff"),
  link2= c(tarObj="sed", tarPar="flux_x", srcObj="wat", srcItem="sett"),
  link3= c(tarObj="sed", tarPar="sWat",   srcObj="wat", srcItem="sWat")
)
# End of settings ##############################################################

# Load packages
library("rodeo")
library("deSolve")

# Create list of rodeo objects
rd <- function(dir,f, ...) {read.table(file=paste0("multiObject/",obj,"_",f),
  sep="\t", header=TRUE, ...)}
models <- list()
for (obj in objects) {
  models[[obj]] <- rodeo$new(
    vars=rd(obj, "vars.txt"), pars=rd(obj, "pars.txt"),
    funs=rd(obj, "funs.txt"), pros=rd(obj, "pros.txt"),
    stoi=as.matrix(rd(obj, "stoi.txt", row.names="process")), asMatrix=TRUE,
    dim=1)
}

# Generate and load Fortran library for selected integrator
if (internal) {
  for (obj in objects)
    models[[obj]]$initStepper(fileFun="functions.f95", method="rk5")
} else {
  for (obj in objects) {
    models[[obj]]$compile("functions.f95")
  }
}

# Set initial parameters and initial values
invisible(lapply(objects, function(obj) {models[[obj]]$setVars(vars[[obj]])}))
invisible(lapply(objects, function(obj) {models[[obj]]$setPars(pars[[obj]])}))

# Function to update parameters of a particular object using the linkage table
# Inputs: 
#   objPar: Parameters of a particular target object (numeric vector)
#   src:    States and process rates of all objects (list of numeric vectors)
#   links:  Object linkage table (matrix of type character)
# Returns:  objPar after updating of values
updatePars <- function (objPar, src, links) {
  if (nrow(links) > 0) {
    f <- function(i) {
      objPar[links[i,"tarPar"]] <<- src[[links[i,"srcObj"]]][links[i,"srcItem"]]
      NULL
    }
    lapply(1:nrow(links), f)
  }
  objPar
}

# Wrapper for integration methods
integr <- function(obj, t0, t1, models, internal, check) {
  if (internal) {
    tmp <- models[[obj]]$step(t0, h=t1-t0, check=check)
  } else {
    tmp <- deSolve::ode(y=models[[obj]]$getVars(), times=c(t0, t1),
      func=models[[obj]]$libFunc(), parms=models[[obj]]$getPars(),
      dllname=models[[obj]]$libName(), outnames=models[[obj]]$namesPros(),
      nout=models[[obj]]$lenPros())
    tmp <- tmp[2,2:ncol(tmp)]
  }
  tmp
}

# Simulate coupled models over a single time step
advance <- function(i, times, objects, models, internal) {
  # Call integrator
  out <- sapply(objects, integr, t0=times[i], t1=times[i+1], models=models,
    internal=internal, check=(i==1), simplify=FALSE)
  # Update parameters affected by coupling
  lapply(objects, function(obj)
    {models[[obj]]$setPars(
    updatePars(models[[obj]]$getPars(useNames=TRUE), out,
      links[links[,"tarObj"]==obj,,drop=FALSE]))})
  # Re-initialize state variables 
  lapply(objects, function(obj)
    {models[[obj]]$setVars(out[[obj]][models[[obj]]$namesVars()])})
  # Return all outputs in a single vector
  unlist(out)
}

# Solve for all time steps
system.time({
  out <- t(sapply(1:(length(times)-1), advance, times=times, objects=objects,
    models=models, internal=internal))
})

# Plot
out <- cbind(time= times[2:length(times)], out)
class(out) <- "deSolve"
plot(out, mfrow=c(4,3))

