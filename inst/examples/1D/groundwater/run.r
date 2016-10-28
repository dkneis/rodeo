# Initial clean-up
rm(list=ls())

# Load packages
library("deSolve")

library('devtools')
install_github('dkneis/rodeo')
library("rodeo")

################################################################################
# Adjustable settings

# Switch between pure R and Fortran-based code
compile <- TRUE

# System and computational parameters
dx <- 10                             # spatial discretization (m)
nx <- 100                            # number of boxes (-)                           
times <- seq(0, 2*365, 1)*86400      # times of interest (seconds)

# End of settings
################################################################################

rd <- function(f) { read.table(file=f, sep="\t", header=TRUE,
  stringsAsFactors=FALSE) }

# Initialize model
model <- rodeo$new(vars=rd("vars.txt"), pars=rd("pars.txt"),
  funs=rd("funs.txt"), pros=rd("pros.txt"),
  stoi=rd("stoi.txt"), asMatrix=FALSE, dim=nx)

# Assign initial values and parameters
model$setVars(cbind(
  h=rep(10, nx)
))
model$setPars(cbind(
  dx=rep(dx, nx),
  kf=rep(100e-5, nx),
  ne=rep(0.17, nx),
  hBed=rep(10, nx),
  widthBed=rep(0.5*dx, nx),
  kfBed= rep(10e-5, nx),
  thickBed= rep(0.1, nx),
  leaky=c(rep(0, nx-1), 1)
))

fileFun <- if (compile) "functions.f95" else "functions.r"

if (!compile) { # R-based version

  # Generate code, parse, and load referenced functions
  code <- model$generate(name="derivs", lang="r")
  derivs <- eval(parse(text=code))
  source(fileFun)

  # Integrate
  out <- ode(y=model$getVars(), times=times, func=derivs,
    parms=model$getPars(), jactype="bandint", bandup=1, banddown=1)
  colnames(out) <- c("time",
    paste(rep(model$namesVars(), each=nx),
      rep(1:nx, model$lenVars()), sep="."),
    paste(rep(model$namesPros(), each=nx),
      rep(1:nx, model$lenPros()), sep="."))

} else { # Fortran-based version

  # Generate code, compile into shared library, load library
  lib <- model$compile(fileFun)              
  dyn.load(lib["libFile"])

  # Integrate
  out <- ode(y=model$getVars(), times=times, func=lib["libFunc"],
    parms=model$getPars(), dllname=lib["libName"],
    nout=model$lenPros()*prod(model$getDim()),
    jactype="bandint", bandup=1, banddown=1)

  # Clean-up
  dyn.unload(lib["libFile"])
  invisible(file.remove(lib["libFile"]))
}

filled.contour(x=out[,"time"]/86400, y=(1:nx)*dx-dx/2, z=out[,names(model$getVars())])

