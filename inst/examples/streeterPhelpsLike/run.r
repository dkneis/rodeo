# Initial clean-up
rm(list=ls())

# Load required packages
library("rodeo")
library("readxl")
library("deSolve")

################################################################################
# Adjustable settings

# Switch between pure R and Fortran-based code
compile <- FALSE

# Tabular model definition
fileTbl <- "def_tables.xlsx"

# User functions referenced in process rates or stoichiometic factors
fileFun <- if (compile) "def_functions.f95" else "def_functions.r"

# Parameters and initial values
pars <- list(kd=1, ka=0.5, s=2.76, temp=20)
vars <- list(OM=1, DO=9.02)

# Times of interest
times <- seq(from=0, to=10, by=1/24)

# End of settings
################################################################################

# Initialize rodeo object
model <- new("rodeo", vars=read_excel(fileTbl, "vars"),
  pars=read_excel(fileTbl, "pars"), funs=read_excel(fileTbl, "funs"),
  pros=read_excel(fileTbl, "pros"), stoi=read_excel(fileTbl, "stoi"),
  asMatrix=TRUE)

# Assign initial values and parameters
v <- model$arrangeVars(vars)
p <- model$arrangePars(pars)

if (!compile) { # R-based version

  # Generate code, parse, and load referenced functions
  code <- model$generate(name="derivs", lang="r")
  derivs <- eval(parse(text=code))
  source(fileFun)

  # Integrate
  out <- ode(y=v, times=times, func=derivs, parms=p, NLVL=1)
  colnames(out) <- c("time", model$namesVars(), model$namesPros())

} else { # Fortran-based version

  # Generate code, compile into shared library, load library
  lib <- model$compile(fileFun, NLVL=1)              
  dyn.load(lib["libFile"])

  # Integrate
  out <- ode(y=v, times=times, func=lib["libFunc"], parms=p,
    dllname=lib["libName"], initfunc="initmod", nout=model$lenPros(),
    outnames=model$namesPros())

  # Clean-up
  dyn.unload(lib["libFile"])
  invisible(file.remove(lib["libFile"]))
}

# Plot method for deSolve objects
plot(out)

