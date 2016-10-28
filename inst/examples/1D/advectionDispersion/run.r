# Initial clean-up
rm(list=ls())

# Load packages
library("readxl")
library("deSolve")

library('devtools')
install_github('dkneis/rodeo')
library("rodeo")

################################################################################
# Adjustable settings

# Switch between pure R and Fortran-based code
compile <- TRUE  # Warning: FALSE leaves the machine busy for several minutes

# Tabular model definition
fileTbl <- "def_tables.xlsx"

# User functions referenced in process rates or stoichiometic factors
fileFun <- if (compile) "def_functions.f95" else "def_functions.r"

# System and computational parameters
u <- 1                                 # advective velocity (m/s)
d <- 30                                # longit. dispersion coefficient (m2/s)
wetArea <- 50                          # wet cross-section area (m2)
dx <- 10                               # length of a sub-section (m)
nCells <- 1000                         # number of sub-sections
inputCell <- 100                       # index of sub-section with tracer input
inputMass <- 10                        # input mass (g)
times <- c(0,30,60,600,1800,3600)      # times of interest (seconds)

# End of settings
################################################################################

# Proper time vector; first element must be 0
times <- sort(unique(c(0, times)))

# Initialize rodeo object
stoi <- read_excel(fileTbl, "stoi")
stoi <- matrix(unlist(stoi[,2:ncol(stoi)]), nrow=nrow(stoi),
  dimnames=list(stoi[,1], names(stoi)[2:ncol(stoi)]))
model <- rodeo$new(vars=read_excel(fileTbl, "vars"),
  pars=read_excel(fileTbl, "pars"), funs=read_excel(fileTbl, "funs"),
  pros=read_excel(fileTbl, "pros"), stoi=stoi,
  asMatrix=TRUE, dim=c(nCells))

# Numerical dispersion for backward finite-difference approx. of advection term
dNum <- u*dx/2

# Assign initial values and parameters
model$setVars(cbind(
  c=ifelse((1:nCells)==inputCell, inputMass/wetArea/dx, 0)
))
model$setPars(cbind(
  u=u,
  d=d-dNum,
  dx=dx,
  leftmost= c(1, rep(0, nCells-1)),
  rightmost= c(rep(0, nCells-1), 1)
))

if (!compile) { # R-based version

  # Generate code, parse, and load referenced functions
  code <- model$generate(name="derivs", lang="r")
  derivs <- eval(parse(text=code))
  source(fileFun)

  # Integrate
  solNum <- ode(y=model$getVars(), times=times, func=derivs,
    parms=model$getPars(), jactype="bandint", bandup=1, banddown=1)
  colnames(solNum) <- c("time",
    paste(rep(model$namesVars(), each=nCells),
      rep(1:nCells, model$lenVars()), sep="."),
    paste(rep(model$namesPros(), each=nCells),
      rep(1:nCells, model$lenPros()), sep="."))

} else { # Fortran-based version

  # Generate code, compile into shared library, load library
  lib <- model$compile(fileFun)              
  dyn.load(lib["libFile"])

  # Integrate
  solNum <- ode(y=model$getVars(), times=times, func=lib["libFunc"],
    parms=model$getPars(), dllname=lib["libName"],
    nout=model$lenPros()*prod(model$getDim()),
    jactype="bandint", bandup=1, banddown=1)

  # Clean-up
  dyn.unload(lib["libFile"])
  invisible(file.remove(lib["libFile"]))
}

# Function providing the analytical solution
solAna <- function (x,t,m,a,d,u) {
  m/a/sqrt(4*3.141593*d*t) * exp(-((x-u*t)^2) / (4*d*t))
}

# Plots to compare numerical and analytical solution 
nc <- 2
nr <- ceiling(length(times) / nc)
layout(matrix(1:(nc*nr), ncol=nc, byrow=TRUE))
par(mar=c(4,4,1,1))
for (t in times) {
  plot(c(0,nCells*dx), c(1e-7,inputMass/wetArea/dx), type="n", xlab="Station (m)",
    ylab="g/m3", log="y")
  # Numeric solution (stair steps of cell-average)
  stations <- seq(from=0, by=dx, length.out=nCells+1)
  concs <- solNum[solNum[,1]==t, paste0("c.",1:nCells)]
  lines(stations, c(concs,concs[length(concs)]), type="s", col="steelblue4")
  # Analytical solution (for center of cells)
  stations <- seq(from=dx/2, to=(nCells*dx)-dx/2, by=dx)
  concs <- solAna(x=stations, t=t, m=inputMass, a=wetArea, d=d, u=u)
  stations <- stations + (inputCell*dx) - dx/2
  lines(stations, concs, col="red", lty=2)
  # Extra
  abline(v=(inputCell*dx) - dx/2, lty=3)
  legend("topright", bty="n", paste("After",t,"sec"))
  if (t == times[1]) legend("right",lty=1:2,
    col=c("steelblue4","red"),legend=c("Numeric", "Exact"),bty="n")
}
layout(1)

