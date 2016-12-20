## @knitr advectionDispersion
rm(list=ls())

# Adjustable settings ##########################################################
fileFun <- "functions.f95"
u <- 1                                 # advective velocity (m/s)
d <- 30                                # longit. dispersion coefficient (m2/s)
wetArea <- 50                          # wet cross-section area (m2)
dx <- 10                               # length of a sub-section (m)
nCells <- 1000                         # number of sub-sections
inputCell <- 100                       # index of sub-section with tracer input
inputMass <- 10                        # input mass (g)
times <- c(0,30,60,600,1800,3600)      # times (seconds)
# End of settings ##############################################################

# Load packages
library("deSolve")
library("rodeo")

# Make sure that vector of times starts with zero
times <- sort(unique(c(0, times)))

# Initialize rodeo object
rd <- function(f) {read.table(file=f,
  header=TRUE, sep="\t", stringsAsFactors=FALSE) }
model <- rodeo$new(vars=rd("vars.txt"), pars=rd("pars.txt"),
  funs=rd("funs.txt"), pros=rd("pros.txt"), stoi=rd("stoi.txt"),
  asMatrix=FALSE, dim=c(nCells))

# Numerical dispersion for backward finite-difference approx. of advection term
dNum <- u*dx/2

# Assign initial values and parameters
model$setVars(cbind(
  c=ifelse((1:nCells)==inputCell, inputMass/wetArea/dx, 0)
))
model$setPars(cbind(
  u=u, d=d-dNum, dx=dx,
  leftmost= c(1, rep(0, nCells-1)),
  rightmost= c(rep(0, nCells-1), 1)
))

# Generate code, compile into shared library, load library
model$compile(fileFun)              

# Numeric solution
solNum <- model$dynamics(times=times, jactype="bandint", bandup=1, banddown=1,
  atol=1e-9)

# Function providing the analytical solution
solAna <- function (x,t,mass,area,disp,velo) {
  mass/area/sqrt(4*pi*disp*t) * exp(-((x-velo*t)^2) / (4*disp*t))
}

# Graphically compare numerical and analytical solution
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
  concs <- solAna(x=stations, t=t, mass=inputMass, area=wetArea, disp=d, velo=u)
  stations <- stations + (inputCell*dx) - dx/2
  lines(stations, concs, col="red", lty=2)
  # Extras
  abline(v=(inputCell*dx) - dx/2, lty=3)
  legend("topright", bty="n", paste("After",t,"sec"))
  if (t == times[1]) legend("right",lty=1:2,
    col=c("steelblue4","red"),legend=c("Numeric", "Exact"),bty="n")
}
layout(1)

