## @knitr diffusion
rm(list=ls())

# Adjustable settings ##########################################################
dx <- 0.01                           # spatial discretization (m)
nCells <- 100                        # number of layers (-)                           
d <- 5e-9                            # diffusion coefficient (m2/s)
cb <- 1                              # boundary concentr. at all times (mol/m3)
times <- c(0,1,6,14,30,89)*86400     # times of interest (seconds)
# End of settings ##############################################################

# Load packages
library("deSolve")
library("rodeo")

# Specification of the model
vars <- data.frame(stringsAsFactors=FALSE,
  name="c", unit="mol/m3", description="concentration")
pars <- data.frame(matrix(c(
    "d",         "m2/s",    "diffusion coefficient",
    "dx",        "m",       "thickness of layers",
    "cb",        "mol/m3",  "boundary concentration",
    "leftmost",  "-",       "0/1 mask to select boundary layer"
  ), ncol=3, byrow=TRUE, dimnames=list(NULL, c("name", "unit", "description"))))
funs <- data.frame(name=character(0), unit=character(0), description=character(0))
# Note: Boundary value is imposed on leftmost cell directly, by omitting
#       diffusion coefficient/length
pros <- data.frame(stringsAsFactors=FALSE,
  name="diff", unit="mol/m3/s", description="diffusion", expression=
  "d/(dx^2) * (left(c) - 2*c + right(c)) + leftmost * (cb - c)")
stoi <- matrix(1, ncol=1, nrow=1, dimnames=list("diff","c"))

# Initialize rodeo object
model <- rodeo$new(vars=vars, pars=pars, funs=funs, pros=pros, stoi=stoi,
  asMatrix=TRUE, dim=(nCells))

# Assign initial values and parameters
model$setVars(cbind(c=rep(0, nCells)))
model$setPars(cbind(d=d, dx=dx,cb=cb,
  leftmost= c(1, rep(0, nCells-1))
))

# Generate code, compile into shared library, load library
lib <- model$compile(NULL)              
dyn.load(lib["libFile"])

# Numeric solution
solNum <- ode(y=model$getVars(), times=times, func=lib["libFunc"],
  parms=model$getPars(), dllname=lib["libName"],
  nout=model$lenPros()*prod(model$getDim()),
  jactype="bandint", bandup=1, banddown=1)

# Clean-up
dyn.unload(lib["libFile"])
invisible(file.remove(lib["libFile"]))

# Function providing the analytical solution
erfc <- function(x) { 2 * pnorm(x * sqrt(2), lower=FALSE) }
solAna <- function (x,t,d,cb) { cb * erfc(x / 2 / sqrt(d*t)) }

# Graphically compare numerical and analytical solution
nc <- 2
nr <- ceiling(length(times) / nc)
layout(matrix(1:(nc*nr), ncol=nc, byrow=TRUE))
par(mar=c(4,4,1,1))
for (t in times) {
  plot(c(0,nCells*dx), c(0,cb), type="n", xlab="Station (m)", ylab="mol/m3")
  # Numeric solution (stair steps of cell-average); graph shifted by 1/2 dx
  # because whole 1st layer was set to boundary value
  stations <- seq(from=0, by=dx, length.out=nCells+1)
  concs <- solNum[solNum[,1]==t, paste0("c.",1:nCells)]
  lines(stations-dx/2, c(concs,concs[length(concs)]), type="s", col="steelblue4")
  # Analytical solution (for center of cells)
  stations <- seq(from=dx/2, to=(nCells*dx)-dx/2, by=dx)
  concs <- solAna(x=stations, t=t, d=d, cb=cb)
  lines(stations, concs, col="red", lty=2)
  # Extras
  legend("topright", bty="n", paste("After",t/86400,"days"))
  if (t == times[1]) legend("right",lty=1:2,
    col=c("steelblue4","red"),legend=c("Numeric", "Exact"),bty="n")
  abline(v=0)
}
layout(1)

