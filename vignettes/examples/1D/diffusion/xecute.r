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

# Initialize rodeo object
rd <- function(f, ...) {read.table(file=f,
  header=TRUE, sep="\t", stringsAsFactors=FALSE, ...) }
model <- rodeo$new(vars=rd("vars.txt"), pars=rd("pars.txt"), funs=NULL,
  pros=rd("pros.txt"), stoi=as.matrix(rd("stoi.txt", row.names="process")),
  asMatrix=TRUE, dim=c(nCells))

# Assign initial values and parameters
model$setVars(cbind(c=rep(0, nCells)))
model$setPars(cbind(d=d, dx=dx,cb=cb,
  leftmost= c(1, rep(0, nCells-1))
))

# Generate code, compile into shared library, load library
model$compile(NULL)              

# Numeric solution
solNum <- ode(y=model$getVars(), times=times, func=model$libFunc(),
  parms=model$getPars(), dllname=model$libName(),
  nout=model$lenPros()*prod(model$getDim()),
  jactype="bandint", bandup=1, banddown=1)

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
  # Numeric solution (stair steps of cell-average)
  stations <- seq(from=0, by=dx, length.out=nCells+1)
  concs <- solNum[solNum[,1]==t, paste0("c.",1:nCells)]
  lines(stations, c(concs,concs[length(concs)]), type="s", col="steelblue4")
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

