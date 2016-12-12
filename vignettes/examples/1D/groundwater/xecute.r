## @knitr groundwater
rm(list=ls())

# Adjustable settings ##########################################################
fileFun <- "functions.f95"
dx <- 10                             # spatial discretization (m)
nx <- 100                            # number of boxes (-)                           
times <- seq(0, 12*365, 30)          # times of interest (days)
# End of settings ##############################################################

# Load packages
library("deSolve")
library("rodeo")

# Initialize model
rd <- function(f) {read.table(file=f,
  header=TRUE, sep="\t", stringsAsFactors=FALSE)}
model <- rodeo$new(vars=rd("vars.txt"), pars=rd("pars.txt"),
  funs=rd("funs.txt"), pros=rd("pros.txt"),
  stoi=rd("stoi.txt"), asMatrix=FALSE, dim=nx)

# Assign initial values and parameters
model$setVars(cbind( h=rep(11, nx) ))
model$setPars(cbind( dx=rep(dx, nx), kf=rep(5., nx), ne=rep(0.17, nx),
  h0=rep(-10, nx), hBed=rep(10, nx), wBed=rep(0.5*dx, nx), kfBed=rep(5., nx),
  tBed=rep(0.1, nx), leaky=c(1, rep(0, nx-1)) ))

# Generate code, compile into shared library, load library
model$compile(fileFun)              

# Integrate
out <- deSolve::ode(y=model$getVars(), times=times, func=model$libFunc(),
  parms=model$getPars(), dllname=model$libName(),
  nout=model$lenPros()*prod(model$getDim()),
  jactype="bandint", bandup=1, banddown=1)

# Plot results
filled.contour(x=out[,"time"]/365.25, y=(1:nx)*dx-dx/2,
  z=out[,names(model$getVars())], xlab="Years", ylab="Distance to river (m)",
  color.palette=colorRampPalette(c("steelblue2","lightyellow","darkorange")),
  key.title= mtext(side=3, "Ground water surf. (m)", padj=-0.5))

