## @knitr twoZonesStirredTank
rm(list=ls())

# Adjustable settings ##########################################################
vars <- c(bM=0, bS=0, sM=0, sS=0)                       # initial values
pars <- c(vol=1000, fS=NA, qM=200, qS=NA,               # fixed parameter values
  bX=1, sX=20, mu=NA, yield=0.1, half=0.1)
sensList <- list(                                       # parameter values for
  fS= seq(from=0.02, to=0.5, by=0.02),                  # sensitivity analysis
  qS= seq(from=2, to=200, by=2),
  mu= c(0.07, 0.1, 0.15)
)
commonScale <- TRUE                                     # controls color scale
# End of settings ##############################################################

# Load packages
library("rootSolve")
library("rodeo")

# Initialize rodeo object
rd <- function(f, ...) {read.table(file=f,
  header=TRUE, sep="\t", stringsAsFactors=FALSE, ...) }
model <- rodeo$new(vars=rd("vars.txt"), pars=rd("pars.txt"), funs=NULL,
  pros=rd("pros.txt"), stoi=as.matrix(rd("stoi.txt", row.names="process")),
  asMatrix=TRUE, dim=c(1))

# Assign initial values and parameters
model$setVars(vars)
model$setPars(pars)

# Generate code, compile into shared library, load library
lib <- model$compile(NULL)
dyn.load(lib["libFile"])

# Function to return the steady-state solution for specific parameters
f <- function(x) {
  testPars <- pars
  testPars[names(sensList)] <- x[names(sensList)]
  model$setPars(testPars)
  st <- rootSolve::runsteady(y=model$getVars(), times=c(0, Inf),
    func=lib["libFunc"], parms=model$getPars(), dllname=lib["libName"],
    nout=model$lenPros(), outnames=model$namesPros())
  if (!attr(st, "steady"))
    st$y <- rep(NA, length(st$y))
  setNames(st$y, model$namesVars())
}

# Set up parameter sets
sensSets <- expand.grid(sensList)

# Apply model to all sets and store results as array
out <- array(apply(sensSets, 1, f),
  dim=c(model$lenVars(), lapply(sensList, length)),
  dimnames=c(list(model$namesVars()), sensList))

# Plot results of sensitivity analysis
xlab <- "Sub-tank volume / Total vol." 
ylab <- "Flow through sub-tank"
VAR <- c("bM", "bS")
MU <- as.character(sensList[["mu"]])

if (commonScale) {
  breaks <- pretty(out[VAR,,,MU], 15)
  colors <- colorRampPalette(c("steelblue2","lightyellow","orange2"))(length(breaks)-1)
}

layout(matrix(1:((length(VAR)+1)*length(MU)), ncol=length(VAR)+1,
  nrow=length(MU), byrow=TRUE))
for (mu in MU) {
  if (!commonScale) {
    breaks <- pretty(out[VAR,,,mu], 15)
    colors <- colorRampPalette(c("steelblue2","lightyellow","orange2"))(length(breaks)-1)
  }
  for (var in VAR) {
    image(x=as.numeric(rownames(out[var,,,mu])),
      y=as.numeric(colnames(out[var,,,mu])), z=out[var,,,mu],
      breaks=breaks, col=colors, xlab=xlab, ylab=ylab)
    mtext(side=3, var, cex=par("cex"))
    legend("topright", bty="n", legend=paste("mu=",mu))
  }
  plot.new()
  br <- round(breaks, 1)
  legend("topleft", bty="n", ncol= 2, fill=colors,
    legend=paste(br[-length(br)],br[-1],sep="-"))
}
layout(1)

# Clean-up
dyn.unload(lib["libFile"])
invisible(file.remove(lib["libFile"]))

