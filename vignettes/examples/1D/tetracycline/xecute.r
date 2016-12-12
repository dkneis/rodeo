rm(list=ls())

## @knitr tetracycline_init

# Adjustable settings ##########################################################

# Properties of the reach not being parameters of the core model
len <-   125000             # reach length (m)
uL  <- 0.5 * 86400          # flow velocity (m/d)
dL  <- 300 * 86400          # longitudinal dispersion coefficient (m2/d)
xsArea <- 0.6 * 15          # wet cross-section area (m2)

# End of settings ##############################################################

# Computational parameters
nTanks <- trunc(uL * len / dL / 2) + 1 # number of tanks; see Elgeti (1996)
dt_max <- 0.5 * len / nTanks / uL      # max. time step (d); Courant criterion

# Load packages
library("rodeo")
library("deSolve")
library("rootSolve")

# Initialize rodeo object
rd <- function(f, ...) { read.table(file=f, header=TRUE, sep="\t", ...) }
model <- rodeo$new(vars=rd("vars.txt"), pars=rd("pars.txt"), funs=rd("funs.txt"),
  pros=rd("pros.txt"), stoi=as.matrix(rd("stoi.txt", row.names="process")),
  asMatrix=TRUE, dim=c(nTanks))

# Generate code, compile into shared library, load library
model$compile(sources="functions.f95")

# Assign initial values
vars <- matrix(rep(as.numeric(model$getVarsTable()$initial), each=nTanks),
  ncol=model$lenVars(), nrow=nTanks, dimnames=list(NULL, model$namesVars()))
model$setVars(vars)

# Assign / update values of parameters; River flow is assumed to be steady
# and uniform (i.e. constant in space and time); Settling and resuspension
# velocities are computed from steady-state mass balance as in Hellweger (2011)
pars <- matrix(
  rep(suppressWarnings(as.numeric(model$getParsTable()$default)), each=nTanks),
  ncol=model$lenPars(), nrow=nTanks, dimnames=list(NULL, model$namesPars()))
pars[,"V"] <- xsArea * len/nTanks                           # tank volumes
pars[,"Q"] <- c(0, rep(uL * xsArea, nTanks-1))              # inflow from upstr.
pars[,"Q_in"] <- c(uL * xsArea, rep(0, nTanks-1))           # inflow to tank 1
pars[,"us"] <- pars[,"kh_s"] * pars[,"ds"] /                # settling velocity
  ((vars[,"POM_w"] / vars[,"POM_s"]) -
  (vars[,"TSS_w"] / vars[,"TSS_s"]))
pars[,"ur"] <- pars[,"us"] * vars[,"TSS_w"] /               # resuspension velo.
  vars[,"TSS_s"]
model$setPars(pars)

## @knitr tetracycline_stoi

# Plot stoichiometry matrix using symbols
m <- model$stoichiometry(box=1)
clr <- function(x, ignoreSign=FALSE) {
  res <- rep("transparent", length(x))
  if (ignoreSign) {
    res[x != 0] <- "black"
  } else {
    res[x < 0] <- "lightgrey"
    res[x > 0] <- "white"
  }
  return(res)
}
sym <- function(x, ignoreSign=FALSE) {
  res <- rep(NA, length(x))
  if (ignoreSign) {
    res[x != 0] <- 21
  } else {
    res[x < 0] <- 25
    res[x > 0] <- 24
  }
  return(res)
}
omar <- par("mar")
par(mar=c(1,6,6,1))
plot(c(1,ncol(m)), c(1,nrow(m)), bty="n", type="n", xaxt="n", yaxt="n",
  xlab="", ylab="")
abline(h=1:nrow(m), v=1:ncol(m), col="grey")
for (ir in 1:nrow(m)) {
  ignoreSign <- grepl(pattern="^transport.*", x=rownames(m)[ir]) ||
    grepl(pattern="^diffusion.*", x=rownames(m)[ir])
  points(1:ncol(m), rep(ir,ncol(m)), pch=sym(m[ir,1:ncol(m)], ignoreSign),
    bg=clr(m[ir,1:ncol(m)], ignoreSign))
}
mtext(side=2, at=1:nrow(m), rownames(m), las=2, line=0.5, cex=0.8)
mtext(side=3, at=1:ncol(m), colnames(m), las=2, line=0.5, cex=0.8)
par(mar=omar)
rm(m)

## @knitr tetracycline_steady

# Estimate steady-state
std <- rootSolve::steady.1D(y=model$getVars(), time=NULL, func=model$libFunc(),
  parms=model$getPars(), nspec=model$lenVars(), dimens=nTanks, positive=TRUE,
  dllname=model$libName(), nout=model$lenPros()*nTanks)
if (!attr(std, which="steady", exact=TRUE))
  stop("Steady-state run failed.")
names(std$y) <- names(model$getVars())

# Plot bacterial densities
stations= ((1:nTanks) * len/nTanks - len/nTanks/2) / 1000     # stations (km)
domains= c(Water="_w", Sediment="_s")                         # domain suffixes
layout(matrix(1:length(domains), ncol=length(domains)))
for (i in 1:length(domains)) {
  R= match(paste0("R",domains[i],".",1:nTanks), names(std$y)) # resistant bac.
  S= match(paste0("S",domains[i],".",1:nTanks), names(std$y)) # susceptibles
  plot(x=range(stations), y=range(std$y[c(S,R)]), type="n",
    xlab=ifelse(i==1,"Station (km)",""), ylab=ifelse(i==1,"mg/l",""))
  lines(stations, std$y[R], lty=1)
  lines(stations, std$y[S], lty=2)
  if (i==1) legend("topleft", bty="n", lty=1:2, legend=c("Resistant","Suscept."))
  mtext(side=3, names(domains)[i])
}

## @knitr tetracycline_dynamic

# Dynamic simulation
times <- seq(0, 7, 1/48)                              # requested output times
dyn <- deSolve::ode(y=model$getVars(), times=times, func=model$libFunc(),
  parms=model$getPars(), NLVL=nTanks, dllname=model$libName(),
  hmax=dt_max, nout=model$lenPros()*nTanks,
  jactype="bandint", bandup=1, banddown=1)
if (attr(dyn, which="istate", exact=TRUE)[1] != 2)
  stop("Dynamic run failed.")

# Plot dynamic solution
stations= (1:nTanks) * len/nTanks - len/nTanks/2
name <- "S_w"
m <- dyn[ ,match(paste0(name,".",1:nTanks), colnames(dyn))]
filled.contour(x=stations, y=dyn[,"time"], z=t(m), xlab="Station", ylab="Days",
  color.palette=colorRampPalette(c("lightskyblue3","khaki","peru")),
  main=name, cex.main=1, font.main=1)

## @knitr tetracycline_sensitivity

# Define parameter values for sensitivity analysis
testList <- list(
  A_in= c(0.002, 0.005),                              #   input of antibiotic
  alpha= c(0, 0.25),                                  #   cost of resistance
  ks= seq(0, 0.02, 0.002),                            #   loss of resistance
  kc= 10^seq(from=-4, to=-2, by=0.5))                 #   transfer of resistance

# Set up parameter sets
testSets <- expand.grid(testList)

# Function to return the steady-state solution for specific parameters
f <- function(set, y0) {
  p <- model$getPars(asArray=TRUE)  
  p[,names(set)] <- rep(as.numeric(set), each=nTanks) # update parameters
  out <- rootSolve::steady.1D(y=y0, time=NULL, func=model$libFunc(),
    parms=p, nspec=model$lenVars(), dimens=nTanks, positive=TRUE,
    dllname=model$libName(), nout=model$lenPros()*nTanks)
  if (attr(out, which="steady", exact=TRUE)) {        # solution found?
    names(out$y) <- names(model$getVars())
    down_S_w <- out$y[paste0("S_w",".",nTanks)]       # bacteria concentrations
    down_R_w <- out$y[paste0("R_w",".",nTanks)]       #   at lower end of reach
    return(unname(down_R_w / (down_R_w + down_S_w)))  # fraction of resistant b.
  } else {
    return(NA)                                        # if solver failed
  }
}

# Use already computed steady state solution as initial guess
y0 <- array(std$y, dim=c(nTanks, model$lenVars()),
  dimnames=list(NULL, model$namesVars()))

# Apply model to all sets and store results as 4-dimensional array
res <- array(apply(X=testSets, MARGIN=1, FUN=f, y0=y0),
  dim=lapply(testList, length), dimnames=testList)

# Plot results of the analysis
omar <- par("mar")
par(mar=c(4,4,1.5,1))
breaks <- pretty(res, 8)
colors <- colorRampPalette(c("steelblue2","khaki2","brown"))(length(breaks)-1)
nr <- length(testList$A_in)
nc <- length(testList$alpha)
layout(cbind(matrix(1:(nr*nc), nrow=nr), rep(nr*nc+1, nr)))
for (alpha in testList$alpha) {
  for (A_in in testList$A_in) {
    labs <- (A_in == tail(testList$A_in, n=1)) && (alpha == testList$alpha[1])
    image(x=log10(as.numeric(dimnames(res)$kc)), y=as.numeric(dimnames(res)$ks),
      z=t(res[as.character(A_in), as.character(alpha),,]),
      zlim=range(res), breaks=breaks, col=colors,
      xlab=ifelse(labs, "log10(kc)", ""), ylab=ifelse(labs, "ks", ""))
    if (A_in == testList$A_in[1])
      mtext(side=3, paste0("alpha = ",alpha), cex=par("cex"), line=.2)
    if (alpha == tail(testList$alpha, n=1))
      mtext(side=4, paste0("A_in = ",A_in), cex=par("cex"), las=3, line=.2)
  }
}
plot.new()
legend("left", bty="n", title="% resistant", fill=colors,
  legend=paste0(breaks[-length(breaks)]*100," - ", breaks[-1]*100))
layout(1)
par(mar=omar)

