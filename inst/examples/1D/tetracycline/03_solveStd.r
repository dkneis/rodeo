# Estimation of steady-state
std <- rootSolve::steady.1D(y=model$getVars(), time=NULL, func=lib["libFunc"],
  parms=model$getPars(), nspec=model$lenVars(), dimens=nTanks, positive=TRUE,
  dllname=lib["libName"], nout=model$lenPros()*nTanks)
if (!attr(std, which="steady", exact=TRUE)) stop("Steady-state run failed.")
names(std$y) <- names(model$getVars())
##----------------------------------------BeginHide
pdf(file=ofile_std, width=5, height=3)
omar= par("mar")
par(mar=c(4,4,3,0.2))
##----------------------------------------EndHide
# Plotting of bacterial densities
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
##----------------------------------------BeginHide
par(mar=omar)
layout(1)
graphics.off()

# Plots of all variables
pdf(file=ofile_std_full, width=6, height=8)
ncol= 2
nrow= ceiling(model$lenVars()/ncol)
layout(matrix(1:((nrow+1)*ncol), ncol=ncol, byrow=TRUE),
  heights=c(rep(1,nrow), 0.5))
omar= par("mar")
par(mar=c(0.5,5,0.5,1))
stations= ((1:nTanks) * len/nTanks - len/nTanks/2 ) / 1000
i= 0
for (name in model$namesVars()) {
  i= i + 1
  inds= match(paste0(name,".",1:nTanks), names(std$y))
  plot(stations, std$y[inds], type="l", xaxt="n",
    xlab="Station", ylab="", las=1)
  axis(side=1, labels=NA)
  legend("top", bty="n", legend=name)
}
for (i in 1:ncol) {
  plot(stations, rep(0, length(stations)), type="n", bty="n", xaxt="n",
    yaxt="n", ylab="")
  axis(side=3, lty=0, line=-2)
  if (i == 1) legend("bottom", bty="n", "River station (km)")
}
par(mar=omar)
layout(1)
graphics.off()
##----------------------------------------EndHide
