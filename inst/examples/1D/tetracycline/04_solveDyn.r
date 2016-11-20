# Dynamic simulation
times <- seq(0, 7, 1/48)                              # requested output times
dyn <- deSolve::ode(y=model$getVars(), times=times, func=lib["libFunc"],
  parms=model$getPars(), NLVL=nTanks, dllname=lib["libName"],
  hmax=dt_max, nout=model$lenPros()*nTanks,
  jactype="bandint", bandup=1, banddown=1)
if (attr(dyn, which="istate", exact=TRUE)[1] != 2) stop("Dynamic run failed.")

# Plot dynamic solution (selected variable at upper and lower reach end)
name <- "S_w"                                         # select variable to plot
i <- match(paste0(name,".",1:nTanks), colnames(dyn))  # find columns in result
plot(x=range(dyn[,"time"]), y=range(dyn[,i]),         # empty plot, proper axes
  type="n", xlab="Time", ylab="Concentration")
lines(dyn[,1], dyn[,i[1]], lty=1)                     # tank at upper reach end
lines(dyn[,1], dyn[,i[length(i)]], lty=2)             # tank at lower reach end
legend("topleft", lty=1:2, legend=c("upper","lower"))
##----------------------------------------BeginHide
# Complete graphical output
pdf(file=ofile_dyn, width=6, height=5)
stations= (1:nTanks) * len/nTanks - len/nTanks/2
for (name in model$namesVars()) {
  m <- dyn[ ,match(paste0(name,".",1:nTanks), colnames(dyn))]
  filled.contour(x=stations, y=dyn[,"time"], z=t(m), xlab="Station", ylab="Days",
    color.palette=colorRampPalette(c("lightskyblue3","khaki","peru")),
    main=name, cex.main=1, font.main=1)
}
graphics.off()
##----------------------------------------EndHide

