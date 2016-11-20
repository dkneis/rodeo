test.list <- list(                            # define test values of parameters
  A_in= c(0.002, 0.005, 0.01),                #   input of antibiotic
  alpha= c(0, 0.1, 0.25),                     #   cost of resistance
  ks= seq(0, 0.02, 0.001),                    #   loss of resistance
  kc= 10^seq(from=-4, to=-2, by=0.2))         #   transfer of resistance

test.comb <- expand.grid(test.list)           # all possible sets of parameters

f <- function(set, y0) {                      # solution for a single param. set
  p <- model$getPars(asArray=TRUE)  
  p[,names(set)] <- rep(as.numeric(set), each=nTanks) # update parameters
  out <- rootSolve::steady.1D(y=y0, time=NULL, func=lib["libFunc"],
    parms=p, nspec=model$lenVars(), dimens=nTanks, positive=TRUE,
    dllname=lib["libName"], initfunc="initmod", nout=model$lenPros()*nTanks)
  if (attr(out, which="steady", exact=TRUE)) {        # solution found?
    names(out$y) <- names(model$getVars())
    down_S_w <- out$y[paste0("S_w",".",nTanks)]       # bacteria concentrations
    down_R_w <- out$y[paste0("R_w",".",nTanks)]       #   at lower end of reach
    return(unname(down_R_w / (down_R_w + down_S_w)))  # fraction of resistant b.
  } else {
    return(NA)                                        # if solver failed
  }
}

y0 <- array(std$y, dim=c(nTanks, model$lenVars()),       # re-use as init. guess
  dimnames=list(NULL, model$namesVars()))
res <- array(apply(X=test.comb, MARGIN=1, FUN=f, y0=y0), # run for all sets,
  dim=lapply(test.list, length), dimnames=test.list)        #  save as 4D-array
##----------------------------------------BeginHide
pdf(file=ofile_sen, width=7, height=5)
omar <- par("mar")
par(mar=c(4,4,1.5,1))
##----------------------------------------EndHide
breaks <- pretty(res, 8)                                    # graphical output
colors <- colorRampPalette(c("steelblue2","khaki2","brown"))(length(breaks)-1)
nr <- length(test.list$A_in)
nc <- length(test.list$alpha)
layout(cbind(matrix(1:(nr*nc), nrow=nr), rep(nr*nc+1, nr)))
for (alpha in test.list$alpha) {
  for (A_in in test.list$A_in) {
    labs <- (A_in == tail(test.list$A_in, n=1)) && (alpha == test.list$alpha[1])
    image(x=log10(as.numeric(dimnames(res)$kc)), y=as.numeric(dimnames(res)$ks),
      z=t(res[as.character(A_in), as.character(alpha),,]),
      zlim=range(res), breaks=breaks, col=colors,
      xlab=ifelse(labs, "log10(kc)", ""), ylab=ifelse(labs, "ks", ""))
    if (A_in == test.list$A_in[1])
      mtext(side=3, paste0("alpha = ",alpha), cex=par("cex"), line=.2)
    if (alpha == tail(test.list$alpha, n=1))
      mtext(side=4, paste0("A_in = ",A_in), cex=par("cex"), las=3, line=.2)
  }
}
plot.new()
legend("left", bty="n", title="% resistant", fill=colors,
  legend=paste0(breaks[-length(breaks)]*100," - ", breaks[-1]*100))
layout(1)
##----------------------------------------BeginHide
par(mar=omar)
graphics.off()
##----------------------------------------EndHide

