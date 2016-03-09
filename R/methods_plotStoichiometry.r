rodeo$methods( plotStoichiometry = function(values, cex=1,
  colPositive="darkorange", colNegative="steelblue4", colGrid="grey") {
  "Plots qualitative stoichiometry information based on the values of variables,
  parameters, and time passed as a named vector in \\code{values}."

  m= .self$stoichiometry(values=values)
  dx=0.2
  dy=sqrt((dx**2)/2)
  mar= 0.5
  plot(0, 0, xlim=c(1-mar,(ncol(m)+mar)), ylim=c(1-mar,(nrow(m)+mar)), type="n",
    bty="n", xaxt="n", yaxt="n", xlab="", ylab="")
  mtext(side=3, at=1:ncol(m), colnames(m), line=0.5, las=2, cex=cex)
  mtext(side=2, at=nrow(m):1, rownames(m), line=0.5, las=2, cex=cex)
  abline(h=c((1:nrow(m))-0.5,nrow(m)+0.5),
    v=c((1:ncol(m))-0.5,ncol(m)+0.5), col=colGrid)
  for (ic in 1:ncol(m)) {
    for (ir in 1:nrow(m)) {
      if (m[ir,ic] > 0) polygon(x=c(ic-dx,ic+dx,ic,ic-dx),
        y=nrow(m)+1-c(ir+dy,ir+dy,ir-dy,ir+dy), col=colPositive, border=NA)
      if (m[ir,ic] < 0) polygon(x=c(ic-dx,ic+dx,ic,ic-dx),
        y=nrow(m)+1-c(ir-dy,ir-dy,ir+dy,ir-dy), col=colNegative, border=NA)
    }
  }
  return(invisible(NULL))
})

