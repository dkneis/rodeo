
rodeo$methods( plot_stox = function() {
  "Creates a graphical representation of the stoichiometry matrix"
  if (!all(is.finite(vars)))
    stop("variable(s) with non-finite value(s) detected")
  if (!all(is.finite(pars)))
    stop("parameter(s) with non-finite value(s) detected")
  if (length(funs) > 0) {
    for (i in 1:length(funs)) {
      if (!exists(funs[1], mode = "function"))
        stop(paste0("function '",funs[1],"' is referenced but not defined"))
    }
  }
  op=par(no.readonly=TRUE)
  par(mar=c(0.5,6,4,0.5))
  plot(x=c(0,ncol(stox)), y=c(0,nrow(stox)), type="n", bty="n", xaxt="n", yaxt="n",
    xlab="", ylab="")
  # create environment holding all data -> required for evaluating expressions in stox
  env= new.env()
  f=tempfile()
  write.table(file=f, x=data.frame(names(vars),vars,stringsAsFactors=FALSE),
    sep="=", col.names=FALSE, row.names=FALSE, quote=FALSE)
  write.table(file=f, x=data.frame(names(pars),pars,stringsAsFactors=FALSE),
    sep="=", col.names=FALSE, row.names=FALSE, quote=FALSE, append=TRUE)
  write.table(file=f, x=data.frame(names(expr),expr,stringsAsFactors=FALSE),
    sep="=", col.names=FALSE, row.names=FALSE, quote=FALSE, append=TRUE)
  sys.source(file=f,envir=env)
  triang= function(x,y,d,val) {
    if (val > 0) {
      polygon(x=c(x-d,x+d,x,x-d), y=c(y-d,y-d,y+d,y-d), col="darkorange")
    } else if (val < 0) {
      polygon(x=c(x-d,x+d,x,x-d), y=c(y+d,y+d,y-d,y+d), col="steelblue")
    } else { }
    return(invisible(NULL))
  }
  d= 1/3
  for (ic in 1:ncol(stox)) {
    for (ir in 1:nrow(stox)) {
      val= eval(parse(text=stox[ir,ic]), envir=env)
      rect(xleft=ic-1,xright=ic,ybottom=nrow(stox)-ir,ytop=nrow(stox)-ir+1,col="grey",border="white")
      triang(x=ic-0.5, y=nrow(stox)-ir+0.5, d=d, val=val)
    }
  }
  axis(side=3,at=(1:ncol(stox))-0.5, labels=names(stox), tick=FALSE, las=2)
  axis(side=2,at=(1:nrow(stox))-0.5, labels=row.names(stox)[nrow(stox):1], tick=FALSE, las=2)
  par(op)
  return(invisible(NULL))
})

