
rodeo$methods( plot = function(pars, vars) {
  "Creates a plot of the stoichiometry matrix for the parameter and
  variable values in \\code{pars} and \\code{vars}, respectively"
  # Check args
  if (!all(is.finite(c(pars, vars))))
    stop("detected non-finite value(s) in input vectors")
  if (length(funs) > 0) {
    for (i in 1:length(funs)) {
      if (!exists(funs[1], mode = "function"))
        stop(paste0("function '",funs[1],"' is referenced but not defined"))
    }
  }
  # Define constants and functions
  d= 1/3
  triang= function(x,y,d,val) {
    if (val > 0) {
      polygon(x=c(x-d,x+d,x,x-d), y=c(y-d,y-d,y+d,y-d), col="darkorange")
    } else if (val < 0) {
      polygon(x=c(x-d,x+d,x,x-d), y=c(y+d,y+d,y-d,y+d), col="steelblue")
    } else { }
    return(invisible(NULL))
  }

  # create environment holding all data -> required for evaluating expressions in stox
  env= new.env()
  f=tempfile()
  write.table(file=f, x=data.frame(.self$vars,sortLikeVars(vars),stringsAsFactors=FALSE),
    sep="=", col.names=FALSE, row.names=FALSE, quote=FALSE)
  write.table(file=f, x=data.frame(.self$pars,sortLikePars(pars),stringsAsFactors=FALSE),
    sep="=", col.names=FALSE, row.names=FALSE, quote=FALSE, append=TRUE)
  write.table(file=f, x=data.frame(names(auxs),auxs,stringsAsFactors=FALSE),
    sep="=", col.names=FALSE, row.names=FALSE, quote=FALSE, append=TRUE)
  sys.source(file=f,envir=env)
  
  op=par(no.readonly=TRUE)
  par(mar=c(0.5,6,4,0.5))
  graphics::plot(x=c(0,ncol(stox)), y=c(0,nrow(stox)), type="n", bty="n", xaxt="n", yaxt="n",
    xlab="", ylab="")
  
  for (ic in 1:ncol(stox)) {
    for (ir in 1:nrow(stox)) {
      val= eval(parse(text=stox[ir,ic]), envir=env)  # evaluated in created env
      rect(xleft=ic-1,xright=ic,ybottom=nrow(stox)-ir,ytop=nrow(stox)-ir+1,col="grey",border="white")
      triang(x=ic-0.5, y=nrow(stox)-ir+0.5, d=d, val=val)
    }
  }
  axis(side=3,at=(1:ncol(stox))-0.5, labels=names(stox), tick=FALSE, las=2)
  axis(side=2,at=(1:nrow(stox))-0.5, labels=row.names(stox)[nrow(stox):1], tick=FALSE, las=2)
  par(op)
  return(invisible(NULL))
})

