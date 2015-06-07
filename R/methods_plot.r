
rodeo$methods( plot = function(values) {
  "Creates a plot of the stoichiometry matrix using the supplied values"
  # Check args
  if (is.null(names(values)) || any(names(values) == ""))
    stop("missing element name(s) in vector 'values'")
  if (any(duplicated(names(values))))
    stop("duplicated element name(s) in vector 'values'")
  if (!all(is.finite(values)))
    stop("non-finite element(s) in 'values'")
  if (nrow(.self$FUNS) > 0) {
    for (i in 1:nrow(.self$FUNS)) {
      if (!exists(.self$FUNS$name[i], mode = "function"))
        stop(paste0("function '",.self$FUNS$name[i],
          "' is referenced but not defined (in R code)"))
    }
  }
  # Define constants and functions for plotting
  d= 1/3
  triang= function(x,y,d,val) {
    if (val > 0) {
      polygon(x=c(x-d,x+d,x,x-d), y=c(y-d,y-d,y+d,y-d), col="darkorange")
    } else if (val < 0) {
      polygon(x=c(x-d,x+d,x,x-d), y=c(y+d,y+d,y-d,y+d), col="steelblue")
    } else { }
    return(invisible(NULL))
  }
  # Create environment holding all data -> required for evaluating expressions
  env= new.env()
  f=tempfile()
cat("tempfile for stoi data is '",f,"'\n")
  write.table(file=f, x=data.frame(names(values),values,stringsAsFactors=FALSE),
    sep="=", col.names=FALSE, row.names=FALSE, quote=FALSE)
cat("data written to '",f,"'\n")
  sys.source(file=f,envir=env)
cat("file '",f,"' sourced \n")
  # Represent stoichiometry as a matrix
  m= .self$stoichiometryMatrix()
  # Create graphics
  op=par(no.readonly=TRUE)
  par(mar=c(0.5,8,4,0.5))
  graphics::plot(x=c(0,ncol(m)), y=c(0,nrow(m)), type="n", bty="n", xaxt="n", yaxt="n",
    xlab="", ylab="")
  for (ic in 1:ncol(m)) {
    for (ir in 1:nrow(m)) {
      tryCatch({
        val= eval(parse(text=m[ir,ic]), envir=env)  # evaluated in created env
      }, error= function(e) {
        stop(paste0("failed to compute stoichiometry factor for variable '",
          colnames(m)[ic],"' and process '",rownames(m)[ir],"'; details: ",e))
      })
      rect(xleft=ic-1,xright=ic,ybottom=nrow(m)-ir,ytop=nrow(m)-ir+1,col="grey",border="white")
      triang(x=ic-0.5, y=nrow(m)-ir+0.5, d=d, val=val)
    }
  }
  axis(side=3,at=(1:ncol(m))-0.5, labels=colnames(m), tick=FALSE, las=2)
  axis(side=2,at=(1:nrow(m))-0.5, labels=rownames(m)[nrow(m):1], tick=FALSE, las=2)
  par(op)
  return(invisible(NULL))
})

