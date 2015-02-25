

rodeo$methods( publish = function(file, tbl,
  comment="%", colsep="&",endline="\\\\", equal=" &= ", startCell="$ ", endCell=" $"
) {
  "Experimental: Exports model equations to a file for inclusion in e.g. LaTex documents"

#TODO NOTE: It appears that it makes limited sense to export the model in a seperate
#TODO documentation format. Reason: The JSON notation is already the best-readable
#TODO representation of the model!

  newline=ifelse(.Platform$OS.type=="windows","\r\n","\n")

  out= paste(comment,"Exported ",Sys.time())
  write(file=file, x=out, ncolumns=1, append=FALSE)

  # Process rates
  out= paste0(newline,comment," Vector of process rates")
  write(file=file, x=out, ncolumns=1, append=TRUE)
  out= paste0("\\begin{align}")
  write(file=file, x=out, ncolumns=1, append=TRUE)
  PROS= pros
  for (k in 1:nrow(tbl)) {
    tryCatch({
      PROS= gsub(pattern=tbl[k,1], replacement=tbl[k,2], x=PROS)
      names(PROS)= gsub(pattern=tbl[k,1], replacement=tbl[k,2], x=names(PROS))
    }, error= function(e) {
      stop(paste("Failed to replace pattern '",tbl[k,1],"'. Details: ",e,sep=""))
    })
  }
  out= paste(paste(names(PROS),PROS,sep=equal), collapse=paste0(endline,newline))
  write(file=file, x=out, ncolumns=1, append=TRUE)
  out= paste0("\\end{align}",newline)
  write(file=file, x=out, ncolumns=1, append=TRUE)

  # Stoichiometry matrix
  out= paste0(newline,comment," Stoichiometry matrix")
  write(file=file, x=out, ncolumns=1, append=TRUE)
  out= paste0("\\begin{tabular}{",paste(rep("c",ncol(stox)),collapse=""),"}")
  write(file=file, x=out, ncolumns=1, append=TRUE)
  out= paste(paste(names(stox), collapse=colsep), endline)
  write(file=file, x=out, ncolumns=1, append=TRUE)
  for (ir in 1:nrow(stox)) {
    ROW= as.character(unlist(stox[ir,]))
    for (k in 1:nrow(tbl)) {
      tryCatch({
        ROW= gsub(pattern=tbl[k,1], replacement=tbl[k,2], x=ROW)
      }, error= function(e) {
        stop(paste("Failed to replace pattern '",tbl[k,1],"'. Details: ",e,sep=""))
      })
    }
    out= paste(startCell, paste(ROW, collapse=paste0(endCell,colsep,startCell)), endCell,endline)
    write(file=file, x=out, ncolumns=1, append=TRUE)
  }
  out= paste0("\\end{tabular}")
  write(file=file, x=out, ncolumns=1, append=TRUE)

  # Auxiliary expressions
  out= paste0(newline,comment," Vector of auxiliary expressions")
  write(file=file, x=out, ncolumns=1, append=TRUE)
  out= paste0("\\begin{align}")
  write(file=file, x=out, ncolumns=1, append=TRUE)
  AUXS= auxs
  for (k in 1:nrow(tbl)) {
    tryCatch({
      AUXS= gsub(pattern=tbl[k,1], replacement=tbl[k,2], x=AUXS)
      names(AUXS)= gsub(pattern=tbl[k,1], replacement=tbl[k,2], x=names(AUXS))
    }, error= function(e) {
      stop(paste("Failed to replace pattern '",tbl[k,1],"'. Details: ",e,sep=""))
    })
  }
  out= paste(paste(names(AUXS),AUXS,sep=equal), collapse=paste0(endline,newline))
  write(file=file, x=out, ncolumns=1, append=TRUE)
  out= paste0("\\end{align}",newline)
  write(file=file, x=out, ncolumns=1, append=TRUE)

  return(invisible(NULL))
})


