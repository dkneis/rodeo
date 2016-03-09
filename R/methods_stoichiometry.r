rodeo$methods( stoichiometry = function(values=NULL) {
  "Returns the stoichiometry information as a matrix of character expressions (if
   \\code{values} is \\code{NULL}) or numeric data (if values of variables,
   parameters, and time are specified as a named vector in \\code{values})."

  # Build the matrix of expressions
  m= matrix("0", ncol=nrow(.self$.vars), nrow=nrow(.self$.pros))
  colnames(m)= .self$.vars$name
  rownames(m)= .self$.pros$name
  for (i in 1:nrow(.self$.stoi)) {
    m[.self$.stoi$process[i], .self$.stoi$variable[i]]= .self$.stoi$expression[i]
  }

  # Return the matrix of expressions if no values are supplied ...
  if (is.null(values)) {
    return(m)
  # ... or return the numeric matrix otherwise
  } else {
    # Check supplied values
    if (is.null(names(values)) || any(names(values) == ""))
      stop("missing element name(s) in vector 'values'")
    if (any(duplicated(names(values))))
      stop("duplicated element name(s) in vector 'values'")
    if (!all(is.numeric(values)))
      stop("non-numeric element(s) in 'values'")
    if (!all(is.finite(values)))
      stop("non-finite element(s) in 'values'")
    # Create environment holding all data -> required for evaluating expressions
    env= new.env()
    f=tempfile()
    write.table(file=f, x=data.frame(names(values),values,stringsAsFactors=FALSE),
      sep="=", col.names=FALSE, row.names=FALSE, quote=FALSE)
    sys.source(file=f,envir=env)
    # Create numeric matrix
    mnum= matrix(0, ncol=ncol(m), nrow=nrow(m))
    colnames(mnum)= colnames(m)
    rownames(mnum)= rownames(m)
    for (ic in 1:ncol(m)) {
      for (ir in 1:nrow(m)) {
        tryCatch({
          mnum[ir,ic]= eval(parse(text=m[ir,ic]), envir=env)  # evaluated in created env
        }, error= function(e) {
          stop(paste0("failed to compute stoichiometry factor for variable '",
            colnames(m)[ic],"' and process '",rownames(m)[ir],"'; details: ",e))
        })
      }
    }
    return(mnum)
  }
})

