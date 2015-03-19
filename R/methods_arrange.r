
# Converts a list into vector after sorting/selecting columns and data recycling
arrangeGeneric= function(x, itemNames, warnUnused=TRUE) {
  # Check inputs
  if (!is.list(x))
    stop("argument 'x' must be a list")
  bad= itemNames[!(itemNames %in% names(x))]
  if (length(bad) > 0)
    stop(paste0("list 'x' does not provide data for the following",
      " item(s): '",paste(bad,collapse="', '"),"'"))
  if (warnUnused) {
    bad= names(x)[!(names(x) %in% itemNames)]
    if (length(bad) > 0)
      warning(paste0("the following element(s) from list 'x' were ignored: '",
        paste(bad,collapse="', '"),"'"))
  }
  # Transform into data frame to get the same number of levels for all variables
  sizes= unique(sapply(x, length))
  if ((length(sizes) > 2) || ((length(sizes) == 2) && (min(sizes) != 1)))
    stop("elements of list 'x' must either be scalars or vectors of a common length")
  x= data.frame(x)
  # Sort/drop columns
  x= x[,itemNames]
  # Transform into vector
  x= unlist(x)
  # Check type
  if (any(!is.numeric(x)))
    stop("non-numeric data detected in list 'x'")
  # Done
  return(x)
}

rodeo$methods( arrangeVars= function(x, warnUnused=TRUE) {
  "Create vector of state variables from list \\code{x}. The list elements can
  either be vectors (with length according to the number of spatial levels) or
  scalars. The latter are recycled for all spatial levels."
  arrangeGeneric(x=x, itemNames=.self$VARS$name, warnUnused=warnUnused)
})

rodeo$methods( arrangePars= function(x, warnUnused=TRUE) {
  "Create vector of parameters from list \\code{x}. The list elements can
  either be vectors (with length according to the number of spatial levels) or
  scalars. The latter are recycled for all spatial levels."
  arrangeGeneric(x=x, itemNames=.self$PARS$name, warnUnused=warnUnused)
})

