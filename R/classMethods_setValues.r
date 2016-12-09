
#' Assign Values to State Variables
#'
#' Assign values to state variables of a \code{\link{rodeo}}-based model.
#'
#' @name setVars
#'
#' @param x A numeric vector or array, depending on the model's spatial
#'   dimensions. See details below.
#'
#' @return \code{NULL} (invisible). The assigned numeric data are stored in the
#'   object and can be accessed by the \code{\link{getVars}} method.
#'
#' @note For a 0-dimensional model (i.e. a model without spatial resolution),
#'   \code{x} must be a numeric vector whose length equals the number of state
#'   variables. The element names of \code{x} must match those returned by the
#'   object's \code{namesVars} method. See the examples for how to bring the
#'   vector elements into required order.
#'
#'   For models with a spatial resolution, \code{x} must be a numeric array of
#'   proper dimensions. The last dimension (cycling slowest) corresponds to the
#'   variables and the first dimension (cycling fastest) corresponds to the
#'   models' highest spatial dimension. Thus, \code{dim(x)} must be equal to
#'   \code{c(rev(obj$getDim()), obj$namesVars())} where \code{obj} is the object
#'   whose variables are to be assigned. The names of the array's last dimension
#'   must match the return value of  \code{obj$namesVars()}.
#'
#'   In the common 1-dimensional case, this just means that \code{x} must be a
#'   matrix with column names matching the return value of
#'   \code{obj$namesVars()} and as many rows as given by \code{obj$getDim()}.
#'
#' @author \email{david.kneis@@tu-dresden.de}
#'
#' @seealso The corresponding 'get' method is \code{\link{getVars}}. Use
#'   \code{\link{setPars}} to assign values to parameters rather than variables.
#'
#' @examples
#' data(vars, pars, funs, pros, stoi)
#' x0 <- c(bac=0.1, sub=0.5)
#'
#' # 0-dimensional model
#' model <- rodeo$new(vars, pars, funs, pros, stoi, dim=c(1))
#' model$setVars(x0)
#' print(model$getVars())
#'
#' # How to sort vector elements
#' x0 <- c(sub=0.5, bac=0.1)              # doesn't match order of variables
#' model$setVars(x0[model$namesVars()])
#'
#' # 1-dimensional model with 3 boxes
#' nBox <- 3
#' model <- rodeo$new(vars, pars, funs, pros, stoi, dim=c(nBox))
#' x1 <- matrix(rep(x0, each=nBox), nrow=nBox, ncol=model$lenVars(),
#'   dimnames=list(NULL, model$namesVars()))
#' model$setVars(x1)
#' print(model$getVars())
#' print(model$getVars(asArray=TRUE))
#'
#' # 2-dimensional model with 3 x 4 boxes
#' model <- rodeo$new(vars, pars, funs, pros, stoi, dim=c(3,4))
#' x2 <- array(rep(x0, each=3*4), dim=c(4,3,model$lenVars()),
#'   dimnames=list(dim2=NULL, dim1=NULL, variables=model$namesVars()))
#' model$setVars(x2)
#' print(model$getVars())
#' print(model$getVars(asArray=TRUE))


rodeo$set("public", "setVars", function(x) {
  # zero-dimensional case
  if (identical(private$dim, as.integer(1))) {
    if (is.vector(x) &&
      identical(length(x), length(private$vars)) &&
      identical(names(x), private$varsTbl$name)) {
      private$vars <- as.numeric(x)
    } else {
      stop("'x' must be a vector with element names: '",
        paste(private$varsTbl$name, collapse="', '"),"'")
    }
  # one- or multi-dimensional case
  } else {
    if (is.array(x) &&
      identical(length(x), length(private$vars)) &&
      identical(dim(x), c(rev(private$dim), nrow(private$varsTbl))) &&
      identical(dimnames(x)[[length(dim(x))]], private$varsTbl$name)) {
      private$vars <- as.numeric(x)
    } else {
      stop("'x' must be an array with dimensions [",
        paste(c(rev(private$dim), nrow(private$varsTbl)), collapse="/"),
        "] and the names of the last dimension should be: '",
        paste(private$varsTbl$name, collapse="', '"),"'")
    }
  }
  return(invisible(NULL))
})

#' Assign Values to Parameters
#'
#' Assign values to parameters of a \code{\link{rodeo}}-based model.
#'
#' @name setPars
#'
#' @param x A numeric vector or array, depending on the model's spatial
#'   dimensions. Consult the help page of the sister method
#'   \code{\link{setVars}} for details on the required input.
#'
#' @return \code{NULL} (invisible). The assigned numeric data are stored in the
#'   object and can be accessed by the \code{\link{getPars}} method.
#'
#' @note Look at the notes and examples for the \code{\link{setVars}} method.
#'
#' @author \email{david.kneis@@tu-dresden.de}
#'
#' @seealso The corresponding 'get' method is \code{\link{getPars}}. Use
#'   \code{\link{setVars}} to assign values to variables rather than parameters.
#'   Consult the help page of the latter function for examples.

rodeo$set("public", "setPars", function(x) {
  # zero-dimensional case
  if (identical(private$dim, as.integer(1))) {
    if (is.vector(x) &&
      identical(length(x), length(private$pars)) &&
      identical(names(x), private$parsTbl$name)) {
      private$pars <- as.numeric(x)
    } else {
      stop("'x' must be a vector with element names: '",
        paste(private$parsTbl$name, collapse="', '"),"'")
    }
  # one- or multi-dimensional case
  } else {
    if (is.array(x) &&
      identical(length(x), length(private$pars)) &&
      identical(dim(x), c(rev(private$dim), nrow(private$parsTbl))) &&
      identical(dimnames(x)[[length(dim(x))]], private$parsTbl$name)) {
      private$pars <- as.numeric(x)
    } else {
      stop("'x' must be an array with dimensions [",
        paste(c(rev(private$dim), nrow(private$parsTbl)), collapse="/"),
        "] and the names of the last dimension should be: '",
        paste(private$parsTbl$name, collapse="', '"),"'")
    }
  }
  return(invisible(NULL))
})


