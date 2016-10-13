
# Helper function to construct names for vector elements
elNames <- function(items, dims) {
  if (sum(dims) == 1) {
    elNames <- items
  } else {
    # Note: In the output of expand.grid, the first factor varies fastest
    nameParts <- expand.grid(lapply(dims[length(dims):1], function(x){1:x}))
    nameParts <- nameParts[,ncol(nameParts):1]
    nameParts <- cbind(rep(items, each=prod(dims)), nameParts)
    elNames <- apply(nameParts, 1, paste, collapse=".")
  }
}

#' Query Values of State Variables
#'
#' Query values of state variables of a \code{\link{rodeo}}-based model.
#'
#' @name getVars
#'
#' @param asArray Logical. If \code{FALSE}, the values of variables are returned
#'  as vector irrespective of the model's spatial resolution. If \code{TRUE},
#'  the values are returned as an \code{\link[base]{array}} with properly named
#'  dimensions. The array's last dimension represents the variables and its
#'  first (fastest cycling) dimension, if any, refers to the model's highest
#'  spatial dimension.
#' @param useNames Logical. Used to enable/disable element names for the return
#'  vector when \code{asArray} is \code{FALSE}. The names follow the pattern
#'  'x.i.j' where 'x' is the variable name and 'i', 'j' are indices of the
#'  sub-units in the first and second spatial dimension. The actual suffix is
#'  controlled by the number of dimensions and in the 0-dimensional case, no
#'  suffix is applied at all, i.e. the pure variable names are used to label the
#'  elements of the vector.
#'  If \code{isArray} is \code{TRUE}, this argument is simply ignored, hence
#'  the dimensions of a returned array are always named.
#'
#' @return A numeric vector or array.
#'
#' @author \email{david.kneis@@tu-dresden.de}
#'
#' @seealso The corresponding 'set' method is \code{\link{setVars}} and examples
#'  can be found there. Use \code{\link{getPars}} to query the values of
#'  parameters rather than variables.

rodeo$set("public", "getVars", function(asArray=FALSE, useNames=TRUE) {
  if (!asArray) {
    if (!useNames) {
      return(private$vars)
    } else {
      return(setNames(private$vars, elNames(private$varsTbl$name, private$dim)))
    }
  } else {
    if (sum(private$dim) == 1) {
      return(array(private$vars, dim=nrow(private$varsTbl),
        dimnames=list(variable=private$varsTbl$name)))
    } else {
      reverseDims <- private$dim[length(private$dim):1]
      namesForDims <- lapply(reverseDims, function(i){1:i})
      names(namesForDims) <- paste("dimension", length(private$dim):1, sep=".")
      return(array(private$vars, dim= c(reverseDims, nrow(private$varsTbl)),
        dimnames= c(namesForDims, list(variable=private$varsTbl$name))))
    }
  }
})

#' Query Values of Parameters
#'
#' Query values of parameters of a \code{\link{rodeo}}-based model.
#'
#' @name getPars
#'
#' @param asArray Logical. If \code{FALSE}, the values of parameters are returned
#'  as vector irrespective of the model's spatial resolution. If \code{TRUE},
#'  the values are returned as an \code{\link[base]{array}} with properly named
#'  dimensions. The array's last dimension represents the parameters and its
#'  first (fastest cycling) dimension, if any, refers to the model's highest
#'  spatial dimension.
#' @param useNames Logical. Used to enable/disable element names for the return
#'  vector when \code{asArray} is \code{FALSE}. The names follow the pattern
#'  'x.i.j' where 'x' is the parameter name and 'i', 'j' are indices of the
#'  sub-units in the first and second spatial dimension. The actual suffix is
#'  controlled by the number of dimensions and in the 0-dimensional case, no
#'  suffix is applied at all, i.e. the pure parameter names are used to label the
#'  elements of the vector.
#'  If \code{isArray} is \code{TRUE}, this argument is simply ignored, hence
#'  the dimensions of a returned array are always named.
#'
#' @return A numeric vector or array.
#'
#' @author \email{david.kneis@@tu-dresden.de}
#'
#' @seealso The corresponding 'set' method is \code{\link{setPars}} and examples
#'  can be found there. Use \code{\link{getVars}} to query the values of
#'  variables rather than parameters.

rodeo$set("public", "getPars", function(asArray=FALSE, useNames=TRUE) {
  if (!asArray) {
    if (!useNames) {
      return(private$pars)
    } else {
      return(setNames(private$pars, elNames(private$parsTbl$name, private$dim)))
    }
  } else {
    if (sum(private$dims) == 1) {
      return(array(private$pars, dim=nrow(private$parsTbl),
        dimnames=list(parameter=private$parsTbl$name)))
    } else {
      reverseDims <- private$dim[length(private$dim):1]
      namesForDims <- lapply(reverseDims, function(i){1:i})
      names(namesForDims) <- paste("dimension", length(private$dim):1, sep=".")
      return(array(private$pars, dim= c(reverseDims, nrow(private$parsTbl)),
        dimnames= c(namesForDims, list(parameter=private$parsTbl$name))))
    }
  }
})

