
#' Return library name
#'
#' Return the pure name of the shared library for use with
#    \code{\link[deSolve]{deSolve}} or \code{\link[rootSolve]{rootSolve}} methods.
#'
#' @name libName
#'
#' @return The base name of the shared library file created with
#'   \code{\link{compile}} after stripping of the the platform specific
#'   extension. This name has to be supplied as the \code{dllname} argument of
#'   the solver methods in \code{\link[deSolve]{deSolve}} or
#'   \code{\link[rootSolve]{rootSolve}}.
#'
#' @author \email{david.kneis@@tu-dresden.de}

rodeo$set("public", "libName", function() {
  if (length(private$lib) > 0) {
    return(private$lib["name"])
  } else {
    stop("library has not been created yet")
  }
})

#' Return name of library function
#'
#' Return the name of the library function for use with
#    \code{\link[deSolve]{deSolve}} or \code{\link[rootSolve]{rootSolve}} methods.
#'
#' @name libFunc
#'
#' @return The name of the function to compute derivatives which is contained in
#'   the library built with \code{\link{compile}}. This name has to be supplied as
#'   the \code{func} argument of the solver methods
#'   in \code{\link[deSolve]{deSolve}} or \code{\link[rootSolve]{rootSolve}}.
#'
#' @author \email{david.kneis@@tu-dresden.de}

rodeo$set("public", "libFunc", function() {
  if (length(private$lib) > 0) {
    return(private$lib["func"])
  } else {
    stop("library has not been created yet")
  }
})

