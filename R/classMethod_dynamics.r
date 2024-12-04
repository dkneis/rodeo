#' Numerical Integration
#'
#' Compute a dynamic solution with the numerical algorithms from package
#'   \code{\link[deSolve]{deSolve}}.
#'
#' @name dynamics
#'
#' @param times Times of interest (numeric vector).
#' @param fortran Switch between compiled Fortran and R code (logical). Default
#'   is \code{FALSE} (was \code{TRUE} until package version 0.8.5).
#' @param proNames Assign names to output columns holding the process rates?
#'   Default is \code{TRUE}.
#' @param ... Auxiliary arguments passed to \code{\link[deSolve]{ode}}.
#'   See notes below.
#'
#' @return The matrix returned by the integrator (see \code{\link[deSolve]{ode}}).
#'
#' @note This method can only be used after \code{\link{compile}} has been
#'   called.
#'
#' The \code{...} argument should \emph{not} be used to assign values to any of 
#'   \code{y}, \code{parms}, \code{times}, \code{func}. If \code{fortran} is
#'   \code{TRUE} it should also not assign values to \code{dllname}, 
#'   \code{nout}, or \code{outnames}. All these arguments of
#'   \code{\link[deSolve]{ode}} get their appropriate values automatically.
#'
#' @author \email{david.kneis@@tu-dresden.de}
#'
#' @seealso Use \code{\link{step}} for integration over a single time step
#'   with a built-in, Fortran-based solver.

rodeo$set("public", "dynamics",
  function(times, fortran=FALSE, proNames=TRUE, ...)
{
  if (any(c(!is.numeric(times), length(times) < 2)))
    stop("'times' must be a numeric vector of length > 1")
  if (fortran) {
    if (length(private$lib) == 0)
      stop("Fortran library not available; need to call 'compile' first")
    out <- deSolve::ode(y=self$getVars(),
      parms=self$getPars(),
      times=times,
      func=self$libFunc(),
      dllname=self$libName(),
      nout=self$lenPros()*prod(self$getDim()),
      outnames=if (proNames) elNames(self$namesPros(),self$getDim()) else NULL,
      ...
    )
  } else {
    if (is.null(self$func))
      stop("R function not available; need to call 'compile' first")
    out <- deSolve::ode(y=self$getVars(),
      parms=self$getPars(),
      times=times,
      func=self$func,
      ...
    )
    colnames(out) <- c("time", elNames(self$namesVars(),self$getDim()),
      elNames(self$namesPros(),self$getDim()))
  }
  out
})

