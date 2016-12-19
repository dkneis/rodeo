#' Numerical Integration
#'
#' Compute a dynamic solution with the numerical algorithms from package
#'   \code{\link[deSolve]{deSolve}} employing compiled Fortran code.
#'
#' @name dynamics
#'
#' @param times Times of interest (numeric vector).
#' @param ... Auxiliary arguments passed to \code{\link[deSolve]{ode}}.
#'   See notes below.
#'
#' @return The matrix returned by the integrator (see \code{\link[deSolve]{ode}}).
#'
#' @note This method can only be used after Fortran code was generated and
#'   compiled with \code{\link{compile}}.
#'
#' The \code{...} argument should \emph{not} be used to assign values to any of 
#'   \code{y}, \code{parms}, \code{times}, \code{func}, \code{dllname}, 
#'   \code{nout}, \code{outnames}. These arguments of \code{\link[deSolve]{ode}}
#'   get their appropriate values automatically.
#'
#' @author \email{david.kneis@@tu-dresden.de}
#'
#' @seealso Use \code{\link{step}} for integration over a single time step
#'   with a built-in solver.

rodeo$set("public", "dynamics",
  function(times, ...)
{
  if (any(c(!is.numeric(times), length(times) < 2)))
    stop("'times' must be a numeric vector of length > 1")
  if (length(private$lib) == 0)
    stop("'compile' needs to be called before this method can be used")
  deSolve::ode(y=self$getVars(),
    parms=self$getPars(),
    times=times,
    func=self$libFunc(),
    dllname=self$libName(),
    nout=self$lenPros()*prod(self$getDim()),
    outnames=elNames(self$namesPros(),self$getDim()),
    ...
  )
})

