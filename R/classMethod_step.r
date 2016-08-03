#' Numerical Integration Over a Single Time Step
#'
#' Performs integration over a single time step using a built-in ODE solver.
#' At present, a single solver is implement with limited options. The interface
#' of this method may change when support for other solvers is added.
#'
#' @name step
#'
#' @param t0 Numeric. Initial time.
#' @param h Numeric. Length of time step of interest.
#' @param hmin Minimum tolerated internal step size. The default of \code{NULL}
#'   sets this to 10 times the value of \code{.Machine$double.eps}.
#' @param maxsteps Maximum tolerated number of sub-steps.
#' @param tol Numeric. Relative accuracy requested. This is currently a global value, i.e.
#'   one cannot set the accuracy per state variable.
#' @param method String. Currently, 'rk5' is the only method implemented. This is
#'   a Runge-Kutta Cash-Karp solver adapted from Press et al. (2002), Numerical
#'   recipes in Fortran 90. It is designed to handle non-stiff problems only.
#' @param check Logical. Can be used to avoid repeated checks of arguments. This
#'   may increase performance in repeated calls.
#'
#' @return A numeric matrix (with column names). There is one column for each
#'   state variable and process rate. The number of rows equals the number of
#'   spatial sections, i.e. the result is a single-column matrix in case of a
#'   zero-dimensional model.
#'
#' @author \email{david.kneis@@tu-dresden.de}
#'
#' @seealso Use \code{\link[deSolve]{deSolve}} for advanced solvers with more
#'   options and capabilities to handle stiff problems.

rodeo$set("public", "step",
  function(t0, h, hmin=NULL, maxsteps=5000, tol=1e-6, method="rk5", check=TRUE)
{
  if (check) {
    if (any(c(!is.numeric(t0), length(t0) != 1)))
      stop("'t0' must be a numeric scalar")
    if (any(c(!is.numeric(h), length(h) != 1), h <= .Machine$double.eps))
      stop("'h' must be a numeric scalar greater than zero")
    if (!is.null(hmin) || (!any(c(!is.numeric(hmin), length(hmin) != 1))))
      stop("'hmin' must be a numeric scalar or NULL")
    if (any(c(!is.integer(as.integer(maxsteps)), length(maxsteps) != 1, maxsteps <= 0)))
      stop("'maxsteps' must be a positive scalar integer")
    if (any(c(!is.numeric(tol), length(tol) != 1)))
      stop("'tol' must be a numeric scalar")
    if (any(c(length(method) != 1, !method %in% names(private$steppers))))
      stop("'method' must be the name of a supported integration method which",
        " has been initialized")
  }
  if (is.null(hmin))
    hmin <- 10 * .Machine$double.eps
  # rk5 method
  if (method == "rk5") {
    res <- .Fortran(private$steppers[["rk5"]]$fncSymb,
      error=as.integer(0),
      var=as.numeric(private$v),
      x1=as.double(t0),
      x2=as.double(t0 + h),
      eps=as.double(tol),
      h1=as.double(h),
      hmin=as.double(hmin),
      nmax=as.integer(maxsteps),
      p=as.numeric(private$p),
      pro=rep(as.double(0), nrow(private$prosTbl)*private$sections)
    )
    if (as.logical(res$error))
      stop("integration from t0=",t0," over dt=",dt," using method '",method,"' failed")
    return(matrix(c(res$var, res$pro),
      ncol=nrow(private$varsTbl)+nrow(private$prosTbl), nrow=private$sections,
      dimnames= list(NULL, c(private$varsTbl$name, private$prosTbl$name)))
    )
  } else {
    stop("integration method '",method,"' not implemented")
  }
})

