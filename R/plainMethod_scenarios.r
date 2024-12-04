#' Run a zero-dimensional model, possibly for multiple scenarios
#'
#' The function executes a 0D model for one to many scenarios and optionally
#' visualizes the outcome. Individual scenarios can differ by the initial state
#' or the values of parameters.
#'
#' @param model A model object of class \code{rodeo} as returned, for example,
#'   by the \code{\link{buildFromWorkbook}} function. The model must be
#'   zero-dimensional (typical simultaneous ODE). See the examples below.
#' @param scenarios Either \code{NULL} or a named list, each element of which
#'   defines a scenario to be considered. In the latter case, list elements must
#'   be (named) numeric vectors used to update the initial values and parameters
#'   provided as defaults in the workbook from which the model was imported.
#'   The vectors for the individual scenarios can differ in length but their
#'   length could also be consistent across scenarios.
#'   If \code{scenarios} is set to \code{NULL}, only the
#'   default scenario will be run. See details and examples.
#' @param times Numeric vector defining the times for which the future
#'   state of the system is computed.
#' @param plot.vars Logical. Plot the dynamics of all state variables?
#' @param plot.pros Logical. Plot the dynamics of process rates?
#' @param leg Keyword to set the position of the legend (if plots are created).
#' @param mar Numeric vector of length 4 to control figure margins. See the
#'   \code{mar} tag of \code{\link[graphics]{par}}.
#' @param ... Possible optional arguments passed to the numerical solver,
#'   namely \code{\link[deSolve]{lsoda}}. Can be used, for example, to limit
#'   the maximum step size through the \code{hmax} argument if boundary
#'   vary on short time scales.
#'
#' @return A data frame with at least three columns and one row for each
#'   time requested via the \code{times} argument. The first column
#'   indicates the scenario, the second column holds the time, and further
#'   columns hold the values of state variables and process rates.
#'
#' @note If the \code{scenarios} argument is used to update initial values and /
#' or parameters, the following applies: For each parameter not being included
#'   (by name) in the vector for a particular scenario, the default value
#'   will be used (as stored in the workbook from which the model was imported).
#'   The same is true for the initial values of variables. See the examples
#'   below.
#'
#' @seealso Look at \code{\link{buildFromWorkbook}} for how to create the
#'   necessary input for the \code{model} argument.
#'
#' @author David Kneis \email{david.kneis@@tu-dresden.de}
#'
#' @export
#'
#' @examples
#'
#' # build the model
#' m <- buildFromWorkbook(system.file("models/SEIR.xlsx", package="rodeo"))
#' 
#' # run with defaults
#' x <- scenarios(model=m, times=0:30, scenarios=NULL)
#' 
#' # run with updated values 
#' x <- scenarios(model=m, times=0:30,
#'   scenarios=list(default=c(t=1, i=0.2, r=0.4), fast=c(t=1, i=0.5, r=0.1))
#' )

scenarios <- function(model, times, scenarios=NULL,
  plot.vars=TRUE, plot.pros=FALSE, leg="topright",
  mar=c(4.5, 4.1, 1, 1), ...) {
  # check inputs
  if (class(model)[1] != "rodeo")
    stop("object passed as 'model' must be of class 'rodeo'")
  if (!identical(model$getDim(), as.integer(1)))
    stop("only zero-dimensional models are currently supported")
  if (!(is.null(scenarios) || is.list(scenarios)))
    stop("'scenarios' must be a list or NULL")
  all.named <- function(x) {!is.null(names(x)) && all(names(x) != "")}
  if (is.list(scenarios) && !all.named(scenarios))
    stop("if 'scenarios' is a list, elements must be named")
  if (is.list(scenarios) && !all(sapply(scenarios, is.numeric)))
    stop("if 'scenarios' is a list, elements must be numeric vectors")
  if (is.list(scenarios) && !all(sapply(scenarios, all.named)))
    stop("if 'scenarios' is a list of numeric vectors, all vector elements must be named")
  if ((length(times) < 2) || (!is.numeric(times)))
    stop("'times' must be a numeric vector of length >= 2")  
  # get defaults
  v.def <- stats::setNames(model$getVarsTable()[,"default"], model$getVarsTable()[,"name"])
  p.def <- stats::setNames(model$getParsTable()[,"default"], model$getParsTable()[,"name"])
  # initialize results
  out <- NULL
  # process scenarios
  if (is.null(scenarios)) {
    model$setVars(v.def)
    model$setPars(p.def)
    out <- rbind(out, data.frame(scenario="default",
      model$dynamics(times=times, fortran=FALSE, ...)))
  } else {
    for (s in names(scenarios)) {
      # set defaults first
      v.scn <- v.def
      p.scn <- p.def
      # update initial values
      for (item.name in names(scenarios[[s]])) {
        if (item.name %in% names(p.scn)) {
          p.scn[item.name] <- scenarios[[s]][[item.name]]
        } else if (item.name %in% names(v.scn)) {
          v.scn[item.name] <- scenarios[[s]][[item.name]]
        } else {
          stop(paste0("a value has been specified for item '",item.name,
            "' in scenario '",s,"' but the model has no parameter or state ",
            "variable of that name"))
        }
      }
      model$setVars(v.scn)
      model$setPars(p.scn)
      out <- rbind(out, data.frame(scenario=s,
        model$dynamics(times=times, fortran=FALSE, ...)))
    }
  }
  # plot if requested
  if (plot.vars || plot.pros) {
    omar <- graphics::par("mar")
    graphics::par(mar=mar)
    items <- c(if (plot.vars) model$namesVars() else c(),
      if (plot.pros) model$namesPros() else c())
    nc <- if (length(items) == 1) 1 else if (length(items) <= 6) 2 else 3
    nr <- ceiling(length(items) / nc)
    graphics::par(mfrow=c(nr, nc))
    if (is.null(scenarios)) {
      clr <- c(default="black")
    } else {
      clr <- stats::setNames(grDevices::colorRampPalette(c("steelblue4",
        "khaki3", "darkorange"))(length(scenarios)), names(scenarios))
    }
    for (it in items) {
      plot(range(times), range(out[,it]), type="n", xlab="time", ylab=it) 
      fn <- function(x, item) {
        graphics::lines(x[,"time"], x[,item], col=clr[unique(x[,"scenario"])])
      }
      by(out, out[,"scenario"], fn, item=it)
    }
    graphics::legend(leg, bty="n", lty=1, col=clr, legend=names(clr))
    graphics::par(mfrow=c(1,1))
    graphics::par(mar=omar)
  }
  # return simulation output for further processing
  out
}