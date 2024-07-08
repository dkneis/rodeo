#' \code{rodeo} Class
#'
#' This documents the \code{rodeo} class to represent an ODE-based
#' model. See the \code{\link{rodeo-package}} main page or type
#' \code{help(package="rodeo")} for an introduction to the package of
#' the same name.
#'
#' @name rodeo-class
#'
#' @field prosTbl A data frame with fields 'name', 'unit', 'description', and
#'   'expression' defining the process rates.
#' @field stoiTbl A data frame with fields 'variable', 'process', and 'expression'
#'   reprenting the stoichiometry matrix in data base format.
#' @field varsTbl A data frame with fields 'name', 'unit', 'description' declaring
#'   the state variables of the model. The declared names become valid
#'   identifiers to be used in the expression fields of \code{prosTbl} or \code{stoiTbl}.
#' @field parsTbl A data frame of the same structure as \code{vars} declaring the
#'   parameters of the model. The declared names become valid
#'   identifiers to be used in the expression fields of \code{prosTbl} or \code{stoiTbl}.
#' @field funsTbl A data frame of the same structure as \code{vars} declaring any
#'   functions referenced in the expression fields of \code{prosTbl} or \code{stoiTbl}.
#' @field dim Integer vector specifying the spatial dimensions.
#' @field vars Numeric vector, holding values of state variables.
#' @field pars Numeric vector, holding values of parameters.
#'
#' @section Class methods:
#'
#' For most of the methods below, a separate help page is available describing
#' its arguments and usage.
#'
#' \itemize{
#'   \item{\code{\link{initialize}}} Initialization method for new objects.
#'   \item{\code{namesVars, namesPars, namesFuns, namesPros}} Functions
#'     returning the names of declared state variables, parameters,
#'     functions, and processes, respectively (character vectors). No arguments.
#'   \item{\code{lenVars, lenPars, lenFuns, lenPros}} Functions
#'     returning the number of declared state variables, parameters, functions
#'     and processes, respectively (integer). No arguments.
#'   \item{\code{getVarsTable, getParsTable, getFunsTable, getProsTable,
#'     getStoiTable}} Functions returning the data frames used to initialize
#'     the object. No arguments
#'   \item{\code{getDim}} Returns the spatial dimensions as an integer vector.
#'     No arguments.
#'   \item{\code{\link{compile}}} Compiles a Fortran library for use with
#'     numerical methods from packages \code{\link[deSolve]{deSolve}} or
#'     \code{\link[rootSolve]{rootSolve}}.
#'   \item{\code{\link{generate}}} Translate the ODE-model specification into a
#'     function that computes process rates and the state variables' derivatives
#'     (either in R or Fortran). Consider to use the high-level method
#'     \code{\link{compile}}.
#'   \item{\code{\link{setVars}}} Assign values to state variables.
#'   \item{\code{\link{setPars}}} Assign values to parameters.
#'   \item{\code{\link{getVars}}} Returns the values of state variables.
#'   \item{\code{\link{getPars}}} Returns the values of parameters.
#'   \item{\code{\link{stoichiometry}}} Returns the stoichiometry matrix, either
#'     evaluated (numeric) or as text.
#'   \item{\code{\link{plotStoichiometry}}} Plots qualitative stoichiometry
#'     information.
#' }
#'
#' @seealso See the \code{\link{rodeo-package}} main page or type
#'   \code{help(package="rodeo")} to find the documentation of any non-class
#'   methods contained in the \code{rodeo} package.
#' 
#' @export
#'
#' @examples
#'
#' ## Example using high-level functions: Epidemiological SEIR model
#'
#' # Import model from workbook shipped with this package 
#' m <- buildFromWorkbook(system.file("models/SEIR.xlsx",
#'   package="rodeo"))
#' 
#' # Set parameters and initial state (defaults stored in workbook)
#' p <- m$getParsTable()
#' m$setPars(setNames(p$default, p$name))
#' 
#' v <- m$getVarsTable()
#' m$setVars(setNames(v$default, v$name))
#' 
#' # Run a dynamic simulations and print parts of the result
#' sim <- m$dynamics(time=1:30, fortran=FALSE)
#' print(head(sim))
#' print(tail(sim))
#' 
#'
#' ### Example using low-level functions: Bacterial growth in bioreactor
#'
#' # Creation of model object (data frames shipped as package data)
#' data(vars, pars, funs, pros, stoi)
#' model <- rodeo$new(vars, pars, funs, pros, stoi, dim=c(1))
#'
#' # Assignment of parameters and initial values
#' model$setPars(c(mu=0.8, half=0.1, yield= 0.1, vol=1000,
#'   flow=50, sub_in=1))
#' model$setVars(c(bac=0.01, sub=0))
#'
#' # Implementation of functions declared in 'funs'
#' monod <- function(c,h) {c/(c+h)}
#'
#' # Creation of derivatives function in a low-level way; calling
#' # the 'compile' method is a more convenient alternative
#' code <- model$generate(name="derivs", lang="r")
#' derivs <- eval(parse(text=code))
#'
#' # Explicit call of an integrator from the deSolve package
#' times <- 0:96
#' out <- deSolve::ode(y=model$getVars(), times=times, func=derivs,
#'   parms=model$getPars())
#' colnames(out) <- c("time", model$namesVars(), model$namesPros())
#' plot(out)

rodeo <- R6Class("rodeo",
  private = list(
    varsTbl=NA,
    parsTbl=NA,
    funsTbl=NA,
    prosTbl=NA,
    stoiTbl=NA,
    dim=integer(0),
    vars=numeric(0),
    pars=numeric(0),
    lib=character(0),
    steppers=list()
  ),
  public = list(
    func=NULL
  )
)

