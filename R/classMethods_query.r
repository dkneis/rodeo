
#' Query Values of State Variables
#'
#' Query values of state variables of a \code{\link{rodeo}}-based model.
#'
#' @name queryVars
#'
#' @param asMatrix Logical. If \code{TRUE}, data are returned as a matrix with
#'   columns named after the variables and one row per spatial section. If
#'   \code{FALSE}, a vector of the concatenated columns is returned. In that
#'   case, element names follow the pattern 'variable.section', i.e. the
#'   section index is appended to the variable name, separated by a period."
#'
#' @return A numeric matrix or vector, depending on the value of \code{asMatrix}.
#'
#' @author \email{david.kneis@@tu-dresden.de}
#'
#' @seealso Other \code{\link{rodeo}} class methods
#'
#' @examples
#' data(exampleIdentifiers, exampleProcesses, exampleStoichiometry)
#' model <- rodeo$new(
#'   vars=subset(exampleIdentifiers, type=="v"),
#'   pars=subset(exampleIdentifiers, type=="p"),
#'   funs=subset(exampleIdentifiers, type=="f"),
#'   pros=exampleProcesses, stoi=exampleStoichiometry,
#'   size=2
#' )
#' model$assignVars(cbind(c_z=c(1,1), c_do=c(9,9), v=c(1e6, 1e6)))
#' print(model$queryVars())
#' print(model$queryVars(asMatrix=TRUE))

rodeo$set("public", "queryVars", function(asMatrix=FALSE) {
  if (any(is.na(private$v)))
    warning("NA values are present in the state variables' values.")
  if (asMatrix)
    private$v
  else
    setNames(as.vector(private$v),
      paste(rep(colnames(private$v), each=private$sections),
      rep(1:private$sections, ncol(private$v)) , sep="."))
})

#' Query Values of Parameters
#'
#' Query values of parameters of a \code{\link{rodeo}}-based model.
#'
#' @name queryPars
#'
#' @param asMatrix Logical. If \code{TRUE}, data are returned as a matrix with
#'   columns named after the parameters and one row per spatial section. If
#'   \code{FALSE}, a vector of the concatenated columns is returned. In that
#'   case, element names follow the pattern 'parameter.section', i.e. the
#'   section index is appended to the parameter name, separated by a period."
#'
#' @return A numeric matrix or vector, depending on the value of \code{asMatrix}.
#'
#' @author \email{david.kneis@@tu-dresden.de}
#'
#' @seealso Other \code{\link{rodeo}} class methods
#'
#' @examples
#' # see example for method 'queryVars'
rodeo$set("public", "queryPars", function(asMatrix=FALSE) {
  "Returns the numeric values of parameters. See \\code{\\link{queryPars}}
 for details."
  if (any(is.na(private$p)))
    warning("NA values are present in the parameters' values.")
  if (asMatrix)
    private$p
  else
    setNames(as.vector(private$p),
      paste(rep(colnames(private$p), each=private$sections),
      rep(1:private$sections, ncol(private$p)) , sep="."))
})

