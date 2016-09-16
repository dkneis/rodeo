
#' Query Values of State Variables
#'
#' Query values of state variables of a \code{\link{rodeo}}-based model.
#'
#' @name getVars
#'
#' @param asMatrix Logical. If \code{TRUE}, data are returned as a matrix with
#'   columns named after the variables and one row per spatial section. If
#'   \code{FALSE}, a vector of the concatenated columns is returned.
#'   In the latter case, element names follow the pattern
#'   'variable.section', i.e. the section index is appended to the variable's
#'   name, separated by a period. If there is a single section only (0D models),
#'   the pure variable names are used, i.e. the extension '.1' is omitted.
#'
#' @return A numeric matrix or vector, depending on the value of \code{asMatrix}.
#'
#' @author \email{david.kneis@@tu-dresden.de}
#'
#' @seealso The corresponding 'set' method is \code{\link{setVars}}. Use
#'   \code{\link{getPars}} to query the values of parameters rather than variables.
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
#' model$setVars(cbind(c_z=c(1,1), c_do=c(9,9), v=c(1e6, 1e6)))
#' print(model$getVars())
#' print(model$getVars(asMatrix=TRUE))

rodeo$set("public", "getVars", function(asMatrix=FALSE) {
  if (asMatrix)
    private$v
  else {
    elNames <- if (private$sections == 1) colnames(private$v) else
      paste(rep(colnames(private$v), each=private$sections),
      rep(1:private$sections, ncol(private$v)) , sep=".")
    setNames(as.vector(private$v), elNames)
  }
})

#' Query Values of Parameters
#'
#' Query values of parameters of a \code{\link{rodeo}}-based model.
#'
#' @name getPars
#'
#' @param asMatrix Logical. If \code{TRUE}, data are returned as a matrix with
#'   columns named after the parameters and one row per spatial section. If
#'   \code{FALSE}, a vector of the concatenated columns is returned.
#'   In the latter case, element names follow the pattern
#'   'parameter.section', i.e. the section index is appended to the parameter's
#'   name, separated by a period. If there is a single section only (0D models),
#'   the pure parameter names are used, i.e. the extension '.1' is omitted.
#'
#' @return A numeric matrix or vector, depending on the value of \code{asMatrix}.
#'
#' @author \email{david.kneis@@tu-dresden.de}
#'
#' @seealso The corresponding 'set' method is \code{\link{setPars}}. Use
#'   \code{\link{getVars}} to query the values of variables rather than parameters.
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
#' x <- c(kd=5.78e-7, h_do=0.5, s_do_z=2.76,
#'   wind=1, depth=2, temp=20, q_in=1, q_ex=1)
#' x <- rbind(x, x)
#' model$setPars(x)
#' print(model$getPars())
#' print(model$getPars(asMatrix=TRUE))

rodeo$set("public", "getPars", function(asMatrix=FALSE) {
  "Returns the numeric values of parameters. See \\code{\\link{getPars}}
 for details."
  if (asMatrix)
    private$p
  else {
    elNames <- if (private$sections == 1) colnames(private$p) else
      paste(rep(colnames(private$p), each=private$sections),
      rep(1:private$sections, ncol(private$p)) , sep=".")
    setNames(as.vector(private$p), elNames)
  }
})

