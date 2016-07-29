
checkInputMatrix <- function(x, itemNames, sections) {
  if (!is.matrix(x))
    stop("expecting 'x' to be a numeric matrix or single-row vector")
  if (is.null(colnames(x)))
    stop("'x' is lacking column names")
  if (nrow(x) != sections)
    stop("number of rows in 'x' (",nrow(x),") does not match the object's",
      " number of spatial sections (",sections,")")
  bad= itemNames[!(itemNames %in% colnames(x))]
  if (length(bad) > 0)
    stop(paste0("'x' does not provide data for the following",
      " item(s): '",paste(bad,collapse="', '"),"'"))
  if (!all(is.numeric(x)))
    stop("non-numeric values detected in 'x'")
  return(invisible(NULL))
}

checkInputTabular <- function(x, itemNames, sections) {
  if (!is.data.frame(x))
    stop("expecting 'x' to be a data frame")
  required= c("name", "section", "value")
  if (!all(required %in% names(x)))
    stop("'x' must have columns '",paste(required, collapse="', '"),"'")
  x$section= as.integer(x$section)
  if (any((x$section < 1) || (x$section > sections)))
    stop("section indices in 'x' are in range [",min(x$section),",",max(x$section),
      "] but they should fall into [1,",sections,"]")
  x$name= as.character(x$name)
  bad= x$name[!(x$name %in% itemNames)]
  if (length(bad) > 0)
    stop(paste0("'x' contains data for unknown item(s): '",
      paste(bad,collapse="', '"),"'"))
  if (!all(is.numeric(x$value)))
    stop("non-numeric data detected in 'value' column of 'x'")
  return(invisible(NULL))
}

#' Assign Values to State Variables
#'
#' Assign values to state variables of a \code{\link{rodeo}}-based model.
#'
#' @name assignVars
#'
#' @param x A matrix, vector, or data frame holding the data to be assigned.
#'   The appropriate type of input depends on the value of \code{tabular}. See
#'   details below.
#' @param tabular If set to \code{FALSE}, then \code{x} must be a numeric matrix
#'   (or vector) holding data for \emph{all} items and sections. If \code{TRUE},
#'   then \code{x} must be a data frame with 3 columns 'name', 'section', and
#'   'value'. Use this to assign data to selected items and/or sections only.
#' @param check Logical. By default, several checks are carried out on the
#'   passed \code{x}. This can be turned off by setting \code{check} to
#'   \code{FALSE}. May be used to avoid unnecessary checks in repeated calls.
#'
#' @return \code{NULL} (invisible). The assigned numeric data are stored in the
#'   object and can be accessed by the \code{\link{queryVars}} method.
#'
#' @note If \code{tabular} is \code{FALSE}, the matrix passed as \code{x} must
#'   have column names correspond to the names of state variables. The number of
#'   rows is expect to match the object's number of spatial sections. In the
#'   common case of a single spatial section, \code{x} can also be a named
#'   vector instead of a single-row matrix.
#'
#' @author \email{david.kneis@@tu-dresden.de}
#'
#' @seealso Other \code{\link{rodeo}} class methods
#'
#' @examples
#' data(exampleIdentifiers, exampleProcesses, exampleStoichiometry)
#' model= new("rodeo",
#'   vars=subset(exampleIdentifiers, type=="v"),
#'   pars=subset(exampleIdentifiers, type=="p"),
#'   funs=subset(exampleIdentifiers, type=="f"),
#'   pros=exampleProcesses, stoi=exampleStoichiometry,
#'   sections=2
#' )
#' # Set all values at a time by passing a matrix
#' x= cbind(c_z=c(1,1), c_do=c(9,9), v=c(1e6,1e6))
#' model$assignVars(x)
#' print(model$queryVars(asMatrix=TRUE))
#' # Set selected values by passing a data frame
#' x= data.frame(name=c("c_z","c_do"), section=c(1,2), value=c(2,11),
#'   stringsAsFactors=FALSE)
#' model$assignVars(x, tabular=TRUE)
#' print(model$queryVars(asMatrix=TRUE))

rodeo$methods( assignVars= function(x, tabular=FALSE, check=TRUE) {
  "Assign values to state variables. See \\code{\\link{assignVars}} for details."
  if (!tabular) {
    if (is.vector(x))
      x <- matrix(x, nrow=1, dimnames=list(NULL, names(x)))
    if (check)
      checkInputMatrix(x, itemNames=.self$.vars$name, sections=.self$.sections)
    .v <<- x[,.self$.vars$name, drop=FALSE]
  } else {
    if (check)
      checkInputTabular(x, itemNames=.self$.vars$name, sections=.self$.sections)
    .v[(match(x$name, colnames(.v))-1)*nrow(.v) + x$section] <<- x$value
  }
  return(invisible(NULL))
})

#' Assign Values to Parameters
#'
#' Assign values to parameters of a \code{\link{rodeo}}-based model.
#'
#' @name assignPars
#'
#' @inheritParams assignVars
#'
#' @return \code{NULL} (invisible). The assigned numeric data are stored in the
#'   object and can be accessed by the \code{\link{queryPars}} method.
#'
#' @author \email{david.kneis@@tu-dresden.de}
#'
#' @seealso Other \code{\link{rodeo}} class methods
#'
#' @examples
#' # see the example for method 'assignVars' which behaves in the same way

rodeo$methods( assignPars= function(x, tabular=FALSE, check=TRUE) {
  "Assign values to parameters. See \\code{\\link{assignPars}} for details."
  if (!tabular) {
    if (is.vector(x))
      x <- matrix(x, nrow=1, dimnames=list(NULL, names(x)))
    if (check)
      checkInputMatrix(x, itemNames=.self$.pars$name, sections=.self$.sections)
    .p <<- x[,.self$.pars$name, drop=FALSE]
  } else {
    if (check)
      checkInputTabular(x, itemNames=.self$.pars$name, sections=.self$.sections)
    .p[(match(x$name, colnames(.p))-1)*nrow(.p) + x$section] <<- x$value
  }
  return(invisible(NULL))
})


