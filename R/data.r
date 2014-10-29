
# This provides the roxygen documentation for the package's data sets

#' Toy model
#'
#' A toy model in JSON notation
#'
#' @name toymodel
#' @format A single character string which can be used to save a toymodel
#'   specification to a file in JSON format (see example)
#' @examples
#'   \dontrun{
#'   data(toymodel)
#'   write(x=toymodel, file="toymodel.json")
#'   }
NULL

#' Toy model parameters
#'
#' Sample parameters for \code{toymodel}
#'
#' @name toymodel_pars
#' @format A data frame with the following columns:
#'   \itemize{
#'     \item{name : }{Name of the parameter}
#'     \item{value : }{Value}
#'     \item{unit : }{Unit of the value}
#'     \item{description : }{Short description of the parameters's meaning}
#'   }
NULL

#' Toy model variables
#'
#' Sample values for the state variables of \code{toymodel}
#'
#' @name toymodel_vars
#' @format A data frame with the following columns:
#'   \itemize{
#'     \item{name : }{Name of the variable}
#'     \item{value : }{Value}
#'     \item{unit : }{Unit of the value}
#'     \item{description : }{Short description of the variable's meaning}
#'   }
NULL

