
# This provides the roxygen documentation for the package's data sets

#' Declaration table
#'
#' Declaration of items (i.e. identifiers) appearing in the model's
#' mathematical expressions.
#'
#' @name identifiers
#' @format A data frame with the following fields:
#'   \itemize{
#'     \item{name : }{Name of the item}
#'     \item{type : }{'v' for variable, 'p' for paraneter, 'f' for function}
#'     \item{units : }{Units of the item}
#'     \item{description : }{Short description (text)}
#'   }
NULL

#' Processes
#'
#' Definition of simulated processes.
#'
#' @name processes
#' @format A data frame with the following fields:
#'   \itemize{
#'     \item{name : }{Name of the process}
#'     \item{units : }{Units of the rate expression}
#'     \item{description : }{Short description (text)}
#'     \item{expression : }{Mathematical expression (as a string)}
#'   }
NULL

#' Stoichiometry
#'
#' Definition of the links between simulated processes and state variables.
#'
#' @name stoichiometry
#' @format A data frame with the following fields:
#'   \itemize{
#'     \item{variable : }{Name of the state variable}
#'     \item{process : }{Name of the process}
#'     \item{expression : }{Mathematical expression (as a string)}
#'   }
NULL

