
# This provides the roxygen documentation for the package's data sets

#' Declaration of Variables
#'
#' Declaration of variables of the bacteria growth example model.
#'
#' @docType data
#' @name vars
#' @format A data frame with the following fields:
#'   \describe{
#'     \item{name : }{Name of the variable}
#'     \item{unit : }{Unit}
#'     \item{description : }{Short description (text)}
#'   }
NULL

#' Declaration of Parameters
#'
#' Declaration of parameters of the bacteria growth example model.
#'
#' @docType data
#' @name pars
#' @format A data frame with the following fields:
#'   \describe{
#'     \item{name : }{Name of the parameter}
#'     \item{unit : }{Unit}
#'     \item{description : }{Short description (text)}
#'   }
NULL

#' Declaration of Functions
#'
#' Declaration of functions referenced at the ODE's right hand sides
#' of the bacteria growth example model.
#'
#' @docType data
#' @name funs
#' @format A data frame with the following fields:
#'   \describe{
#'     \item{name : }{Name of the function}
#'     \item{unit : }{Unit of the return value}
#'     \item{description : }{Short description (text)}
#'   }
NULL

#' Declaration of Processes
#'
#' Definition of processes of the bacteria growth example model.
#'
#' @docType data
#' @name pros
#' @format A data frame with the following fields:
#'   \describe{
#'     \item{name : }{Name of the process}
#'     \item{unit : }{Unit of the rate expression}
#'     \item{description : }{Short description (text)}
#'     \item{expression : }{Mathematical expression (as a string)}
#'   }
NULL

#' Specification of Stoichiometry
#'
#' Definition of the links between simulated processes and state variables
#' in the bacteria growth example model.
#'
#' @docType data
#' @name stoi
#' @format A data frame with the following fields:
#'   \describe{
#'     \item{variable : }{Name of the state variable}
#'     \item{process : }{Name of the process}
#'     \item{expression : }{Mathematical expression (as a string)}
#'   }
NULL

