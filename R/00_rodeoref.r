
#' @import methods
library("methods")
#' @import rjson
library("rjson")


#' A reference class to represent an ODE-based model.
#'
#' @field pros A named vector of character strings holding process rate expressions.
#' @field stox A data frame holding character strings to represent the
#'   stoichiometry factors (colums = variables, rows = processes). 
#' @field auxs A named vector of character strings holding auxiliary expressions being referenced
#'   in, e.g., \code{pros} and/or \code{stox}.
#' @field vars A vector of character strings holding the names of state variables.
#' @field pars A vector of character strings holding the names of parameters.
#' @field funs A vector of character strings holding the names of functions.
#'   appearing in any of the model's expressions, i.e. \code{pros}, \code{stox},
#'   and \code{auxs}.
#'
#' @examples
#' # Create file with model definition in json format
#' data(toymodel)
#' tf= tempfile()
#' write(x=toymodel, file=tf)
#'
#' # Instantiate object and import model from file
#' x=new("rodeo", file=tf)
#'
#' # Display the entire model
#' x$show()   # built-in method
#' x$print()
#'
#' # Define functions referenced in the model
#' DOSAT= function(t) {14.652 - 0.41022*t + 0.007991*t^2 - 0.000077774*t^3}
#' k2= function(u, d) {(0.728*sqrt(u) - 0.317*u + 0.0372*u^2) / d / 86400}

#' # Assign values to state variables and parameters
#' # then visualize the stoichiometry matrix
#' data(toymodel_vars)
#' vars= x$sortLikeVars(setNames(toymodel_vars$value, toymodel_vars$name))
#' data(toymodel_pars)
#' pars= x$sortLikePars(setNames(toymodel_pars$value, toymodel_pars$name))
#'
#' x$plot(vars=vars, pars=pars)
#'
#' @export
rodeo= setRefClass(
  Class = "rodeo",
  fields = c(auxs="vector", pros="vector", stox="data.frame",
    vars="vector", pars="vector", funs="vector")
)

