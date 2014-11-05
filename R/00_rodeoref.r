
#' @import methods
library("methods")
#' @import rjson
library("rjson")


#' A reference class to represent an ODE-based model.
#'
#' @field proc A named vector of character strings holding process rate expressions.
#' @field stox A data frame holding character strings to represent the
#'   stoichiometry factors (colums = variables, rows = processes). 
#' @field auxx A named vector of character strings holding auxiliary expressions being referenced
#'   in, e.g., \code{proc} and/or \code{stox}.
#' @field vars A named numerical vector holding state variables.
#' @field pars A named numerical vector holding parameters.
#' @field funs A named vector of character strings holding the names of functions
#'   appearing in any of the model's expressions, i.e. \code{proc}, \code{stox},
#'   and \code{auxx}.
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
#' # Assign values to state variables and parameters
#' data(toymodel_vars)
#' x$setVars(setNames(toymodel_vars$value, toymodel_vars$name))
#' data(toymodel_pars)
#' x$setPars(setNames(toymodel_pars$value, toymodel_pars$name))
#'
#' # Display the entire model
#' x$show()
#'
#' # Define functions referenced in the model,
#' # then visualize the stoichiometry matrix
#' DOSAT= function(t) {14.652 - 0.41022*t + 0.007991*t^2 - 0.000077774*t^3}
#' k2= function(u, d) {(0.728*sqrt(u) - 0.317*u + 0.0372*u^2) / d / 86400}
#' x$plot_stox()
#'
#' @export
rodeo= setRefClass(
  Class = "rodeo",
  fields = c(auxx="vector", proc="vector", stox="data.frame",
    vars="vector", pars="vector", funs="vector")
)

