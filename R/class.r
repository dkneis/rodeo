#' A reference class to represent an ODE-based model.
#'
#' @field .pros A data frame with fields 'name', 'unit', 'description', and
#'   'expression' defining the process rates.
#' @field .stoi A data frame with fields 'variable', 'process', and 'expression'
#'   reprenting the stoichiometry matrix in data base format.
#' @field .vars A data frame with fields 'name', 'unit', 'description' declaring
#'   the state variables of the model. The declared names become valid
#'   identifiers to be used in the expression fields of \code{.pros} or \code{.stoi}.
#' @field .pars A data frame of the same structure as \code{vars} declaring the
#'   parameters of the model. The declared names become valid
#'   identifiers to be used in the expression fields of \code{.pros} or \code{.stoi}.
#' @field .funs A data frame of the same structure as \code{vars} declaring any
#'   functions referenced in the expression fields of \code{.pros} or \code{.stoi}.

#' @examples
#' # see vignette
#'
#' @export

rodeo= setRefClass(
  Class = "rodeo",
  fields = c(
    .vars="data.frame",
    .pars="data.frame",
    .funs="data.frame",
    .pros="data.frame",
    .stoi="data.frame")
)

