#' A reference class to represent an ODE-based model.
#'
#' @field pros A data frame with fields 'name', 'unit', 'description', and
#'   'expression' defining the process rates.
#' @field stoi A data frame with fields 'variable', 'process', and 'expression'
#'   reprenting the stoichiometry matrix in data base format.
#' @field vars A data frame with fields 'name', 'unit', 'description' declaring
#'   the state variables of the model. The declared names become valid
#'   identifiers to be used in the expression fields of \code{pros} or \code{stox}.
#' @field pars A data frame of the same structure as \code{vars} declaring the
#'   parameters of the model. The declared names become valid
#'   identifiers to be used in the expression fields of \code{pros} or \code{stox}.
#' @field funs A data frame of the same structure as \code{vars} declaring any
#'   functions referenced in the expression fields of \code{pros} or \code{stox}.

#' @examples
#' # Create model object initialized with sample data
#' data(identifiers, processes, stoichiometry)
#' model= new("rodeo", vars=subset(identifiers,type=="v"),
#'   pars=subset(identifiers,type=="p"), funs=subset(identifiers,type=="f"),
#'   pros=processes, stoi=stoichiometry)
#'
#' # Display the object's contents (built-in method)
#' model$show()
#'
#' # Show stoichiometry information as a matrix
#' print(model$stoichiometryMatrix())
#'
#' # Define functions referenced in the model's mathematical expressions
#' O2sat= function(t) {14.652 - 0.41022*t + 0.007991*t^2 - 0.000077774*t^3}
#' ka= function(u, d) {(0.728*sqrt(u) - 0.317*u + 0.0372*u^2) / d / 86400}
#' monod= function(s,h) {s / (s + h)}
#'
#' # Define parameters and initial values
#' pars= list(kd=5.78e-7, h_do=0.5, s_do_z=2.76, wind=1, depth=2,
#'   temp=20, q_in=1, q_ex=1, c_z_in=0.1, c_do_in=9.022)
#' vars= list(c_z=1, c_do=9.022, v=1.e6)
#' p= model$arrangePars(pars)
#' v= model$arrangeVars(vars)
#'
#' # Graphical representation of the stoichiometry matrix
#' model$plot(c(v, p))
#'
#' # Generate code to compute the d/dt of state variables
#' code= model$generate(name="derivs",lang="r")
#' derivs= eval(parse(text=code))
#'
#' # Integrate the model over some time steps
#' library(deSolve)
#' t= seq(0, 30*86400, 3600)
#' out= ode(y=v, times=t, func=derivs, parms=p, NLVL=1)
#' plot(out)
#'
#' # Adapt the model to simulate multiple control volumes (boxes) at a time
#' # For that, at least one variable and at least one parameter must be a vector
#' nbox= 3
#' pars= list(kd=rep(5.78e-7, nbox), h_do=0.5, s_do_z=2.76, wind=1, depth=2,
#'   temp=20, q_in=1, q_ex=1, c_z_in=0.1, c_do_in=9.022)
#' vars= list(c_z=seq(from=0, to=50, length.out=nbox), c_do=9.022, v=1.e6)
#' p= model$arrangePars(pars)
#' v= model$arrangeVars(vars)
#' out= ode(y=v, times=t, func=derivs, parms=p, NLVL=nbox)
#' plot(out, which=paste0("c_do",1:nbox))
#'
#' @export

rodeo= setRefClass(
  Class = "rodeo",
  fields = c(
    VARS="data.frame",
    PARS="data.frame",
    FUNS="data.frame",
    PROS="data.frame",
    STOI="data.frame")
)

