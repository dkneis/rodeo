#' Return the Stoichiometry Matrix
#'
#' Return and optionally evaluate the mathematical expression appearing in the
#' stoichiometry matrix.
#'
#' @name stoichiometry
#'
#' @param box Either \code{NULL} or a vector of positive integers pointing
#'   to a spatial sub-unit of the model.
#'   If \code{NULL}, the mathematical expressions appearing in the
#'   stoichiometry matrix are not evaluated, hence, they are returned as
#'   character strings. If a spatial sub-unit is specified, a numeric matrix is
#'   returned. In the latter case, the values of state variables and parameters
#'   must have been set using the \code{\link{setVars}} and
#'   \code{\link{setPars}} methods.
#' @param time Time. The value is ignored in the case of autonomous models.
#'
#' @return A matrix of numeric or character type, depending on the value of
#'  \code{box}.
#'
#' @note If the stoichiometric factors are mathematical expressions involving
#'   function references, these functions must be defined in R (even if the
#'   numerical computations are based on generated Fortran code).
#'
#' @author \email{david.kneis@@tu-dresden.de}
#'
#' @seealso See other methods of the \code{\link{rodeo-class}} or
#'   \code{\link{plotStoichiometry}} for a graphical representation of the
#'   stoichiometric factors only.
#'
#' @examples
#' data(vars, pars, funs, pros, stoi)
#' model <- rodeo$new(vars, pars, funs, pros, stoi, dim=c(1))
#' model$setPars(c(mu=0.8, half=0.1, yield= 0.1, vol=1000, flow=50, subs_in=1))
#' model$setVars(c(bacs=0.1, subs=0.5))
#' monod <- function(c,h) {c/(c+h)}
#' print(model$stoichiometry(box=NULL))
#' print(model$stoichiometry(box=c(1)))

rodeo$set("public", "stoichiometry", function(box=NULL, time=0) {

  # Build the matrix of expressions
  m <- matrix("0", ncol=nrow(private$varsTbl), nrow=nrow(private$prosTbl))
  colnames(m) <- private$varsTbl$name
  rownames(m) <- private$prosTbl$name
  for (i in 1:nrow(private$stoiTbl)) {
    m[private$stoiTbl$process[i], private$stoiTbl$variable[i]] <- private$stoiTbl$expression[i]
  }

  # Return the matrix of expressions unevaluated
  if (is.null(box)) {
    return(m)
  # ... or return the numeric matrix otherwise
  } else {
    if (length(box) != length(private$dim))
      stop("length of argument 'box' (",length(box),") does not match with",
        " the model's spatial resolution (",private$dim,")")
    box <- as.integer(box)
    if (any(box < 1) || any(box > private$dim))
      stop("elements of vector 'box' must be positive integers not exceeding",
        " the number of spatial sub-units in the respective dimension")
    # Create environment holding all data -> required for evaluating expressions
    env <- new.env()
    f <- tempfile()
    iVar <- seq(from=prod(box), by=prod(private$dim), length.out=nrow(private$varsTbl))
    iPar <- seq(from=prod(box), by=prod(private$dim), length.out=nrow(private$parsTbl))
    values <- setNames(c(private$vars[iVar], private$pars[iPar], time),
      c(private$varsTbl$name, private$parsTbl$name, "time"))
    write.table(file=f, x=data.frame(names(values),values,stringsAsFactors=FALSE),
      sep="=", col.names=FALSE, row.names=FALSE, quote=FALSE)
    sys.source(file=f,envir=env)
    # Create numeric matrix
    mnum <- matrix(0, ncol=ncol(m), nrow=nrow(m))
    colnames(mnum) <- colnames(m)
    rownames(mnum) <- rownames(m)
    for (ic in 1:ncol(m)) {
      for (ir in 1:nrow(m)) {
        tryCatch({
          mnum[ir,ic] <- eval(parse(text=m[ir,ic]), envir=env)  # evaluated in created env
        }, error= function(e) {
          stop(paste0("failed to compute stoichiometry factor for variable '",
            colnames(m)[ic],"' and process '",rownames(m)[ir],"'; details: ",e))
        })
      }
    }
    return(mnum)
  }
})

