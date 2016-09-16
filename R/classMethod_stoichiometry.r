#' Return the Stoichiometry Matrix
#'
#' Return and optionally evaluate the mathematical expression appearing in the
#' stoichiometry matrix.
#'
#' @name stoichiometry
#'
#' @param section Either \code{NULL} or a positive integer representing a spatial
#'   section. If \code{NULL}, the mathematical expressions appearing in the
#'   stoichiometry matrix are not evaluated, hence, they are returned as
#'   character strings. If a spatial section is specified, a numeric matrix is
#'   returned. In the latter case, the values of state variables and parameters
#'   must have been set using the \code{\link{setVars}} and
#'   \code{\link{setPars}} methods.
#' @param time Time. The value is ignored in the case of autonomous models.
#'
#' @return A matrix of numeric or character type, depending on the value of
#'  \code{section}.
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
#' data(exampleIdentifiers, exampleProcesses, exampleStoichiometry)
#' model <- rodeo$new(
#'   vars=subset(exampleIdentifiers, type=="v"),
#'   pars=subset(exampleIdentifiers, type=="p"),
#'   funs=subset(exampleIdentifiers, type=="f"),
#'   pros=exampleProcesses, stoi=exampleStoichiometry
#' )
#' c_z_in <- function(time) {0.1}
#' c_do_in <- function(time) {8.0}
#' model$setVars(c(c_z=1, c_do=9.022, v=1.e6))
#' model$setPars(c(kd=5.78e-7, h_do=0.5, s_do_z=2.76, wind=1, depth=2,
#'   temp=20, q_in=1, q_ex=1))
#' print(model$stoichiometry(section=NULL))
#' print(model$stoichiometry(section=1))

rodeo$set("public", "stoichiometry", function(section=NULL, time=0) {

  # Build the matrix of expressions
  m <- matrix("0", ncol=nrow(private$varsTbl), nrow=nrow(private$prosTbl))
  colnames(m) <- private$varsTbl$name
  rownames(m) <- private$prosTbl$name
  for (i in 1:nrow(private$stoiTbl)) {
    m[private$stoiTbl$process[i], private$stoiTbl$variable[i]] <- private$stoiTbl$expression[i]
  }

  # Return the matrix of expressions unevaluated
  if (is.null(section)) {
    return(m)
  # ... or return the numeric matrix otherwise
  } else {
    if ((section > private$sections) || (section < 1))
      stop("'section' must be in range [1, ",private$sections,"] for this object")
    # Create environment holding all data -> required for evaluating expressions
    env <- new.env()
    f <- tempfile()
    values <- setNames(c(private$v[section,], private$p[section,], time),
      c(colnames(private$v), colnames(private$p), "time"))
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

