#' Create Fortran library
#'
#' Create a Fortran library for use with numerical methods from packages
#' \code{\link[deSolve]{deSolve}} or \code{\link[rootSolve]{rootSolve}}.
#'
#' @name compile
#'
#' @inheritParams solverInterface
#' @param fileFun Fortran source file defining a module 'functions' that
#'   contains any user-defined functions referenced in process rates or
#'   stoichiometric factors.
#'
#' @return A vector of character strings with named elements as follows:
#' \itemize{
#'   \item{\code{libFile}} File path of the generated library. Needs to be
#'     passed to, e.g., \code{\link[base]{dyn.load}}.
#'   \item{\code{libName}} The pure library name, which is the base name of
#'     \code{libFile} with the platform specific extension stripper. This name
#'     has to be supplied as the \code{dllname} argument of the solver methods
#'     in \code{\link[deSolve]{deSolve}} or \code{\link[rootSolve]{rootSolve}}.
#'   \item{\code{libFunc}} Name of the method contained in the built library
#'     which computes the derivatives. This name has to be supplied as the
#'     \code{func} argument of the solver methods
#'     in \code{\link[deSolve]{deSolve}} or \code{\link[rootSolve]{rootSolve}}.
#' }
#'
#' @author \email{david.kneis@@tu-dresden.de}
#'
#' @seealso This method internally calls \code{\link{generate}} and the
#'   non-class method \code{\link{solverInterface}}.
#'
#' @examples
#' # to be added

rodeo$methods( compile = function(fileFun, NLVL) {
  "Compile Fortran library for use with numerical methods from packages
   \\code{\\link[deSolve]{deSolve}} or \\code{\\link[rootSolve]{rootSolve}}.
  See \\code{\\link{compile}} for details."

  srcFiles <- c(funcs=normalizePath(fileFun), derivs= paste0(tempfile(),".f95"),
    wrapper= paste0(tempfile(),".f95"))
  srcFiles <- gsub("\\", "/", srcFiles, fixed=TRUE)
  write(.self$generate(name="derivs", lang="f95"), file=srcFiles["derivs"])
  libFunc= "derivs_wrapped"
  write(solverInterface(NLVL, "derivs", libFunc), file=srcFiles["wrapper"])
  libFile <- tempfile()
  libName <- basename(libFile)
  libFile <- gsub("\\", "/", paste0(libFile,.Platform$dynlib.ext), fixed=TRUE)
  wd= getwd()
  setwd(tempdir())
  command <- paste0("R CMD SHLIB ",paste(srcFiles, collapse=" "),
    " --preclean --clean -o ",libFile)
  if (system(command) != 0) {
    setwd(wd)
    stop("Compilation failed.")
  }
  invisible(file.remove(list.files(path=tempdir(), pattern=".+[.]mod$")))
  setwd(wd)
  return(c(libFile=libFile, libName=libName, libFunc=libFunc))
})

