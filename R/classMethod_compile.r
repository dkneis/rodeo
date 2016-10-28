#' Create Fortran Library
#'
#' Create a Fortran library for use with numerical methods from packages
#' \code{\link[deSolve]{deSolve}} or \code{\link[rootSolve]{rootSolve}}.
#'
#' @name compile
#'
#' @param sources Name(s) of fortran source file(s) where a module with the
#'   fixed name 'functions' is implemented. This module must contain all
#'   user-defined functions referenced in process rates or
#'   stoichiometric factors. Can be the name of a single file or, if the
#'   Fortran code is split over several dependent files, a vector of file names.
#' @param target Name of a target 'environment' for which the library is
#'   compiled. Currently, 'deSolve' is the only supported value.

#'
#' @return A vector of character strings with named elements as follows:
#' \itemize{
#'   \item{\code{libFile}} File path of the created library. Needs to be
#'     passed to, e.g., \code{\link[base]{dyn.load}}. The library is generally
#'     created in the folder returned by \code{\link[base]{tempdir}}.
#'   \item{\code{libName}} The pure library name, which is the base name of
#'     \code{libFile} with the platform specific extension stripped. This name
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
#' @seealso This method internally calls \code{\link{generate}}.
#'
#' @examples
#' data(vars, pars, funs, pros, stoi)
#' model <- rodeo$new(vars, pars, funs, pros, stoi, dim=c(1))
#' # This would trigger compilation assuming that 'functionsCode.f95' contains
#' # a Fortran implementation of all functions; see vignette for full example
#' \dontrun{
#' lib <- model$compile(sources="functionsCode.f95")
#' }

rodeo$set("public", "compile", function(sources, target="deSolve") {
  tmpdir <- gsub(pattern="\\", replacement="/", x=tempdir(), fixed=TRUE)
  if (identical(target, "deSolve")) {
    srcFiles <- c(funcs=normalizePath(sources),
      derivs= tempfile(pattern="rodeo", tmpdir=tmpdir, fileext=".f95"),
      wrapper= tempfile(pattern="rodeo", tmpdir=tmpdir, fileext=".f95"))
    srcFiles <- gsub("\\", "/", srcFiles, fixed=TRUE)
    write(self$generate(name="derivs", lang="f95"), file=srcFiles["derivs"])
    libFunc <- "derivs_wrapped"
    libFile <- tempfile(pattern="rodeo", tmpdir=tmpdir)
    libName <- basename(libFile)
    libFile <- gsub("\\", "/", paste0(libFile,.Platform$dynlib.ext), fixed=TRUE)
    write(solverInterface(prod(private$dim), libName, "derivs", libFunc), file=srcFiles["wrapper"])
    wd <- getwd()
    setwd(tmpdir)
    command <- paste0("R CMD SHLIB ",paste(srcFiles, collapse=" "),
      " --preclean --clean -o ",libFile)
    if (system(command) != 0) {
      setwd(wd)
      stop("Compilation failed.")
    }
    invisible(file.remove(list.files(path=tmpdir, pattern=".+[.]mod$")))
    setwd(wd)
    return(c(libFile=libFile, libName=libName, libFunc=libFunc))
  } else {
    stop("target not supported")
  }
})


# Internal method called by 'compile'
solverInterface <- function (boxes, libName, funcName, wrapperName) {
  paste0("
! GENERATED CODE -- YOU PROBABLY DO NOT WANT TO EDIT THIS

! Definition of the number of spatial boxes
module spatial_dimension
  implicit none
  integer, parameter:: ",rodeoConst$genIdent$len["boxes"],"=",boxes,"
end module

! Routine for parameter initialization
subroutine ",libName,"(extfun)
  use dimensions_and_indices   ! Module is provided by the generated code
  use spatial_dimension
  external extfun
  double precision, dimension(",rodeoConst$genIdent$len['pars'],"*",rodeoConst$genIdent$len["boxes"],"):: par
  common /params/ par
  call extfun(",rodeoConst$genIdent$len['pars'],"*",rodeoConst$genIdent$len["boxes"],", par)
end subroutine

! Wrapper around the generated code
subroutine ",wrapperName," (neq, t, y, ydot, yout, ip)
  use dimensions_and_indices   ! Module is provided by the generated code
  use spatial_dimension
  implicit none
  ! Inputs
  integer, intent(in):: neq
  double precision, intent(in):: t
  double precision, dimension(neq), intent(in):: y
  integer, dimension(*), intent(in)::ip
  ! Outputs
  double precision, dimension(neq), intent(out)::ydot
  double precision, dimension(ip(2)), intent(out)::yout
  ! Import parameters
  double precision, dimension(",rodeoConst$genIdent$len['pars'],"*",rodeoConst$genIdent$len["boxes"],"):: par
  common /params/ par
  !Call to generated code
  call ",funcName,"(t, y, par, ",rodeoConst$genIdent$len["boxes"],", ydot, yout)
end subroutine
"
)
}

