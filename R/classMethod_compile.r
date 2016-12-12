#' Create Fortran Library
#'
#' Creates and loads a Fortran library for use with numerical methods from
#' package \code{\link[deSolve]{deSolve}} or \code{\link[rootSolve]{rootSolve}}.
#'
#' @name compile
#'
#' @param sources Name(s) of fortran source file(s) where a module with the
#'   fixed name 'functions' is implemented. This module must contain all
#'   user-defined functions referenced in process rates or
#'   stoichiometric factors. Can be \code{NULL}, the name of a single file, or
#'   a vector of file names if the Fortran code is split over several files.
#' @param target Name of a target 'environment' for which the library is
#'   compiled. Currently, 'deSolve' is the only supported value.
#'
#' @return \code{invisible(NULL)}
#'
#' @note The name of the generated library as well as the name
#'   of the function to compute the derivatives are stored in the object.
#'   These names can be queried with \code{\link{libName}} and
#'   \code{\link{libFunc}}, respectively.
#'
#' The library is generally created in the folder returned by
#' \code{\link[base]{tempdir}} and it is loaded with \code{\link[base]{dyn.load}}.
#' It is automatically unloaded with \code{\link[base]{dyn.unload}} when the
#' object's \code{\link{finalize}} method is called.
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
#' model$compile(sources="functionsCode.f95")
#' }

rodeo$set("public", "compile", function(sources=NULL, target="deSolve") {
  tmpdir <- gsub(pattern="\\", replacement="/", x=tempdir(), fixed=TRUE)
  if (identical(target, "deSolve")) {
    srcFiles <- c(funcs=if (is.null(sources)) "" else normalizePath(sources),
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

    # Load library
    dyn.load(libFile)
    if (!is.loaded(libFunc, PACKAGE=libName)) {
  #    print(getLoadedDLLs())
      stop("failed to load fortran subroutine '",libFunc,"' (library '",libName,"')")
    }

    # Save names for use with the query methods and the finalize method
    private$lib <- c(file=libFile, name=libName, func=libFunc)

  } else {
    stop("target not supported")
  }
  return(invisible(NULL))
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

