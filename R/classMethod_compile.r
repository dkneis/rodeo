#' Generate Executable Code
#'
#' Creates and 'compiles' a function for use with numerical methods from
#' package \code{\link[deSolve]{deSolve}} or \code{\link[rootSolve]{rootSolve}}.
#'
#' @name compile
#'
#' @param sources Name(s) of source files(s) where functions appearing in
#'   process rates or stoichiometric factors are implemented. Can be \code{NULL}
#'   if no external functions are required, the name of a single file, or a
#'   vector of file names. See notes below.
#' @param fortran If \code{TRUE}, Fortran code is generated and compiled into a shared
#'   library. If \code{FALSE}, R code is generated.
#' @param target Name of a 'target environment'. Currently, 'deSolve' is the
#'    only supported value.
#' @param lib Absolute file path to be used for the generated library (without
#'   the platform specific extension). A relative path can't be used here!
#'   By default, the file is created in R's temporary folder under a random name.
#' @param reuse If \code{TRUE}, an already existing library file will be loaded.
#'   Use this to prevent
#'   unnecessary re-compilation but note that R is likely to crash in case
#'   of any mismatches between the object and the existing library. Default is
#'   \code{FALSE}, i.e. the library is unconditionally build from scratch.
#'
#' @return \code{invisible(NULL)}
#'
#' @note The expected language of the external code passed in \code{sources}
#'   depends on the value of \code{fortran}.
#'
#'   If \code{fortran} is \code{FALSE}, R code is generated and made executable
#'   by \code{\link[base]{eval}} and \code{\link[base]{parse}}. Auxiliary code
#'   passed via \code{sources} is made available via \code{\link[base]{source}}.
#'   The created R function is stored in the object.
#'
#'   If \code{fortran} is \code{TRUE}, the external code passed in 
#'   \code{sources} must implement a module with the fixed name 'functions'.
#'   This module must contain all user-defined functions referenced in process
#'   rates or stoichiometric factors.
#'
#'   If \code{fortran} is \code{TRUE}, a shared library is created. The library
#'   is immediately loaded with \code{\link[base]{dyn.load}} and it is
#'    automatically unloaded with \code{\link[base]{dyn.unload}} when the
#'   object's \code{\link{finalize}} method is called.
#'   
#'   The name of the library (base name without extension) as well as the name
#'   of the function to compute the  derivatives are stored in the object.
#'   These names can be queried with the
#'   \code{\link{libName}} and \code{\link{libFunc}} methods, respectively.
#'   Unless a file path is specified via the \code{lib} argument, the library is
#'   created in the folder returned by \code{\link[base]{tempdir}} under a
#'   unique random name.
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

rodeo$set("public", "compile", function(sources=NULL, fortran=TRUE,
  target="deSolve", lib=NULL, reuse=FALSE
) {
  tmpdir <- gsub(pattern="\\", replacement="/", x=tempdir(), fixed=TRUE)
  funcname <- "drvs"
  if (identical(target, "deSolve")) {
    # Generation of Fortran library
    if (fortran) {
      libFunc <- paste0(funcname,"_wrapped")
      libFile <- if (is.null(lib)) {
          tempfile(pattern="rodeo", tmpdir=tmpdir)
        } else {
          gsub(pattern="\\", replacement="/",
            x=suppressWarnings(normalizePath(lib)), fixed=TRUE)
        }
      libName <- basename(libFile)
      libFile <- gsub("\\", "/", paste0(libFile,.Platform$dynlib.ext), fixed=TRUE)
      if (!file.exists(libFile) || !reuse) {
        srcFiles <- c(funcs=if (is.null(sources)) "" else normalizePath(sources),
          derivs= tempfile(pattern="rodeo", tmpdir=tmpdir, fileext=".f95"),
          wrapper= tempfile(pattern="rodeo", tmpdir=tmpdir, fileext=".f95"))
        srcFiles <- gsub("\\", "/", srcFiles, fixed=TRUE)
        write(self$generate(name=funcname, lang="f95"), file=srcFiles["derivs"])
        write(solverInterface(prod(private$dim), libName, funcname, libFunc), file=srcFiles["wrapper"])
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
      }
      # Load library
      if (!file.exists(libFile))
        stop("library file '",libFile,"' not found")
      dyn.load(libFile)
      #print(dyn.load(libFile))  # for debugging
      #print(getLoadedDLLs())    # for debugging
      if (!is.loaded(libFunc, PACKAGE=libName)) {
        stop("failed to load fortran subroutine '",libFunc,"' (library '",libName,"')")
      }
      # Save names for use with the query methods and the finalize method
      private$lib <- c(file=libFile, name=libName, func=libFunc)
    # Generation of R function
    } else {
      if (!is.null(sources))
        lapply(sources, source)
      rcode <- self$generate(name=funcname, lang="r")
      eval(parse(text=rcode))
      self$func <- get(funcname)
    }
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

