
#' Initialize Internal ODE Solver
#'
#' Initializes \code{rodeo}'s built-in ODE solver. This method must be called
#' prior to using \code{\link{step}}.
#'
#' @name initStepper
#'
#' @param sources Name(s) of fortran source file(s) where a module with the
#'   fixed name 'functions' is implemented. This module must contain all
#'   user-defined functions referenced in process rates or
#'   stoichiometric factors. Can be \code{NULL}, the name of a single file, or
#'   a vector of file names if the Fortran code is split over several files.
#' @param method Name of a the solver. Currently, 'rk5' is the only supported
#'   value (Runge-Kutta method of Cash and Karp).
#'
#' @return \code{invisible(NULL)}
#'
#' @note After this method was called, \code{\link{step}} can be used to
#'   perform the integration.
#'
#' @author \email{david.kneis@@tu-dresden.de}
#'
#' @seealso To perform integration with the solvers from package
#'   \code{\link[deSolve]{deSolve}} use \code{\link{compile}} instead of this
#'   method.

rodeo$set("public", "initStepper",
  function(sources, method="rk5")
{

  # Required files to build the library
  srcFiles <- c(
    integrators= paste0(tempfile(),".f95"),
    funcs= normalizePath(sources),
    derivs= paste0(tempfile(),".f95"),
    wrapper= paste0(tempfile(),".f95")
  )
  file.copy(from=system.file('fortran/integrators.f95',package='rodeo'),
    to=srcFiles["integrators"], overwrite=TRUE)
  
  # Register basic stepper info
  fl <- gsub("\\", "/", tempfile(pattern=method), fixed=TRUE)
  private$steppers[[method]] <- list(
    libFile= paste0(fl, .Platform$dynlib.ext),
    libName= basename(fl),
    fncName= paste0("step_",method),
    fncSymb= NA
  )

  # Generation of derivatives code
  srcFiles <- gsub("\\", "/", srcFiles, fixed=TRUE)
  write(self$generate(name="derivs", lang="f95"), file=srcFiles["derivs"])

  # Wrapper generation
  if (method == "rk5") {
    wrapper <- paste0(
      " ! GENERATED CODE -- YOU PROBABLY DO NOT WANT TO EDIT THIS\n",
      "\n",
      "  ! Module to pass a specific derivatives routine to the generic solver\n",
      "  module wrapper\n",
      "  implicit none\n",
      "\n",
      "  ! Number of boxes\n",
      "  integer, parameter:: ",rodeoConst$genIdent$len["boxes"],"=",prod(private$dim),"\n",
      "\n",
      "  ! Generic interface of the generated derivatives method\n",
      "  interface\n",
      "    subroutine derivs(time, var, par, ",rodeoConst$genIdent$len["boxes"],", dydt, pro)\n",
      "      use dimensions_and_indices\n",
      "      use functions\n",
      "      implicit none\n",
      "      ! Inputs\n",
      "      double precision, intent(in):: time\n",
      "      double precision, dimension(",rodeoConst$genIdent$len["vars"],"*",
        rodeoConst$genIdent$len["boxes"],"), intent(in):: var\n",
      "      double precision, dimension(",rodeoConst$genIdent$len["pars"],"*",
        rodeoConst$genIdent$len["boxes"],"), intent(in):: par\n",
      "      integer, intent(in):: ",rodeoConst$genIdent$len["boxes"],"\n",
      "      ! Outputs\n",
      "      double precision, dimension(",rodeoConst$genIdent$len["vars"],"*",
        rodeoConst$genIdent$len["boxes"],"), intent(out):: dydt\n",
      "      double precision, dimension(",rodeoConst$genIdent$len["pros"],"*",
        rodeoConst$genIdent$len["boxes"],"), intent(out):: pro\n",
      "   end subroutine\n",
      "  end interface\n",
      "\n",
      "  contains\n",
      "\n",
      "  ! Specific derivatives method to be passed to the solver\n",
      "  subroutine derivs_wrapped(t,u,par,dudt,error)\n",
      "  use dimensions_and_indices, only: ",rodeoConst$genIdent$len["vars"],
        ", ",rodeoConst$genIdent$len["pars"],", ",rodeoConst$genIdent$len["pros"],"\n",
      "  implicit none\n",
      "  ! in\n",
      "  double precision, intent(in):: t\n",
      "  double precision, dimension(:), intent(in):: u\n",
      "  double precision, dimension(:), intent(in):: par\n",
      "  ! out\n",
      "  logical, intent(out):: error\n",
      "  double precision, dimension(:), intent(out):: dudt\n",
      "  ! local\n",
      "  double precision, dimension(",rodeoConst$genIdent$len["pros"],"):: pro\n",
      "  ! call to generated method\n",
      "  call derivs(time=t, var=u, par=par, ",
        rodeoConst$genIdent$len["boxes"],"=",rodeoConst$genIdent$len["boxes"],
        ", dydt=dudt, pro=pro)\n",
      "  end subroutine\n",
      "\n",
      "  end module\n",
      "\n",
      "  ! Method to call the rk5 solver\n",
      "  subroutine ",private$steppers[[method]]$fncName,"(error,var,x1,x2,eps,h1,hmin,nmax,par,pro)\n",
      "  use integrators, only: rk5\n",
      "  use dimensions_and_indices, only: ",rodeoConst$genIdent$len["vars"],
        ", ",rodeoConst$genIdent$len["pars"],", ",rodeoConst$genIdent$len["pros"],"\n",
      "  use wrapper\n",
      "  implicit none\n",
      "  ! in\n",
      "  integer, intent(in):: nmax\n",
      "  double precision, intent(in):: x1, x2, eps, h1, hmin\n",
      "  double precision, dimension(",rodeoConst$genIdent$len["pars"],"*",
        rodeoConst$genIdent$len["boxes"],"), intent(in):: par\n",
      "  ! inout\n",
      "  double precision, dimension(",rodeoConst$genIdent$len["vars"],"*",
        rodeoConst$genIdent$len["boxes"],"), intent(inout):: var\n",
      "  ! out\n",
      "  double precision, dimension(",rodeoConst$genIdent$len["pros"],"*",
        rodeoConst$genIdent$len["boxes"],"), intent(out):: pro\n",
      "  logical, intent(out):: error\n",
      "  ! local\n",
      "  double precision, dimension(",rodeoConst$genIdent$len["vars"],"*",
        rodeoConst$genIdent$len["boxes"],"):: dydt\n",
      "  ! call integrator\n",
      "  call rk5(error,var,x1,x2,eps,h1,hmin,nmax,par,derivs_wrapped)\n",
      "  ! additional call to get process rates at final state\n",
      "  if (",rodeoConst$genIdent$len["pros"]," .gt. 0) then\n",
      "    call derivs(time=x2, var=var, par=par, ",
        rodeoConst$genIdent$len["boxes"],"=",rodeoConst$genIdent$len["boxes"],
        ", dydt=dydt, pro=pro)\n",
      "  end if\n",
      "  end subroutine\n"
    )
    write(wrapper, file=srcFiles["wrapper"])
  } else {
    stop("no support for '",method,"' method")
  }

  # Create library
  wd <- getwd()
  setwd(tempdir())
  command <- "R"
  args <- paste0("CMD SHLIB ",paste(srcFiles, collapse=" "),
    " --preclean --clean -o ",private$steppers[[method]]$libFile)
  if (system2(command, args) != 0) {
    setwd(wd)
    stop("Failed to build shared library.")
  }
  invisible(file.remove(list.files(path=tempdir(), pattern=".+[.]mod$")))
  setwd(wd)

  # Load library
  dyn.load(private$steppers[[method]]$libFile)
  if (!is.loaded(private$steppers[[method]]$fncName,
    PACKAGE=private$steppers[[method]]$libName)) {
#    print(getLoadedDLLs())
    stop("failed to load fortran subroutine '",private$steppers[[method]]$fncName,
      "' (library '", private$steppers[[method]]$libName,"')")
  }

  # Register symbol
  private$steppers[[method]]$fncSymb <- getNativeSymbolInfo(
    private$steppers[[method]]$fncName, PACKAGE=private$steppers[[method]]$libName)

  invisible(NULL)
})

