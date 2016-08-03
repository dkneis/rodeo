
rodeo$set("public", "initStepper",
  function(fileFun, method="rk5")
{

  # Required files to build the library
  srcFiles <- c(
    integrators= system.file('fortran/integrators.f95',package='rodeo'),
    funcs= normalizePath(fileFun),
    derivs= paste0(tempfile(),".f95"),
    wrapper= paste0(tempfile(),".f95")
  )
  
  # Register basic stepper info
  fl <- gsub("\\", "/", tempfile(pattern=method),, fixed=TRUE)
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
      "  ! Number of sections\n",
      "  integer, parameter:: NLVL=",private$sections,"\n",
      "\n",
      "  ! Generic interface of the generated derivatives method\n",
      "  interface\n",
      "    subroutine derivs(time, var, par, NLVL, dydt, pro)\n",
      "      use dimensions_and_indices\n",
      "      use functions\n",
      "      implicit none\n",
      "      ! Inputs\n",
      "      double precision, intent(in):: time\n",
      "      double precision, dimension(NVAR*NLVL), intent(in):: var\n",
      "      double precision, dimension(NPAR*NLVL), intent(in):: par\n",
      "      integer, intent(in):: NLVL\n",
      "      ! Outputs\n",
      "      double precision, dimension(NVAR*NLVL), intent(out):: dydt\n",
      "      double precision, dimension(NPRO*NLVL), intent(out):: pro\n",
      "   end subroutine\n",
      "  end interface\n",
      "\n",
      "  contains\n",
      "\n",
      "  ! Specific derivatives method to be passed to the solver\n",
      "  subroutine derivs_wrapped(t,u,par,dudt,error)\n",
      "  use dimensions_and_indices, only: NVAR, NPAR, NPRO\n",
      "  implicit none\n",
      "  ! in\n",
      "  double precision, intent(in):: t\n",
      "  double precision, dimension(:), intent(in):: u\n",
      "  double precision, dimension(:), intent(in):: par\n",
      "  ! out\n",
      "  logical, intent(out):: error\n",
      "  double precision, dimension(:), intent(out):: dudt\n",
      "  ! local\n",
      "  double precision, dimension(NPRO):: pro\n",
      "  ! call to generated method\n",
      "  call derivs(time=t, var=u, par=par, NLVL=NLVL, dydt=dudt, pro=pro)\n",
      "  end subroutine\n",
      "\n",
      "  end module\n",
      "\n",
      "  ! Method to call the rk5 solver\n",
      "  subroutine ",private$steppers[[method]]$fncName,"(error,var,x1,x2,eps,h1,hmin,nmax,par,pro)\n",
      "  use integrators, only: rk5\n",
      "  use dimensions_and_indices, only: NVAR, NPAR, NPRO\n",
      "  use wrapper\n",
      "  implicit none\n",
      "  ! in\n",
      "  integer, intent(in):: nmax\n",
      "  double precision, intent(in):: x1, x2, eps, h1, hmin\n",
      "  double precision, dimension(NPAR*NLVL), intent(in):: par\n",
      "  ! inout\n",
      "  double precision, dimension(NVAR*NLVL), intent(inout):: var\n",
      "  ! out\n",
      "  double precision, dimension(NPRO*NLVL), intent(out):: pro\n",
      "  logical, intent(out):: error\n",
      "  ! local\n",
      "  double precision, dimension(NVAR*NLVL):: dydt\n",
      "  ! call integrator\n",
      "  call rk5(error,var,x1,x2,eps,h1,hmin,nmax,par,derivs_wrapped)\n",
      "  ! additional call to get process rates at final state\n",
      "  if (NPRO .gt. 0) then\n",
      "    call derivs(time=x2, var=var, par=par, NLVL=NLVL, dydt=dydt, pro=pro)\n",
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
  command <- paste0("R CMD SHLIB ",paste(srcFiles, collapse=" "),
    " --preclean --clean -o ",private$steppers[[method]]$libFile)
  if (system(command) != 0) {
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
    stop("fortran subroutine '",private$steppers[[method]]$fncName,
      "' (library '", private$steppers[[method]]$libName,"') not loaded")
  }

  # Register symbol
  private$steppers[[method]]$fncSymb <- getNativeSymbolInfo(
    private$steppers[[method]]$fncName, PACKAGE=private$steppers[[method]]$libName)

  invisible(NULL)
})

