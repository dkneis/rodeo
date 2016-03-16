#' Wrapping generated Fortran for use with deSolve and rootSolve
#'
#' Creates wrapper code to make \code{rodeo}-generated Fortran code compatible
#' with the numerical methods from packages \code{deSolve} or \code{rootSolve}.
#'
#' @param NLVL The desired number of spatial levels (boxes). Defaults to 1.
#' @param funcname Name of the generated function that computes derivatives.
#'
#' @return A character string holding generated Fortran code. Must be written to
#'   disk, e.g. using \code{write} prior to compilation. The generated code
#'   provides two subroutines.
#'   The first subroutine with the fixed name 'initmod' is to be passed to
#'   the \code{initfunc} argument of \code{deSolve::ode} or
#'   \code{rootSolve::steady}, for example.
#'   The second subroutine is to be passed to the \code{func} argument of the
#'   \code{deSolve} or \code{rootSolve} methods. The subroutines names is
#'   created by appending the suffix '_wrapper' to the value of \code{funcname}.
#'   Hence, if \code{funcname} is 'derivs' (default), the subroutine name
#'   passed to the solver must be 'derivs_wrapped'.
#'
#' @author David Kneis \email{david.kneis@@tu-dresden.de}
#'
#' @export

solverInterface= function (NLVL=1, funcname="derivs") {
  paste0(
  "
  ! Definition of the number of spatial levels
  module spatial_dimension
    implicit none
    integer, parameter:: NLVL=",NLVL,"
  end module

  ! Generic routine for parameter initialization
  subroutine initmod(extfun)
    use dimensions_and_indices   ! Module is provided by the generated code
    use spatial_dimension
    external extfun
    double precision, dimension(NPAR*NLVL):: par
    common /params/ par
    call extfun(NPAR*NLVL, par)
  end subroutine

  ! Generic wrapper around the generated code
  subroutine ",funcname,"_wrapped (neq, t, y, ydot, yout, ip)
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
    double precision, dimension(NPAR*NLVL):: par
    common /params/ par
    !Call to generated code
    call ",funcname,"(t, y, par, NLVL, ydot, yout)
  end subroutine
  "
  )
}

