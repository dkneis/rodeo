#' Fortran wrapper for deSolve methods
#'
#' Creates wrapper code to make generated Fortran code compatible with the
#' integrators from package \code{deSolve} or methods from \code{rootSolve}.
#'
#' @param NLVL The desired number of spatial levels (boxes). Defaults to 1.
#' @param funcname Name of the function to compute derivatives.
#'   Default is 'derivs'.
#'
#' @return A character string representing Fortran code. Must be written to
#'   disk, e.g. using \code{write} prior to compilation.
#'
#' @author David Kneis \email{david.kneis@@tu-dresden.de}
#'
#' @export

fortranWrapper_deSolve= function (NLVL=1, funcname="derivs") {
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
  subroutine derivs_wrapped (neq, t, y, ydot, yout, ip)
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

