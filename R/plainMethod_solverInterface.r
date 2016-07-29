#' Wrapping of Generated Fortran for Use with Solver Packages
#'
#' Creates wrapper code to make \code{rodeo}-generated Fortran code compatible
#' with the numerical methods from packages \code{\link[deSolve]{deSolve}} and
#' \code{\link[rootSolve]{rootSolve}}. Consider to use the
#' \code{\link{rodeo-class}} class method \code{\link{compile}} instead of this
#' low-level function. 
#'
#' @param sections The number of spatial sections. Defaults to 1.
#' @param funcname Name of the function for which a wrapper is to be created
#'   (rather than the name for the wrapper). It is assumed that a function with
#'   that name was (or will be) created by a call to the \code{\link{generate}}
#'   class method.
#' @param outname Name for the created wrapper function.
#'
#' @return A character string holding generated Fortran code. Must be written to
#'   disk, e.g. using \code{\link[base]{write}} prior to compilation. The
#'   generated code provides two subroutines.
#'   The first subroutine with the fixed name 'initmod' is to be passed to
#'   the \code{initfunc} argument of, e.g., method \code{\link[deSolve]{ode}}
#'   (from package \code{\link[deSolve]{deSolve-package}}) or
#'   \code{\link[rootSolve]{steady}} (from package
#'   \code{\link[rootSolve]{rootSolve-package}}).
#'   The second subroutine whose name is given by \code{outname} is to be passed
#'   to the \code{func} argument of the \code{\link[deSolve]{deSolve}} or
#'   \code{\link[rootSolve]{rootSolve}} methods.
#'
#' @author David Kneis \email{david.kneis@@tu-dresden.de}
#'
#' @seealso The \code{\link{rodeo-class}} method \code{\link{compile}} is a
#'   convenient high-level function that calls \code{\link{solverInterface}}
#'   internally.
#'
#' @export
#'
#' @examples
#' fortranCode= solverInterface(sections=3)
#' write(fortranCode, file="")

solverInterface= function (sections=1, funcname="derivs", outname="derivs_wrapped") {
  paste0("
!#################################################
!###  THIS IS A GENERATED FILE -- DO NOT EDIT  ###
!#################################################

! Definition of the number of spatial sections
module spatial_dimension
  implicit none
  integer, parameter:: ",rodeoConst$genIdent$len["secs"],"=",sections,"
end module

! Generic routine for parameter initialization
subroutine initmod(extfun)
  use dimensions_and_indices   ! Module is provided by the generated code
  use spatial_dimension
  external extfun
  double precision, dimension(",rodeoConst$genIdent$len['pars'],"*",rodeoConst$genIdent$len["secs"],"):: par
  common /params/ par
  call extfun(",rodeoConst$genIdent$len['pars'],"*",rodeoConst$genIdent$len["secs"],", par)
end subroutine

! Generic wrapper around the generated code
subroutine ",outname," (neq, t, y, ydot, yout, ip)
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
  double precision, dimension(",rodeoConst$genIdent$len['pars'],"*",rodeoConst$genIdent$len["secs"],"):: par
  common /params/ par
  !Call to generated code
  call ",funcname,"(t, y, par, ",rodeoConst$genIdent$len["secs"],", ydot, yout)
end subroutine
"
)
}

