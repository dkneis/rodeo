#' wrapper_code
#' @description Produces Fortran 95 wrapper code with the required number of "boxes" / "levels" and saves it to a temp file. Returns the path to the temp file
#' 
#' @param NLVS number of levels / boxes required
#' @return a file path
#' @export wrapper_code
#' 

wrapper_code <- function(NLVL){
  
  file_auxCode= paste0(gsub("\\","/",tempfile(),fixed=TRUE), ".f95")
  
  write(gsub("NLVL_REPLACE", x=
               "
! Definition of the number of spatial levels
module spatial_dimension
             implicit none
             integer, parameter:: NLVL = NLVL_REPLACE
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
             call derivs(t, y, par, NLVL, ydot, yout)
             end subroutine
             
             ", replace = NLVL), file_auxCode)
  cat("Fortran wrapper code written to", file_auxCode)
  return(file_auxCode)
}


 