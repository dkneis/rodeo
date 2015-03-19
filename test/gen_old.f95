! THIS IS A GENERATED FILE -- DO NOT EDIT
subroutine reac(time, y, p, NLVL, dydt, pros)
  use functions
  implicit none
  ! Dimension constants
  integer, parameter:: NVAR=2
  integer, parameter:: NPAR=4
  integer, parameter:: NPRO=2
  ! Inputs
  double precision, intent(in):: time
  double precision, dimension(NVAR*NLVL), intent(in):: y
  double precision, dimension(NPAR*NLVL), intent(in):: p
  integer, intent(in):: NLVL
  ! Outputs
  double precision, dimension(NVAR*NLVL), intent(out):: dydt
  double precision, dimension(NPRO*NLVL), intent(out):: pros
  ! Local variables
  integer:: spatial_level, i
  ! Index constants for variables
  integer, parameter:: X=1
  integer, parameter:: Z=2
  ! Index constants for parameters
  integer, parameter:: kd=1
  integer, parameter:: ka=2
  integer, parameter:: s=3
  integer, parameter:: temp=4
  ! Index constants for processes
  integer, parameter:: decay=1
  integer, parameter:: aerat=2

  ! Set vector of process rates (all spatial levels)
  do spatial_level = 1, NLVL
    pros((/(i, i=spatial_level, ((NPRO-1)*NLVL+spatial_level), NLVL)/))= pros0D(spatial_level)
  end do
  
  ! Set vector of derivatives (all spatial levels)
  do spatial_level = 1, NLVL
    dydt((/(i, i=spatial_level, ((NVAR-1)*NLVL+spatial_level), NLVL)/))= dydt0D(spatial_level)
  end do
  
  contains  ! Internal functions follow
  
  ! Function to compute the process rates at a particular level
  function pros0D(i_)
  implicit none
  ! Inputs
  integer, intent(in):: i_
  ! Outputs
  double precision, dimension(NPRO):: pros0D
  ! Local
  integer, parameter:: l=1
  
  pros0D=(/&
    ! Process rate 'decay'
      p((kd-l)*NLVL+i_) * y((Z-l)*NLVL+i_)&
    ,&
    ! Process rate 'aerat'
      p((ka-l)*NLVL+i_) * (Xsat(p((temp-l)*NLVL+i_))-y((X-l)*NLVL+i_))&
  /)

  end function
  
  ! Function to compute the derivatives at a particular level
  function dydt0D(i_)
  implicit none
  ! Inputs
  integer, intent(in):: i_
  ! Outputs
  double precision, dimension(NVAR):: dydt0D
  ! Local
  integer, parameter:: l=1
  
  dydt0D= (/&
    ! Variable 'X'
 pros((decay-l)*NLVL+i_) * (-p((s-l)*NLVL+i_)) +  pros((aerat-l)*&
NLVL+i_) * (1d0)&
    ,&
    ! Variable 'Z'
 pros((decay-l)*NLVL+i_) * (-1d0)&
  /)

  end function
end subroutine

