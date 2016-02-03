!#################################################
!###  THIS IS A GENERATED FILE -- DO NOT EDIT  ###
!#################################################

module dimensions_and_indices
  implicit none
  integer, private:: i
  ! Dimension constants
  integer, parameter:: NVAR=3
  integer, parameter:: NPAR=8
  integer, parameter:: NPRO=4
  ! Constant index arrays (for OD case or 1st level, respectively)
  integer, dimension(NVAR), parameter:: ivar0D =(/(i, i=1, NVAR)/)
  integer, dimension(NPAR), parameter:: ipar0D =(/(i, i=1, NPAR)/)
  integer, dimension(NPRO), parameter:: ipro0D =(/(i, i=1, NPRO)/)
  ! Modifyable index arrays (to be adjusted for each spatial level)
  integer, dimension(NVAR), target:: ivar
  integer, dimension(NPAR), target:: ipar
  integer, dimension(NPRO), target:: ipro

  ! Lists of pointers to index arrays
  type t_var
    integer, pointer:: c_do => ivar(1)
    integer, pointer:: c_z => ivar(2)
    integer, pointer:: v => ivar(3)
  end type
  type t_par
    integer, pointer:: q_in => ipar(1)
    integer, pointer:: q_ex => ipar(2)
    integer, pointer:: kd => ipar(3)
    integer, pointer:: s_do_z => ipar(4)
    integer, pointer:: h_do => ipar(5)
    integer, pointer:: temp => ipar(6)
    integer, pointer:: wind => ipar(7)
    integer, pointer:: depth => ipar(8)
  end type
  type t_pro
    integer, pointer:: flow => ipro(1)
    integer, pointer:: flushing => ipro(2)
    integer, pointer:: decay => ipro(3)
    integer, pointer:: aeration => ipro(4)
  end type
  ! Instances of the above lists
  type (t_var):: v
  type (t_par):: p
  type (t_pro):: r
end module

subroutine derivs(time, var, par, NLVL, dydt, pro)
  use dimensions_and_indices
  use functions
  implicit none
  ! Inputs
  double precision, intent(in):: time
  double precision, dimension(NVAR*NLVL), intent(in):: var
  double precision, dimension(NPAR*NLVL), intent(in):: par
  integer, intent(in):: NLVL
  ! Outputs
  double precision, dimension(NVAR*NLVL), intent(out):: dydt
  double precision, dimension(NPRO*NLVL), intent(out):: pro
  ! Local variables
  integer:: level, i

  ! Set vector of process rates (all spatial levels)
  do level = 1, NLVL
    pro((/(i, i=level, ((NPRO-1)*NLVL+level), NLVL)/))= pro0D(level)
  end do

  ! Set vector of derivatives (all spatial levels)
  do level = 1, NLVL
    dydt((/(i, i=level, ((NVAR-1)*NLVL+level), NLVL)/))= dydt0D(level)
  end do

  contains  ! Internal functions follow

  ! Process rates at a particular level
  function pro0D(level)
    implicit none
    ! Inputs
    integer, intent(in):: level
    ! Outputs
    double precision, dimension(NPRO):: pro0D
    ! Update indices
    ivar= (ivar0D - 1) * NLVL + level
    ipar= (ipar0D - 1) * NLVL + level
    ! Set return vector
    pro0D=(/&
      ! Process rate 'flow'
      par(p%q_in) - par(p%q_ex)&
    ,&
      ! Process rate 'flushing'
      par(p%q_in) / var(v%v)&
    ,&
      ! Process rate 'decay'
      par(p%kd) * var(v%c_z) * monod(var(v%c_do), par(p%h_do))&
    ,&
      ! Process rate 'aeration'
      ka(par(p%wind),par(p%depth)) * (O2sat(par(p%temp)) - var(v%c_do))&
    /)
  end function

  ! Derivatives at a particular level
  function dydt0D(level)
    implicit none
    ! Inputs
    integer, intent(in):: level
    ! Outputs
    double precision, dimension(NVAR):: dydt0D
    ! Update indices
    ivar= (ivar0D - 1) * NLVL + level
    ipar= (ipar0D - 1) * NLVL + level
    ipro= (ipro0D - 1) * NLVL + level
    ! Set return vector
    dydt0D= (/&
      ! Variable 'c_do'
       pro(r%flushing) * (c_do_in(time) - var(v%c_do)) +  pro(r%decay) *&
 (-par(p%s_do_z)) +  pro(r%aeration) * (1d0)&
    ,&
      ! Variable 'c_z'
       pro(r%flushing) * (c_z_in(time) - var(v%c_z)) +  pro(r%decay) * (-&
1d0)&
    ,&
      ! Variable 'v'
       pro(r%flow) * (1d0)&
    /)
  end function
end subroutine

