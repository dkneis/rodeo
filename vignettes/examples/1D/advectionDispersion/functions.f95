module functions
  implicit none

  double precision, parameter:: ZERO= 0d0

  contains

  function cUp (time) result (r)
    double precision, intent(in):: time
    double precision:: r
    r= ZERO                               ! same as for analytic solution
  end function

  function cDn (time) result (r)
    double precision, intent(in):: time
    double precision:: r
    r= ZERO                               ! same as for analytic solution
  end function

end module

