module functions
  implicit none
  contains

  function DOsat(t) result (r)
    double precision, intent(in):: t
    double precision:: r
    r= 14.652d0 - 0.41022d0*t + 7.991d-3*(t**2d0) - 7.7774d-5*(t**3d0)
  end function

end module
