module functions
  implicit none
  contains

  double precision function O2sat(t) 
    double precision, intent(in):: t
    O2sat= 14.652d0 - 0.41022d0*t + 7.991d-3*(t**2d0) - &
      7.7774d-5*(t**3d0)
  end function

  double precision function ka(u, d) 
    double precision, intent(in):: u, d
    ka= (0.728d0*sqrt(u) - 0.317d0*u + 0.0372d0*(u**2.d0)) / &
      d / 86400.d0
  end function

  double precision function monod(s, h) 
    double precision, intent(in):: s, h
    monod= s / (s + h)
  end function

  double precision function c_z_in(seconds) 
    double precision, intent(in):: seconds
    c_z_in= 0.1d0 * seconds/(7d0*86400d0 + seconds)
  end function

  double precision function c_do_in(seconds) 
    double precision, intent(in):: seconds
    c_do_in= 10.d0
  end function

end module

