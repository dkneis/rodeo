module functions
  implicit none
  contains

  double precision function monod(c, h) 
    double precision, intent(in):: c, h
    monod= c / (c + h)
  end function

end module

