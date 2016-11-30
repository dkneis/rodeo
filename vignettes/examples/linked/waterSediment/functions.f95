module functions
implicit none

contains

function q(time) result (res)
  double precision, intent(in):: time
  double precision:: res
  res= 86400d0  
end function

function xInf(time) result (res)
  double precision, intent(in):: time
  double precision:: res
  res= 10.0d0  
end function

function sInf(time) result (res)
  double precision, intent(in):: time
  double precision:: res
  res= 0.0d0  
end function

end module
