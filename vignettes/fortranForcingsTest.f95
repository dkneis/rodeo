subroutine rexit(x)
  character(len=*),intent(in):: x
  write(*,*)x
  stop
end subroutine

program test
use functions

implicit none

write(*,*) temperature(0.d0)
write(*,*) temperature(5184000.d0)
write(*,*) temperature(2592000.d0)
write(*,*) temperature(31536000.d0)
write(*,*) temperature(11772000.d0)
write(*,*) temperature(18360000.d0)

end program

