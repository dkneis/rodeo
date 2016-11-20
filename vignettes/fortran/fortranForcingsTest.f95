
! auxiliary routines for testing outside R
subroutine rwarn(x)
  character(len=*),intent(in):: x
  write(*,*)x
end subroutine

subroutine rexit(x)
  character(len=*),intent(in):: x
  write(*,*)x
  stop
end subroutine

! test program
program test
use forcings  ! imports generated module with forcing functions

implicit none

integer:: i
double precision, dimension(5):: times= &
  dble((/ 1., 1.5, 2., 2.5, 3. /))

do i=1, size(times)
  write(*,*) times(i), temp(times(i)), humid(times(i))
end do
end program

